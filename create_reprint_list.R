reprint_list <- c('General Grievous','Flank','Secret Facility')

swdb <- 'C:/Users/marlog/Documents/swdestinydb-json-data-ml/set'
library(rjson)
library(jsonlite)
library(dplyr)
library(pbapply)
files <- list.files(swdb,full.names = TRUE)
allcards <- pbapply::pblapply(files, FUN = function(file){
  cards <- rjson::fromJSON(file = file)
})
names(allcards) <- tools::file_path_sans_ext(basename(files))

lookup <- pbapply::pblapply(allcards, FUN = function(set){
  lookup <- lapply(set, FUN = function(card){
    data.frame(code = card$code,
               name = card$name,
               set = card$set_code)
  }) %>% bind_rows()
}) %>% bind_rows()
lookup <- lookup %>% arrange(code)
lookup$set_num <- substr(lookup$code,start = 1,stop = 2)

# Combine with BotF data
balance <- rjson::fromJSON(file = file.path(swdb,'../formats.json'))
inf <- balance[[3]]
arh <- balance[[4]]
balanceinf <- inf$data$balance
balancearh <- arh$data$balance

reprint_all <- lapply(reprint_list, FUN = function(cardname){
  tmp <- lookup %>% filter(name == cardname)
  tmp <- tmp %>% filter(as.numeric(set_num) == min(as.numeric(tmp$set_num)))
  myset <- allcards[names(allcards)==tmp$set]
  mycard <- lapply(myset[[1]], FUN = function(card){
    card$name == cardname
  }) %>% unlist()
  card <- myset[[1]][mycard][[1]]
  card$reprint_of <- card$code
  card$position <- which(cardname==reprint_list)
  newcode <- card$position %>% as.character()
  newcode <- paste0(paste0(rep('0',3-nchar(newcode)),collapse = ''),newcode)
  card$code <- paste0('17',newcode)
  card$set_code <- 'RP'
  if(card$reprint_of %in% names(balancearh)){
    card$points <- balancearh[card$reprint_of][[1]]
  } else if(card$reprint_of %in% names(balanceinf)){
    card$points <- balanceinf[card$reprint_of][[1]]
  }
  card <- card[order(names(card),decreasing=FALSE)]
  card
})

json_file <- jsonlite::toJSON(reprint_all, auto_unbox = TRUE) %>%
  jsonlite::prettify()
write(gsub('\\\\/','/',json_file),
      file = 'set/RP.json')

# Now move the images
img_fol <- 'C:\\Users\\marlog\\Documents\\swdimg\\swdestinydb-img\\en'
files_to_move <- pbapply::pblapply(reprint_all, FUN = function(card){
  set_num  <- substr(card$reprint_of,1,2)
  img_name <- paste0(card$reprint_of,'.jpg')
  file.copy(from = file.path(img_fol,set_num,img_name),
            to = file.path(img_fol,'104',paste0(card$code,'.jpg')))
})
