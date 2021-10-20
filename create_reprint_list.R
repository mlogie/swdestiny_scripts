
library('dplyr')
#install.packages('Rcpp')
#setwd("swdestiny_scripts")
reprint_list <- readxl::read_xlsx('Reprints1and2.xlsx') %>% data.frame() %>%
  select(5:8)
names(reprint_list) <- c('Cards','Set','Type','Aff')
reprint_list <- reprint_list[!is.na(reprint_list$Cards),] %>% data.frame()
order <- c('Character','Downgrade','Event','Plot',
           'Support','Upgrade','Battlefield')
Affiliations <- c('Villain','Hero','Neutral')
cardtype <- 'Character'
affi <- 'Villain'
reprint_list <- lapply(order, FUN = function(cardtype){
  tmp <- reprint_list[reprint_list$Type==cardtype,]
  lapply(Affiliations, FUN = function(affi){
    temp <- tmp[tmp$Aff==affi,]
    arrange(temp, by = Cards)
  }) %>% bind_rows()
}) %>% bind_rows()
reprint_list$Set[is.na(reprint_list$Set)] <- ''

swdb <- 'set'
library(rjson)
library(jsonlite)
library(dplyr)
library(pbapply)
unlink('set/EoD.json')
files <- list.files(swdb,full.names = TRUE)
allcards <- pbapply::pblapply(files, FUN = function(file){
  cards <- rjson::fromJSON(file = file)
})
names(allcards) <- tools::file_path_sans_ext(basename(files))
lookup <- pbapply::pblapply(allcards, FUN = function(set){
  setname <- names(set)
  cat(setname)
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
i <- 28
lookup$name[grepl(' Vu',lookup$name)] <- 'Deja Vu'
reprint_all <- lapply(1:nrow(reprint_list), FUN = function(i){
  cardname <- reprint_list$Cards[i]
  cat(paste0(cardname,' ',i,'\n'))
  cardname <- stringr::str_replace_all(cardname,'’',"'")
  tmp <- lookup %>% filter(tolower(name) == tolower(cardname))
  if(reprint_list$Set[i]==''){
    tmp <- tmp %>% filter(as.numeric(set_num) == min(as.numeric(tmp$set_num)))
  } else {
    tmp <- tmp %>% filter(tolower(reprint_list$Set[i]) == tolower(tmp$set))
  }
  myset <- allcards[names(allcards)==tmp$set]

  mycard <- lapply(myset[[1]], FUN = function(card){
    if(cardname == 'Deja Vu'){
      ismatch <- grepl(' Vu', card$name)
    } else {
      ismatch <- (tolower(card$name) == tolower(cardname))
    }
    ismatch
  }) %>% unlist()
  card <- myset[[1]][mycard][[1]]
  card$reprint_of <- card$code
  card$position <- i
  newcode <- card$position %>% as.character()
  newcode <- paste0(paste0(rep('0',3-nchar(newcode)),collapse = ''),newcode)
  card$code <- paste0('17',newcode)
  card$set_code <- 'EoD'
  if(card$reprint_of %in% names(balancearh)){
    card$points <- balancearh[card$reprint_of][[1]]
  } else if(card$reprint_of %in% names(balanceinf)){
    card$points <- balanceinf[card$reprint_of][[1]]
  }
  if(!is.null(card$subtypes)){
    card$subtypes <- lapply(card$subtypes, FUN = function(subtype){
      subtype
    })
  }
  card <- card[order(names(card),decreasing=FALSE)]
  card
})

json_file <- jsonlite::toJSON(reprint_all, auto_unbox = TRUE) %>%
  jsonlite::prettify()
write(gsub('\\\\/','/',json_file),
      file = 'set/EoD.json')

# Now move the images
img_fol <- 'swdimg'
old_files <- list.files(file.path(img_fol,'104'), full.names = TRUE)
unlink(old_files)
files_to_move <- pbapply::pblapply(reprint_all, FUN = function(card){
  set_num  <- substr(card$reprint_of,1,2)
  img_name <- paste0(card$reprint_of,'.jpg')
  file.copy(from = file.path(img_fol,set_num,img_name),
            to = file.path(img_fol,'104',paste0(card$code,'.jpg')))
})
