library(stringr)
library(jsonlite)
library(readxl)
library(dplyr)
spoiled <- NULL
spoiled <- c('16041','16042','16043')
HS_lookup <- data.frame(Symbol = c("A", "B", "C", "D", "E", "F", "G",
                                   "H", "I", "J"),
                        Replacement = c("MD", "RD", "ID", "Sp", "R",
                                        "Dr", "Dc", "F", "Sp", "-"),
                        Order = c(2, 1, 3, 7, 8, 5, 6, 4, 9, 10))

# Load the data
HS <- data.frame(readxl::read_excel('HighStakes_DataMerge.xlsx',
                                    sheet = 'Copy for Datamerge'))

# Script to find cards which have die symbols in the wrong order
HS_comp <- HS %>%
  select(Side1B, Side2B, Side3B, Side4B, Side5B, Side6B, Name) %>%
  filter(!is.na(Side1B))
i <- 1
ordered_HS <- lapply(1:6, FUN = function(i){
  tmp <- data.frame(Symbol = HS_comp[,i], Name = HS_comp$Name)
  tmp <- merge(tmp, HS_lookup %>% select(Symbol, Order),
        by.x = 'Symbol', by.y = 'Symbol', all.x = TRUE, all.y = FALSE) %>%
    arrange(Name) %>% select(Order)
  names(tmp) <- paste0('Order',i)
  tmp
}) %>% bind_cols()
wrong_order_cards <-
  (HS_comp %>% arrange(Name) %>% pull(Name))[lapply(1:5, FUN = function(i){
    which(ordered_HS[,i+1]<ordered_HS[,i])
    }) %>% unlist()]

# Work out the position numbers
HS$position <- str_extract(string = HS$Set.Number, pattern = '[0-9]+') %>% as.numeric()
HS$letter <- str_extract(string = HS$Set.Number, pattern = '[A-z]+')
HS <- HS %>% arrange(position, letter)
HS$Set.Number <- lapply(HS$Set.Number, FUN = function(sn){
  paste0('16',
         paste0(rep('0',3-nchar(as.character(str_extract(sn,'[0-9]+')))),
                collapse = ''),
         sn)
}) %>% unlist()
HS$position <- 1:length(HS$position)

# Now, start building that dataframe
new_cards <- data.frame(code = HS$Set.Number, position = HS$position)
new_cards$set_code <- 'HS'
new_cards$name <- str_to_title(HS$Name)
droid_codes <- new_cards$position[(new_cards$subtype1=='droid'|
                                     new_cards$subtype2=='droid')&
                     grepl('-',new_cards$name)]
droid_codes <- droid_codes[!is.na(droid_codes)]
new_cards$name[droid_codes] <- toupper(new_cards$name[droid_codes])
new_cards$name[droid_codes] <-
  str_replace_all(new_cards$name[droid_codes],'DROID','Droid')
new_cards$subtitle <- str_to_title(HS$Subtitle)
new_cards$type_code <- tolower(HS$Type)
new_cards$points <- NA
new_cards$BoFpoints <- NA
new_cards$cost <- NA

new_cards$affiliation_code <- tolower(HS$X.Faction)
new_cards$points[new_cards$type_code %in% c('character','plot')] <-
  HS$Points[new_cards$type_code %in% c('character','plot')]
new_cards$points[new_cards$type_code %in% c('character','plot') &
                   is.na(new_cards$points)] <- '0'
new_cards$cost[!(new_cards$type_code %in% c('character','plot','battlefield'))] <-
  HS$Cost[!(new_cards$type_code %in% c('character','plot','battlefield'))]
new_cards$health <- HS$Health

sideslookup <- data.frame(ARH = c('A', 'B', 'C', 'D', 'E',
                                  'F', 'G', 'H','I', 'J'),
                          SWD = c('MD','RD','ID','Sh','R',
                                  'Dr','Dc','F','Sp','-'))

diesides <- lapply(1:6, function(i){
  lapply(1:nrow(HS), FUN = function(j){
    value <- HS[[paste0('Side',i,'A')]][j]
    if(is.na(value)){
      value <- ''
    }
    symbol <- HS[[paste0('Side',i,'B')]][j]
    if(is.na(symbol)){
      symbol <- ''
    } else {
      symbol <- sideslookup$SWD[sideslookup$ARH==symbol]
    }
    if(paste0('Side',i,'C') %in% names(HS)){
      payvalue <- HS[[paste0('Side',i,'C')]][j]
      if(is.na(payvalue)){
        payvalue <- ''
      }
    } else {
      payvalue <- ''
    }
    sides <- paste0(value, symbol, payvalue)
    if(sides == '') sides <- NA
    sides
  }) %>% unlist()
})

for(i in 1:6){
  new_cards[[paste0('side',i)]] <- diesides[[i]]
}

new_cards$faction_code <- tolower(HS$X.Color)
new_cards$faction_code[new_cards$faction_code=='grey'] <- 'gray'
new_cards$text <- HS$Text
new_cards$is_unique <- ifelse(HS$X.Unique=='Yes',TRUE,FALSE)
new_cards$is_unique[is.na(new_cards$is_unique)] <- FALSE
new_cards$deck_limit <- lapply(1:nrow(new_cards), function(rown){
  if(new_cards$is_unique[rown]&
     (new_cards$type_code[rown]=='character')){
    dl <- 1
  } else if(!new_cards$is_unique[rown]&
            (new_cards$type_code[rown]=='character')){
    dl <-
      (30/(new_cards$points[rown] %>% gsub('/[0-9]+$','',.) %>% as.numeric())) %>%
      floor()
  } else if(new_cards$type_code[rown]=='plot'){
    dl <- 1
  } else {
    dl <- 2
  }
}) %>% unlist()

new_cards$has_die <- !is.na(new_cards$side1)
new_cards$has_errata <- FALSE
#new_cards$illustrator <- HS$illustrator
new_cards$illustrator <- 'ARH'
new_cards$rarity_code <- 'S'

#new_cards$ttscardid <- new_cards$ttscardid %>% as.character()
subtypes <- str_split(HS$Subtype,pattern = ' - ')
new_cards$subtype1 <- lapply(subtypes, function(st){tolower(st[1])}) %>% unlist()
new_cards$subtype2 <- lapply(subtypes, function(st){tolower(st[2])}) %>% unlist()
new_cards$subtype3 <- lapply(subtypes, function(st){tolower(st[3])}) %>% unlist()
new_cards$subtype4 <- lapply(subtypes, function(st){tolower(st[4])}) %>% unlist()
new_cards$subtype1[new_cards$type_code == 'battlefield'] <- NA
new_cards$reprint_of <- NA
new_cards$text <- lapply(new_cards$text, FUN = function(texti){
  str_replace_all(texti, '  ',' ')
}) %>% unlist()

parallel_die_df <-
  data.frame(parallel_die = as.character(c('01013','03015','03019',
                                           '03031','05015','07087',
                                           '07142','02057')),
             code = c('16010','16011','16026',
                      '16038','16094','16095',
                      '16101','16105'))
new_cards <- merge(new_cards, parallel_die_df, by.x = 'code', by.y = 'code', all.x = TRUE)
saveRDS(new_cards,'HS_update.rds')

###############################################

new_cards <- readRDS('HS_update.rds')
if(!is.null(spoiled)){
  new_cards <- new_cards %>% filter(code %in% spoiled)
}

replacements <- read.csv('replacements.csv')
cards <- lapply(1:nrow(new_cards), function(i){
  row <- new_cards[i,]
  # turn dice sides into a list
  if(!is.na(row$side1)){
    sides <- lapply(c(row$side1, row$side2, row$side3,
                      row$side4, row$side5, row$side6),
                    function(side){
                      gsub('^ ','',side)
                    })
  } else {
    sides <- NA
  }
  # turn subtypes into a list
  if(!is.na(row$subtype1)){
    subtypes <- list(row$subtype1, row$subtype2, row$subtype3)
    subtypes <- subtypes[!is.na(subtypes)]
  } else {
    subtypes <- NA
  }
  
  # turn remainder of columns into a list
  cols <- lapply(1:ncol(row), function(j){
    cell_value <- row[,j]
    gsub('^ ','',cell_value)
  })
  names(cols) <- names(row)
  
  # overwrite side1 and subtype1 with side and subtype lists
  names(cols)[names(cols)=='side1'] <- 'sides'
  names(cols)[names(cols)=='subtype1'] <- 'subtypes'
  cols$sides <- sides
  cols$subtypes <- subtypes
  # remove all the extra side and subtype items
  cols <- cols[!grepl('[0-9]',names(cols))]
  cols <- cols[!grepl('BoFpoints', names(cols))]
  cols <- cols[!is.na(cols)]
  cols <- cols[order(names(cols))]
  # return the finished list
  if('deck_limit' %in% names(cols)) if(!is.na(cols$deck_limit)) cols$deck_limit <- as.numeric(cols$deck_limit)
  if('health' %in% names(cols)) if(!is.na(cols$health)) cols$health <- as.numeric(cols$health)
  if('position' %in% names(cols)) if(!is.na(cols$position)) cols$position <- as.numeric(cols$position)
  if('cost' %in% names(cols)) if(!is.na(cols$cost)) cols$cost <- as.numeric(cols$cost)
  if('flavor' %in% names(cols)) if(cols$flavor == '') cols <- cols[!(names(cols)=='flavor')]
  if('subtitle' %in% names(cols)) if(cols$subtitle == '') cols <- cols[!(names(cols)=='subtitle')]
  if('text' %in% names(cols)){
    if(cols$text == ''){
      cols <- cols[!(names(cols)=='text')]
    } else {
      for(i in 1:nrow(replacements)){
        cols$text <- gsub(replacements$source[i],
                          replacements$replacement[i],
                          cols$text)
      }
    }
  }
  if('subtypes' %in% names(cols)) if(cols$subtypes[[1]] == '') cols <- cols[!(names(cols)=='subtypes')]
  cols
})

json_file <- jsonlite::toJSON(cards, auto_unbox = TRUE) %>% prettify()
write(gsub('<\\\\','<\\',json_file) %>%
        gsub('\\\\r','',.) %>% 
        gsub(' \\\\n','\\\\n',.) %>%
        gsub('\\\\n\\\\n','\\\\n',.) %>%
        gsub('\\\\n \\\\n','\\\\n',.) %>%
        gsub('’',"'",.) %>% gsub('–',"-",.) %>%
        gsub('"FALSE"','false',.) %>% gsub('"TRUE"','true',.),
      file = 'set/HS.json')
