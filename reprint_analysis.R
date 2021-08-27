repvot <- readxl::read_xlsx('Reprint_List_Voting.xlsx')

library(dplyr)
eventsChoices <- lapply(repvot$`Events - Choose 40`, FUN = function(events){
  tmp <- stringr::str_split(events, ';')
  #tmp <- stringr::str_extract_all(events, '[ A-z\\!\\?\\-\\"\']+(?=;)')[[1]]
  #tmp2 <- stringr::str_extract_all(events, '(?<=;)[ A-z\\!\\?\\-\\"\']+')[[1]]
  #tmp <- unique(c(tmp, tmp2))
  tmp
})
#list_events <- allevents %>% unlist() %>% unique()
#write.csv(list_events, 'reprint_events.csv')
allevents <- read.csv('reprint_events.csv')
mitigation <- allevents$event[allevents$mitigation==1]
eventsChoices[6]
mitCount <- lapply(eventsChoices, FUN = function(choices){
  sum(choices %in% mitigation)/length(choices)
}) %>% unlist()
library(ggplot2)
ggplot(data = data.frame(Count = mitCount)) +
  geom_histogram(aes(x = Count), binwidth = .05, fill = 'blue', alpha = .5) +
  geom_vline(xintercept = 18/40) + xlab('Proportion of Mitigation Cards')
ggsave('mitcount.png')

reprints_events <- read.csv('reprints_events.csv')
mitCountIn <- lapply(eventsChoices, FUN = function(choices){
  sum((choices %in% mitigation)&(choices %in% reprints_events$event))/length(choices)
}) %>% unlist()
mitCountOut <- lapply(eventsChoices, FUN = function(choices){
  sum((choices %in% mitigation)&(!(choices %in% reprints_events$event)))/length(choices)
}) %>% unlist()
ggplot(data = data.frame(Count = mitCountIn)) +
  geom_histogram(aes(x = Count), binwidth = .05, fill = 'blue', alpha = .5) +
  xlab('Proportion of Mitigation Cards in List')
ggsave('mitcountIn.png')
ggplot(data = data.frame(Count = mitCountOut)) +
  geom_histogram(aes(x = Count), binwidth = .05, fill = 'blue', alpha = .5) +
  xlab('Proportion of Mitigation Cards not in List')
ggsave('mitcountOut.png')

list.files('C:/Users/marlog/Downloads',pattern = 'Reprint')
list.files('C:/Users/marlog/Downloads/Reprint List Voting.xlsx')
names(repvot)

i <- 1
allChoices <- lapply(1:ncol(repvot), FUN = function(i){
  cards <- repvot[,i]
  cardtype <- stringr::str_extract(names(cards), pattern = '[A-z]+(?= -)')
  #tmp <- stringr::str_split(cards[[1]], ';')
  eventsChoices <- lapply(1:nrow(cards), FUN = function(events){
    tmp <- stringr::str_split(as.vector(cards[events,]), ';')[[1]]
    data.frame(name = tmp, type = cardtype, user = events)
  }) %>% bind_rows()
  eventsChoices
}) %>% bind_rows()

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

choiceAllMatch <- pblapply(1:nrow(allChoices), FUN = function(choice){
  cardname <- allChoices$name[choice]
  if(!is.na(cardname)){
    if(grepl('Chirrut', cardname)) cardname <- 'Chirrut Îmwe'
    if(grepl('Vu', cardname)) cardname <- 'Déjà Vu'
    myset <- stringr::str_extract(cardname,
                                '(?<=\\()(AWK|SoR|EaW|LEG|2PG1|RIV|WoTF|WOTF|ATG)(?=\\))')
    if(!is.na(myset)){
      cardname <- stringr::str_extract(cardname,
                                       '[ A-z0-9\\!\\?\\-\\"\']+(?= \\()')
    }
    tmp <- lookup %>% filter(tolower(name) == tolower(cardname))
    if(!is.na(myset)){
      if(myset=='AWK') myset <- 'AW'
      if(myset=='2PG1') myset <- 'TPG'
      if(myset=='WoTF') myset <- 'WotF'
      if(myset=='WOTF') myset <- 'WotF'
      if(myset=='ATG') myset <- 'AtG'
      tmp <- tmp %>% filter(set == myset)
      
    } else {
      tmp <- tmp %>% filter(as.numeric(set_num) == min(as.numeric(tmp$set_num)))
    }
  } else {
    return(NULL)
  }
  data.frame(code = tmp$code,
             name = cardname,
             type = allChoices$type[choice],
             user = allChoices$user[choice],
             set = tmp$set,
             set_num = tmp$set_num)
})

choiceAllMatch <- choiceAllMatch %>% bind_rows()
groupedSet <- choiceAllMatch %>% group_by(set_num, set) %>% summarise(n = n())
groupedLookupSet <- lookup %>% group_by(set_num, set) %>% summarise(n = n())
groupedMergedSet <-
  merge(groupedSet, groupedLookupSet,
        by.x = 'set_num', by.y = 'set_num',
        all.x = TRUE, all.y = FALSE)
groupedMergedSet$proportion <- groupedMergedSet$n.x/groupedMergedSet$n.y
groupedMergedSet <- as.data.frame(groupedMergedSet)
groupedMergedSet$Set <- factor(groupedMergedSet$set.x,
                               levels = groupedMergedSet$set.x)
ggplot(data = groupedMergedSet) +
  geom_col(aes(x = Set, y = proportion), fill = 'blue', alpha = .5) +
  ylab('Relative Popularity')
ggsave('relative_set_popularity.png')
names(choiceAllMatch)

groupedName <- choiceAllMatch %>%
  group_by(name, set) %>% summarise(n = n()) %>% as.data.frame()
groupedName <- groupedName %>% arrange(n)

groupedCard <- choiceAllMatch %>%
  group_by(set_num, set, name) %>% summarise(n = n()) %>% as.data.frame()
groupedCard <- groupedCard %>% arrange(set_num, n)
groupedCard$PosName <- 1:nrow(groupedCard)
groupedCardNum <- lapply(groupedCard$set, FUN = function(typ){
  temp <- groupedCard %>% filter(set == typ)
  temp$pos <- temp$PosName
  temp$pos <- temp$pos - min(temp$pos)
  temp$pos <- temp$pos * 100/max(temp$pos)
  temp
}) %>% bind_rows()
groupedCardNum$Set <- groupedCardNum$set
ggplot(data = groupedCardNum) +
  geom_line(aes(x = pos, y = n, color = Set)) +
  scale_color_manual(values = c('black','blue','red','green','light blue',
                                'dark green','yellow','dark grey')) +
  xlab('Cards') + ylab('Number of Votes') + scale_y_log10() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggsave('popularity_curve_by_set_log.png')

groupedType <- choiceAllMatch %>%
  group_by(type, name) %>% summarise(n = n()) %>% as.data.frame()
groupedType <- groupedType %>% arrange(type, n)
groupedType$name <- factor(groupedType$name, levels = groupedType$name)
typ <- 'Events'
groupedTypeNum <- lapply(groupedType$type, FUN = function(typ){
  temp <- groupedType %>% filter(type == typ)
  temp$pos <- as.numeric(temp$name)
  temp$pos <- temp$pos - min(temp$pos)
  temp$pos <- temp$pos * 100/max(temp$pos)
  temp
}) %>% bind_rows()
groupedTypeNum$`Card Type` <- groupedTypeNum$type
ggplot(data = groupedTypeNum) +
  geom_line(aes(x = pos, y = n, color = `Card Type`)) +
  scale_color_manual(values = c('black','blue','red',
                                'dark green','yellow','grey')) +
  scale_y_log10() +
  xlab('Cards') + ylab('Number of Votes') +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggsave('popularity_curve_by_type_log.png')

lapply(unique(choiceAllMatch$type), FUN = function(typ){
  choiceAllMatch %>% filter(type == typ) %>% group_by(name, set) %>%
    summarise(n = n()) %>% data.frame() %>% arrange(n) %>% tail(20)
})

choiceAllMatch %>% filter(type == 'Characters') %>% group_by(name, set) %>%
  summarise(n = n()) %>% data.frame() %>% arrange(-n) %>% filter(name == 'Dryden Vos')

groupedUserSet <- choiceAllMatch %>%
  group_by(user, set, set_num) %>% summarise(n = n()) %>% as.data.frame()
tmplook <- lookup %>% group_by(set) %>% summarise(n = n()) %>% data.frame()
groupedUserSet <- merge(groupedUserSet, tmplook,
                        by.x = 'set', by.y = 'set',
                        all.x = TRUE, all.y = FALSE)
groupedUserSet <- groupedUserSet %>% arrange(user, set_num)
groupedUserSet$user <- factor(groupedUserSet$user,
                              levels = unique(groupedUserSet$user))
groupedUserSet$set <- factor(groupedUserSet$set,
                             levels = unique(groupedUserSet$set))
groupedUserSet$relpop <- groupedUserSet$n.x/groupedUserSet$n.y
groupedUserSet$set_num <- as.numeric(groupedUserSet$set_num)
groupedUserSet <- groupedUserSet[!(groupedUserSet$set %in% c('TPG','RIV')),]
groupedUserSet$set_num[groupedUserSet$set_num==5] <- 4
groupedUserSet$set_num[groupedUserSet$set_num==7] <- 5
groupedUserSet$set_num[groupedUserSet$set_num==8] <- 6
sett <- 'AW'
lapply(unique(groupedUserSet$set), FUN = function(sett){
  temp <- groupedUserSet %>% filter(set == sett)
  temp$mean <- mean(temp$relpop)
  temp$sd <- sd(temp$relpop)
  temp %>% filter(set, mean, sd)
}) %>% bind_rows()
ggplot(data = (groupedUserSet)) +
  geom_path(aes(x = set_num, y = relpop, colour = user)) +
  xlab('Sets') + ylab('Number of Votes') +
  theme(legend.position = 'none') + scale_x_discrete(limits = unique(groupedUserSet$set))
ggsave('popularity_curve_by_type_log.png')
