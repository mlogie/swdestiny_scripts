library(dplyr)
library(ggplot2)
allSWD <- read.csv('allSWD.csv')
allSWD$text
allSWD$text_raw <-
  stringr::str_replace_all(string = allSWD$text,pattern = '<[a-z]+>','') %>%
  stringr::str_replace_all('</[a-z]+>','') %>%
  stringr::str_replace_all('\\[[A-z]+\\]','X') %>%
  stringr::str_replace_all('\\n',' ') %>%
  stringr::str_replace_all('\\([A-z \'\\\\./,0-9\\+\\-]+\\)','')

allSWD$textchar <- nchar(allSWD$text_raw)
allSWD$setnum <- substr(allSWD$code,1,2) %>% as.numeric()
allSWD$textchar[is.na(allSWD$textchar)] <- 0
allSWD <- allSWD %>% filter(!(set_code %in% c('TPG','RIV','AoN','TPU', 'TR', 'EC')))
allSWDtop50 <- lapply(unique(allSWD$set_code), function(setcd){
  tmp <- allSWD %>% filter(set_code == setcd)
  tmp <- arrange(tmp,-textchar)
  tmp$rank <- 1:nrow(tmp)
  tmp <- tmp[tmp$rank<=50,]
}) %>% bind_rows()

wordstop50 <- allSWDtop50 %>% group_by(setnum, set_code) %>%
  summarise(n = n(), averagechar = mean(textchar)) %>%
  data.frame()
wordstop50$set_code <- factor(wordstop50$set_code, levels = wordstop50$set_code)
ggplot(wordstop50 %>% arrange(setnum), aes(x = set_code, y = averagechar, group = 1)) +
  geom_path(colour = '#2F4A34', size = 1.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.background = element_rect(fill = '#B5E0BE')) +
  xlab('Set') + ylab('Average Number of Letters (top50)')
ggsave('char_length3.png')
words <- allSWD %>% group_by(setnum, set_code) %>%
  summarise(n = n(), averagechar = mean(textchar)) %>%
  data.frame()
words$set_code <- factor(words$set_code, levels = words$set_code)
ggplot(words %>% arrange(setnum), aes(x = set_code, y = averagechar, group = 1)) +
  geom_path(colour = '#2F4A34', size = 1.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.background = element_rect(fill = '#B5E0BE')) +
  xlab('Set') + ylab('Average Number of Letters') +
  coord_cartesian(ylim = c(75,175))
ggsave('char_length2.png')
words
names(allSWD)
lapply(words$set_code, FUN = function(set){
  num_char <- words %>% filter(set_code == set) %>% pull(averagechar)
  names <- allSWD %>% filter(set_code == set) %>%
    filter(textchar > (floor(num_char) - 1)) %>%
    filter(textchar < (ceiling(num_char) + 1)) %>%
    pull(name)
  names2 <- allSWD %>% filter(set_code == set) %>%
    filter(textchar > (floor(num_char) - 2)) %>%
    filter(textchar < (ceiling(num_char) + 2)) %>%
    pull(name)
  names3 <- allSWD %>% filter(set_code == set) %>%
    filter(textchar > (floor(num_char) - 3)) %>%
    filter(textchar < (ceiling(num_char) + 3)) %>%
    pull(name)
  names4 <- allSWD %>% filter(set_code == set) %>%
    filter(textchar > (floor(num_char) - 4)) %>%
    filter(textchar < (ceiling(num_char) + 4)) %>%
    pull(name)
  rep_set <- c(rep(0,length(names)),rep(1,length(names2)),
               rep(2,length(names3)),rep(3,length(names4)))
  data.frame(names = c(names,names2,names3,names4),
             match = rep_set,
             set = set)
})
names(allSWD)
allSWD$set_code_num <- substr(allSWD$code,1,2) %>% as.numeric()
az <- allSWD %>% group_by(set_code_num, set_code, type_code) %>% summarise(total = n()) %>%
  data.frame() %>% arrange(set_code_num)
az2 <- allSWD %>% group_by(set_code) %>% summarise(count = n()) %>% data.frame()
az <- merge(az, az2, by.x = 'set_code', by.y = 'set_code', all.x = TRUE)
az$percent <- round(100*az$total/az$count,1)
write.csv(az,'az.csv')

allSWD <- merge(allSWD, words %>% select(set_code, n))
allSWD$prop <- 1/allSWD$n
aff <- allSWD %>% group_by(setnum, set_code, affiliation_code, faction_code) %>%
  summarise(naf = n(), contrib = sum(prop)) %>% data.frame()
aff$affiliation_code <- factor(aff$affiliation_code, levels = c('villain','hero','neutral'))
aff$faction_code <- factor(aff$faction_code, levels = c('blue','red','yellow','gray'))
aff$set_code <- factor(aff$set_code, levels = aff %>% arrange(setnum) %>% pull(set_code) %>% unique())
tmp <- merge(data.frame(setnum = unique(aff$setnum), set_code = unique(aff$set_code)),
             data.frame(affiliation_code = unique(aff$affiliation_code))) %>%
  merge(data.frame(faction_code = unique(aff$faction_code)))
tmp$naf <- 0
tmp$contrib <- 0
tmp$all <- paste0(tmp$affiliation_code, tmp$set_code, tmp$faction_code)
aff$all <- paste0(aff$affiliation_code, aff$set_code, aff$faction_code)
aff <- bind_rows(aff, tmp[!(tmp$all %in% aff$all),])
aff$contrib[aff$affiliation_code=='hero'&aff$contrib!=0] <-
  aff$contrib[aff$affiliation_code=='hero'&aff$contrib!=0]-.0006
ggplot(aff,
       aes(x = set_code, y = contrib,
           colour=faction_code, linetype = affiliation_code,
           group=interaction(affiliation_code, faction_code))) +
  scale_color_manual(values = c('#2261a3','#9c000d','#cccc27','#6e6e6e')) +
  scale_linetype_manual(values = c(1,2,3)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.background = element_rect(fill = '#ededed')) + geom_point() + geom_line() +
  ylab('Proportion of Set')
ggsave('aff.png',width = 5, height = 5)

ggplot(aff %>% filter(aff$affiliation_code!='neutral'),
       aes(x = set_code, y = contrib,
           colour=faction_code, linetype = affiliation_code,
           group=interaction(affiliation_code, faction_code))) +
  scale_color_manual(values = c('#2261a3','#9c000d','#cccc27','#6e6e6e')) +
  scale_linetype_manual(values = c(1,2,3)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.background = element_rect(fill = '#ededed')) + geom_point() + geom_line() +
  ylab('Proportion of Set')
ggsave('aff_no_neutral.png',width = 5, height = 5)
allSWD$prop

aff2 <- allSWD %>% group_by(setnum, set_code, faction_code) %>%
  summarise(naf = n(), contrib = sum(prop)) %>% data.frame()
aff2$faction_code <- factor(aff2$faction_code, levels = c('blue','red','yellow','gray'))
aff2$set_code <- factor(aff2$set_code, levels = aff2 %>% arrange(setnum) %>% pull(set_code) %>% unique())
tmp <- merge(data.frame(setnum = unique(aff$setnum), set_code = unique(aff$set_code)),
             data.frame(faction_code = unique(aff$faction_code)))
tmp$naf <- 0
tmp$contrib <- 0
tmp$all <- paste0(tmp$affiliation_code, tmp$set_code, tmp$faction_code)
aff2$all <- paste0(aff2$affiliation_code, aff2$set_code, aff2$faction_code)
aff2 <- bind_rows(aff2, tmp[!(tmp$all %in% aff2$all),])
aff2$contrib[aff2$affiliation_code=='hero'&aff2$contrib!=0] <-
  aff2$contrib[aff2$affiliation_code=='hero'&aff2$contrib!=0]-.0006
aff2$totals <- 0
aff2$totals[1:24] <- aff2[1:24,] %>% group_by(faction_code) %>%
  mutate(cumsum = cumsum(naf)) %>% pull(cumsum)
aff2$totals2 <- 0
aff2$totals2[13:36] <- aff2[13:36,] %>% group_by(faction_code) %>%
  mutate(cumsum = cumsum(naf)) %>% pull(cumsum)
aff2$totals[25:44] <- aff2[25:44,] %>% group_by(faction_code) %>%
  mutate(cumsum = cumsum(naf)) %>% pull(cumsum)
aff2$totals[25:36] <- aff2$totals2[25:36]
aff2 <- aff2 %>% select(-totals2)
ggplot(aff2,
       aes(x = set_code, y = contrib,
           colour=faction_code,
           group=interaction(faction_code))) +
  scale_color_manual(values = c('#2261a3','#9c000d','#cccc27','#6e6e6e')) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.background = element_rect(fill = '#ededed')) + geom_point() + geom_line() +
  ylab('Proportion of Set') + labs(colour="Faction") + xlab('Set')
ggsave('aff2.png',width = 5, height = 3)
ggplot(aff2,
       aes(x = set_code, y = totals,
           colour=faction_code,
           group=interaction(faction_code))) +
  scale_color_manual(values = c('#2261a3','#9c000d','#cccc27','#6e6e6e')) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.background = element_rect(fill = '#ededed')) + geom_point() + geom_line() +
  ylab('Number of Cards in Standard') + labs(fill="Faction") + xlab('Set')
ggsave('aff_cum2.png',width = 5, height = 5)

aff3 <- allSWD %>% group_by(setnum, set_code, affiliation_code) %>%
  summarise(naf = n(), contrib = sum(prop)) %>% data.frame()
aff3$affiliation_code <- factor(aff3$affiliation_code, levels = c('villain','hero','neutral'))
aff3$set_code <- factor(aff3$set_code, levels = aff3 %>% arrange(setnum) %>% pull(set_code) %>% unique())
tmp <- merge(data.frame(setnum = unique(aff$setnum), set_code = unique(aff$set_code)),
             data.frame(affiliation_code = unique(aff$affiliation_code)))
tmp$naf <- 0
tmp$contrib <- 0
tmp$all <- paste0(tmp$affiliation_code, tmp$set_code, tmp$faction_code)
aff3$all <- paste0(aff3$affiliation_code, aff3$set_code, aff3$faction_code)
aff3 <- bind_rows(aff3, tmp[!(tmp$all %in% aff3$all),])
aff3$contrib[aff3$affiliation_code=='hero'&aff3$contrib!=0] <-
  aff3$contrib[aff3$affiliation_code=='hero'&aff3$contrib!=0]-.0006
ggplot(aff3,
       aes(x = set_code, y = contrib,
           colour=affiliation_code,
           group=interaction(affiliation_code))) +
  scale_color_manual(values = c('#000000','#FFFFFF','#6e6e6e')) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.background = element_rect(fill = '#ededed')) + geom_point() + geom_line() +
  ylab('Proportion of Set') + ylim(.2,.5)
ggsave('aff3.png',width = 5, height = 5)

farp <- read.csv('FA Release Party.csv')
farp
farp2 <- farp %>% group_by(Deck) %>% summarise(n = n()) %>% arrange(-n) %>%
  data.frame()
farp2$Deck[farp2$n==1] <- 'other'
othercount <- sum(farp2$Deck=='other')
farp2 <- farp2[!duplicated(farp2),]
farp2$n[farp2$Deck=='other'] <- othercount
farp2 <- farp2 %>% arrange(n)
farp2$Deck <- factor(farp2$Deck, levels = farp2$Deck)
ggplot(farp2) +
  geom_bar(aes(x = Deck, y = n), fill = 'dark green',
           alpha = .5, stat = 'identity') + coord_flip() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1.3,vjust = .4),
        axis.text.y = element_text(vjust = .4))
ggsave('release.png',width = 5, height = 5)
unique(farp$Deck)

farp$score <- farp$Record %>% substr(1,1) %>% as.numeric()
farp2
getnum <- function(i){
  lapply(1:i, function(k){
    sample(c(0:6),1,replace = TRUE,prob = c(1,6,15,20,15,6,1)/64)
  }) %>% unlist() %>% sum()
}
resu <- data.frame(n = pbapply::pblapply(1:10000, function(m){
  getnum(3)
}) %>% unlist()) %>% group_by(n) %>% summarise(sum = n()/10000) %>% data.frame()
quantiles <- lapply(unique(farp2$n), function(rett){
resu <- pbapply::pblapply(1:50000, function(m){
  getnum(rett)
}) %>% unlist() %>% quantile(c(.05,.25,.5,.75,.95)) %>% as.numeric()})
stats <- data.frame(n = unique(farp2$n),
           p05 = c(quantiles[[1]][1]/unique(farp2$n)[1],
                   quantiles[[2]][1]/unique(farp2$n)[2],
                   quantiles[[3]][1]/unique(farp2$n)[3],
                   quantiles[[4]][1]/unique(farp2$n)[4],
                   quantiles[[5]][1]/unique(farp2$n)[5],
                   quantiles[[6]][1]/unique(farp2$n)[6]),
           p25 = c(quantiles[[1]][2]/unique(farp2$n)[1],
                   quantiles[[2]][2]/unique(farp2$n)[2],
                   quantiles[[3]][2]/unique(farp2$n)[3],
                   quantiles[[4]][2]/unique(farp2$n)[4],
                   quantiles[[5]][2]/unique(farp2$n)[5],
                   quantiles[[6]][2]/unique(farp2$n)[6]),
           p50 = c(quantiles[[1]][3]/unique(farp2$n)[1],
                   quantiles[[2]][3]/unique(farp2$n)[2],
                   quantiles[[3]][3]/unique(farp2$n)[3],
                   quantiles[[4]][3]/unique(farp2$n)[4],
                   quantiles[[5]][3]/unique(farp2$n)[5],
                   quantiles[[6]][3]/unique(farp2$n)[6]),
           p75 = c(quantiles[[1]][4]/unique(farp2$n)[1],
                   quantiles[[2]][4]/unique(farp2$n)[2],
                   quantiles[[3]][4]/unique(farp2$n)[3],
                   quantiles[[4]][4]/unique(farp2$n)[4],
                   quantiles[[5]][4]/unique(farp2$n)[5],
                   quantiles[[6]][4]/unique(farp2$n)[6]),
           p95 = c(quantiles[[1]][5]/unique(farp2$n)[1],
                   quantiles[[2]][5]/unique(farp2$n)[2],
                   quantiles[[3]][5]/unique(farp2$n)[3],
                   quantiles[[4]][5]/unique(farp2$n)[4],
                   quantiles[[5]][5]/unique(farp2$n)[5],
                   quantiles[[6]][5]/unique(farp2$n)[6]))
stats
merge(farp2, stats)
dupdeck <- farp$Deck[duplicated(farp$Deck)]
farp3 <- farp
farp3$Deck[!(farp3$Deck %in% dupdeck)] <- 'other'
farp4 <- data.frame(Deck = unique(farp3$Deck),
           score = lapply(unique(farp3$Deck), function(deck){
             tmp <- farp3 %>% filter(Deck == deck)
             sum(tmp$score)/nrow(tmp)
           }) %>% unlist(),
           n = lapply(unique(farp3$Deck), function(deck){
             tmp <- farp3 %>% filter(Deck == deck)
             nrow(tmp)
           }) %>% unlist())
farp5 <- merge(farp4, stats)
str(farp5)
farp5 <- arrange(farp5, score)
farp5$Deck <- factor(farp5$Deck, levels = farp5$Deck)
ggplot(farp5[c(21:22),]) + geom_boxplot(aes(ymin = p05, lower = p25, middle = p50,
                                 upper = p75, ymax = p95, x = Deck),
                             stat = 'identity', fill = 'dark green', alpha = .5) +
  geom_point(aes(x = Deck, y = score), stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
ggsave('release_tournament.png',width = 5, height = 5)
farp[grepl('3PO',farp$Deck),]
farp5
library(ggplot2)
