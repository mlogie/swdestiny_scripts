library(dplyr)
deckcosts <- read.csv('Deck Costs by Meta.csv')

block_costs <- lapply(unique(deckcosts$block), FUN = function(blocks){
  costs <- deckcosts %>% filter(block == blocks) %>% pull(totalresources)
  quantiles <- quantile(costs, c(.1,.25,.5,.75,.9)) %>% as.numeric()
  data.frame(Block = blocks,
             P10 = min(costs),
             P25 = quantiles[2],
             P50 = quantiles[3],
             P75 = quantiles[4],
             P90 = max(costs))
}) %>% bind_rows()
block_costs$Block <- factor(x = block_costs$Block, levels = block_costs$Block)
str(block_costs)
library(ggplot2)
ggplot(block_costs) +
  geom_boxplot(aes(x = Block, ymin = P10/30, lower = P25/30, middle = P50/30,
                   upper = P75/30, ymax = P90/30),
               stat = 'identity', fill = 'dark blue', alpha = .3) +
  ylab('Average Card Cost') + scale_y_continuous(breaks = seq(.7,1.8,.1)) +
  coord_cartesian(ylim = c(.7,1.8)) +
  theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())
ggsave('avCardCost.png')  

tmp <- deckcosts %>% filter(block == 'ARH')
block_costs_merchant <- lapply(unique(tmp$ResourceCheating), FUN = function(RC){
  costs <- tmp %>% filter(ResourceCheating == RC) %>% pull(totalresources)
  quantiles <- quantile(costs, c(.1,.25,.5,.75,.9)) %>% as.numeric()
  data.frame(Freighter = RC,
             P10 = min(costs),
             P25 = quantiles[2],
             P50 = quantiles[3],
             P75 = quantiles[4],
             P90 = max(costs))
}) %>% bind_rows()

ggplot(block_costs_merchant) +
  geom_boxplot(aes(x = Freighter, ymin = P10/30, lower = P25/30, middle = P50/30,
                   upper = P75/30, ymax = P90/30),
               stat = 'identity', fill = 'dark blue', alpha = .3) +
  ylab('Average Card Cost') + scale_y_continuous(breaks = seq(1.1,1.7,.1)) +
  xlab('') +
  coord_cartesian(ylim = c(1.1,1.7)) +
  theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())
ggsave('avCardCostMerchant.png')  

card_hash <- lapply(unique(deckcosts$block), FUN = function(blocks){
  costs <- deckcosts %>% filter(block == blocks)
  data.frame(Count = c(mean(costs$X0),
                       mean(costs$X1),
                       mean(costs$X2),
                       mean(costs$X3),
                       mean(costs$X4),
                       mean(costs$X5),
                       mean(costs$X6)),
             CardCost = c(0,1,2,3,4,5,6),
             Block = blocks)
}) %>% bind_rows()
card_hash$Block <- factor(card_hash$Block, levels = unique(card_hash$Block))

ggplot(card_hash %>% filter(CardCost != 6)) +
  geom_path(aes(y = Count, x = CardCost, colour = Block), size = 1) +
  scale_x_continuous(breaks = seq(0,5,1)) +
  coord_cartesian(xlim = c(0,5)) + xlab('Resource Cost') + ylab('') +
  scale_color_manual(values = c('Black','Grey','White','Blue')) +
  theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())
ggsave('avCostCurve.png')  

tmp <- deckcosts %>% filter(block == 'ARH')
block_costs_FARM <- lapply(unique(tmp$meta), FUN = function(metas){
  costs <- tmp %>% filter(meta == metas) %>% pull(totalresources)
  quantiles <- quantile(costs, c(.1,.25,.5,.75,.9)) %>% as.numeric()
  data.frame(Meta = metas,
             P10 = min(costs),
             P25 = quantiles[2],
             P50 = quantiles[3],
             P75 = quantiles[4],
             P90 = max(costs))
}) %>% bind_rows()

ggplot(block_costs_FARM) +
  geom_boxplot(aes(x = Meta, ymin = P10/30, lower = P25/30, middle = P50/30,
                   upper = P75/30, ymax = P90/30),
               stat = 'identity', fill = 'dark blue', alpha = .3) +
  ylab('Average Card Cost') + scale_y_continuous(breaks = seq(1.2,1.7,.1)) +
  xlab('') +
  coord_cartesian(ylim = c(1.15,1.7)) +
  theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())
ggsave('avCardCostFARM.png')  

card_hash_FARM <- lapply(c('FA','RM'), FUN = function(metas){
  costs <- deckcosts %>% filter(meta == metas)
  data.frame(Count = c(mean(costs$X0),
                       mean(costs$X1),
                       mean(costs$X2),
                       mean(costs$X3),
                       mean(costs$X4),
                       mean(costs$X5),
                       mean(costs$X6)),
             CardCost = c(0,1,2,3,4,5,6),
             Meta = metas)
}) %>% bind_rows()
card_hash_FARM$Block <- factor(card_hash_FARM$Block,
                               levels = unique(card_hash_FARM$Block))

ggplot(card_hash_FARM %>% filter(CardCost != 6)) +
  geom_path(aes(y = Count, x = CardCost, colour = Meta), size = 1) +
  scale_x_continuous(breaks = seq(0,5,1)) +
  coord_cartesian(xlim = c(0,5)) + xlab('Resource Cost') + ylab('') +
  scale_color_manual(values = c('Black','White')) +
  scale_y_continuous(breaks = seq(0,12,1)) +
  theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())
ggsave('avCostCurveFARM.png')
tmp
51/30

regex_lookups <- c('northern([_\\. ])?ireland|ni','united([_\\. ])?kingdom|uk')
grepl(regex_lookups, 'NI')
unlist(lapply(regex_lookups, grepl, tolower('NI')))
regex_match <- sapply(regex_lookups, grepl, tolower('NI'))
c(1,2)[regex_match]
grepl()