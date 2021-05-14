library(dplyr)
results <- read.csv('results.csv')
head(results)
first_names <- results[seq(3,156,by = 6),]
first_result <- results[seq(4,156,by = 6),]
second_names <- results[seq(5,156,by = 6),]
second_result <- results[seq(6,156,by = 6),]

dfs <- lapply(list(first_names,first_result,second_names,second_result),
              FUN = function(df){
  lapply(1:ncol(df), FUN = function(j){
    tmp <- df[,j]
    data.frame(name = tmp[tmp!=''],
               GO = (1+floor((j-1)/6)))
  }) %>% bind_rows()
}) %>% bind_cols()
dfs <- dfs[,c(1,3,5,7,8)]
dfs[,5][dfs[,5]==2] <- 3
dfs <- lapply(1:nrow(dfs), FUN = function(i){
  if(dfs[i,2]=='Win'){
    data.frame(winner = dfs[i,1],
               loser = dfs[i,3],
               GO = dfs[i,5])
  } else {
    data.frame(winner = dfs[i,3],
               loser = dfs[i,1],
               GO = dfs[i,5])
  }
}) %>% bind_rows()

dfs$winner <- tolower(dfs$winner)
dfs$loser <- tolower(dfs$loser)
dfs <- dfs %>% filter(loser != 'bye')
GO2 <- read.csv('GO2.csv', stringsAsFactors = FALSE)
GO2 <- data.frame(winner = GO2$winner[GO2$winner!=''],
                  loser = GO2$loser[GO2$loser!=''],
                  GO = 2)
dfs <- bind_rows(dfs,GO2)
nrow(dfs)

decklists <- read.csv('GO1.csv')
decklists$Player <- tolower(decklists$Player)

results_names <- unique(c(dfs$winner,dfs$loser))
decklists_names <- unique(decklists %>% pull(Player))
decklists_names[!(decklists_names %in% results_names)]
names(decklists)
dfs$winner_GO <- paste0(dfs$winner,'_',dfs$GO)
dfs$loser_GO <- paste0(dfs$loser,'_',dfs$GO)
decklists$player_GO <- paste0(decklists$Player,'_',decklists$GO)
dfs$game <- 1:nrow(dfs)
all_res <- bind_cols(
  merge(dfs %>% select(winner_GO,game),
      decklists %>% select(player_GO,Lineup,Archetype),
      by.x = 'winner_GO', by.y = 'player_GO', all.x = TRUE) %>% arrange(game),
  merge(dfs %>% select(loser_GO,game),
        decklists %>% select(player_GO,Lineup,Archetype),
        by.x = 'loser_GO', by.y = 'player_GO', all.x = TRUE) %>% arrange(game))
names(all_res) <- c('winner','game','win_lineup','win_archetype',
                    'loser','game_again','lose_lineup','lose_archetype')
all_res <- all_res %>% select(win_lineup,win_archetype,lose_lineup,lose_archetype)
nrow(all_res)
all_res <- all_res[(!is.na(all_res$lose_lineup)&!is.na(all_res$win_lineup)),]
all_archetypes <- c(all_res$win_archetype,all_res$lose_archetype)
all_archetypes <- names(table(all_archetypes))[(table(all_archetypes) %>% as.numeric())>10]
all_matchups <- lapply(all_archetypes, FUN = function(archetype){
  df_arch <- lapply(all_archetypes, FUN = function(archetype2){
    tmp <- all_res %>% filter((win_archetype == archetype&lose_archetype == archetype2)|
                                (win_archetype == archetype2&lose_archetype == archetype))
    if(nrow(tmp)==0){
      temp <- data.frame(winrate = NA,
                         games = 0,
                         archetype = archetype,
                         opponent = archetype2)
    } else if(archetype == archetype2){
      temp <- data.frame(winrate = NA,
                         games = nrow(tmp),
                         archetype = archetype,
                         opponent = archetype2)
    } else {
      wins <- tmp %>% filter(win_archetype == archetype) %>% nrow()
      losses <- tmp %>% filter(lose_archetype == archetype) %>% nrow()
      temp <- data.frame(
        winrate = wins/sum(wins,losses),
        games = sum(wins,losses),
        archetype = archetype,
        opponent = archetype2)
    }
    temp
  }) %>% bind_rows()
  wins <- all_res %>% filter(win_archetype == archetype) %>% nrow()
  losses <- all_res %>% filter(lose_archetype == archetype) %>% nrow()
  winrate <- round(wins/sum(wins,losses),2)
  games <- wins+losses
  bind_cols(df_arch,
            data.frame(total_winrate = rep(winrate,nrow(df_arch)),
                       total_games = rep(games,nrow(df_arch))))
}) %>% bind_rows()

library(ggplot2)
all_matchups
all_matchups$winrate <- 100*round(all_matchups$winrate,2)
ggplot(all_matchups) +
  geom_tile(aes(x = opponent, y = archetype, fill = winrate, alpha = games)) +
  scale_fill_continuous(low = 'red', high = 'green') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none") +
  scale_y_discrete(limits=rev) +
  geom_text(aes(x = opponent, y = archetype, label = winrate)) +
  xlab('Opponent') + ylab('Deck')
ggsave('GOmatch.png', width = 14, height = 14, units = 'cm')

all_matchups_sig <- all_matchups
all_matchups_sig$winrate[all_matchups_sig$games<4] <- NA
sig <- unique(all_matchups_sig$archetype)[
  lapply(unique(all_matchups_sig$archetype), FUN = function(arch){
    !all(is.na(all_matchups_sig$winrate[all_matchups_sig$archetype==arch]))
  }) %>% unlist()]
all_matchups_sig <- all_matchups_sig %>% filter(archetype %in% sig) %>%
  filter(opponent %in% sig)
  
ggplot(all_matchups_sig) +
  geom_tile(aes(x = opponent, y = archetype, fill = winrate, alpha = games)) +
  scale_fill_continuous(low = 'red', high = 'green') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none") +
  scale_y_discrete(limits=rev) +
  geom_text(aes(x = opponent, y = archetype, label = winrate)) +
  xlab('Opponent') + ylab('Deck')
ggsave('GOsig.png', width = 14, height = 14, units = 'cm')

decklists$Lineup <- stringr::str_replace_all(decklists$Lineup,'Any Means Necessary','AMN')
temp <- table(decklists$Lineup) %>% sort(decreasing = TRUE)
temp <- temp[temp>2]
temp <- temp %>% data.frame()
ggplot(temp) +
  geom_col(aes(x = Var1, y = Freq/137), fill = 'dark blue', alpha = .5) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
  ylab('% of decks') + xlab('') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
ggsave('GOpop.png', width = 8, height = 9, units = 'cm')
decklists_tmp <- names((table(decklists$Lineup) %>% sort())>2
sum(grepl('Dak',decklists$Lineup))
unique(decklists$Lineup)[grepl('Anakin',unique(decklists$Lineup))]
sum(grepl('Han',decklists$Lineup))

res_power <- all_res[all_res$win_lineup!=all_res$lose_lineup,]
num_to_do <- lapply(unique(c(res_power$win_lineup,res_power$lose_lineup)),
                    FUN = function(line){
                      tmp_win <- nrow(res_power[res_power$win_lineup==line,])
                      tmp_lose <- nrow(res_power[res_power$lose_lineup==line,])
                      sum(tmp_win,tmp_lose)
                    }) %>% unlist() %>% unique() %>% sort()
powers <- lapply(num_to_do, FUN = function(i){
  lapply(1:100000, FUN = function(j){
    sum(sample(c(0,1), i, replace = TRUE)==1)/i
  }) %>% unlist() %>% quantile(c(.05,.25,.5,.75,.95))
})
powrs <- lapply(1:length(powers), FUN = function(i){
  data.frame(Q5 = as.numeric(powers[[i]][1]), Q25 = as.numeric(powers[[i]][2]),
             Q50 = as.numeric(powers[[i]][3]), Q75 = as.numeric(powers[[i]][4]),
             Q95 = as.numeric(powers[[i]][5]), num_games = num_to_do[i],
             stringsAsFactors = FALSE)
}) %>% bind_rows()

success_df <- lapply(unique(c(res_power$win_lineup,res_power$lose_lineup)),
                    FUN = function(line){
                      tmp_win <- nrow(res_power[res_power$win_lineup==line,])
                      tmp_lose <- nrow(res_power[res_power$lose_lineup==line,])
                      data.frame(lineup = line,
                                 success = tmp_win/sum(tmp_win,tmp_lose),
                                 num_games = sum(tmp_win,tmp_lose),
                                 stringsAsFactors = FALSE)
}) %>% bind_rows()
success_df <-
  merge(success_df, powrs, by.x = 'num_games',
        by.y = 'num_games', all.x = TRUE)
success_df <- success_df %>% filter(num_games > 6)
success_df <- success_df %>% arrange(success)
success_df$lineup <-
  stringr::str_replace_all(success_df$lineup,'Any Means Necessary','AMN')
success_df$lineup <- factor(success_df$lineup,success_df$lineup)
success_df$Q50 <- .5
success_df$colour <- '#610c08'
success_df$colour[success_df$success>=success_df$Q5] <- 'red'
success_df$colour[success_df$success>=success_df$Q25] <- '#6670bd'
success_df$colour[success_df$success>success_df$Q75] <- '#0d9e31'
success_df$colour[success_df$success>success_df$Q95] <- '#054715'

trmp <- lapply(1:100000, FUN = function(i){
  sum(sample(c(0,1), 77, replace = TRUE)==1)/77
}) %>% unlist()
1/(1-ecdf(trmp)(0.6753247))

trmp <- lapply(1:100000, FUN = function(i){
  sum(sample(c(0,1), 69, replace = TRUE)==1)/69
}) %>% unlist()
1-ecdf(trmp)(0.5797101)


trmp <- lapply(1:100000, FUN = function(i){
  sum(sample(c(0,1), 23, replace = TRUE)==1)/23
}) %>% unlist()
1/(1-ecdf(trmp)(0.7391304))


trmp <- lapply(1:100000, FUN = function(i){
  sum(sample(c(0,1), 12, replace = TRUE)==1)/12
}) %>% unlist()
1/(1-ecdf(trmp)(0.8333333))
success_df

ggplot(success_df) +
  geom_boxplot(aes(ymin = Q5,
                   ymax = Q95,
                   middle = Q50,
                   lower = Q25,
                   upper = Q75,
                   x = lineup,
                   fill = colour), stat = 'identity', fill = success_df$colour, alpha = .5) +
  #scale_color_manual(values = success_df$colour) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_point(aes(x = lineup, y = success)) + ylab('Success Rate') + xlab('')
ggsave('GOpower.png', width = 16, height = 9, units = 'cm')

res_power_a <- all_res[all_res$win_archetype!=all_res$lose_archetype,]
num_to_do_a <- lapply(unique(c(res_power_a$win_archetype,res_power_a$lose_archetype)),
                    FUN = function(line){
                      tmp_win <- nrow(res_power_a[res_power_a$win_archetype==line,])
                      tmp_lose <- nrow(res_power_a[res_power_a$lose_archetype==line,])
                      sum(tmp_win,tmp_lose)
                    }) %>% unlist() %>% unique() %>% sort()
powers_a <- lapply(num_to_do_a, FUN = function(i){
  lapply(1:100000, FUN = function(j){
    sum(sample(c(0,1), i, replace = TRUE)==1)/i
  }) %>% unlist() %>% quantile(c(.05,.25,.5,.75,.95))
})
powrs_a <- lapply(1:length(powers_a), FUN = function(i){
  data.frame(Q5 = as.numeric(powers_a[[i]][1]), Q25 = as.numeric(powers_a[[i]][2]),
             Q50 = as.numeric(powers_a[[i]][3]), Q75 = as.numeric(powers_a[[i]][4]),
             Q95 = as.numeric(powers_a[[i]][5]), num_games = num_to_do_a[i],
             stringsAsFactors = FALSE)
}) %>% bind_rows()

success_df_a <- lapply(unique(c(res_power_a$win_archetype,res_power_a$lose_archetype)),
                     FUN = function(line){
                       tmp_win <- nrow(res_power_a[res_power_a$win_archetype==line,])
                       tmp_lose <- nrow(res_power_a[res_power_a$lose_archetype==line,])
                       data.frame(lineup = line,
                                  success = tmp_win/sum(tmp_win,tmp_lose),
                                  num_games = sum(tmp_win,tmp_lose),
                                  stringsAsFactors = FALSE)
                     }) %>% bind_rows()
success_df_a <-
  merge(success_df_a, powrs_a, by.x = 'num_games',
        by.y = 'num_games', all.x = TRUE)
success_df_a <- success_df_a %>% filter(num_games > 6)
success_df_a <- success_df_a %>% arrange(success)
success_df_a$Q50 <- .5
success_df_a$lineup <- factor(success_df_a$lineup,success_df_a$lineup)
success_df_a$colour <- '#610c08'
success_df_a$colour[success_df_a$success>=success_df_a$Q5] <- 'red'
success_df_a$colour[success_df_a$success>=success_df_a$Q25] <- '#6670bd'
success_df_a$colour[success_df_a$success>success_df_a$Q75] <- '#0d9e31'
success_df_a$colour[success_df_a$success>success_df_a$Q95] <- '#054715'

ggplot(success_df_a) +
  geom_boxplot(aes(ymin = Q5,
                   ymax = Q95,
                   middle = Q50,
                   lower = Q25,
                   upper = Q75,
                   x = lineup,
                   fill = colour), stat = 'identity',
               fill = success_df_a$colour, alpha = .5) +
  #scale_color_manual(values = success_df$colour) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_point(aes(x = lineup, y = success)) + ylab('Success Rate') + xlab('')
ggsave('GOpower_arch.png', width = 16, height = 9, units = 'cm')

trmp <- lapply(1:100000, FUN = function(i){
  sum(sample(c(0,1), 16, replace = TRUE)==1)/16
}) %>% unlist()
1/(1-ecdf(trmp)(0.8750000))

