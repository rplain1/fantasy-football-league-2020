scores <- leauge %>% 
  filter(Week > 0) %>% 
  group_by(Name) %>% 
  #arrange(Score) %>% 
  #mutate(rn = row_number()) %>% 
  #filter(rn > 1 & rn < 10) %>% 
  summarise(avg_score = mean(Score),
         sd_score= sd(Score))
         #low_score = min(Score),
         #high_score = max(Score)) 

matchups <- read_csv("matchups.csv")

leauge_actual <- left_join(df, df2, 
                           by=c('Name'='Opponent',
                                'Week'='Week')) %>% 
  filter(Week > 0) %>% 
  rename(Score = Score.x,
         Opponent_Score = Score.y) %>% 
  select(-Name.y) %>% 
  group_by(Week) %>% 
  mutate(League_Median = median(Score)) %>% 
  ungroup() %>% 
  mutate(win = ifelse(Score > Opponent_Score & Week > 0, TRUE, FALSE),
         median_win = ifelse(Score > League_Median & Week > 0, TRUE, FALSE)) %>% 
  group_by(Name) %>% 
  summarise(wins = sum(win) + sum(median_win),
         loss = sum(win == FALSE) + sum(median_win == FALSE),
         pf = sum(Score), .groups='drop') %>% 
  arrange(-wins)

season <- matchups %>% 
  left_join(.,scores,by='Name') %>% 
  rename(p1_avg_score=avg_score,
         p1_sd_score=sd_score) %>% 
  left_join(., scores, by=c("Opponent"="Name")) %>%  
  rename(p2_avg_score=avg_score,
         p2_sd_score=sd_score)

single_season <- function() {
  #pb$tick()

  season_complete <- season %>% 
    group_by(Name, Opponent, Week) %>% 
    mutate(p1=rnorm(1, 117, 42),
           p2=rnorm(1, 117, 42)) %>% 
    mutate(p1_win = ifelse(p1 > p2, 1, 0)) %>% 
    pivot_longer(
      cols = c(p1,p2),
      names_to= "key",
      values_to = "value"
    ) %>% 
    group_by(Week) %>%
    mutate(median_score = median(value)) %>% 
    pivot_wider(id_cols = Name:p1_win,
                names_from = key,
                values_from = value:median_score) %>% 
    rename(median_score = median_score_p1,
           p1 = value_p1,
           p2 = value_p2) %>% 
    select(-median_score_p2) %>% 
    mutate(p1_median_win = ifelse(p1 > median_score, 1, 0),
           p2_median_win = ifelse(p2 > median_score, 1, 0)) %>% 
    select(-ends_with("score")) %>% 
    ungroup() %>% 
    mutate(p2_win = 1-p1_win) %>% 
    rename(p1_name = Name,
           p2_name = Opponent) 
  
  s1 <- season_complete %>% 
    select(starts_with("p1")) %>% 
    rename(Name = p1_name, Score = p1, median_win = p1_median_win, win = p1_win)
  
  s2 <- season_complete %>% 
    select(starts_with("p2")) %>% 
    rename(Name = p2_name, Score = p2, median_win = p2_median_win, win = p2_win)
  
  season_final <- bind_rows(s1, s2)
  
  season_final <- season_final %>% 
    group_by(Name) %>% 
    summarise(PF = sum(Score),
              Wins = sum(win) + sum(median_win), .groups='drop') %>% 
    left_join(leauge_actual, by=("Name")) %>% 
    mutate(Wins = Wins + wins, 
           PF = PF + pf) %>% 
    select(-wins, -loss, -pf) %>% 
    arrange(-Wins, -PF) %>% 
    mutate(rank = 1:10)
  
  return(season_final)

}
#library(progress)

num_sims <- 10000
#pb <- progress_bar$new(total=num_sims)
x <- seq_along(1:num_sims) %>% 
  map_df(~single_season())


x %>% 
  ggplot(group=Name) +
  geom_histogram(aes(rank,fill=Name)) +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal() +
  coord_flip() +
  scale_x_reverse() +
  geom_vline(xintercept = 6.5)

xx <- x %>% 
  mutate(playoffs = ifelse(rank <= 6, 1, 0),
         bye_week = ifelse(rank <= 2, 1, 0)) %>% 
  group_by(Name) %>% 
  summarise(playoffs = sum(playoffs)/num_sims,
            first_round_bye = sum(bye_week)/num_sims) %>% 
  mutate(playoffs = round(playoffs * 100),
         first_round_bye = round(first_round_bye* 100)) %>% 
  arrange(-playoffs) %>% 
  left_join(., images, by="Name")

xx %>% 
  ggplot() +
  #coord_polar(theta = "y") +
  geom_col(aes(x=reorder(Name, playoffs),y=playoffs, fill=Name), alpha=0.5, width=0.9) + 
  geom_col(aes(x=reorder(Name, playoffs), y=first_round_bye, fill=Name), alpha=0.5, width = 0.9) +
  geom_image(aes(x=reorder(Name, playoffs), y=playoffs, image=sleeper_url)) +
  geom_text(aes(x=reorder(Name, playoffs),y=playoffs, label=paste0(as.character(playoffs),'%')),hjust=1.5, size=6) +
  geom_text(aes(x=reorder(Name, playoffs), y=0, label=paste0("(",as.character(first_round_bye),'%)')),hjust=-0.1, size=5, data = . %>% filter(Name != "Ryan", Name != "Jacob", Name != "Zach")) +
  #geom_text(aes(x=reorder(Name, playoffs), 
                #y=0, 
                #label=glue::glue('{Name}: {playoffs}% ({first_round_bye}%)')),hjust=1.05) +
  coord_flip() +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal() +
  #geom_hline(yintercept = 103) +
  #annotate("text", label="I Suck at Fantasy! license plate", color="black", x=4, y=87.7, size=4) +
  #annotate("text", label="1 lbs of SugarFree Gummy Bears", color="black", x=3, y=86.2, size=4) +
  #annotate("text", label="Photo Shoot (?)", color="black", x=2, y=95, size=4) +
  #annotate("text", label="Beer Mile", color="black", x=1, y=98, size=4) +
  annotate("text", label="Gets his Chubb back", color="black", x=2, y=35, size=4) +
  annotate("text", label="Not gonna make it !", color="black", x=5, y=85, size=4) +
  annotate("text", label="Colluded his way to the top", color="black", x=8, y=50, size=4) +
  labs(title = "Playoff Sims", subtitle = "(First Round Bye)") + #, caption = "*I'm don't actually know what I'm doing but 60% of the time, it works every time") +
  ylab(element_blank()) +
  xlab(element_blank()) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 4.5) 

ggsave("playoffs.png", dpi=300)
ggsave("playoffs.png", height =5 ,width = 5, dpi=500, units="in")                      
