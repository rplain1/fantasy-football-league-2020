library(tidyverse)
library(ggrepel)
library(ggridges)
library(ggimage)
library(gganimate)
library(RColorBrewer)
df <- read_csv('scores.csv')

df2 <- df

df2

leauge <- left_join(df, df2, 
          by=c('Name'='Opponent',
               'Week'='Week')) %>% 
  rename(Score = Score.x,
         Opponent_Score = Score.y) %>% 
  select(-Name.y) %>% 
  group_by(Week) %>% 
  mutate(League_Median = median(Score)) %>% 
  ungroup() %>% 
  mutate(win = ifelse(Score > Opponent_Score & Week > 0, TRUE, FALSE),
         median_win = ifelse(Score > League_Median & Week > 0, TRUE, FALSE)) 
  group_by(Name) %>% 
  mutate(wins = sum(win) + sum(median_win),
            loss = sum(win == FALSE) + sum(median_win == FALSE)) %>% 
  arrange(-wins)


images <- read_csv("sleeper_images.csv")

leauge <- leauge %>% 
  left_join(., images, by="Name")
  
## Play with Viz ---------------------------
leauge %>% 
    filter(Week > 0) %>% 
    group_by(Name) %>% 
    mutate(med = median(Score)) %>% 
    ungroup() %>% 
    ggplot() +
    geom_boxplot(aes(x=reorder(Name, med), y=Score)) +
    geom_point(aes(
      x=Name, 
      y=Score, 
      color=ifelse(win==TRUE,"dark green","red")), 
      alpha=0.5)+
    scale_color_identity()+
    theme_minimal()+
    xlab(element_blank())
ggsave("boxplot.png", dpi=600)



leauge %>% 
  filter(Week > 0) %>% 
  ggplot(aes(x=Week, y=Score)) +
  geom_point(aes(color=ifelse(win == TRUE, "dark green", "red"))) +
  scale_color_identity() +
  geom_text_repel(aes(label=Name)) +
  annotate(
    "text", label = "Game of the",
    x = 6.7, y = 93, size = 5, colour = "red"
  ) +
  annotate(
    "text", label = "Year",
    x = 6.7, y = 89, size = 5, colour = "red"
  ) +
  annotate(
    "text", label="Sadness", colour = "red",
    x = 4.7, y = 160, size = 5
  ) +
  theme_minimal() 
ggsave("scores_by_week.png", dpi=600)


leauge %>% 
  filter(Week > 0) %>% 
  group_by(Name) %>% 
  summarize(wins = sum(win),
         median_wins = sum(median_win),
         total_wins = sum(win) + sum(median_win),
         total_score = sum(Score)) %>% #summarise(median(total_score), median(total_wins))
  mutate(
    xmid = median(total_wins),
    ymid = median(total_score),
    total_score = ifelse(Name == "Jacob", total_score + 10, total_score)) %>% 
  inner_join(., images, by="Name") %>% 
  ggplot(aes(x=total_score, y=total_wins, group=Name)) +
  geom_image(aes(x=total_score, y=total_wins,image=sleeper_url)) +
  geom_text_repel(aes(label=Name)) +
  geom_hline(yintercept = 9) +
  geom_vline(xintercept = 1173) +
  geom_point() +
  annotate("text", label="Good Teams", colour = "dark green",x = 1075, y = 11, size = 5) +
  annotate("text", label="Bad Teams", color="dark red", x=985, y=6, size=5) +
  annotate("text", label="Lucky AF!", color="blue", x=1000, y=10, size=5) +
  ylab("Wins") +
  xlab("Total Points Scored") +
  theme_minimal()
ggsave("scores_to_wins.png", dpi=600)

library(ggridges)
leauge %>% 
  filter(Week > 0) %>% 
  group_by(Name) %>% 
  mutate(med = mean(Score)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_density_ridges(aes(x=Score,y=reorder(Name,med),fill=Name),
                      quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x))+
  theme_minimal() +
  theme(legend.position= 'none') +
  ylab(element_blank())

ggsave("density.png", dpi=600)


leauge %>% 
  group_by(Name) %>% 
  mutate(med = median(Score),
         margin = Score - Opponent_Score,
         median_margin = Score - League_Median) %>% 
  summarise(win_margin = mean(margin),
            median_margin = mean(median_margin),
            wins=sum(win) + sum(median_win)) %>% 
  arrange(-wins) %>% 
  ggplot() +
  geom_col(aes(
    x=reorder(Name,win_margin), 
    y=win_margin, 
    fill=ifelse(win_margin > 0,"green","red"))) +
  scale_fill_identity() +
  geom_text(aes(
    x=reorder(Name, win_margin),
    y=win_margin,
    label=round(median_margin,1),
  )) +
  theme(legend.position = "none") +
  labs(title = "Avg Win Differential (Label is Avg Win Against the Median)",
       subtitle = "Claton has been Unlucky, I suck") +
  xlab(element_blank())+
  ylab("Win Margin")+
  theme_minimal()
ggsave("differentials.png",dpi=1000)  




p <- leauge %>% 
  group_by(Name) %>% 
  summarise(wins = sum(win) + sum(median_win),
            points = sum(Score)) %>%
  mutate(wins=wins+(points/1000000)) %>% 
  ggplot() +
  geom_col(aes(x=reorder(Name,wins),y=wins,fill=points)) +
  geom_text(aes(x=reorder(Name,wins),y=wins,label=round(points,1)),hjust=-0.1)+
  scale_y_continuous(breaks = seq(2,10,2))+
  coord_flip() +
  theme_minimal()+
  theme(legend.position = "bottom") +
  #scale_fill_viridis_c()+
  xlab(element_blank())
p


p <- leauge %>% 
  group_by(Name, Week) %>% 
  summarise(wins = sum(win) + sum(median_win),
            points = sum(Score)) %>% 
  arrange(Name, Week) %>% 
  mutate(total_wins = cumsum(wins),
         total_points = cumsum(points)) %>%
  mutate(total_wins1=total_wins+(total_points/1000000)) %>% 
  ungroup() %>% 
  group_by(Week) %>% 
  mutate(rank = min_rank(desc(total_wins1)) * 1.0) %>% 
  ungroup() %>% 
  left_join(., images, by="Name") %>% 
  ggplot(aes(rank, group=Name, fill=Name)) +
  geom_tile(aes(y=total_wins/2, height=total_wins), color="black", width=0.9, alpha=0.7)+
  #geom_col(aes(x=reorder(Name,total_wins1),y=total_wins1,fill=Name))+
  geom_text(aes(y=total_wins,label=team_name,hjust=1.2), size=5)+
  geom_image(aes(y=total_wins, image=sleeper_url)) +
  #scale_y_continuous(breaks = seq(2,18,2))+
  coord_cartesian(clip = "off", expand = FALSE) +
  coord_flip() +
  scale_x_reverse() +
  scale_fill_brewer(palette = "Spectral") +
  theme_classic() + 
  theme(legend.position = 'none', 
        plot.title = element_text(hjust = 1, size = 26),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  labs(title = 'Week {closest_state}', x="")+
  ylab("Wins") +
  transition_states(Week, transition_length = 4, state_length =8)+
  ease_aes('cubic-in-out') +
  enter_grow() +
  exit_fade()


p
animate(p, nframes = 200, fps = 10, width = 800, height = 800)
anim_save("filename.gif")


leauge %>% 
  group_by(Name) %>% 
  summarise(x = sum(win) + sum(median_win),
            pf = sum(Score)) %>%
  arrange(-x, -pf)




