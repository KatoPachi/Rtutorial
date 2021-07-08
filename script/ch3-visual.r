#' # 3章：{ggplot2}による可視化

# /* パッケージとデータの読み込み
library(xfun)
xfun::pkg_attach2("tidyverse")

# path <- "https://raw.githubusercontent.com/KatoPachi/Rtutorial/main/data/daycare_fine_shape.csv"
# df_fix <- readr::read_csv(path)
df_fix <- readr::read_csv("data/daycare_fine_shape.csv")
# */

# summary of late parent by week in treated day-care center
df_fix %>% 
  filter(treat == 1) %>% 
  group_by(week) %>% 
  summarize_at(
    vars(late_parent),
    list(
      mean =~mean(.),
      sd =~sd(.),
      min =~min(.),
      median =~median(.),
      max =~max(.)
    )
  )

# summary of control group by week in untreated day-care center
df_fix %>% 
  filter(treat == 0) %>% 
  group_by(week) %>% 
  summarize_at(
    vars(late_parent),
    list(
      mean =~mean(.),
      sd =~sd(.),
      min =~min(.),
      median =~median(.),
      max =~max(.)
    )
  )

# summary of late parent by period and day-care center
stat1 <- df_fix %>% 
  group_by(care_center, period) %>%
  summarize_at(vars(late_parent), list(mean =~mean(.)))

stat1 %>% 
  pivot_wider(values_from = "mean", names_from = "period") %>% 
  rename(period1 = "1", period2 = "2", period3 = "3")

# summary of late parent by period and treatment group
df_fix %>% 
  group_by(treat, period) %>% 
  summarize_at(
    vars(late_parent),
    list(
      mean =~mean(.),
      sd =~sd(.),
      min =~min(.),
      median =~median(.),
      max =~max(.)
    )
  )

# summary of ratio of late parent to childen by period and treatment group
df_fix %>% 
  group_by(treat, period) %>% 
  summarize_at(
    vars(p_late_parent),
    list(
      mean =~mean(.),
      sd =~sd(.),
      min =~min(.),
      median =~median(.),
      max =~max(.)
    )
  )

# drop some unused variables
df_fix2 <- df_fix %>% select(-child)

# tiem-series of average number of late parents per week
df_fix2 %>% 
  group_by(week, treat) %>% 
  summarize_at(vars(late_parent), list(mean=~mean(.))) %>%
  mutate(treat = factor(treat, levels = c(1, 0), labels = c("Group with fine", "Control group"))) %>%
  ggplot(aes(x = week, y = mean, group = treat)) +
    geom_point(aes(shape = treat), size = 3) +
    geom_line(aes(linetype = treat), size = 1) + 
    labs(x = "Week Number", y = "Average late arrivals") +
    ylim(c(0, 25)) + 
    theme_minimal() +
    theme(
      # setting: background
      plot.background = element_rect(color = "transparent"),
    
      # setting: plot
      panel.border = element_rect(color = "white", fill = NA),
      panel.background = element_rect(fill = "white"),   
      panel.grid = element_line(color = "grey80"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
    
      # setting: text
      plot.title = element_text(hjust=0.5,size=20),       
      plot.caption = element_text(size=11),       
    
      # setting: axis
      axis.text = element_text(color="black",size=13),    
      axis.title = element_text(size=13),
      axis.ticks.length = unit(0.25, "cm"),
      axis.ticks.x = element_line(),
      axis.ticks.y = element_line(),
      axis.line = element_line(),
    
      # setting: legend
      legend.title = element_blank(),               
      legend.text = element_text(size=12),                
      legend.key.size = unit(1,"cm"),
      legend.background = element_rect(color = "black"), 
      legend.position = "bottom"
    )

# my ggplot template
my_ggtemp <- theme_minimal() +
  theme(
    # setting: background
    plot.background = element_rect(color = "transparent"),
    
    # setting: plot
    panel.border = element_rect(color = "white", fill = NA),
    panel.background = element_rect(fill = "white"),   
    panel.grid = element_line(color = "grey80"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # setting: text
    plot.title = element_text(hjust=0.5,size=20),       
    plot.caption = element_text(size=11),       
    
    # setting: axis
    axis.text = element_text(color="black",size=13),    
    axis.title = element_text(size=13),
    axis.ticks.length = unit(0.25, "cm"),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.line = element_line(),
    
    # setting: legend
    legend.title = element_blank(),               
    legend.text = element_text(size=12),                
    legend.key.size = unit(1,"cm"),
    legend.background = element_rect(color = "black"), 
    legend.position = "bottom"
  )

# Time-series of the rate of late-coming parents per week by treatment group
df_fix2 %>% 
  group_by(week, treat) %>% 
  summarize_at(vars(p_late_parent), list(mean=~mean(.))) %>%
  mutate(treat = factor(treat, levels = c(1, 0), labels = c("Group with fine", "Control group"))) %>%
  ggplot(aes(x = week, y = mean, group = treat)) +
    geom_point(aes(shape = treat), size = 3) +
    geom_line(aes(linetype = treat), size = 1) + 
    labs(x = "Week Number", y = "Late arrivals rate") +
    ylim(c(0, 1)) +
    my_ggtemp
  

# Time-series of average late-coming parents per week by center
ggplot(df_fix2, aes(x = week, y = p_late_parent)) +
  geom_point(size = 3) +
  geom_line(size = 1) + 
  labs(x = "Week Number", y = "Late arrivals ratio") +
  ylim(c(0, 1.1)) +
  scale_x_continuous(breaks = seq(1,20,1)) +
  facet_wrap(~care_center) +
  my_ggtemp 
