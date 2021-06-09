
library(xfun)
xfun::pkg_attach2("tidyverse")

# read data from github
df <- readr::read_csv("https://raw.githubusercontent.com/KatoPachi/Rtutorial/main/data/daycare_fine.csv")

str(df)
head(df)

# from wide to long table
df_fix <- df %>% 
  pivot_longer(-(care_center:child), names_to = "week", values_to = "late_parent", names_prefix = "week") %>% 
  mutate(week = as.numeric(week))

# make some variables
df_fix <- df_fix %>% 
  mutate(
    treat = recode(treat, "treat" = 1, "control" = 0),
    period = case_when(
      week <= 4 ~ 1,
      week <= 16 ~ 2,
      TRUE ~ 3
    ),
    p_late_parent = late_parent/child
  )

# write shaped dataset
readr::write_csv(df_fix, file = "data/daycare_fine_shape.csv")