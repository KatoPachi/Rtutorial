#' # 3章：{ggplot2}による可視化

# /* パッケージとデータの読み込み
library(xfun)
xfun::pkg_attach2("tidyverse")

# path <- "https://raw.githubusercontent.com/KatoPachi/Rtutorial/main/data/daycare_fine_shape.csv"
# df_fix <- readr::read_csv(path)
df_fix <- readr::read_csv("data/daycare_fine_shape.csv")
# */

#' ## 記述統計量の関数を定義
#'
#' トリートメント・実験三期間でグループ化した記述統計量を算出する関数を定義
#+
summary_y <- function(var, data) {
  tab <- data %>%
    group_by(treat, period) %>%
    summarize_at(
      vars({{ var }}),
      list(
        mean = ~ mean(.),
        sd = ~ sd(.),
        min = ~ min(.),
        median = ~ median(.),
        max = ~ max(.)
      )
    )
  return(tab)
}

#' ## 遅れて迎えに来た両親の数
#+
summary_y(late_parent, data = df_fix)

#' ## 遅れて迎えに来た両親の比率
#+
summary_y(p_late_parent, data = df_fix)

#' ## {ggplot2}テンプレートの作成
#+
my_theme <- theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text = element_text(color = "black", size = 13),
      axis.title = element_text(size = 13),
      axis.ticks.length = unit(0.25, "cm"),
      axis.ticks.x = element_line(),
      axis.ticks.y = element_line(),
      axis.line = element_line(),
      legend.text = element_text(size = 13),
      legend.key.size = unit(2, "cm"),
      legend.position = "bottom"
    )

#' ## 遅れて迎えに来た両親の数の可視化（コード）
#+
plot1 <- df_fix %>%
  group_by(week, treat) %>%
  summarize_at(vars(late_parent), list(mean = ~mean(.))) %>%
  mutate(treat = factor(
    treat,
    levels = c(1, 0),
    labels = c("Group with fine", "Control group")
  )) %>%
  ggplot(aes(x = week, y = mean, group = treat)) +
    geom_point(aes(shape = treat), size = 3) +
    geom_line(aes(linetype = treat), size = 1) + 
    labs(x = "Week Number", y = "Average late arrivals") +
    ylim(c(0, 25)) +
    my_theme

#' ## 遅れて迎えに来た両親の数の可視化（結果）
#+
plot1

#' ## 遅れて迎えに来た両親の比率の可視化（コード）
#+
plot2 <- df_fix %>%
  group_by(week, treat) %>%
  summarize_at(vars(p_late_parent), list(mean = ~mean(.))) %>%
  mutate(treat = factor(
    treat,
    levels = c(1, 0),
    labels = c("Group with fine", "Control group"))
  ) %>%
  ggplot(aes(x = week, y = mean, group = treat)) +
    geom_point(aes(shape = treat), size = 3) +
    geom_line(aes(linetype = treat), size = 1) + 
    labs(x = "Week Number", y = "Late arrivals rate") +
    ylim(c(0, 1)) +
    my_theme

#' ## 遅れて迎えに来た両親の比率の可視化（結果）
#+
plot2
