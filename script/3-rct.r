# /*
# knit
knitr::spin(
  "script/3-rct.r",
  knit = TRUE
)
# */

#+ include = FALSE
knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  echo = TRUE,
  cache = FALSE,
  include = TRUE,
  fig.width = 10
)

library(here)
knitr::opts_knit$set(
  root.dir = here::here()
)

options(
  knitr.kable.NA = " ",
  knitr.table.format = "html",
  modelsummary_stars_note = FALSE
)

#'
#' ## ランダム化比較試験による因果効果の推定
#'
#' この研究はランダム化比較試験（RCT）となっているので、標準的な線形回帰分析で因果効果を推定することができる。
#'
#' はじめに、`xfun::pkg_attach2()`を用いて、使用するパッケージをロードする。
#+
library(xfun)
xfun::pkg_attach2(c(
  "tidyverse", "rlang", "rlist",
  "fixest",
  "kableExtra", "flextable", "officer", "modelsummary"
))

#'
#' 次に、標準誤差を計算する関数と{ggplot2}のテンプレート関数を定義する。
#'
#+
se <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- na.omit(x)
  }
  sqrt(var(x) / length(x))
}

ggtemp <- function(flip = FALSE,
                   family = NULL,
                   size = list(
                     title = 13,
                     text = 9,
                     caption = 11
                   ),
                   legend_key_size = 1) {
  my_theme <- theme_minimal(base_family = family) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text = element_text(
        color = "black", size = size$text, family = family
      ),
      axis.title = element_text(size = size$title, family = family),
      axis.ticks.length = unit(0.25, "cm"),
      axis.ticks.x = element_line(),
      axis.ticks.y = element_line(),
      axis.line = element_line(),
      legend.text = element_text(size = size$text, family = family),
      legend.key.size = unit(legend_key_size, "cm"),
      legend.title = ggplot2::element_text(size = size$title),
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(size = size$caption)
    )

  if (flip) {
    my_theme <- my_theme +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line()
      )
  }

  return(my_theme)
}

#'
#' 前処理したデータのcsvファイルを`readr::read_csv()`で呼び出す。
#'
#+
dt <- readr::read_csv("data/daycare_fine_shape.csv")

#'
#' ### ランダム化比較試験は何を推定しているのか？
#'
#' 託児所$i$の第$t$週における遅れて迎えに来る両親の数を$y_{it}$とする。
#' $y_{it}$は以下のような線形モデルで決まるとする。
#'
#' $$ y_{it} = a_i + b_i d_{it} + \delta_t + v_{it} $$
#'
#' ここで、$a_i$と$\delta_t$はそれぞれ託児所固定効果と時間固定効果である。
#' 託児所固定効果は時間に対して不変な託児所$i$特有の効果であり、
#' 時間固定効果は託児所に共通のある週特有の効果である。
#' $d_{it}$は託児所$i$が第$t$週に罰金を導入していたら1を取るダミー変数である。
#' $v_{it}$は誤差項であり、二つの固定効果とダミー変数で捉えきれないすべての影響を含む。
#'
#' 我々は罰金導入時期にのみサンプルを限定する。
#' よって、変数$d_{it}$は時間に依存しない変数（すべての週で罰金を導入するかしないか）となるので、
#' $d_{it} = d_i$と書き直せる。
#' また、$d_i$は時間に依存しない変数なので、託児所固定効果も推定できない。
#'
#' ここで、罰金を導入しないときの託児所$i$における第$t$週の遅れて迎えに来る両親の数を$y_{it}(0)$とする。
#' 一方で、罰金を導入するときの託児所$i$における第$t$週の遅れて迎えに来る両親の数を$y_{it}(1)$とする。
#' このとき、それぞれのアウトカムは以下のように得られる。
#'
#' $$ y_{it}(0) = a_i + \delta_t + v_{it} $$
#' $$ y_{it}(1) = a_i + b_i + \delta_t + v_{it} $$
#'
#' よって、$b_i = y_{it}(1) - y_{it}(0)$となり、$b_i$は個別介入効果と呼ばれる。
#' しかしながら、我々は$y_{it}(1)$もしくは$y_{it}(0)$のどちらか一方しか観察できないので、
#' 個別介入効果を推定することができない（fundamental problem of policy evaluation）。
#'
#' そこで、個別介入効果$b_i$でなく、
#' すべての託児所の平均効果$b$の推定を考える。
#' これは平均介入効果（Average Treatment Effect, ATE）と呼ばれる。
#' そのために、線型モデルを以下のように書き換える
#'
#' $$ y_{it} = a_i + b d_{it} + \delta_t + (v_{it} + (b_i - b) d_{it}) $$
#'
#' このモデルにおいて、OLS推定量$b$は二群の平均値の差を識別する。
#'
#' $$
#' E(\hat{b})
#' = b +
#' \{E(v_{it} | d_i = 1) - E(v_{it} | d_i = 0)\} +
#' E(b_i - b | d_{it} = 1)
#' $$
#'
#' 第二項はselection biasであり、第三項はsoritng gainと呼ばれる。
#' sorting gainは個人がトリートメント効果をある程度予想して、
#' 実際にトリートメントを受けるかどうかを決めるときに、
#' 生じる問題である（essential heterogeneityとも呼ばれる）。
#'
#' 罰金の導入がランダムに決まるとき、selection biasとsorting gainは発生しない。
#' このようなRCTの設定を用いた回帰分析は平均因果効果を識別できる。
#'
#' 罰金を導入する権利がランダムに決まり、
#' 実際に導入するかどうかが託児所の意思決定によって決まるケースを考える。
#' このとき、selection biasはないが、sorting gainは生じる可能性がある。
#' このようなRCTの設定を用いた回帰分析は
#' $b + E(b_i - b | d_{it} = 1) = E(b_i | d_{it} = 1)$
#' を識別する。
#' これは罰金を導入した託児所の平均因果効果
#' （Average treatment effect on the treated, ATT）と呼ばれる。
#'
#' Gneezy and Rustichini (2002)のフィールド実験は
#' 罰金の導入の権利がランダムに決まるのではなく、
#' 実際の罰金の導入がランダムに決まる。
#' よって、この研究のRCTの設定を用いた回帰分析はATEを識別する。
#'
#' ### 因果効果を推定する
#'
#' {fixest}パッケージの`feols()`を用いて、
#' 固定効果モデル$ y_{it} = b_i d_i + \delta_t + v_{it} $を推定する。
#' 託児所固定効果は`care_center`で、時間固定効果は`week`である。
#'
#' 遅れて迎えに来る両親の数をアウトカム変数$y_{it}$とするとき、
#' 罰金の導入は平均的に遅れて迎えに来る両親の数を7.215人増やした。
#'
#+
mod <- list(
  "(1)" = late ~ fine,
  "(2)" = late ~ fine | week
)

mod %>%
  purrr::map(~ fixest::feols(
    ., data = subset(dt, period == 2)
  )) %>%
  modelsummary(
    gof_omit = "R2|R2 Within|AIC|BIC|Log",
    stars = c("*" = .1, "**" = .05, "***" = .01)
  )

#'
#' 遅れて迎えに来る両親の比率をアウトカム変数として用いて、同様の分析を行った。
#' 結果として、罰金の導入は遅れて迎えに来る両親の割合を21.6%ポイント増やした。
#'
#+
mod2 <- list(
  "(1)" = rate_late ~ fine,
  "(2)" = rate_late ~ fine | week
)

mod2 %>%
  purrr::map(~ fixest::feols(
    ., data = subset(dt, period == 2)
  )) %>%
  modelsummary(
    gof_omit = "R2|R2 Within|AIC|BIC|Log",
    stars = c("*" = .1, "**" = .05, "***" = .01)
  )
