



## ランダム化比較試験による因果効果の推定

この研究はランダム化比較試験（RCT）となっているので、標準的な線形回帰分析で因果効果を推定することができる。

はじめに、`xfun::pkg_attach2()`を用いて、使用するパッケージをロードする。


```r
library(xfun)
xfun::pkg_attach2(c(
  "tidyverse", "rlang", "rlist",
  "kableExtra", "flextable", "officer", "modelsummary"
))
```


次に、標準誤差を計算する関数と{ggplot2}のテンプレート関数を定義する。



```r
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
```


前処理したデータのcsvファイルを`readr::read_csv()`で呼び出す。



```r
dt <- readr::read_csv("data/daycare_fine_shape.csv")
```


トリートメントグループ$g$に属する託児所$i$の第$t$週における遅れて迎えに来る両親の数を$y_{igt}$とする。
$y_i$は以下のような線形モデルで決まるとする。

$$ y_{igt} = a_i + b_i d_{gt} + \delta_t + v_{igt} $$

ここで、$a_i$と$\delta_t$はそれぞれ託児所固定効果と時間固定効果である。
託児所固定効果は時間に対して不変な託児所$i$特有の効果であり、
時間固定効果は託児所に共通のある週特有の効果である。
$d_{it}$は託児所$i$が第$t$週に罰金を導入していたら1を取るダミー変数である。
$v_{it}$は誤差項であり、二つの固定効果とダミー変数で捉えきれないすべての影響を含む。

我々は罰金導入時期にのみサンプルを限定する。
よって、変数$d_{gt}$は時間に依存しない変数（すべての週で罰金を導入するかしないか）となるので、
$d_{gt} = d_g$と書き直せる。

ここで、罰金を導入しないときの託児所$i$における第$t$週の遅れて迎えに来る両親の数を$y_{igt}(0)$とする。
一方で、罰金を導入するときの託児所$i$における第$t$週の遅れて迎えに来る両親の数を$y_{igt}(1)$とする。
このとき、それぞれのアウトカムは以下のように得られる。

$$ y_{igt}(0) = a_i + \delta_t + v_{igt} $$
$$ y_{igt}(1) = a_i + b_i + \delta_t + v_{igt} $$

よって、$b_i = y_{igt}(1) - y_{igt}(0)$となり、$b_i$は個別介入効果と呼ばれる。
しかしながら、我々は$y_{igt}(1)$もしくは$y_{igt}(0)$のどちらか一方しか観察できないので、
個別介入効果を推定することができない（fundamental problem of policy evaluation）。

しかしながら、我々は二群の平均値の差を推定することはできる。
すなわち、

$$
E(y_{igt}|d_{gt} = 1) - E(y_{igt}|d_{gt} = 0)
= E(y_{igt}(1)|d_{gt} = 1) - E(y_{igt}(0) | d_{gt} = 0)
$$

これを書き直すと、

$$
E(y_{igt}|d_{gt} = 1) - E(y_{igt}|d_{gt} = 0)
= E(b_i | d_{gt} = 1) + \{E(v_{igt} | d_{gt} = 1) - E(v_{igt} | d_{gt} = 0)\}
$$
