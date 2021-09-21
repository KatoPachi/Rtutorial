



## データの可視化

前処理したデータを可視化して、罰金の効果を視覚的に確かめる。
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


トリートメント・実験期間ごとの遅れて迎えに来る両親の**数**の平均を表で示す。
この表のポイントは以下の通り。

- トリートメントグループの単純な前後比較だと、罰金は遅れて迎えに来る両親の数を8.44人増やした。
  - これは経時的なトレンドとして解釈することもできるので、罰金の因果効果として解釈することは難しい。
- 罰金導入期の二群の比較だと、罰金は遅れて迎えに来る両親の数を7.19人増やした。
  - この研究はRCTなので、罰金導入期の二群の比較でも、罰金の平均的な効果を得られる。
- DIDの考え方（後述）だと、罰金は遅れて迎えに来る両親の数を9.21人増やした。
  - DIDはトリートメントグループの前後比較をコントロールグループの前後比較で差し引く
  - コントロールグループの前後比較をトリートメントグループの経時的なトレンドとして仮定する（パラレルトレンド）



```r
dt %>%
  mutate(
    fine = factor(fine, labels = c("Control", "Treatment (introduce fine)")),
    period = factor(period, labels = c("before fine", "with fine", "postfine"))
  ) %>%
  modelsummary::datasummary(
    fine * late ~ period * (mean + se),
    data = .
  )
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
<tr>
<th style="empty-cells: hide;border-bottom:hidden;" colspan="2"></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">before fine</div></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">with fine</div></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">postfine</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> fine </th>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> se </th>
   <th style="text-align:right;"> mean  </th>
   <th style="text-align:right;"> se  </th>
   <th style="text-align:right;"> mean   </th>
   <th style="text-align:right;"> se   </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Control </td>
   <td style="text-align:left;"> late </td>
   <td style="text-align:right;"> 10.00 </td>
   <td style="text-align:right;"> 1.11 </td>
   <td style="text-align:right;"> 9.23 </td>
   <td style="text-align:right;"> 0.61 </td>
   <td style="text-align:right;"> 8.25 </td>
   <td style="text-align:right;"> 0.95 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Treatment (introduce fine) </td>
   <td style="text-align:left;"> late </td>
   <td style="text-align:right;"> 8.00 </td>
   <td style="text-align:right;"> 0.60 </td>
   <td style="text-align:right;"> 16.44 </td>
   <td style="text-align:right;"> 0.84 </td>
   <td style="text-align:right;"> 18.71 </td>
   <td style="text-align:right;"> 1.39 </td>
  </tr>
</tbody>
</table>


トリートメント・実験期間ごとの遅れて迎えに来る両親の**割合**の平均を表で示す。
この表のポイントは以下の通り。

- トリートメントグループの単純な前後比較だと、罰金は遅れて迎えに来る両親の割合を25%ポイント増やした。
- 罰金導入期の二群の比較だと、罰金は遅れて迎えに来る両親の割合を21%ポイント増やした。
- DIDの考え方だと、罰金は遅れて迎えに来る両親の割合を27%ポイント増やした。



```r
dt %>%
  mutate(
    fine = factor(fine, labels = c("Control", "Treatment (introduce fine)")),
    period = factor(period, labels = c("before fine", "with fine", "postfine"))
  ) %>%
  modelsummary::datasummary(
    fine * rate_late ~ period * (mean + se),
    data = .
  )
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
<tr>
<th style="empty-cells: hide;border-bottom:hidden;" colspan="2"></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">before fine</div></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">with fine</div></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">postfine</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> fine </th>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> se </th>
   <th style="text-align:right;"> mean  </th>
   <th style="text-align:right;"> se  </th>
   <th style="text-align:right;"> mean   </th>
   <th style="text-align:right;"> se   </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Control </td>
   <td style="text-align:left;"> rate_late </td>
   <td style="text-align:right;"> 0.30 </td>
   <td style="text-align:right;"> 0.03 </td>
   <td style="text-align:right;"> 0.28 </td>
   <td style="text-align:right;"> 0.02 </td>
   <td style="text-align:right;"> 0.25 </td>
   <td style="text-align:right;"> 0.03 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Treatment (introduce fine) </td>
   <td style="text-align:left;"> rate_late </td>
   <td style="text-align:right;"> 0.24 </td>
   <td style="text-align:right;"> 0.02 </td>
   <td style="text-align:right;"> 0.49 </td>
   <td style="text-align:right;"> 0.03 </td>
   <td style="text-align:right;"> 0.56 </td>
   <td style="text-align:right;"> 0.04 </td>
  </tr>
</tbody>
</table>


週単位で二群の遅れて迎えに来る両親の数の平均をプロットする。
このとき、罰金導入直前の第4週の平均値が1となるように調整した。
これは罰金導入前の二群のトレンド（DIDのパラレルトレンドの仮定の検証）を比較しやすい形にするためである。
一見すると、罰金導入前のトレンドは群間で異なっていて、DIDのパラレルトレンドが成立しているとは言い難い。



```r
dt %>%
  mutate(
    fine = factor(fine, labels = c("Control", "Treatment (introduce fine)"))
  ) %>%
  group_by(week, fine) %>%
  summarize(late_mu = mean(late)) %>%
  tidyr::pivot_wider(names_from = "week", values_from = "late_mu") %>%
  mutate(base = `4`) %>%
  dplyr::select(fine, base, everything()) %>%
  tidyr::pivot_longer(- (fine:base), values_to = "mu", names_to = "week") %>%
  mutate(mu = mu / base, week = as.numeric(week)) %>%
  ggplot(aes(x = week, y = mu, group = fine)) +
  geom_point(aes(shape = fine), size = 2) +
  geom_line() +
  geom_vline(aes(xintercept = 4.5), linetype = 3) +
  geom_vline(aes(xintercept = 16.5), linetype = 3) +
  labs(
    x = "Week", y = "Late arrivals (week 4 = 1)",
    shape = "Treatment Status"
  ) +
  scale_x_continuous(breaks = seq(1, 20, by = 1)) +
  ggtemp()
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)


同じ手続きを遅れて迎えに来る両親の割合で試した。
結果として、罰金導入前の二群のトレンドは視覚的に平行しているとは言い難いので、
DIDのパラレルトレンドの仮定が成立していないかもしれない。



```r
dt %>%
  mutate(
    fine = factor(fine, labels = c("Control", "Treatment (introduce fine)"))
  ) %>%
  group_by(week, fine) %>%
  summarize(rate_late_mu = mean(rate_late)) %>%
  tidyr::pivot_wider(names_from = "week", values_from = "rate_late_mu") %>%
  mutate(base = `4`) %>%
  dplyr::select(fine, base, everything()) %>%
  tidyr::pivot_longer(- (fine:base), values_to = "mu", names_to = "week") %>%
  mutate(mu = mu / base, week = as.numeric(week)) %>%
  ggplot(aes(x = week, y = mu, group = fine)) +
  geom_point(aes(shape = fine), size = 2) +
  geom_line() +
  geom_vline(aes(xintercept = 4.5), linetype = 3) +
  geom_vline(aes(xintercept = 16.5), linetype = 3) +
  labs(
    x = "Week", y = "Percentage of late arrivals (week 4 = 1)",
    shape = "Treatment Status"
  ) +
  scale_x_continuous(breaks = seq(1, 20, by = 1)) +
  ggtemp()
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)
