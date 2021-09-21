


# 罰金は行動を抑制するのか

ここでは以下の研究論文に掲載されているデータをcsvに加工したものを使用する

- Gneezy, Uri, and Aldo Rustichini. 2000. “A FINE IS A PRICE.” Journal of Legal Studies 29 (1): 1-17. 
   - [[本文リンク](https://drive.google.com/file/d/1ywOEdZCKRP89_CSUWmg9vC2H9q3iiehx/view?usp=sharing)]

この研究は罰金の導入が罰金の対象となる行動を抑制するという標準的な仮説をイスラエルの託児所でフィールド実験した。
当時のイスラエルの託児所の特徴は以下の通り。

- イスラエルのハイファにある私営の託児所は午前7時半から午後4時まで開いていた。
- 託児所の月額はNIS1,400（イスラエルの通貨で、当時の対ドル為替レートで計算すると約380ドル）
- 両親が子供を遅れて迎えにくるとき、どのような措置が取られるかということは契約上明記されていなかった。
- 特に、この介入研究が行われる前は、遅れて迎えに来る両親に対して罰金を課していなかった。

この現状を踏まえて、この研究は遅れて迎えに来ることに対して少額の罰金を導入し、その効果を検証した。
具体的な実験内容については以下の通り

- 実験に協力した私営託児所10校は20週に渡って、遅れて迎えに来る両親の数を記録した
- 実験期間は以下の三期間に分けられる
    1. week 1-4: すべての託児所で罰金を導入しなかった
    1. week 5-16: 無作為に抽出された託児所6校のみで罰金を導入
        - 罰金は10分以上の遅れが対象で、子供一人あたりNIS10が課された
        - 罰金の導入は託児所の掲示板で告知された（多くの両親はその掲示板を毎日確認する）
    1. week 17-20: すべての託児所で罰金を導入しなかった

## パッケージとデータの読み込み

パッケージの読み込みは{xfun}パッケージにある`pkg_attach2()`関数を使用する。



```r
library(xfun)
xfun::pkg_attach2("tidyverse")
```


{tidyverse}にある{readr}パッケージの`read_csv()`を使って、前処理をしていない生データ（raw data）を読み込む。



```r
path <- "https://raw.githubusercontent.com/KatoPachi/Rtutorial/main/data/daycare_fine.csv"
df <- readr::read_csv(path)
head(df)
```

```
## # A tibble: 6 x 23
##   care_center treat child week1 week2 week3 week4 week5 week6 week7 week8 week9 week10 week11 week12 week13 week14 week15 week16 week17 week18 week19 week20
##   <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
## 1 A           treat    37     8     8     7     6     8     9     9    12    13     13     15     13     14     16     14     15     16     13     15     17
## 2 B           treat    35     6     7     3     5     2    11    14     9    16     12     10     14     14     16     12     17     14     10     14     15
## 3 C           treat    35     8     9     8     9     3     5    15    18    16     14     20     18     25     22     27     19     20     23     23     22
## 4 D           treat    34    10     3    14     9     6    24     8    22    22     19     25     18     23     22     24     17     15     23     25     18
## 5 E           treat    33    13    12     9    13    15    10    27    28    35     10     24     32     29     29     26     31     26     35     29     28
## 6 F           treat    28     5     8     7     5     5     9    12    14    19     17     14     13     10     15     14     16      6     12     17     13
```


生データにある変数の定義は以下の通り。

- `care_center`：託児所ラベル
- `treat`：罰金を導入した託児所(`treat`)かそうでない託児所(`control`)
- `child`：託児所が預かっている子供の数
- `week1:week20`：各週における遅れて迎えに来る両親の数

## 前処理１：wide型からlong型へ

生データの行単位（unit of observation・観測単位）は「託児所」レベルである。
この行単位のデータは分析の都合が悪いので、行単位を「託児所×週」レベルに変更する。
すなわち、一つの行は「第X週における託児所Y」のデータを格納する。
これは{tidyr}パッケージの`pivot_longer()`で実現できる。



```r
df_fix <- df %>%
  tidyr::pivot_longer(
    week1:week20,
    names_to = "week",
    values_to = "late",
    names_prefix = "week"
  )

head(df_fix)
```

```
## # A tibble: 6 x 5
##   care_center treat child week   late
##   <chr>       <chr> <dbl> <chr> <dbl>
## 1 A           treat    37 1         8
## 2 A           treat    37 2         8
## 3 A           treat    37 3         7
## 4 A           treat    37 4         6
## 5 A           treat    37 5         8
## 6 A           treat    37 6         9
```

## 前処理２：変数の作成

`pivot_longer()`の`names_to`で指定した列は文字列が入る。
今回の例ではweek列は第X週の情報を格納しているので、`as.numeric()`で変数`week`を数値化する。
さらに、20週の実験を3期間に大別する変数`period`を作成する。



```r
df_fix2 <- df_fix %>%
  mutate(
    week = as.numeric(week),
    period = case_when(
      week <= 4 ~ 1,
      week <= 16 ~ 2,
      TRUE ~ 3
    )
  )
  
class(df_fix$week)
```

```
## [1] "character"
```

```r
class(df_fix2$week)
```

```
## [1] "numeric"
```


罰金を導入した託児所ならば1を取るダミー変数`fine`を作成する。


```r
df_fix2 <- df_fix2 %>%
  mutate(fine = if_else(treat == "treat", 1, 0))
```


遅れて来る親の割合を示す変数`rate_late`を作成する。
すべての両親が一人の子供を預けていると仮定すると、両親の数は子供の数と同じになるはず。
したがって、それを仮定すると、遅れて迎えに来る両親の数を託児所の子供の数で割ることで、
遅れて迎えに来る両親の割合を計算できる。



```r
df_fix2 <- df_fix2 %>%
  mutate(rate_late = late / child)
```


最終的なデータセットは次のようになる



```r
head(df_fix2)
```

```
## # A tibble: 6 x 8
##   care_center treat child  week  late period  fine rate_late
##   <chr>       <chr> <dbl> <dbl> <dbl>  <dbl> <dbl>     <dbl>
## 1 A           treat    37     1     8      1     1     0.216
## 2 A           treat    37     2     8      1     1     0.216
## 3 A           treat    37     3     7      1     1     0.189
## 4 A           treat    37     4     6      1     1     0.162
## 5 A           treat    37     5     8      2     1     0.216
## 6 A           treat    37     6     9      2     1     0.243
```

## 前処理３：前処理後のデータをcsvファイルで保存

`readr`パッケージの`write_csv()`関数を用いて、データフレーム`df_fix2`をcsvで保存する


```r
readr::write_csv(df_fix2, file = "data/daycare_fine_shape.csv")
```

