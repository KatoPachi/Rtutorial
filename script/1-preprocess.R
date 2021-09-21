#' # 2章　データの前処理

#' ## Example dataset: Gneezy and Rustichini (2000)
#'
#' ここで使用するデータセットは以下の研究論文に掲載されているデータをcsvに加工したものを使用
#'
#' - Gneezy, Uri, and Aldo Rustichini. 2000. “A FINE IS A PRICE.” Journal of Legal Studies 29 (1): 1-17. 
#'    - [[本文リンク](https://drive.google.com/file/d/1ywOEdZCKRP89_CSUWmg9vC2H9q3iiehx/view?usp=sharing)]

#' ## Gneezy and Rustichini (2000)
#'
#' ある行動に対する罰金の導入がその行動を抑制するという標準的な仮説をイスラエルの託児所でフィールド実験した研究
#'
#' イスラエルの託児所の特徴
#'
#' - イスラエルのハイファにある私営の託児所は午前7時半から午後4時まで開いていた。
#' - 託児所の月額はNIS1,400（イスラエルの通貨で、当時の対ドル為替レートで計算すると約380ドル）
#' - 両親が子供を遅れて迎えにくるとき、どのような措置が取られるかということは契約上明記されていなかった。
#' - 特に、この介入研究が行われる前は、遅れて迎えに来る両親に対して罰金を課していなかった。
#'
#' ## フィールド実験の概要
#'
#' 実験に協力した私営託児所10校は20週に渡って、遅れて迎えに来る両親の数を記録した。
#' 実験期間は以下の三期間に分けられる
#'
#' 1. week 1-4: すべての託児所で罰金を導入しなかった
#' 1. week 5-16: 無作為に抽出された託児所6校のみで罰金を導入
#'    - 罰金は10分以上の遅れが対象で、子供一人あたりNIS10が課された
#'    - 罰金の導入は託児所の掲示板で告知された（多くの両親はその掲示板を毎日確認する）
#' 1. week 17-20: すべての託児所で罰金を導入しなかった
#'

#' ## パッケージの読み込み
#'
#' 私はパッケージの読み込みを`xfun`パッケージにある`pkg_attach2()`関数を使用
#' - この関数はインストールしていないパッケージに対して、インストールをした上で読み込んでくる。結構便利。
#+
library(xfun)
xfun::pkg_attach2("tidyverse")

#' ## データの読み込み
#'
#' `tidyverse`にある`readr`というパッケージの`read_csv()`を使って、
#' raw data（前処理をしていない生データ）を読み込む。
#+
path <- "https://raw.githubusercontent.com/KatoPachi/Rtutorial/main/data/daycare_fine.csv"
df <- readr::read_csv(path)

#' ## データの確認
#+
head(df)

#' ## 変数の説明
#'
#' - `care_center`：託児所ラベル
#' - `treat`：罰金を導入した託児所(`treat`)かそうでない託児所(`control`)
#' - `child`：託児所が預かっている子供の数
#' - `week1:week20`：各週における遅れて迎えに来る両親の数

#' ## データをwide型からlong型へ
#'
#' `pivot_longer()`でデータの行単位（unit of observation・観測単位）を
#' 「託児所」レベルから「託児所×週」レベルに変更する
#+
df_fix <- df %>%
  pivot_longer(
    week1:week20,
    names_to = "week", values_to = "late_parent",
    names_prefix = "week")

#' ## 変数の作成(1)
#'
#' - `pivot_longer()`の`names_to`の変数は文字列で入るので、`as.numeric()`で変数`week`を数値化する
#' - 変数`treat`は文字列で入っているので、`treat`ならば1を取るダミー変数に変更
#' - 遅れて来る親の割合を示す変数`p_late_parent`を新規に作成
#'    - すべての両親が一人の子供を預けていると仮定すると、両親の数は子供の数と同じになるはず
#+
df_fix <- df_fix %>%
  mutate(
    week = as.numeric(week),
    treat = recode(treat, "treat" = 1, "control" = 0),
    p_late_parent = late_parent / child
  )

#' ## 変数の作成(2)
#'
#' 実験期間を`week`は1-20週になっているが、それを三期間に分ける変数`period`を作成する
#+
df_fix <- df_fix %>%
  mutate(
    period = case_when(
      week <= 4 ~ 1,
      week <= 16 ~ 2,
      TRUE ~ 3
    )
  )

#' ## 前処理後のデータを確認
#+
head(df_fix)

#' ## 前処理したデータをcsvファイルで保存する
#'
#' `readr`パッケージの`write_csv()`関数を用いて、データフレーム`df_fix`をcsvで保存する
#+ eval = FALSE
readr::write_csv(df_fix, file = "data/daycare_fine_shape.csv")