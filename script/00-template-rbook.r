
#------------------------------# 
#「私たちのR」練習用テンプレート
#------------------------------#

# tidyverseパッケージの読み込み

library(tidyverse)

# サンプルデータの読み込み関数を定義
# df <- read_csv_rbook("Ramen.csv")

read_csv_rbook <- function(file, ...) {

  pass <- paste("https://raw.githubusercontent.com/JaehyunSong/RBook/master/Data/", file, sep = "")

  if (require(readr)) {
    return(readr::read_csv(pass, ...))
  } else {
    stop("'readr'というパッケージがありません。")
  }
}

#------------------------------# 

