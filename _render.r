
#' RスクリプトをR markdownに変換する
#+
ezknitr::ezspin(
    "script/ch2-preprocess.r",
    out_dir = "Rmarkdown",
    keep_rmd = TRUE,
    keep_md = FALSE,
    keep_html = FALSE
)
