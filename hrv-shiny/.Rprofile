source("renv/activate.R")
# 日本語コメント・日本語メッセージを含むファイルを確実に読み込むため、
# UTF-8ロケールを試行する（既にUTF-8系ロケールの場合は何もしない）。
local({
  cur <- tryCatch(Sys.getlocale("LC_CTYPE"), error = function(e) "")
  if (!grepl("UTF-8|UTF8|utf8", cur, ignore.case = TRUE)) {
    for (loc in c("en_US.UTF-8", "C.UTF-8", "C.utf8")) {
      ok <- tryCatch({
        Sys.setlocale("LC_ALL", loc)
        TRUE
      }, error = function(e) FALSE, warning = function(w) FALSE)
      if (isTRUE(ok)) break
    }
  }
})
