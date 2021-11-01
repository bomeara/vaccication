library(targets)
setwd("/Users/bomeara/Documents/MyDocuments/GitClones/vaccication")
options(timeout=600) # let things download for at least ten minutes
options(download.file.method = "libcurl")
tar_make()
system("git commit -m'updated data' -a")
system('git push')

