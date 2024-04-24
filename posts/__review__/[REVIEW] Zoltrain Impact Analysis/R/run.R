


x <- list.files("posts/Zoltrain Impact Analysis/R", full.names = TRUE)

for (fp in x) {
  rm(list = ls(pattern = "[^xfp]"))
  print(fp)
  source(fp)
}
