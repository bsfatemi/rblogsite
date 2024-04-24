create_rdb_post <- function(title = NULL,
                            descr = NULL,
                            author = NULL,
                            tags = NULL,
                            open = TRUE) {



  if (!rstudioapi::isAvailable())
    stop("rstudio not detected", call. = FALSE)

  proj_home <- rstudioapi::getActiveProject()

  if (fs::path_file(proj_home) != "rblogsite")
    stop("rblogsite not set as active project")

  if (is.null(title))
    title <- rstudioapi::showPrompt("Title", "Title of new post:", "My New Blog Post")
  if (is.null(descr))
    descr <- rstudioapi::showPrompt("Description", "Describe objective or goal of post:", "Description of the blog post goes here.")
  if (is.null(author))
    author <- rstudioapi::showPrompt("Author", "Name of Post Author", "Bobby Fatemi")
  if (is.null(tags))
    tags <- rstudioapi::showPrompt("Tags", "Example: insights, cannabis", "TBD")

  ## if inputs are still null, then user cancelled the prompts, so exit
  if (is.null(title))
    stop("Title for post required", call. = FALSE)
  if (is.null(descr))
    stop("Description for post required", call. = FALSE)
  if (is.null(author))
    stop("Author for post required", call. = FALSE)
  if (is.null(tags))
    stop("Tag(s) for post required", call. = FALSE)

  # format tags
  tags <- stringr::str_flatten_comma(tags)

  ## Remove invalid chars for dir name
  pname <- stringr::str_replace_all(title, ":", "-")
  pname <- stringr::str_replace_all(pname, "&", "and")
  pname <- stringr::str_remove_all(pname, stringr::fixed(".,!?'#$%"))

  created <- lubridate::today()
  post_dir <- fs::path(proj_home, "posts/__drafts__", paste("[DRAFT]", pname))

  if (fs::dir_exists(fs::path(proj_home, "posts", pname)))
    stop("Published post by that name already exists")
  if (fs::dir_exists(post_dir))
    stop("Draft post by that name already exists")

  # create empty R script file
  r_script <- fs::file_create(fs::dir_create(post_dir), "analysis.R")
  txt <- stringr::str_glue("# Code to generate output(s) for post: {pname}\n#\n# Created on {created}\n#\n")
  cat(file = r_script, txt)

  # create empty data and image directories
  fs::path(post_dir, c("data", "images")) |>
    fs::dir_create(recurse = TRUE)

  img <- "images/generic-draft-thumb.png"
  fs::file_copy(fs::path(proj_home, img), fs::path(post_dir, img))

  # index.qmd file
  index_qmd <- fs::file_create(post_dir, "index.qmd")
  txt <- '---
  title: "{title}"
  author: "{author}"
  description: "{descr}"
  categories: [{tags}]
  image: images/generic-draft-thumb.png
  date-created: "{created}"
  date-modified: ""
  date: ""
  draft: true
  ---
  '
  cat(file = index_qmd, stringr::str_glue(txt))

  if (open) {
    setwd(post_dir)
    r_id <- rstudioapi::documentOpen(r_script, moveCursor = FALSE)
    q_id <- rstudioapi::documentOpen(index_qmd, moveCursor = FALSE)
    rstudioapi::documentSave(r_id)
    rstudioapi::documentSave(q_id)
  }
  return(invisible(post_dir))
}
