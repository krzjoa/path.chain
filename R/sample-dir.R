#' @name create_sample_dir
#' @title Create sample directory
#' @description Creates sample nested directory to test and learn path.chain package
#' @examples
#' create_sample_dir()
#' list.files("files", all.files = TRUE, recursive = TRUE, include.dirs = TRUE)
#' @export
create_sample_dir <- function(root = "."){
  dir.create(file.path(root, "files"))
  dir.create(file.path(root, "files", "docs"))
  dir.create(file.path(root, "files", "data"))
  file.create(file.path(root, "files", "data", "example1.RData"))
  file.create(file.path(root, "files", "data", "example2.RData"))
  file.create(file.path(root, "files", "data", "persons.csv"))
  file.create(file.path(root, "files", "docs", "schema.txt"))
}

