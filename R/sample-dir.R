#' @name create_sample_dir
#' @title Create sample directory
#' @description Creates sample nested directory to test and learn path.chain package
#' @examples
#' create_sample_dir(name = "files")
#' list.files("files", all.files = TRUE, recursive = TRUE, include.dirs = TRUE)
#' @export
create_sample_dir <- function(root = ".", name = "files"){
  dir.create(file.path(root, name))
  dir.create(file.path(root, name, "docs"))
  dir.create(file.path(root, name, "data"))
  file.create(file.path(root, name, "data", "example1.RData"))
  file.create(file.path(root, name, "data", "example2.RData"))
  file.create(file.path(root, name, "data", "persons.csv"))
  file.create(file.path(root, name, "docs", "schema.txt"))
}

