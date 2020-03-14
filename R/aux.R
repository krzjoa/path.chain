#' @name path_children
#' @title Get children nodes, i.e. all the suddiectories in the given directory
#' @param path.chain object of `path_chain` class
#' @return a list of `path_chain` objects
#' @examples
#' unlink("files", recursive = TRUE)
#' path.chain <- create_path_chain("files")
#' path_children(path.chain)
#' unlink("files", recursive = TRUE)
#' @export
path_children <- function(path.chain){
  path.chain[-length(path.chain)]
}

#' @name print
#' @title Print path_chain object
#' @examples
#' level2.b <- path_chain("fileA.RData")
#' level2.a <- path_chain("fileB.RData")
#' level1   <- path_chain("data", list(level2.a = level2.a , level2.b = level2.b))
#' root     <- path_chain("files", list(level1))
#' print(root)
#'
#' unlink("files", recursive = TRUE)
#' create_sample_dir(name = "files")
#' chainable.path <- create_path_chain("files")
#' print(chainable.path)
#' unlink("files", recursive = TRUE)
#' @export
print.path_chain <- function(x, ...){
  cat(sprintf("path_chain \n root: %s \n childen: %d",
              attr(x, 'node'),
              length(x)))
}

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
