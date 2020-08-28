#' @name file_path
#' @title Construct path to file without doubled separators
#' @param ... character vectors
#' @param fsep the path separator to use
#' @return character file path
#' @examples
#' file.path("files/", "data/", "cars.RData")
#' file_path("files/", "data/", "cars.RData")
#' @export
file_path <- function(..., fsep = .Platform$file.sep){
  args <- as.list(gsub("\\/+$", "", list(...)))
  fun <- function(...) file.path(..., fsep = fsep)
  do.call(fun, args)
}

#' @name path_children
#' @title Get children nodes, i.e. all the suddiectories in the given directory
#' @param path.chain object of `path_chain` class
#' @return a list of `path_chain` objects
#' @examples
#' tmp <- create_temp_dir("files")
#' create_sample_dir(tmp, override = TRUE)
#' path.chain <- path_chain(tmp)
#' path_children(path.chain)
#' @export
path_children <- function(path.chain){
  path.chain[-length(path.chain)]
}

#' @name print
#' @title Print path_chain object
#' @param x `path_chain` object
#' @param ... elipsis for API consistency, does nothing
#' @examples
#' level2.b <- path_link("fileA.RData")
#' level2.a <- path_link("fileB.RData")
#' level1   <- path_link("data", list(level2.a = level2.a , level2.b = level2.b))
#' root     <- path_link("files", list(level1))
#' print(root)
#'
#' tmp <- create_temp_dir("files")
#' create_sample_dir(tmp, override = TRUE)
#' chainable.path <- path_chain(tmp)
#' print(chainable.path)
#' @export
print.path_chain <- function(x, ...){
  cat(sprintf("path_chain \n root: %s \n childen: %d",
              attr(x, 'node'),
              length(x)))
}

#' @name create_sample_dir
#' @title Create sample directory
#' @param path path for the new sample folder
#' @param override boolean: override folder if it already exists
#' @description Creates sample nested directory to test and learn path.chain package
#' @examples
#' tmp <- create_temp_dir("files")
#' create_sample_dir(tmp, override = TRUE)
#' list.files(tmp, all.files = TRUE, recursive = TRUE, include.dirs = TRUE)
#' fs::dir_tree(tmp)
#' @export
create_sample_dir <- function(path = "files", override = FALSE){

  if (override)
    unlink(path, recursive = TRUE)

  dir.create(path, showWarnings = !override)
  dir.create(file.path(path, "docs"), showWarnings = !override)
  dir.create(file.path(path, "data"), showWarnings = !override)
  file.create(file.path(path, "data", "example1.RData"))
  file.create(file.path(path, "data", "example2.RData"))
  file.create(file.path(path, "data", "persons.csv"))
  file.create(file.path(path, "docs", "schema.txt"))
}

#' @name create_temp_dir
#' @title Create temporary diectory and return its name
#' @param ... arbitrary character objects
#' @param warn warn, if folder already exists
#' @param recursive ogical. Should elements of the path other than the last be created?
#' If true, like the Unix command mkdir -p
#' @param fsep the path separator to use
#' @examples
#' # Simply create and return temporal directory
#' create_temp_dir()
#' # Create temp dir and return concatenated path
#' # Keep in mind, that 'files' and 'report_2020.xls' will not be created.
#' create_temp_dir("files", "report_2020.xls")
#' @export
create_temp_dir <- function(..., warn = FALSE, recursive = FALSE, fsep = .Platform$file.sep){
  tmp <- tempdir()
  tmp <- file.path(tmp, ..., fsep = fsep)
  dir.create(tmp, showWarnings = warn, recursive = recursive)
  tmp
}

#' @name temp_path
#' @title Construct path to file in a temporary directory
#' @param ... arbitrary character objects
#' @param fsep the path separator to use.
#' @details Be careful: if you call this function, it only creates a path for temporary file/dir.
#' All the rest has to be created on your own, e.g. calling \link[base]{dir.create} function.
#' @return a path
#' @examples
#' temp_path("files", "report.csv")
#' @export
temp_path <- function(..., fsep = .Platform$file.sep){
  file.path(tempdir(), ..., fsep = fsep)
}

