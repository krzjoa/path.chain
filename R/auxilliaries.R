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
#' path.chain <- create_path_chain(tmp)
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
#' level2.b <- path_chain("fileA.RData")
#' level2.a <- path_chain("fileB.RData")
#' level1   <- path_chain("data", list(level2.a = level2.a , level2.b = level2.b))
#' root     <- path_chain("files", list(level1))
#' print(root)
#'
#' tmp <- create_temp_dir("files")
#' create_sample_dir(tmp, override = TRUE)
#' chainable.path <- create_path_chain(tmp)
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
#' @param ... arbitrary character objects (optional)
#' @param warn warn, if folder already exists
#' @details Be careful: if you call this function, it only creates the root temporary file.
#' Even if using concatenation, the only object created in file system is tmp directory.
#' All the rest has to be created on your own, e.g. calling \link[base]{dir.create} function.
#' @examples
#' # Simply create and return temporal directory
#' create_temp_dir()
#' # Create temp dir and return concatenated path
#' # Keep in mind, that 'files' and 'report_2020.xls' will not be created.
#' create_temp_dir("files", "report_2020.xls")
#' @export
create_temp_dir <- function(..., warn = FALSE){
  tmp <- tempdir()
  dir.create(tmp, showWarnings = warn)
  file.path(tmp, ...)
}


