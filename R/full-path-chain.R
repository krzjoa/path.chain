#' @name full_path_chain
#' @title Full path chain
#' @param path root path
#' @param naming naming function
#' @description `full_path_chain` represents another approach to creating chainable paths
#' In contrast to `path_chain`, this functon creates just a list with nested list with full paths as a leaves.
#' @examples
#' fs::dir_list()
#' chainable.path <- full_path_chain(".")
#' chainable.path
#' @return list of lists and character objects
#' @export
full_path_chain <- function(path = ".", naming = basename){
  if(dir.exists(path)){
    file.list <- list.files(path, recursive = FALSE,
                            include.dirs = TRUE, full.names = TRUE)
    file.list <- setNames(file.list, naming(file.list))
    c(list('.' = path), as.list(Map(full_path_chain, file.list)))
  } else {
    path
  }
}
