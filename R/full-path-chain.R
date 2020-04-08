#' @name full_path_chain
#' @title Full path chain
#' @param path root path
#' @param root.name naming convention for root directory
#' @param naming naming function
#' @description `full_path_chain` represents another approach to creating chainable paths
#' In contrast to `path_chain`, this functon creates just a list with nested list with full paths as a leaves.
#' @importFrom rlang as_function
#' @importFrom stats setNames
#' @examples
#' tmp <- create_temp_dir("files")
#' create_sample_dir(tmp, override = TRUE)
#' fs::dir_tree(tmp)
#' chainable.path <- full_path_chain(tmp)
#' chainable.path
#' @return list of lists and character objects
#' @export
full_path_chain <- function(path = ".", root.name = ".", naming = basename){

  naming <- rlang::as_function(naming)

  if(dir.exists(path)){
    file.list <- list.files(path, recursive = FALSE,
                            include.dirs = TRUE, full.names = TRUE)
    file.list <- setNames(file.list, naming(file.list))
    root <- list()
    root[root.name] <- path
    call_full_path_chain <- function(x) full_path_chain(x,
                                                        root.name = root.name,
                                                        naming    = naming)
    c(root, as.list(Map(call_full_path_chain, file.list)))
  } else {
    path
  }
}
