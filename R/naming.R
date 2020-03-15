#' @name naming_k
#' @title Naming convention, which adds k prefix for each key, capitalizes and removes file extension
#' @param path full path or its element
#' @importFrom tools file_path_sans_ext
#' @importFrom stringi stri_trans_totitle
#' @examples
#' naming_k("path/to/myfile.txt")
#' # Using with full_path_chain
#' create_sample_dir(name = "files", override = TRUE)
#' full.path.chain <- full_path_chain("files", naming = naming_k)
#' full.path.chain
#' create_sample_dir(name = "files")
#' # Using with path_chain / create_path_chain
#' path.chain <- create_path_chain("files", naming = naming_k)
#' path.chain %>%
#'   as.list()
#' unlink(root, recursive = TRUE)
#' @export
naming_k <- function(path){
  paste0("k", tools::file_path_sans_ext(stringi::stri_trans_totitle(basename(path))))
}
