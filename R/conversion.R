#' @name as_path_chain
#' @title Create chainable path
#' @param nested.list `list` object with nested lists/strings inside
#' @param root.namekKey for root directory
#' @description This function always treats first object in the nested list
#' as a subdirectory root path
#' @return path_chain object
#' @examples
#' library(magrittr)
#' # Manually created nested list
#' nested.list <- list(kRoot = "root", "file1.txt", list("subdir", "file2.csv"))
#' chainable.path <- as_path_chain(nested.list)
#' class(chainable.path)
#' chainable.path$.
#' chainable.path$subdir$files2.csv
#' # Nested list from config file
#' create_sample_dir("files", override = TRUE)
#' fs::dir_tree("files")
#' create_path_chain("files", naming = naming_fun) %>%
#'   as.list(root.name = "kRoot") %>%
#'   as_config("default", "kDirs") %>%
#'   yaml::write_yaml("config.yaml")
#' chainable.path <- config::get("kDirs", "defaul", "config.yaml") %>%
#'  as_path_chain()
#' class(chainable.path)
#' chainable.path$.
#' chainable.path$kData$kExample1
#' @export
as_path_chain <- function(nested.list, root.name = 'kRoot'){
  if(length(nested.list) > 1){
    node <- nested.list[[root.name]]
    children <- nested.list[which(names(nested.list) != root.name)]
    path_chain(node, Map(as_path_chain, children))
  } else {
    path_chain(node = nested.list[[1]])
  }
}

#' @name as.list
#' @title Convert object of type `path_chain` to list
#' @param patch.chain a path_chain object
#' @param root.name key for root directory; default: 'root.dir'
#' @examples
#' unlink("files", recursive = TRUE)
#' create_sample_dir(name = "files")
#' path.chain <- create_path_chain("files")
#' as.list(path.chain)
#' unlink("files", recursive = TRUE)
#' @export
as.list.path_chain <- function(path.chain, root.name = "root.dir"){
  if (length(path.chain) == 1) {
    attr(path.chain, 'node')
  } else {
    l <- list()
    l[[root.name]] <- path.chain$.
    call_as.list.path_chain <- function(x) as.list.path_chain(x, root.name = root.name)
    c(l, Map(call_as.list.path_chain, path_children(path.chain)))
  }
}

#' @name as_config
#' @title Prepare list to be saved as config .yaml file
#' @param x list with directory structure
#' @param config configuration name
#' @param wrap key name to wrap directory structure
#' @return list compatible with `{config}` package
#' @description This function is provided to keep compatibility
#' with `{config}` package, which requires existence of \textbf{default} key.
#' Additionally, we can at once wrap our structure with some other keys,
#' in order to not to mix directory structur with different keys.
#' @examples
#' library(magrittr)
#' # Initalizaing sample directory
#' create_sample_dir(name = "files", override = TRUE)
#' full_path_chain("files", "kRoot", naming_fun) %>%
#'    list(kDirs = .) %>%
#'    list(default = .) %>%
#'    yaml::write_yaml("config.yaml")
#' # We can simply use such function
#' full_path_chain("files", "kRoot", naming_fun) %>%
#'    as_config("default", "kDirs") %>%
#'    yaml::write_yaml("config.yaml")
#' @export
as_config <- function(x, config = "default", wrap = "dirs") {

  if (is.character(wrap)){
    wrapped <- list()
    wrapped[[wrap]] <- x
    x <- wrapped
  }

  default.wrap <- list()
  default.wrap[[config]] <- x

  return(default.wrap)
}

