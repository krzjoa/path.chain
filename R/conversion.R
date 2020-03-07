#' @name as_path_chain
#' @title Create chainable path
#' @param path path (character)
#' @param root.name Key for root directory
#' @description This function returns
#' @return path_chain object
#' @examples
#' nested.list <- list(kRoot = "root", "file1.txt", list("subdir", "file2.csv"))
#' chainable.path <- as_path_chain(nested.list)
#' @export
as_path_chain <- function(config.section, root.name = 'kRoot'){
  if(length(config.section) > 1){
    node <- config.section[[root.name]]
    children <- config.section[which(names(config.section) != root.name)]
    path_chain(node, Map(as_path_chain, children))
  } else {
    path_chain(node = config.section[[1]])
  }
}

#' @name as.list
#' @title Convert object of type `path_chain` to list
#' @param patch.chain a path_chain object
#' @param root.name key for root directory; default: 'root.dir'
#' @examples
#' unlink("files", recursive = TRUE)
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
    c(l, Map(as.list.path_chain, path_children(path.chain)))
  }
}





