#' @name path_chain
#' @title Create path_chain object - a directory or a file
#' @description A 'link' of path_chain object
#' @param node Current node name; character
#' @param children list of children - path_chains
#' @return path_chain object
#' @examples
#' # If we want to create our chain manually, we have start from the leaves
#' level2.b <- path_chain("fileA.RData")
#' level2.a <- path_chain("fileB.RData")
#' level1   <- path_chain("data", list(level2.a = level2.a , level2.b = level2.b))
#' root     <- path_chain("files", list(level1))
#' # Print root path
#' root$.
#' # Print file path using chaining
#' root$data$level2.a
#' @export
path_chain <- function(node = NULL, children = NULL){
  if(is.null(names(children)) & !is.null(children))
    children <- setNames(children, sapply(children, function(x) attr(x, 'node')))
  nms <- names(children)
  path.chain <- structure(list(), class = "path_chain")
  attr(path.chain, 'node') <- node
  for(i in seq_along(children)){path.chain[[nms[[i]]]] <- children[[i]]}
  path.chain[['.']] <- node
  return(path.chain)
}

#' @name $.path_chain
#' @title  Access path_chain object
#' @param node path_chain
#' @param child nested path_chain name
#' @return path_chain or character, if path indicates leaf of structure tree
#' @examples
#' #' If we want to create our chain manually, we have start from the leaves
#' level2.b <- path_chain("fileA.RData")
#' level2.a <- path_chain("fileB.RData")
#' level1   <- path_chain("data", list(level2.a = level2.a , level2.b = level2.b))
#' root     <- path_chain("files", list(level1))
#' # Print root path
#' root$.
#' # Print file path using chaining
#' root$data$level2.a
#' @export
`$.path_chain` <- function(node, child){

  if(length(node[[child]]) == 1 || child == "."){
    raw.string    <- deparse(substitute(node))
    splitted.path <- strsplit(raw.string, "\\$")[[1]]
    root.object   <- get(splitted.path[1], parent.frame())

    splitted.path     <- splitted.path[-1]
    elem.to.be.removed <- grepl("`.*`", splitted.path)
    splitted.path[elem.to.be.removed] <- substr(splitted.path[elem.to.be.removed], 2,
                                                nchar(splitted.path[elem.to.be.removed]))
    splitted.path <- c(splitted.path, child)

    fun <- function(x, y){
      obj <- x[[1]]
      last.node <- attr(obj[[y]], 'node')
      last.node <- if(is.null(last.node)) "" else last.node
      list(obj[[y]], file.path(x[[2]], last.node))
    }

    Reduce(fun, splitted.path, list(root.object, attr(root.object, 'node')))[[2]]
  } else {
    node[[child]]
  }
}

#' @name create_path_chain
#' @title Get directory structure and create path_chain object
#' @param path Path
#' @description This function returns
#' @return path_chain object
#' @examples
#' unlink("files", recursive = TRUE)
#' chainable.path <- create_path_chain("files")
#' fs::dir_tree("files")
#' @export
create_path_chain <- function(path){
  if(dir.exists(path)){
    file.list <- list.files(path, recursive = FALSE,
                            include.dirs = TRUE)
    file.list <- setNames(file.path(path, file.list), file.list)
    path_chain(node = basename(path), as.list(Map(create_path_chain, file.list)))
  } else {
    path_chain(node = basename(path))
  }
}
