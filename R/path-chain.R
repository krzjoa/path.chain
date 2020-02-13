#' @name path_elem
#' @title Path element - directory or file name
#' @description A 'link' of path_chain object
#' @param node Current node name; character
#' @param children list of children - path_elems
#' @return path_elem object
#' @examples
#' # If we want to create our chain manually, we have start from the leaves
#' level2.b <- path_elem("fileA.RData")
#' level2.a <- path_elem("fileB.RData")
#' level1 <- path_elem("data", list(level2.a = level2.a , level2.b = level2.b))
#' root <- path_elem("files", list(level1))
#' # Print root path
#' root$.
#' # Print file path using chaining
#' root$data$level2.a
#' @export
path_elem <- function(node = NULL, children = NULL){
  if(is.null(names(children)) & !is.null(children))
    children <- setNames(children, sapply(children, function(x) attr(x, 'node')))
  nms <- names(children)
  path.elem <- structure(list(), class = "path_elem")
  attr(path.elem, 'node') <- node
  for(i in seq_along(children)){path.elem[[nms[[i]]]] <- children[[i]]}
  path.elem[['.']] <- node
  return(path.elem)
}

#' @name print
#' @title Print path_elem object
#' @export
print.path_elem <- function(x, ...){
  cat(sprintf("path_elem \n root: %s \n childen: %d",
              attr(x, 'node'),
              length(x))
  )
  x
}

#' @name $.path_elem
#' @title  Access path_elem object
#' @param node path_elem
#' @param child nested path_elem name
#' @return path_elem or chaacter, if path indicates leaf of structure tree
#' @export
`$.path_elem` <- function(node, child){

  if(length(node[[child]]) == 1 || child == "."){
    raw.string <- deparse(substitute(node))
    splitted.path <- strsplit(raw.string, "\\$")[[1]]
    root.object <- get(splitted.path[1], parent.frame())

    splitted.path <- splitted.path[-1]
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

#' @name relative_path_chain
#' @title Create chainable path
#' @param path Path
#' @description This function returns
#' @return path_elem object
#' @examples
#' chained.path <- path_chain(".")
#' @export
path_chain <- function(path){
  if(dir.exists(path)){
    file.list <- list.files(path, recursive = FALSE,
                            include.dirs = TRUE)
    file.list <- setNames(file.list, file.list)
    path_elem(node = path, as.list(Map(path_chain, file.list)))
  } else {
    path_elem(node = path)
  }
}

#' @name path_chain_config
#' @title Load path chain from configuration file
#' @param config.section
#' @param root.key
#' @export
path_chain_config <- function(config.section, root.key = 'kRoot'){
  if(length(config.section) > 1){
    node <- config.section[[root.key]]
    children <- config.section[which(names(config.section) != root.key)]
    path_elem(node, Map(dir_structure, children))
  } else {
    path_elem(node = config.section[[1]])
  }
}
