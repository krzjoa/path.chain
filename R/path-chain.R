#' @name path_link
#' @title Creates a link of path chain - a directory or a file
#' @description It returns basic package's object: an object representing a link in the chain.
#' Each link has the path_chain class - it can represents a one-element path chain
#' @param node Current node name; character
#' @param children list of children - path_chains
#' @return path_chain object
#' @importFrom stats setNames
#' @examples
#' # If we want to create our chain manually, we have start from the leaves
#' level2.b <- path_link("fileA.RData")
#' level2.a <- path_link("fileB.RData")
#' level1   <- path_link("data", list(level2.a = level2.a , level2.b = level2.b))
#' root     <- path_link("files", list(level1))
#' # Print root path
#' root$.
#' # Print file path using chaining
#' root$data$level2.a
#' @export
path_link <- function(node = NULL, children = NULL){
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
#' level2.b <- path_link("fileA.RData")
#' level2.a <- path_link("fileB.RData")
#' level1   <- path_link("data", list(level2.a = level2.a , level2.b = level2.b))
#' root     <- path_link("files", list(level1))
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
      list(obj[[y]], file_path(x[[2]], last.node))
    }
    final.path <- Reduce(fun, splitted.path, list(root.object, attr(root.object, 'node')))[[2]]

    # Check, if path exists
    on.path.not.exists <- on_path_not_exists()
    if (!is.null(on.path.not.exists) & !file.exists(final.path))
      on.path.not.exists(final.path)

    # Call path validation
    on.validate.path<- on_validate_path()
    if (!is.null(on.validate.path )) on.validate.path(final.path)

    return(final.path)
  } else {
    node[[child]]
  }
}

#' @name path_chain
#' @title Get directory structure and create path_chain object
#' @param path root of the directory structure
#' @param naming function which defines naming convention
#' @param levels number of hierarchy levels that recursion should go deep; defaults to +Inf
#' @param only.directories boolean to ignore files and only considers directories.
#' @description Returns `path_chain` object, which reflects
#' structure of the folder passed with `path` param
#' @return path_chain object
#' @importFrom stats setNames
#' @examples
#' tmp <- create_temp_dir("files")
#' create_sample_dir(tmp, override = TRUE)
#' fs::dir_tree(tmp)
#' chainable.path <- path_chain(tmp)
#' chainable.path$data$persons.csv
#' # With customized naming convention
#' chainable.path <- path_chain(tmp, naming = naming_k)
#' chainable.path$kData$kPersons
#' @export
path_chain <- function(path, naming = basename, levels = +Inf, only.directories = FALSE) {
  if (dir.exists(path)) {
    file.list <- list.files(path, recursive = FALSE,
                            include.dirs = TRUE)
    file.list <- setNames(file.path(path, file.list), file.list)
    call_path_chain <- function(x, levels) {
      path_chain(x, naming = naming, levels = levels - 1, only.directories)
    }
    if (levels > 0) {
      children <- Map(call_path_chain, file.list, levels)
      children <- setNames(children, naming(file.list))
      path_link(node = basename(path), children)
    }
  } else {
    if (!only.directories) path_link(node = basename(path))
  }
}
