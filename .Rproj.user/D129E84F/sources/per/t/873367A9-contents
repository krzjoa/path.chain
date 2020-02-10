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

print.path_elem <- function(x, ...){
  cat(sprintf("path_elem \n root: %s \n childen: %d", 
              attr(x, 'node'),
              length(x))
      )
  x
}

`$.path_elem` <- function(a, b){
  
  if(length(a[[b]]) == 1 || b == "."){
    raw.string <- deparse(substitute(a))
    splitted.path <- strsplit(raw.string, "\\$")[[1]]
    root.object <- get(splitted.path[1], parent.frame())
    
    splitted.path <- splitted.path[-1]
    elem.to.be.removed <- grepl("`.*`", splitted.path)
    splitted.path[elem.to.be.removed] <- substr(splitted.path[elem.to.be.removed], 2,
                                                nchar(splitted.path[elem.to.be.removed]))
    splitted.path <- c(splitted.path, b)
    
    fun <- function(x, y){
      obj <- x[[1]]
      last.node <- attr(obj[[y]], 'node')
      last.node <- if(is.null(last.node)) "" else last.node
      list(obj[[y]], file.path(x[[2]], last.node))
    }
    
    Reduce(fun, splitted.path, list(root.object, attr(root.object, 'node')))[[2]]
  } else {
    a[[b]]
  }
}

dir_structure <- function(file.section, root.key = 'kRoot'){
  # browser()
  if(length(file.section) > 1){
    node <- file.section[[root.key]]
    children <- file.section[which(names(file.section) != root.key)]
    # path_elem(node, Map(function (x) dir_structure(x, root.key = root.key, 
    #                                                extension = extension), 
    #                     children))
    path_elem(node, Map(dir_structure, children))
  } else {
    # node <- file.section[[1]]
    # node <- if(!is.null(extension)) paste0(node, extension) else node
    path_elem(node = file.section[[1]])
  }
}
                                          
nested_path <- function(path = ".", naming = basename){
  if(dir.exists(path)){
    file.list <- list.files(path, recursive = FALSE, 
                            include.dirs = TRUE, full.names = TRUE)
    file.list <- setNames(file.list, naming(file.list))
    c(list('.' = path), as.list(Map(nested_path, file.list)))
  } else {
    path
  }
}

                                          

cnf <- config::get(config = 'default', file = 'conf.yml')
file.section <- cnf$kFiles
cnfs <- dir_structure(file.section)

cnfs$kNestedData$kLastFolder$kUltimateFile

cnfs$kNestedData$kLastFolder$kLastR
cnfs$kNestedData$kFile3

# sapply(list('a', 'b', 'c'), function(x) path_elem(node = x)) -> a
# Map(function(x) path_elem(node = x), list('a', 'b', 'c'))
# a1 <- path_elem("fileA.RData")
# a2 <- path_elem("fileB.RData")
# a3 <- path_elem("data", list(A1 = a1, A2 = a2))
# a4 <- path_elem("files", list(a3))

# a4$data$A1
# a4$data$.


are <- function(char){
  function(...) Reduce("&", Map(function (x) inherits(x, char), list(...)), TRUE)
}

!are('matrix')(matrix(0, 3, 3), m(1:3 | 4:6))
                                
                                
 # To matricks package
square_matrix <- function(x, round.method = floor){
  matrix(x, nrow = round.method(sqrt(length(x))))
}                       
