#' @name on_path_not_exists
#' @title Function called if path does not exists
#' @param fun a function, one-side formula or NULL; if missing,
#' eturns value of the path.chain.on.path.not.exists option
#' @importFrom rlang is_formula as_function
#' @examples
#' on_path_not_exists(print)
#' on_path_not_exists()
#' @export
on_path_not_exists <- function(fun){
  if (missing(fun))
    return(getOption("path.chain.on.path.not.exists"))
  if (is_formula(fun))
    fun <- as_function(fun)
  if (is.function(fun) | is.null(fun))
    options(path.chain.on.path.not.exists = fun)
  else
    return(NULL)
}

#' @name on_validate_path
#' @title Function called to validate path correctness
#' @param fun a function; if missing, returns value of the path.chain.on.path.not.exists option
#' @importFrom rlang is_formula as_function
#' @examples
#' is_path_valid <- function(x) grepl("\\.fst", x)
#' on_validate_path(is_path_valid)
#' on_validate_path()
#' @export
on_validate_path <- function(fun){
  if (missing(fun))
    return(getOption("path.chain.on.validate.path"))
  if (is_formula(fun))
    fun <- as_function(fun)
  if (is.function(fun) | is.null(fun))
    options(path.chain.on.validate.path = fun)
  else
    return(NULL)
}



