.onUnload <- function ( libpath ) {
  #' Removing options, which cab be set when functions
  #' `on_path_not_exists` or `on_validate_path` were called
  .Options$path.chain.on.path.not.exists <- NULL
  .Options$path.chain.on.validate.path <- NULL
}
