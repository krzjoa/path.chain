#' @name create_sample_dir
#' @title Create sample directory
#' @description Creates sample nested directory to test and learn path.chain package
#' @export
create_sample_dir <- function(root = "."){
  dir.create(file.path(root, "files"))
}
