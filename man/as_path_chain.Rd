% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conversion.R
\name{as_path_chain}
\alias{as_path_chain}
\title{Create chainable path}
\usage{
as_path_chain(nested.list, root.name = "kRoot")
}
\arguments{
\item{nested.list}{`list` object with nested lists/strings inside}

\item{root.name}{key for root directory}
}
\value{
path_chain object
}
\description{
This function always treats first object in the nested list
as a subdirectory root path
}
\examples{
library(magrittr)
# Manually created nested list
nested.list <- list(kRoot = "root", "file1.txt", list("subdir", "file2.csv"))
chainable.path <- as_path_chain(nested.list)
class(chainable.path)
chainable.path$.
chainable.path$subdir$files2.csv
# Nested list from config file
tmp <- create_temp_dir("files")
create_sample_dir(tmp, override = TRUE)
fs::dir_tree(tmp)
path_chain(tmp, naming = naming_k) \%>\%
  as.list(root.name = "kRoot") \%>\%
  as_config("default", "kDirs") \%>\%
  yaml::write_yaml(temp_path("config.yaml"))
chainable.path <- config::get("kDirs", "defaul", temp_path("config.yaml")) \%>\%
 as_path_chain()
class(chainable.path)
chainable.path$.
chainable.path$kData$kExample1
}
