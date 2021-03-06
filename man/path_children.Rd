% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auxilliaries.R
\name{path_children}
\alias{path_children}
\title{Get children nodes, i.e. all the suddiectories in the given directory}
\usage{
path_children(path.chain)
}
\arguments{
\item{path.chain}{object of `path_chain` class}
}
\value{
a list of `path_chain` objects
}
\description{
Get children nodes, i.e. all the suddiectories in the given directory
}
\examples{
tmp <- create_temp_dir("files")
create_sample_dir(tmp, override = TRUE)
path.chain <- path_chain(tmp)
path_children(path.chain)
}
