% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conversion.R
\name{as.list}
\alias{as.list}
\alias{as.list.path_chain}
\title{Convert object of type `path_chain` to list}
\usage{
\method{as.list}{path_chain}(path.chain, root.name = "root.dir")
}
\arguments{
\item{root.name}{key for root directory; default: 'root.dir'
unlink("files", recursive = TRUE)
path.chain <- create_path_chain("files")
as.list(path.chain)
unlink("files", recursive = TRUE)}

\item{patch.chain}{a path_chain object}
}
\description{
Convert object of type `path_chain` to list
}