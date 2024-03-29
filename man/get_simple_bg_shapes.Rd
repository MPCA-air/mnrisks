% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_simple_bg_shapes.R
\name{get_simple_bg_shapes}
\alias{get_simple_bg_shapes}
\title{get_simple_bg_shapes}
\usage{
get_simple_bg_shapes(geoid = NULL)
}
\arguments{
\item{geoid}{c(x,y,z) a vector of block group IDs to return}
}
\description{
Load Census block group shapefile for Minnesota.
}
\examples{
# All block groups
bgs <- get_simple_bg_shapes()

# A single block group
bgs <- get_simple_bg_shapes(geoid = c(271090017024))
}
\keyword{"block}
\keyword{blockgroups}
\keyword{census}
\keyword{group"}
\keyword{mn}
