% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_receptors.R
\name{map_receptors}
\alias{map_receptors}
\title{map_receptors}
\usage{
map_receptors(
  data = NULL,
  result_col = "cancer_risk",
  receptor_col = "receptor",
  colors = "viridis",
  reverse_colors = TRUE,
  size = 5,
  signif_digits = 3,
  silent = FALSE
)
}
\arguments{
\item{data}{The dataframe or sf object containing the modeling results for mapping.}

\item{result_col}{Column name containing the modeling concentration/risk results.}

\item{receptor_col}{Column name containing the receptor IDs.}

\item{colors}{Color palette to use for block group fill, ex. "viridis", "inferno", "Blues", "Greens"}

\item{reverse_colors}{Flip the color palette order}

\item{size}{Receptor or circle radius. Default = 5}

\item{signif_digits}{Number of significant figures to show in map labels}

\item{silent}{Return map as object. Default = FALSE}
}
\description{
Map the average block group mnrisks results with leaflet
}
\examples{
\dontrun{
map_receptors(data = mnrisk_results,
              result_col = "cancer_risk",
              colors = "inferno")
}               
}
\keyword{Map}
\keyword{leaflet}
\keyword{mnrisks}
\keyword{receptors}
