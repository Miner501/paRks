% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/osm_fetching.R
\name{get_greenspaces}
\alias{get_greenspaces}
\title{Get green spaces within a travel zone}
\usage{
get_greenspaces(zone, mode = "standard")
}
\arguments{
\item{zone}{An \code{sf} polygon (from \code{travel_zone()})}

\item{mode}{"standard" (parks & forests) or "broad" (adds meadows, grass, etc.)}
}
\value{
An \code{sf} object of green polygons intersecting the zone, or NULL if none found
}
\description{
Returns parks, gardens, forests, meadows, and other green spaces within the travel zone.
}
