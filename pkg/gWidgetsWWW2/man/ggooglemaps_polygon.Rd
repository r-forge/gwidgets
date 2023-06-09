% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/ggooglemaps.R
\name{ggooglemaps_polygon}
\alias{ggooglemaps_polygon}
\title{Draw a polygon on the map}
\usage{
ggooglemaps_polygon(path, stroke = list(color = "#0000FF", opacity = 1, weight
  = 2), fill = list(color = "#FF0000", opacity = 0.25), map, ...)
}
\arguments{
\item{path}{path of polygon as matrix or data frame of points}

\item{stroke}{list of stroke parameters (color, opacity, weight)}

\item{fill}{list of fill parameters (color, opacity)}

\item{map}{ggooglemaps object}

\item{...}{ignored}
}
\value{
a polygon instance, but has no methods
}
\description{
Draw a polygon specified as  matrix of data frame of latitude/longitude values
}

