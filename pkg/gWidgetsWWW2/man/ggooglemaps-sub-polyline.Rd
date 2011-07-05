\name{ggooglemaps_polyline}
\alias{ggooglemaps_polyline}
\title{A method to write a polyline to a map...}
\usage{ggooglemaps_polyline(path, stroke=list(color = "#0000FF", opacity = 1, weight = 2), map, ...)
}
\description{A method to write a polyline to a map}
\details{A polyline is equivalent to the \code{lines} function. The line is
specified as a data frame or matrix of latituted and longitude
pairs. The options include stroke values.\code{ggooglemaps_polyline}: A polyline is equivalent to the \code{lines} function. The line is
specified as a data frame or matrix of latituted and longitude
pairs. The options include stroke values.


}
\value{an instance of Ggooglemapspolyline\code{ggooglemaps_polyline}: an instance of Ggooglemapspolyline


}
\arguments{\item{path}{a matrix or data frame of lat/lng pairs}
\item{stroke}{A list with values for color (hex RGB), opacity a
number in 0.0 to 1.0 and weight, a value for the weight of the
stroke.}
\item{map}{A ggooglemaps instance.}
\item{...}{passed to init but ignored for now}
}

\alias{ggooglemaps_polyline}

