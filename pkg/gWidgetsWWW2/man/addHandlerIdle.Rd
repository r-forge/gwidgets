% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/gwidgets-methods.R
\name{addHandlerIdle}
\alias{addHandlerIdle}
\title{Assign handler to be called repeatedly after some interval}
\usage{
addHandlerIdle(x, interval = 1000, handler, action = NULL)
}
\arguments{
\item{x}{the object}

\item{interval}{interval in ms}

\item{handler}{handler to call}

\item{action}{optinal value to pass to handler}
}
\value{
a callback id, used with \code{removeHandler}
}
\description{
Assign handler to be called repeatedly after some interval
}

