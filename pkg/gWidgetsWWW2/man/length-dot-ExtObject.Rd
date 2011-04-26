\name{length.ExtObject}
\alias{length.ExtObject}
\title{length method for ExtObject's...}
\usage{length(x, ...)}
\description{length method for ExtObject's}
\details{This definition has some sideeffects. Namely, if one uses \code{sapply} and
returns such objects, then the call to \code{simplify2array} will cause an error.
Use \code{lapply}, or \code{simplify=FALSE}.}
\alias{length.ExtObject}
\arguments{\item{x}{object}
\item{...}{passed along to \code{len} method}
}

