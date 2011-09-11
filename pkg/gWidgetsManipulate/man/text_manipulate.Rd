\name{text_manipulate}
\alias{text_manipulate}
\title{Manipulate an expression that produces text output}
\usage{
  text_manipulate(._expr, ...)
}
\arguments{
  \item{._expr}{expression to produce output.}

  \item{...}{used to specify controls. See \code{picker},
  \code{checkbox}, \code{slider}.

  These controls may also be specified through a object,
  from which the control is guessed. A logical maps to
  \code{checkbox}. A character maps to \code{picker}. A
  numeric to \code{slider}. This mapping can be specified
  as an arithmetic sequence of points (length 5 or
  greater), or as a numeric vector of length 2 to 4 with
  defaults like: \code{c(min, max, step=1, initial=min)}}
}
\description{
  Manipulate an expression that produces text output
}
\examples{
\dontrun{
## test it out
x <- rnorm(1:100, 10, 10); y <- rnorm(1:100, 5,5);
text_manipulate({
cut_it <- function(x, n) cut(x, seq(min(x), max(x), length.out=n))
xtabs(~ cut_it(y, m) + cut_it(x, n))
}, m= slider(2, 20, 2, 1), n = slider(2, 20, 2, 1))
}
}

