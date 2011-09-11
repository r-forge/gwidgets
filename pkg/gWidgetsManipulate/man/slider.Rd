\name{slider}
\alias{slider}
\title{Slider interface}
\usage{
  slider(min, max, initial = min, step = -1, ticks = TRUE,
  label = NULL)
}
\arguments{
  \item{min}{minimum value}

  \item{max}{maximum value}

  \item{initial}{initial value. Must be in min <= max}

  \item{step}{step size. Use -1 for default}

  \item{ticks}{logical. are ticks drawn?}

  \item{label}{optional label for control}
}
\value{
  return Slider instance
}
\description{
  Slider interface
}

