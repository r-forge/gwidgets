\name{gcommandline}
\alias{gcommandline}
\title{A command line interface}
\description{
  This constructs a simple command line interface for R
}
\usage{
gcommandline(command = "", assignto = NULL, useGUI = TRUE, useConsole = FALSE, prompt = getOption("prompt"), width = 500, height = 0.6 * width, container = NULL, ...,toolkit =
                 guiToolkit())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{command}{Initial command to evalues }
  \item{assignto}{Assigns output to this variable is non-NULL}
  \item{useGUI}{Is result also printed to GUI. Use FALSE to get
    text-only instance}
  \item{useConsole}{Is result also printed to console?}
  \item{prompt}{Prompt to use}
  \item{width}{Width of widget in pixels}
  \item{height}{Height of widget in pixels}
  \item{container}{Optional container to attach to}
  \item{\dots}{Ignored for now}
  \item{toolkit}{Which GUI toolkit to use}
}
\details{
  
  Additional commands can be added programmatically with the
  \code{svalue<-} method. The The value assigned is a string containing
  the command. If it has a \code{names} attribute, this is taken as the
  variable name to assign the output to.

  The \code{svalue} method returns the command history.

  The \code{"["} method can be used to retrieve the command history as well.
}
% \value{}
% \references{}
% \author{}
% \note{}
% \seealso{}
\examples{
\dontrun{
  obj = gcommandline(container=TRUE)
  svalue(obj) <- "2+2"
  ## assign to x
  command = "rnorm(100)"; names(command) = "x"
  svalue(obj) <- command
  ## look at history
  obj[]
}
}
\keyword{interface}% at least one, from doc/KEYWORDS
