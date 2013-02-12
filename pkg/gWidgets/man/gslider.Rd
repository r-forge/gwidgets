\name{gslider}
\alias{gslider}
\alias{gspinbutton}
\title{ Constructors for widgets to select a value from a sequence.}
\description{
	The gslider widget and gspinbutton widget allow the user to
	select a value from a sequence using the mouse. In the slider
	case, a slider is dragged left or right (or up or down) to
	change the value. For a spin button a text box with arrows
	beside allow the user to scroll through the values by clicking
	the arrows.

	Some toolkits only allow integer values for these.
}
\usage{
gslider(from = 0, to = 100, by = 1, length.out=NULL, along.with=NULL, value = from[1], horizontal = TRUE, handler = NULL, action = NULL, container = NULL, ..., toolkit = guiToolkit())

gspinbutton (from = 0, to = 10, by = 1, length.out=NULL, along.with=NULL, value = from, digits = 0, 
    handler = NULL, action = NULL, container = NULL, ..., toolkit = guiToolkit()) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{from}{Starting point in range. For \code{glider}, this may be a vector of values that
    can be sorted from \code{sort(unique(...))}. In this case, \code{to},
    \code{by}, etc. are ignored.}
  \item{to}{Ending point in range}
  \item{by}{Step size between values in the sequence}
  \item{length.out}{In place of by, take number of steps from this}
  \item{along.with}{In place of length.out, take length from this vector}
  \item{value}{ The initial value }
  \item{digits}{The number of digits shown}
  \item{horizontal}{Specifies orientation of gslider widget}
  \item{handler}{Called on a change event.}
  \item{action}{Passed to handler }
  \item{container}{ Optional container to attach widget to}
  \item{\dots}{Passed to \code{add} method of container}
    \item{toolkit}{Which GUI toolkit to use}

}
\details{
  
	Widgets to select from a vector of values. This vector is
	usually a sequence, as is returned by \code{seq}, hence the
	similar arguments, although the implementation is a bit less
	general, as \code{length.out} and \code{along.with} are used to
	compute \code{by} when given.
	

	For \code{gslider} any ordered vector may be used if specified
	to \code{from}.  Some toolkits only allow integer sequences, see
	the respective \pkg{gWidgetsXXX} packages for details.


	The \code{svalue} method returns the selected value. If all
	values are specified to \code{from}, then \code{index}
	argument is respected.

	The \code{svalue<-} method is used to set the selected value.
	If all values are specified to \code{from}, then \code{index}
	argument is respected.

	The \code{[<-} method can be used to change the sequence that the
	value is selected from. It expects a regular sequence, or if all
	values were originally specified to \code{from} a sortable
	sequence.

	The \code{addhandlerchanged} handler is called when the
	widgets' value is changed. 

}
% \value{}
% \references{}
% \author{}
% \note{}
% \seealso{}
\examples{
\dontrun{
  x <- rnorm(100)

  ## our handler
  plotHist <- function(...)
    hist(x, col=gray(svalue(sb)), breaks = svalue(sl))

   

  w <- gwindow("Slider and spinbox example")
  tbl = glayout(container=w)
  tbl[1,1] <- "Slide value to adjust shade"
  tbl[1,2] <- (sb <- gspinbutton(from=0,to=1,by=0.05,value=.5, container=tbl,
    handler=plotHist))
  tbl[2,1] <- "No. breaks"
  tbl[2,2, expand=TRUE] <- (sl <- gslider(from = 1, to= 100, by=1, value = 10,
    container = tbl, handler = plotHist))

## update slider using [<-
  sl[] <- seq(2,50, by=2)


  ## other sequence:
  w <- gwindow("Slider with sequence")
  sl <- gslider(letters, container=w)
  svalue(sl, index=TRUE)
  svalue(sl) <- "m"
  sl[] <- LETTERS ## can be sorted via sort(unique(LETTERS))
}
}
\keyword{interface}% at least one, from doc/KEYWORDS
