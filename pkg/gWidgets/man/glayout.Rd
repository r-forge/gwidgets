\name{glayout}
\alias{glayout}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A container for aligning widgets in a table}
\description{
  A container for laying out widgets in a table. The widgets are added
  using matrix notation (\code{[i,j]<-}).

}
\usage{
glayout(homogeneous = FALSE, spacing = 10, container = NULL, ..., toolkit = guiToolkit())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{homogeneous}{A logical indicating if the  cells are all the same size}
  \item{spacing}{Spacing in pixels between cells}
  \item{container}{Optional container to attach widget to.}
  \item{\dots}{Passed to \code{add} method of container}
  \item{toolkit}{Which GUI toolkit to use}

}
\details{

  Widgets are added using matrix notation. A widget can span several
  cells, for instance \code{obj[1:2,2:3] <- widget} would place the
  widget in the first and second rows and second and third columns.  The
  matrix notation is to specify the space allocated to the widget. 

  For \code{gWidgetstcltk}, it is necessary for a child widget to have the
  layout object as its parent container and to call the \code{[<-}
  method to add the widget. (See the example.)

  
  As a convenience, if the value to be assigned is a character it will
  be turned into a \code{glabel} object before being added.


  Like \code{ggroup}, the extra argument \code{expand} can be used to
  force the widget to expand to fill all the space allocated to it.
  
  Like \code{ggroup}, the extra argument \code{anchor} can by used to
  anchor the child within the space allocated when this space islarger
  than needed by the widget. This is specified as a pair of values from
  -1,0,1 to indicating the x and y positioning of the widget within the
  cell.
  
  Like  \code{ggroup}, the extra argument \code{fill} can by used when \code{expand} is given, but not \code{anchor}, to have the widget expand in the \code{x} direction, the \code{y} direction, or \code{both} (the default). (Toolkit specific). 

  The method \code{[} can be used to subset. In the simplest usage, it
  returns the item at index i,j. (The item at i,j may be in other
  cells too. The return value is a gwidget if 1x1, a list if 1xn or
  mx1 (n>1), or a mxn matrix of items.


}
	
% \value{}
% \references{}
% \author{}
% \note{}
% \seealso{}
\examples{
\dontrun{
  ## show part of mtcars dataframe in a layout
  w <- gwindow("glayout example")
  tbl <- glayout(container = w)
  tbl[1,1] <- "a label"
  ## need container argument in gWidgetstcltk, gWidgetsRwxwidgets
  ## so we always use it.
  tbl[1,2, expand = TRUE] <- gedit("edit here", container=tbl)
  tbl[2,1, anchor = c(-1,-1)] <- glabel("ll", container = tbl)

  ## extraction:
  tbl[1,1] # glabel instance
  tbl[1,2] # gedit instance, ...
}
}
\keyword{interface}% at least one, from doc/KEYWORDS
