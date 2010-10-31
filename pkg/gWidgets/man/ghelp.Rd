\name{ghelp}
\alias{ghelp}
\alias{ghelpbrowser}

\title{Widget to interface with help pages}
\description{
  A widget to interface with the help pages and that widget placed in a
  browser. The widget is a notebook capable of showing several pages at once.
}
\usage{
ghelp(topic = NULL, package = NULL, container = NULL, ..., toolkit = guiToolkit())
ghelpbrowser(title = "Help browser", maxTerms=100, width=1000, height=600, ..., toolkit = guiToolkit()) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{topic}{Help topic}
  \item{package}{Which package to look for topic in}
  \item{container}{Optional container to attach widget to}
  \item{title}{Title of help browser}
  \item{maxTerms}{Maximum number of search responses}
  \item{width}{Width of browser window in pixels}
  \item{height}{Height  of browser window in pixels}
  \item{\dots}{Passed to \code{add} method of container}
  \item{toolkit}{Which GUI toolkit to use}
}
\details{
  
	The \code{ghelp} widget is a notebook to hold help pages. One
	can add pages with \code{add}. The help page is specified with a
	character vector or list. The first component is the topic, the
	second and optional package designation. The \code{svalue}
	method returns a list with topic and package. The \code{dispose} method
	will remove the current page. For toolkits that support them,
	closebuttons appear on the etabs

	The \code{ghelpbrowser} constructor produces a stand alone GUI for
	browsing help pages. Unlike other \pkg{gWidgets} constructors, this
	has no \code{container} argument.
  
}
% \value{}
% \references{}
% \author{}
% \note{}
\seealso{The \pkg{helpr} package provides a much better interface to R's help pages.}
\examples{
\dontrun{
  w <- gwindow("Help browser", visible=FALSE)
  g <- ggroup(horizontal=FALSE, cont=w)
  g1 <- ggroup(cont=g)
  addSpring(g1)
  glabel("Help on:", cont=g1)
  e <- gedit("", cont=g1)
  helpWidget <- ghelp(cont = g, expand=TRUE)
  addHandlerChanged(e, handler=function(h,...) {
     add(helpWidget, svalue(h$obj))
  })
  visible(w) <- TRUE
  ## add others
  add(helpWidget,"base:::mean")
  add(helpWidget, list(topic="mean", package="base"))
  add(helpWidget, "boxplot")

}
}
\keyword{interface}% at least one, from doc/KEYWORDS
