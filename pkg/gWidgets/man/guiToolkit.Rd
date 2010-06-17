\name{guiToolkit}
\alias{guiToolkit}
\alias{guiWidgetsToolkit-class}
\alias{guiWidgetsToolkitRGtk2-class}
\alias{guiWidgetsToolkitrJava-class}
\alias{guiWidgetsToolkitSJava-class}
\alias{guiWidgetsToolkittcltk-class}
\alias{guiWidgetsToolkitRwxWidgets-class}
\title{Function to select the GUI toolkit used by gWidgets}
\description{
	A GUI toolkit is a separate package that implements the
	gWidgets API. This function allows one to select the toolkit
	to be used by default.
}
\usage{
guiToolkit(name = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{The name matches the package name without the initial
  gWidgets. For instance, "RGtk2" refers to the implementation
  provided by gWidgetsRGtk2.}
}
\details{
	Toolkits are named gWidgetsXXX.
}
\value{
	This function returns a subclass of guiWidgetsToolkit that is
	used for dispatching purposes by gWidgets. For example, the
	method svalue dispatches on its first argument and the value
	of the toolkit class stored in the toolkit slot of the object.
}
\examples{
	guiToolkit("RGtk2")
}
\keyword{ interface }% at least one, from doc/KEYWORDS

