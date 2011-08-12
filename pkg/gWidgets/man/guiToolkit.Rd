\name{guiToolkit}
\alias{guiToolkit}
\alias{gtoolkit}
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

gtoolkit()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{The name matches the package name without the initial
  gWidgets. For instance, "RGtk2" refers to the implementation
  provided by gWidgetsRGtk2.}
}
\details{
	Toolkits are named gWidgetsXXX. This function is used to get the toolkit class, or have the user select one. The \code{gtoolkit} function returns a string form of the toolkit. One way to set a toolkit is to use the \code{guiToolkit} option, as in \code{options(guiToolkit="RGtk2")}.
}
\value{
	The \code{guiToolkit} function returns a subclass of guiWidgetsToolkit that is
	used for dispatching purposes by gWidgets. For example, the
	method svalue dispatches on its first argument and the value
	of the toolkit class stored in the toolkit slot of the object.

	The \code{gtoolkit} function returns a character string for the toolkit.
}
\examples{
	guiToolkit("RGtk2")
}
\keyword{ interface }% at least one, from doc/KEYWORDS

