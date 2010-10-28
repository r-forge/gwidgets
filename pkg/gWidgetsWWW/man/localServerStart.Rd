\name{localServerStart}
\alias{localServerStart}
\alias{localServerSource}
\alias{gWidgetsWWWIsLocal}
\alias{makeSessionID}
%% these are deprecated
\alias{localServerStop}
\alias{localServerRestart}
\alias{localServerOpen}

\title{ Function to interact with local web server }

\description{

  The \pkg{gWidgetsWWW} package allows \code{gWidgets} scripts to be run
  through a web browser. For wider deployment, the package can be
  run through the \pkg{RApache} package. To develop scripts and to
  run scripts standalone, the local server may be used.  \cr
	
  The package uses R's dynamic help web server for the local server. The
  main functions are \code{localServerStart}, \code{localServerOpen} and \code{localServerSource}.
}
\usage{
localServerStart(file = "", port = 8079, package = NULL, ...)

localServerOpen(file, package=NULL, ...)

localServerSource(file_or_url)

gWidgetsWWWIsLocal()

localServerStop()

localServerRestart()
}

\arguments{
  \item{file}{Name of file to open. If no \code{package} is specified, this is a file relative to the directory returned by \code{getwd}. If the \code{package} argument is given, then the file is found from the call \code{system.file(file, package=package)}.}
  \item{package}{ If specified, files are found reltive to this package through \code{system.file(file,package=package)}.}
  \item{file\_or\_url}{A file or url to source in to create the web page.}
  \item{port}{Deprected}
  \item{...}{passed to constructor}

}
\details{
  The calls \code{localServerOpen} and \code{localServerSource} will
  start the help system web server if it isn't already started.\cr
  
  Use \code{localServerSource} to download a file from a website to
  run. This allows one to distribute scripts quite easily.\cr

	
  There is no way to send information to the browser from the R
  console. The browser must initiate the request.  \cr
  
  The local server logs error messages in the console.  \cr
  
  The functions \code{localServerStop} and
  \code{localServerRestart} are deprected.
  
}

\value{
  For \code{localServerStart}, \code{localServerOpen} and \code{localServerSource} a new browser page is opened.
}

\references{The dynamic help system is well described by Duncan
Murdoch and Simon Urbanek here in "The New R Help System." The R
Journal, 1(2):60-65, December 2009.  The use of a custom handler was
derived by work of Romain Francois's highlight package
\url{http://romainfrancois.blog.free.fr/}.}

\author{ John Verzani }

\seealso{ \pkg{Rpad} where this idea originated.}

\examples{
	\dontrun{
	localServerStart()	# opens browser with files in working directory
	localServerOpen(file="Examples/ex-index.R", package="gWidgetsWWW")
	}
}

\keyword{ misc }

