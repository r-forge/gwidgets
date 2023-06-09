% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/load.R
\name{load_app}
\alias{load_app}
\title{Load a web app defined by a gWidgetsWWW2 script}
\usage{
load_app(script_name, app_name = NULL, port = NULL,
  session_manager = make_session_manager(), open_page = TRUE,
  show.log = FALSE, ...)
}
\arguments{
\item{script_name}{path to gWidgetsWWW2 script}

\item{app_name}{base name for script, unique per project. Derived
from script name if not specified.}

\item{port}{Initial port for Rhttpd server, provided it hasn't already started.}

\item{session_manager}{an instance of \code{make_session_manager}}

\item{open_page}{logical. If \code{TRUE} call \code{browseURL} to open the app.}

\item{show.log}{If TRUE, logged information is written to the console.}

\item{...}{Passed to WebPage object. Named arguments
\code{body} and \code{head} can be used to insert information into
each, such as loading of style sheets etc. through \code{head}. One can
also subclass \code{WebPage} and pass this in via \code{webpage}.}
}
\description{
There are two basic tasks that gWidgetsWWW2 does: a) create the
javascript to populate a page (or part of the page) and b) create
a means for AJAX requests between the web browser and the R
process. For b) there isn't much to say except that it is supposed
to just work.
}
\details{
As for a) there are things one can pay attention to. The simplest
case is really pretty simple. Write some script, save it to a file
and call this function with the script name (and path if needed).
This simple case will create a full screen app. Each app runs in
its own environment in the R session, so running lots of instances
can be expensive. That is, don't expect gWidgetsWWW2 to scale.

To embed an app within a web page, use an \code{iframe} tag.
}
\examples{
## open an app that takes the entire page
gw_script <-  system.file("examples/ex-hello-world.R", package="gWidgetsWWW2")
if(interactive()) load_app(gw_script, "HelloApp")
}

