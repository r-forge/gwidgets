\name{gfile}
\alias{gfile}
\alias{gfilebrowse}
\alias{gcalendar}

\title{Dialogs for file and date selection}
\description{
	These functions provide dialogs for file selection (files or
	directories) and date selections.
}
\usage{
gfile(text = "", type = c("open", "save", "selectdir"), initialfilename = NULL, 
    filter = list("All files" = list(patterns = c("*")), "R files" =
    list(patterns = c("*.R","*.Rdata")),
        "text files" = list(mime.types = c("text/plain"))
        ), multi=FALSE, handler = NULL, action = NULL, ..., toolkit = guiToolkit())

gfilebrowse (text = "Select a file...", type = "open", quote = TRUE, 
    container = NULL, ..., toolkit = guiToolkit()) 

gcalendar(text = "", format = "\%Y-\%m-\%d", handler=NULL, action=NULL, container = NULL, ..., toolkit = guiToolkit()) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{Initial text. For thecalendar, an optional date in a form matching \code{format}}
  \item{type}{When selecting a file it can be selected for opening,
  for saving or you may want to select a directory.}
\item{initialfilename}{Suggested name for file save}
  \item{filter}{Filter for files shown during selection }
  \item{multi}{Logical. Allow selection of multiple files?}
  \item{quote}{Is result quoted}
  \item{format}{Format of date, default is year-month-day}
  \item{handler}{Handler for when file is changed. The component
    \code{file} of the first argument contains the file name}
  \item{action}{ Passed to handler }
  \item{container}{Optional container to attach widget to}
  \item{\dots}{Passed to \code{gedit} instance}
  \item{toolkit}{Which GUI toolkit to use}
}
\details{
	The \code{gfile} dialog is modal, meaning no action can take
	place until a selection is made. Whereas the
	\code{gfilebrowse} dialog consists of a \code{gedit} instance
	to hold a filename and a button to click if the dialog is
	desired to fill in this filename.

	The \code{gcalendar} widget is similar to the
	\code{gfilebrowse} widget.

	For both \code{gcalendar} and \code{gfilebrowse} any
	\code{...} arguments are passed to \code{gedit}. The
	\code{coerce.with} argument can be used to here to quote the
	values, or coerce them otherwise such as with
	\code{as.Date}. Otherwise, the \code{svalue} method returns
	a character string containing the value shown in the
	\code{gedit} box.


	The \code{svalue<-()} method may be used to set the value for
	both \code{gcalendar} and \code{gfilebrowse} .

	The return value is the filename selected. A \code{NA} value is
	returned if no file is selected.

}
% \value{}
% \references{}
% \author{}
% \note{}
% \seealso{}
\examples{
\dontrun{
## source a file using a handler
sourceFile <- function() gfile("Select a file",type="open", handler =
function(h,...) source(h$file))

## source a file using fact that dialog is modal
source(gfile())

## apply a generic action to the file
countLines <- function(filename) print(length(readLines(filename)))
chooseFile <- function() gfile("Select a file",type="open",
action="countLines", handler = function(h,...) do.call(h$action,list(h$file)))
}
}
\keyword{interface}

