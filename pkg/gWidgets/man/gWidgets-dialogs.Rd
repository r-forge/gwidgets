\name{gWidgets-dialogs}
\alias{gWidgets-dialogs}
\alias{gmessage}
\alias{galert}
\alias{gconfirm}
\alias{ginput}
\alias{gbasicdialog}

\title{Basic dialog constructors}
\description{
  A dialog is a widget that draws its own window. These dialogs are used
  for simple things -- confirming a choice, gathering a single line of
  input, etc. Dialogs are always modal, meaning they must be closed
  before R can be interacted with again.
}
\usage{
gmessage(message, title="message",
icon = c("info", "warning", "error", "question"),
        parent = NULL,
     handler = NULL, 
    action = NULL, ..., toolkit=guiToolkit()) 

ginput(message, text="", title="Input", icon = c("info", "warning",
 "error", "question"), parent=NULL, 
 handler = NULL, action = NULL,..., toolkit=guiToolkit())

gconfirm(message, title="Confirm", icon = c("info", "warning", "error",
 "question"), parent=NULL,
 handler = NULL, action = NULL, ..., toolkit=guiToolkit())

gbasicdialog(title = "Dialog", widget, parent=NULL, handler = NULL, action=NULL, ..., toolkit=guiToolkit())

galert(message, title = "message", delay=3, parent=NULL, ..., toolkit=guiToolkit())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{message}{Message shown for widget}
  \item{title}{Title of window}
  \item{icon}{Which icon to show}
  \item{text}{default value for ginput text}
  \item{widget}{Widget to place in basic dialog. If missing, dialog returns a container.}
  \item{parent}{A gwindow() instance. If specified, dialog will be
    located in relation to this}
  \item{handler}{Handler called on OK selection.}
  \item{action}{Value passed to handler}
  \item{delay}{For galert, how long the transient message will appear}
  \item{\dots}{Ignored}
  \item{toolkit}{Toolkit to use for GUI}
}
\details{
  These basic dialogs do slightly different things.

  The \code{gmessage} dialog shows a message with an icon and a dismiss
  button. This dialog returns \code{TRUE} or \code{FALSE} as
  appropriate. 

  The \code{gconfirm} dialog shows a message with an icon and an OK
  button and a dismiss button. A handler may be attached to the OK
  button selection. This dialog returns \code{TRUE} or \code{FALSE} as
  appropriate. 

  The \code{ginput} dialog adds an edit box for gathering user
  information. The \code{text} argument sets the default value. This is
  then passed to the handler via the component \code{input} of the first
  argument of the handler. This dialog returns the value of the string
  if OK is clicked, otherwise \code{NA}.

  For toolkits which support constructors without parent containers,
  the \code{gbasicdialog} dialog wraps a widget around an OK and
  dismiss button. The handler is called when the OK button is
  clicked. This dialog returns the \code{TRUE} when OK is clicked,
  otherwise \code{FALSE}.

  If the toolkit does not support parentless constructors, then the
  use is different. The \code{widget} argument is missing and the
  return value is a container. This is basically a top-level window
  with OK and cancel buttons. The container is shown when the
  \code{visible} method is called with a value of
  \code{set=TRUE}. This creates a modal dialog. The handler specified
  to the constructor is called when OK is clicked and \code{TRUE} is
  returned. The value of \code{FALSE} is returned on cancel, and
  \code{NA} otherwise.


  These dialogs are modal. This means that the R session freezes until
  the dialog is dismissed. This may be confusing to users if the window
  should appear below a currently drawn window.

  The \code{galert} dialog is non-modal and does not grab the
  focus. Like \code{gmessage} it shows a message but unlike it, only
  for a short period of time and is unobtrusive.

}
% \value{}
% \references{}
% \author{}
% \note{}
% \seealso{}
\examples{
\dontrun{
   gmessage("Hi there")
   gconfirm("Are we having fun?", handler = function(h,...)
   print("Yes"))

   ginput("Enter your name", icon="question", handler = function(h,...) cat("Hi",h$input,"\n"))

   ## gbasicdialog
   w <- gbasicdialog(title="Select a state", handler = function(h,...) 
      print(svalue(tbl)))
   tbl <- gtable(data.frame(states = rownames(state.x77)), expand=TRUE, cont = w)
   visible(w, set=TRUE) ## show dialog

}
}
\keyword{interface}% at least one, from doc/KEYWORDS
