\name{gWidgetsWWW2-callbacks}
\alias{addHandlerChanged}
\title{Assign handler to default event...}
\usage{addHandlerChanged(x, ...)
addHandlerClicked(x, ...)
addHandlerDoubleClick(x, ...)
addHandlerColumnClicked(x, ...)
addHandlerColumnDoubleClick(x, ...)
addHandlerMouseMotion(x, ...)
addHandlerKeystroke(x, ...)
addHandlerFocus(x, ...)
addHandlerBlur(x, ...)
addHandlerDestroy(x, ...)
addHandlerIdle(x, interval=1000, handler, action)
removeHandler(x, cbid)
blockHandler(x, cbid)
unblockHandler(x, cbid)
}
\description{Assign handler to default event}
\details{\code{addHandlerChanged}: The default event is specified in the definition of the
object. Using this function allows one to have a "generic" name
for adding a handler.

\code{addHandlerMouseMotion}: This handler gets passed back when the mouse moves. This can be really slow!


\code{addHandlerKeystroke}: This handler gets passed back the key that was pressed in the
\code{key} component.


}
\arguments{\item{x}{the object}
\item{...}{Used to pass through \code{handler}, and \code{action}
values. The handler is a function whose first argument is a list
that contains components \code{obj} to return the object and
\code{action}, holding the action value, and possibly others.}
\item{interval}{interval in ms}
\item{handler}{handler to call}
\item{action}{optinal value to pass to handler}
\item{cbid}{the callback id returned by an addHandlerXXX call}
}
\alias{addHandlerClicked}
\value{\code{addHandlerClicked}: a callback id, used with \code{removeHandler}

\code{addHandlerDoubleClick}: a callback id, used with \code{removeHandler}


\code{addHandlerColumnClicked}: a callback id, used with \code{removeHandler}


\code{addHandlerColumnDoubleClick}: a callback id, used with \code{removeHandler}


\code{addHandlerMouseMotion}: a callback id, used with \code{removeHandler}


\code{addHandlerKeystroke}: a callback id, used with \code{removeHandler}


\code{addHandlerFocus}: a callback id, used with \code{removeHandler}


\code{addHandlerBlur}: a callback id, used with \code{removeHandler}


\code{addHandlerDestroy}: a callback id, used with \code{removeHandler}


\code{addHandlerIdle}: a callback id, used with \code{removeHandler}


\code{removeHandler}: NULL


\code{blockHandler}: NULL


\code{unblockHandler}: NULL


}
\alias{addHandlerDoubleClick}
\alias{addHandlerColumnClicked}
\alias{addHandlerColumnDoubleClick}
\alias{addHandlerMouseMotion}
\alias{addHandlerKeystroke}
\alias{addHandlerFocus}
\alias{addHandlerBlur}
\alias{addHandlerDestroy}
\alias{addHandlerIdle}
\alias{removeHandler}
\alias{blockHandler}
\alias{unblockHandler}

