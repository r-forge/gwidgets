\name{guiWidget-class}
\docType{class}
\alias{guiWidget-class}
\alias{guiComponent-class}
\alias{guiContainer-class}
\alias{guiDialog-class}
\alias{guiWidgetOrNULL-class}
\alias{gWidgetANY-class}
\alias{gComponentANY-class}
\alias{gContainerANY-class}

\title{Class "guiWidget" ~~~ }
\description{Base class for gWidget objects}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("guiWidget", ...)}.

	These objects have two slots: a widget provided by a
	guiToolkit and a toolkit. Method dispatch is done on both values.

}
\section{Slots}{
	 \describe{
    \item{\code{toolkit}:}{Object of class
    \code{"guiWidgetsToolkit"}. A specification of which GUI toolkit
    the widget uses.}
    \item{\code{widget}:}{Object of class \code{"ANY"}. A widget
    returned by the corresponding toolkit function. }
  }
}
\section{Methods}{
  \describe{
    \item{.add}{\code{signature(obj = "guiWidget", toolkit = "guiWidgetsToolkitRGtk2", value = "ANY")}: ... }
    \item{.add}{\code{signature(obj = "guiWidget", toolkit = "guiWidgetsToolkitRGtk2", value = "guiWidgetORgWidgetRGtkORRGtkObject")}: ... }
    \item{.add}{\code{signature(obj = "gContainerRGtk", toolkit = "guiWidgetsToolkitRGtk2", value = "guiWidget")}: ... }
    \item{.add}{\code{signature(obj = "gMenuRGtk", toolkit = "guiWidgetsToolkitRGtk2", value = "guiWidget")}: ... }
    \item{.add}{\code{signature(obj = "gNotebookRGtk", toolkit = "guiWidgetsToolkitRGtk2", value = "guiWidget")}: ... }
    \item{.add}{\code{signature(obj = "gTextRGtk", toolkit = "guiWidgetsToolkitRGtk2", value = "guiWidget")}: ... }
    \item{.delete}{\code{signature(obj = "gContainerRGtk", toolkit = "guiWidgetsToolkitRGtk2", widget = "guiWidget")}: ... }
    \item{.delete}{\code{signature(obj = "RGtkObject", toolkit = "guiWidgetsToolkitRGtk2", widget = "guiWidget")}: ... }
    \item{.delete}{\code{signature(obj = "gMenuRGtk", toolkit = "guiWidgetsToolkitRGtk2", widget = "guiWidget")}: ... }
    \item{.svalue<-}{\code{signature(obj = "gMenuRGtk", toolkit = "guiWidgetsToolkitRGtk2", value = "guiWidget")}: ... }
    \item{.tag}{\code{signature(obj = "guiWidget", toolkit = "guiWidgetsToolkitRGtk2")}: ... }
    \item{.tag<-}{\code{signature(obj = "guiWidget", toolkit = "guiWidgetsToolkitRGtk2")}: ... }
    \item{[}{\code{signature(x = "guiWidget")}: ... }
    \item{[<-}{\code{signature(x = "guiWidget")}: ... }
    \item{add3rdmousepopupmenu}{\code{signature(obj = "guiWidget")}: ... }
    \item{add}{\code{signature(obj = "guiWidget")}: ... }
    \item{addSpace}{\code{signature(obj = "guiWidget")}: ... }
    \item{addSpring}{\code{signature(obj = "guiWidget")}: ... }
    \item{adddropmotion}{\code{signature(obj = "guiWidget")}: ... }
    \item{adddropsource}{\code{signature(obj = "guiWidget")}: ... }
    \item{adddroptarget}{\code{signature(obj = "guiWidget")}: ... }
    \item{addhandlerchanged}{\code{signature(obj = "guiWidget")}: ... }
    \item{addhandlerclicked}{\code{signature(obj = "guiWidget")}: ... }
    \item{addhandlerdestroy}{\code{signature(obj = "guiWidget")}: ... }
    \item{addhandlerdoubleclick}{\code{signature(obj = "guiWidget")}: ... }
    \item{addhandlerexpose}{\code{signature(obj = "guiWidget")}: ... }
    \item{addhandleridle}{\code{signature(obj = "guiWidget")}: ... }
    \item{addhandlerkeystroke}{\code{signature(obj = "guiWidget")}: ... }
    \item{addhandlerrightclick}{\code{signature(obj = "guiWidget")}: ... }
    \item{addhandlerunrealize}{\code{signature(obj = "guiWidget")}: ... }
    \item{addpopupmenu}{\code{signature(obj = "guiWidget")}: ... }
    \item{defaultWidget}{\code{signature(obj = "guiWidget")}: ... }
    \item{defaultWidget<-}{\code{signature(obj = "guiWidget")}: ... }
    \item{delete}{\code{signature(obj = "guiWidget")}: ... }
    \item{dim}{\code{signature(x = "guiWidget")}: ... }
    \item{dimnames}{\code{signature(x = "guiWidget")}: ... }
    \item{dimnames<-}{\code{signature(x = "guiWidget")}: ... }
    \item{dispose}{\code{signature(obj = "guiWidget")}: ... }
    \item{editable}{\code{signature(obj = "guiWidget")}: ... }
    \item{editable<-}{\code{signature(obj = "guiWidget")}: ... }
    \item{enabled}{\code{signature(obj = "guiWidget")}: ... }
    \item{enabled<-}{\code{signature(obj = "guiWidget")}: ... }
    \item{focus}{\code{signature(obj = "guiWidget")}: ... }
    \item{focus<-}{\code{signature(obj = "guiWidget")}: ... }
    \item{tooltip<-}{\code{signature(obj = "guiWidget")}: ... }
    \item{font}{\code{signature(obj = "guiWidget")}: ... }
    \item{font<-}{\code{signature(obj = "guiWidget")}: ... }
    \item{id}{\code{signature(obj = "guiWidget")}: ... }
    \item{id<-}{\code{signature(obj = "guiWidget")}: ... }
    \item{length}{\code{signature(x = "guiWidget")}: ... }
    \item{names}{\code{signature(x = "guiWidget")}: ... }
    \item{names<-}{\code{signature(x = "guiWidget")}: ... }
    \item{removehandler}{\code{signature(obj = "guiWidget")}: ... }
    \item{size}{\code{signature(obj = "guiWidget")}: ... }
    \item{size<-}{\code{signature(obj = "guiWidget")}: ... }
    \item{svalue}{\code{signature(obj = "guiWidget")}: ... }
    \item{svalue<-}{\code{signature(obj = "guiWidget")}: ... }
    \item{tag}{\code{signature(obj = "guiWidget")}: ... }
    \item{tag<-}{\code{signature(obj = "guiWidget")}: ... }
    \item{update}{\code{signature(object = "guiWidget")}: ... }
    \item{visible}{\code{signature(obj = "guiWidget")}: ... }
    \item{visible<-}{\code{signature(obj = "guiWidget")}: ... }
    \item{isExtant}{\code{signature(obj = "guiWidget")}: ... }
	 }
}
\author{John Verzani}
\note{
	Within gWidgets there are three main subclasses guiContainers,
	guiComponents and guiDialogs. The distinctions are more clear
	at the toolkit level.


}
% \seealso{}
% \examples{}
\keyword{classes}
