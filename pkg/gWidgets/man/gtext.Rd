\name{gtext}
\alias{gtext}
\title{Constructor for widget for multi-line editable text input}
\description{
	The
	\code{gtext} widget creates a text buffer for handling multiple lines
	of text. 
}
\usage{
gtext (text = NULL, width = NULL, height = 300, font.attr = NULL, 
    wrap = TRUE, handler = NULL, action = NULL, container = NULL, 
    ..., toolkit = guiToolkit()) 
}
\arguments{
  \item{text}{Initial text in widget}
  \item{width}{Width of widget in pixels}
  \item{height}{Height of gtext widget in pixels}
  \item{font.attr}{Optional specification of font attributes}
  \item{wrap}{For gtext, are long lines wrapped?}
  \item{handler}{Handler called when text is changed.}
  \item{action}{ Passed to handler}
  \item{container}{Optional container to attach widget to}
  \item{\dots}{Passed to add method of container}
  \item{toolkit}{Which GUI toolkit to use}
}
\details{
	The \code{gtext} widget has the following methods.

	The \code{svalue} method returns the text held in the
	buffer. If \code{drop=TRUE}, then only the text in the
	currently selection is returned.

	The \code{svalue<-} method replaces the text in the buffer
	with the new text.

	New text is added with the \code{insert} method. The basic
	usage is \code{insert(obj,text)} where "text" could be a
	single line or a vector of text, or --for gWidgetsRGtk2 -- a
	gwidget (although some, like gedit, are kind of flaky). Extra
	arguments include \code{do.newline} a logical indicating if a
	new line after the last line should be added (default is
	\code{TRUE}); \code{font.attr} to specify any font attributes;
	\code{where} indicating where to add the text (either
	\code{end} or \code{beginning}). The \code{insert} generic
	replaces the overused \code{add} for \code{gtext}, but
	\code{add} will still work.

	The font can be changed. The \code{font.attr} argument to the
	constructor is used to specify font properties for the
	buffer. When specifed to the \code{add} method, the font
	specification applies to the new text. Both uses use a named
	character vector to specify the font properties. For instance
	\code{c(style="normal", weights="bold",sizes="medium")}.  The
	command \code{obj[['tags']]} will produce a list containing
	all the available attributes.

	The \code{font<-} method is used to change the font of the
	currently selected text. It too takes a named character vector
	specifying the font attributes. If there is no currently selected text, the 
	entire buffer will have the new font attribute.

	The \code{dispose} method clears the text in the buffer.

	The \code{addHandlerKeystroke} method for \code{gedit}and
	\code{gtext} is called for 
	each keystroke. In \code{gtext} or \code{RGtk2} the component
	\code{key} of the \code{h} argument contains the keystroke.

      }
% \value{}
% \references{}
% \author{}
% \note{}
% \seealso{}
\examples{
\dontrun{
        ## gtext example
        obj <- gtext("First line", container=gwindow())
	insert(obj,"second line", font.attr=c(family="monospace"))
        insert(obj,"third line", font.attr=c(foreground.colors="red"))
}
}
\keyword{interface }
