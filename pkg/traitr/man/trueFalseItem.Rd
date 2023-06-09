\name{trueFalseItem}
\alias{trueFalseItem}
\title{Item for Boolean values...}
\usage{trueFalseItem(value=TRUE, name, label=name, help="", tooltip="", attr,
    model, editor, ...)}
\description{Item for Boolean values}
\value{A \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.}
\seealso{\code{\link{Item}}}
\arguments{\item{value}{Default value for the model}
\item{name}{Required name for object. Names should be unique within a group of items}
\item{label}{Optional label, default value is the name}
\item{help}{Optional help string}
\item{tooltip}{Optional tooltip to display}
\item{attr}{A list of attributes to pass to widget on construction. Eg. attr=list(size=c(100,200))}
\item{model}{Optional model. Useful if one wishes to use same model for multiple views}
\item{editor}{Specification of editor (a view) to override default}
\item{...}{Passed to parent proto object during call to proto. The value \code{editor_style="compact"} will pass
this information to the editor causing it to render as a checkbox.}}
\examples{## basic usage
a <- trueFalseItem(TRUE, name="x")
a$get_x()
a$set_x(FALSE)
a$get_x()}
