\name{ggenericwidget}
\alias{ggenericwidget}
%%\alias{autogenerategeneric}
%%\alias{geditlist}
%%\alias{geditnamedlist}
%%\alias{gvariables}

\title{A constructor to create widgets for evaluating functions}
\description{
  This constructor creates a widget for collecting arguments for a
  function using a list to define the widget's components. When called
  with a function name a list is created on the fly which can be used as
  is, or modified as desired.
}
\usage{
ggenericwidget(lst,  cli = NULL, container = NULL, ..., toolkit = guiToolkit())
}

%% also could do:
%autogenerategeneric(f, help = fName, variableType = NULL)

%geditlist(...) 

%geditnamedlist(...)

%gvariables(variableType = NULL, ...) 

\arguments{
  \item{lst}{Either a list defining the widget or a function name as a
    string. In the latter case, the defining list may be retrieved by
    the \code{svalue} method.}
%%  \item{variableType}{Type of variable. One of
%%  \code{c("univariate","univariatetable", "bivariate", "model",
%%    "lattice")}}
  \code{x=} for initial variable name.
  \item{cli}{An instance of \code{gcommandline} or NULL. If NULL, then a
  new command line pops up in its own window}
  \item{container}{Optional container to attach widget to}
%%  \item{f}{Name of function}
%%  \item{help}{What man page should be used to look up arguments to function}
  \item{\dots}{Currently ignored by \code{ggenericwidget}, but passed
    along to \code{gedit} by \code{geditlist} and \code{geditnamedlist}}
  \item{toolkit}{Which GUI toolkit to use}
}
\details{
  
  This widget provides an easy way to create dialogs that collect the
  arguments for a function evaluation. When the OK button is clicked,
  the arguments are collected and passed along to the function specified
  via the \code{action} part of the list. When collecting the arguments,
  empty strings are not passed along. 

  The easiest usage is to simply provide a function name and have
  \code{autogenerategeneric} take a stab. However, in the long run it
  might be better to use \code{autogenerategeneric} to create an initial
  list, and then modify this to adjust the widget's look.

  
  The list contains several named components
  \describe{
  \item{title}{The title for the widget}
  \item{help}{What help page is called}
  \item{type}{Either "text" or "graphic." Currently ignored.}
  \item{variableType}{Describes the type of variable. Either
    "univariate", "univariatetable","fileurl","bivariate", "model",
    "lattice", "lmer" or \code{NULL}. 
    This value is passed directly to \code{gvariables}.
    For non-NULL values, the widget shows an
    appropriate area for collecting the main variable. For the
    model and lattice interfaces buttons allow editing of fields
    by subsequent dialogs. }
  \item{variableTypeExtras}{An optional list with components \code{name}
    and \code{value} containing a name and value passed along to the
    constructor for the variable type. Useful to override default
  }
  \item{assignto}{If TRUE, creates box for collecting name for
    assigning output}
  \item{action}{a list with named components \code{beginning}
    and \code{ending}. The arguments are collected and pasted
    together to form a string containing the R command to
    execute. These get put at the beginning and end of the
    string. A typical pair would be something like "prop.test("
    and ")".}
  \item{arguments}{a list with named components. In the simplest
    usage the names are argument names, and the components are
    lists with entries that create the corresponding widget. The
    first such component is called \code{type} and is the name of
    a gWidget, such as \code{"gradio"}. Subsequent components are
    passed to this function using \code{do.call}.

    The constructors \code{geditlist} and \code{geditnamedlist} can be
    used when the input is to be a list of values or a list of named values.
    
    In the more complicated cases, these named components can be
    grouped into a list component. The name of this is then used
    to block the arguments. See the example.
  }
  }	   

  The \code{svalue} method returns the value of the list. This can be
  used to retrieve the list that is created when the constructor is
  called with a function name.
  
}
% \value{}
% \references{}
% \author{}
\note{This function may be improved and the list defining it changed.}
%\seealso{}
\examples{
\dontrun{
	## a sample list definition
## Save some typing by defining a list to be used more than once later
TRUE.list <- list(
  type = "gradio",
  items = c("TRUE","FALSE")
  )

## define a list for producing a histogram widget	  
hist.list <- list(
  title = "hist()",
  help = "hist",
  action = list(
    beginning = "hist(",
    ending = ")"
    ),
  type = "graphic",                      # either text or graphic
  variableType = "univariate",           # single variable
  arguments = list(
    adjustments = list(
      breaks= list(
        type="gdroplist",
        items=c("\"Sturges\"","\"Scott\"","\"Friedman-Diaconis\"")
        ),
      probability = TRUE.list,
      include.lowest = TRUE.list,
      right = TRUE.list,
      shading = list(
        density = list(
            type="gedit",
            text=NULL
        ),
      angle = list(
        type="gedit",
        coerce.with="as.numeric",
        text="45"
        )
        )
      )
    )
  )

ggenericwidget(hist.list, container=TRUE)
## or to autogenerate one
ggenericwidget("boxplot.default", container=TRUE)
	
}
}
\keyword{interface}
