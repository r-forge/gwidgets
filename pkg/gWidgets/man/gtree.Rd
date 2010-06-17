\name{gtree}
\alias{gtree}
\title{Constructor for widget to display heirarchical dta}
\description{
	This widget allows tree-like data to be presented. Each node
	on the tree should be a data frame with the same column
	structure. The first column is treated like a key, and should
	be unique. Offspring are specified through a function of the
	keys which are ancestors. This function returns the data frame
	to be displayed. Values in the tree can be selected with the
	mouse. This value can be retrieved through a method, or a
	handler can be assigned to double click events.
}
\usage{
gtree(offspring = NULL, hasOffspring = NULL, offspring.data = NULL,
col.types = NULL, icon.FUN = NULL, chosencol = 1, multiple = FALSE,
handler = NULL, action = NULL, container = NULL, ..., toolkit = guiToolkit())
}
\arguments{
  \item{offspring}{A function to produce a data frame. 

  The first column of the data frame is used as a key. It should be
  unique, otherwise the updating will not work properly.

  The \code{offspring} function has two arguments, the first being the
  path (the first column of the offspring data frame is the key, and
  the path is the vector of keys) and the value of
  \code{offspring.data}. The data frame can determine  whether an
  entry has offspring, by having the second column be a logical
  vector, \code{TRUE} if there is offspring, \code{FALSE} if not.
 
 }
  \item{hasOffspring}{Whether an entry has an offspring is determined
  by a) if this function is non-\code{NULL} and it returns a
  \code{TRUE} value when called on the offspring data frame for this
  row, b) if the second column of the offspring data frame is a
  logical vector and for this row is \code{TRUE}. If this function is
  \code{NULL} and the second column is not a logical vector then it is
  assumed that there are no offspring.}

  \item{offspring.data}{Passed to \code{offspring} function to
  parameterize that function.}

  \item{col.types}{Used to determine the type of column, given as a
  data frame with 1 or more rows. Otherwise it
  is determined by first row of offspring data frame. If this can
  return an empty data frame, then this argument should be given.}

  \item{icon.FUN}{An optional function to determine an icon place into
  the first column. This function gets called with the data in
  \code{offspring}, and should return a row vector of length
  \code{nrow(offspring)}. The icons are stock icons, and should be
  referenced by name. The helper function \code{getStockIcons} list
  all the available stock icons.}

  \item{chosencol}{The column used when requesting the selected row's
  value. Defaults to first} 

  \item{multiple}{ A logical to determine if multiple selection is
  allowed. Defaults to \code{FALSE}}
  \item{handler}{ Handler for double click events}
  \item{action}{ Passed to handler }
  \item{container}{Optional container to attach widget to.}
  \item{...}{Passed to \code{add} method of container}
  \item{toolkit}{Which GUI toolkit to use}  
}
\details{
	In an abstract sense, these trees are specified by a function
  which produces the value at a given node from the ancestry of the
  given node, and a function specifying if a node has offspring.

  The \code{offspring} function determines the displayed data for a
  certain node. It has signature \code{(path, offspring.data)}, where
  the path consists of the ancestors and \code{offspring.data} is an
  optional value passed in when the tree object is constructed. This
  function returns a data frame. Its first column should consist of
  unique values, as it is treated like a key.

  The \code{hasOffspring} function is called on the return value of
  \code{offspring}. It should return a logical indicating which rows
  have offspring. If this argument is not present, then the second
  column of the return values of \code{offspring} are consulted. If
  these are logical, then they are used to determine if offspring are
  present. Otherwise, no offspring are assumed.

  The \code{icon.FUN} function is called on the return value of
  \code{offspring}. If present, it should return a vector of stock
  icon names.

  The \code{svalue} method returns the current key. If \code{index} is
  set to a column number, that column's value will be returned.

  The \code{"["} method refers to the vector of keys for the selected object.
  
  The \code{addhandlerdoubleclick} handler can be set to respond to
  a double click event.
}
% \value{}
% \references{}
% \author{}
% \note{}
% \seealso{}
\examples{
\dontrun{
  ## function to find offspring
  offspring <- function(path, user.data=NULL) {
    if(length(path) > 0) 
      directory <- paste(getwd(),"/",paste(path,sep="/", collapse=""),sep="",collapse="")
    else
      directory <- getwd()
    files <- file.info(dir(path=directory))[,c(2,1,3)]
     
    files <- cbind(rownames(files), files)
    names(files)[1] <- "filename"
    return(files)
 }
 hasOffspring <- function(children,user.data=NULL, ...) {
   return(children$isdir)
 }
 icon.FUN <- function(children,user.data=NULL, ...) {
   x <- rep("file", length=nrow(children))
   x[children$isdir] <- "directory"
   return(x)
  }
  ## shows isdir directory, as hasOffspring is specified
  w <- gwindow("test with isdir showing")
  gtree(offspring, hasOffspring, icon.FUN = icon.FUN, container=w)

  		   ## does not show isdir directory, as hasOffspring=NULL and
  ## second column is a logical
  w <- gwindow("tree test no dir column")
  gtree(offspring, hasOffspring=NULL, icon.FUN = icon.FUN, container=w)
}
}

\keyword{interface}% at least one, from doc/KEYWORDS
