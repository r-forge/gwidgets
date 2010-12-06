##  Copyright (C) 2010 John Verzani
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  A copy of the GNU General Public License is available at
##  http://www.r-project.org/Licenses/

## XXX no methods defined here!!!

##' Menubar implementation
##' 
##' @param menulist list of actions. Actions must have parent specified
##' @param popup ignored. Logical indicating if this is a popup widget
##' @param action parameterizes handler in action
##' @param container parent container
##' @param ... passed to add method of parent container
##' @export
gmenu <- function(menulist,  popup = FALSE, action=NULL, container = NULL,...) {

  if(popup) {
    warning("gmenu: popup is not implemented")
    return()
  }
  

  
  widget <- EXTComponent$new(toplevel=container$toplevel)
  class(widget) <- c("gMenu",class(widget))
  ## for menubar, we get ID not from adding, but directly
  widget$ID <- container$newID()

  widget$setValue(value=menulist)

  ## put into subwindow?
  if(inherits(container,"gSubwindow")) {
    widget$mbContainer <- container
  } else {
    widget$mbContainer <- widget$toplevel
  }

  widget$mbContainer$..menuBar <- widget

  widget$writeMenu <- function(., menulist= svalue(.), out) {
    ## write out menu in Ext tbar format
    ## this menu gets called recursively
    if(missing(out)) out <- String("[")
    
    for(i in names(menulist)) {
      data <- menulist[[i]]
      if(is(data,"gSeparator"))
        data <- list(separator=TRUE)
      
      if(inherits(data, "gAction")) {
        out <- out + data$asCharacter() + ','
      } else if(!is.null(data$separator)) {
        out <- out + '"-"' + ','
      } else if(!is.null(data$handler)) {
        ## make an action, add
        label <- ifelse(is.null(data$label), i, data$label)
        a <- gaction(label=label,
                     tooltip = ifelse(is.null(data$tooltip),label, data$tooltip),
                     icon = data$icon,
                     handler = data$handler,
                     parent = .$toplevel)
        out <- out + a$asCharacter() + ','
      } else if(is.list(data)) {
        ## recurse, wrap in text and {}
        out <- out + '{text:' + shQuote(i) + ', menu: ['
        out <- .$writeMenu(menulist = data, out)
        out <- out + '}' + ','
      } else {
        ## do nothing
      }
    }

    ## if ends in ',' chop
    out <- gsub(',$','',out)
    ## add in trailing "]"
    out <- out + ']'
    
    return(out)
  }

  invisible(widget)
}
  
  
