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

##' calendar widget
##'
##' @param text date as text
##' @param format formate of date
##' @param handler handler called when date changed
##' @param action action passed to handler
##' @param container parent container
##' @param ... passed to \code{add} method of container
##' @export
gcalendar <- function(text = "", format = "%Y-%m-%d",
                      handler=NULL, action=NULL, container = NULL, ... ) {


    widget <- EXTComponentNoItems$new(toplevel=container$toplevel,
                               ..format = format)
    class(widget) <- c("gCalendar",class(widget))

    widget$extDateFormat <- "%a %b %d %Y %H:%M:%S"
    
    if(text != "") {
      tmp <- as.Date(text, widget$..format)
      if(!is.na(tmp))
        text <- format(tmp, widget$extDateFormat)
    }
    
    widget$setValue(value=text)           # no day
    widget$getValueJSMethod <- "getValue"
    widget$transportSignal <- c("change")
    ## coerceValues calls ..format

    widget$coerceValues <- function(., value) {
      ## Wed Jun 11 2008 00:00:00 GMT-0400 (EDT) -- ext format
      theDate = as.Date(value,.$extDateFormat)
      if(is.na(theDate))
        as.Date(value, .$..format)
      else
        format(theDate,.$..format)
    }
      
      
    
    ## override writeConstructor of show method
    widget$writeConstructor <- function(.) {
      lst <- list(xtype = "datefield",
                  
                  id =  as.character(String(.$ID) + "date"))
      if(is.na(.$getValue()) || .$getValue() == "") {
        lst['emptyText'] <- "Select a date..."
      } else {
        lst['emptyText'] <- format(as.Date(.$..data, .$extDateFormat),"%m/%d/%Y")
        lst['value'] <- String('new Date("') + .$..data +'")'#.$..text,
      }


      ## size doesn't work here, as we the style thing isn't
      ## applied to 
      if(exists("..width",envir = .,inherits=FALSE))
        lst[["width"]] <- .$..width
      else
        lst[["width"]] <- "auto"

      if(exists("..height",envir = .,inherits=FALSE))
        lst[["height"]] <- .$..height
      else
        lst[["height"]] <- "auto"
      
      out <- String() +
        paste(sprintf("%sdate = new Ext.Panel({\n", .$asCharacter()),
              sprintf("id: '%s',", .$ID),
              sprintf("renderTo: %s,",.$toplevel$..renderTo),
              sprintf("items:[%s]", .$mapRtoObjectLiteral(lst)),
              "});\n",
              sep="")

      out <- out +
        sprintf("%sdate.addClass('x-hidden');\n", .$asCharacter())
      
      out <- out +
        sprintf("%s = %sdate.getComponent(0);\n", .$asCharacter(), .$asCharacter())
      return(out)
    }
        

    container$add(widget,...)

      
    if(!is.null(handler))
      widget$addHandlerChanged(handler, action=action)
    
    
    invisible(widget)
  }

