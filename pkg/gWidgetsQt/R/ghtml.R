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

## begin
setClass("gHtmlQt",
         contains="gTextWidgetQt",
         prototype=prototype(new("gTextWidgetQt"))
         )

##' basic html display widget
##' Simply passes off to gtext and sets readonlyggo
setMethod(".ghtml",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   x,
                   handler = NULL, action=NULL,
                   container=NULL, ...) {

            if(length(x) == 1) {
              isURL <- length(grep("^(ftp|http|file)://", x)) > 0L
              if(isURL)
                x <- readLines(x)
            }

            ## pass off to gtext
            obj <- .gtext(toolkit=toolkit, text=x, handler=handler, action=action,
                          container=container, ...)
            w <- getWidget(obj)
            w$setReadOnly(TRUE)

            invisible(obj)
          })
