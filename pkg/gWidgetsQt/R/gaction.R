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


## reusuabel chunk of code
## begin
setClass("gActionQt",
         contains="gComponentQt",
         prototype=prototype(new("gComponentQt"))
         )


setMethod(".gaction",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   label,
                   tooltip = NULL,
                   icon = NULL,
                   key.accel = NULL,
                   handler = NULL, action = NULL,
                   parent=NULL,
                   ...) {
            
            force(toolkit)

            ## QAcation needs a parent
            ## we use the same environment to store this
            if(is.null(parent))
              parent <- Qt$QWidget()
            else
              parent <- getBlock(parent)
            a <- Qt$QAction(label, parent)

            if(!is.null(tooltip))
              a$setToolTip(tooltip)
            ## icon -- take for stock if not specified
            if(is.null(icon))
              icon <- label
            icon <- getStockIconFromName(icon)
            if(!is.null(icon))
              a$setIcon(icon)

            if(!is.null(key.accel)) {
              ks <- Qt$QKeySequence(key.accel)
              a$setShortcut(ks)         # not working??
            }
            if(!is.null(handler))
              qconnect(a, "triggered", function(h,...) {
                if(!is.list(h))
                  h <- ..1
                handler(h)
              }, user.data=list(obj=a, action=action))

            obj <- new("gActionQt", block=a, widget = a,
                       toolkit=toolkit)
            
            return(obj)
          })



setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gActionQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            w <- getWidget(obj)
            w$text
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gActionQt"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   w <- getWidget(obj)
                   w$setText(as.character(value))
                   
                   return(obj)
                 })
