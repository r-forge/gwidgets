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

QtPredefinedIcons <- c("question"=Qt$QMessageBox$Question,
                       "info"=Qt$QMessageBox$Information,
                       "warning"=Qt$QMessageBox$Warning,
                       "error"=Qt$QMessageBox$Critical)

## Dialogs
setMethod(".gmessage",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   message,
                   title = "message",
                   icon = c("info","warning","error","question"),
                   parent = NULL,
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {

            if(!is.null(parent)) {
              toplevel <- getTopLevel(parent)
              parentw <- getBlock(toplevel)
              mb <- Qt$QMessageBox(parentw)
            } else {
              mb <- Qt$QMessageBox()
            }

            

            

            mb$setWindowTitle(title)
            mb$setText(message[1])
            if(length(message) >= 2)
              mb$setInformativeText(message[2])

            icon = match.arg(icon)
            mb$setIcon(QtPredefinedIcons[icon])

            mb$setStandardButtons(Qt$QMessageBox$Ok) 

            ret <- mb$exec()
            return(TRUE)
            
          })
  
## if OK then run handler, else not
setMethod(".gconfirm",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   message,
                   title = "Confirm",
                   icon = c("info", "warning", "error", "question"),
                   parent = NULL,
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {


            if(!is.null(parent)) {
              toplevel <- getTopLevel(parent)
              parentw <- getBlock(toplevel)
              mb <- Qt$QMessageBox(parentw)
            } else {
              mb <- Qt$QMessageBox()
            }

            
            mb$setWindowTitle(title)
            mb$setText(message[1])
            if(length(message) >= 2)
              mb$setInformativeText(message[2])

            icon = match.arg(icon)
            mb$setIcon(QtPredefinedIcons[icon])

            mb$setStandardButtons(Qt$QMessageBox$Ok | Qt$QMessageBox$Cancel)

            ret <- mb$exec()
            if(ret == 1024 ) {
              ## OK, runhandler, return TRUE
              h <- list(action=action)
              if(!is.null(handler))
                handler(h)
              return(TRUE)
            } else {
              ## cancel
              return(FALSE)
            }

            
          })

 
## Add input to the above
## h,... in handler has componets action, input (for value)
setMethod(".ginput",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   message,             # meesage
                   text = "",           # initial text
                   title = "Input",
                   icon = c("info","warning","error","question"),
                   parent = NULL,
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {

            ## Add icon?
            
            widget <- ggroup(horizontal=FALSE)
            l <- glabel(message, cont=widget)
            font(l) <- c(weight="bold")
            
            e <- gedit(text, cont=widget)

            d <- .gbasicdialognoparent(toolkit, title, parent)
            add(d, widget, expand=TRUE)

            ret <- as.logical(visible(d, TRUE))
            if(ret) {
              if(!is.null(handler))
                handler(list(obj=e, action=action))
              val <- svalue(e)
            } else {
              val <- NA
            }
            return(val)
          })

## add a widget to the dialog. This is modal
## see next one for one that gets called here
setMethod(".gbasicdialog",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   title = "Dialog",
                   widget,
                   parent = NULL,
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {

            d <- .gbasicdialognoparent(toolkit, title, parent, handler, action, ...)
            add(d, widget, expand=TRUE)

            ret <- visible(d, TRUE)
            return(ret)
          })

## with no paret
setClass("gBasicDialogNoParentQt",
         contains="gContainerQt",
         prototype=prototype(new("gContainerQt"))
         )

setMethod(".gbasicdialognoparent",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   title = "Dialog",
                   parent=NULL,                   
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {

            if(!is.null(parent)) {
              toplevel <- getTopLevel(parent)
              parentw <- getBlock(toplevel)
              dlg <- Qt$QDialog(parentw)
            } else {
              dlg <- Qt$QDialog()
            }
            lyt <- Qt$QVBoxLayout()
            dlg$setLayout(lyt)
            
            

            
            obj <- new("gBasicDialogNoParentQt", block=dlg, widget=lyt,
                       toolkit=toolkit,
                       e=new.env(), ID=getNewID()  
                       )

            tag(obj,"handler") <- handler
            tag(obj,"action") <- action
            
            return(obj) 
          })


setMethod(".add",
          signature=c(toolkit="guiWidgetsToolkitQt",
            obj="gBasicDialogNoParentQt", value="guiWidget"),
          function(obj, toolkit, value, ...) {
            .add(obj, toolkit, value@widget)
          })

setMethod(".add",
          signature=c(toolkit="guiWidgetsToolkitQt",
            obj="gBasicDialogNoParentQt", value="gWidgetQt"),
          function(obj, toolkit, value, ...) {
            tag(obj, "widget") <- value
            callNextMethod()
          })

## Dispose method to delete dialog
setMethod(".dispose",
          signature=c(toolkit="guiWidgetsToolkitQt",
            obj="gBasicDialogNoParentQt"),
          function(obj, toolkit,...) {
             dlg <- getBlock(obj)
             dlg$done(Qt$QMessageBox$Ok)
          })

## visible(dlg, TRUE), not visible(dlg) <- TRUE
setMethod(".visible",
                 signature(toolkit="guiWidgetsToolkitQt",
                           obj="gBasicDialogNoParentQt"),
                 function(obj, toolkit, set=NULL,  ...) {
                   ## Pass in do.buttons=FALSE to hide buttons
                   
                   
                   if(is.null(set) || !is.logical(set) || !set)
                     return(NULL)       # no call

                   
                   dlg <- getBlock(obj)
                   ans <- FALSE
                   
                   handler <- tag(obj,"handler")
                   action <- tag(obj,"action")
                   widget <- tag(obj,"widget")
                   
                   args <- list(...)
                   ## means to bypass buttons. Need dispose(dlg) in some handler
                   if(getWithDefault(args$do.buttons, TRUE)) {
                     buttonGroup = ggroup(cont=obj, expand=FALSE)
                     addSpring(buttonGroup)
                     ans <<- FALSE
                     Cancelbutton = gbutton("Cancel",cont=buttonGroup,
                       handler=function(h,...) {
                         ans <<- FALSE
                         dlg$done(Qt$QMessageBox$Cancel)
                       })
                     addSpace(buttonGroup, 12)
                     OKbutton = gbutton("OK",cont=buttonGroup,
                       handler=function(h,...) {
                         ans <<- TRUE
                         dlg$done(Qt$QMessageBox$Ok)
                       })
                     defaultWidget(OKbutton)
                   }
                   ## make modal
                   dlg$exec()

                   ## process request
                   if(ans) {
                     ## yes
                     if(!is.null(handler)) {
                       handler(list(obj=widget,action=action))
                     }
                     return(ans)
                   } else {
                     ## no
                     return(ans)
                   }
                   
                   ## else nothing
                   return(NA)
                 })




## alert
setMethod(".galert",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   message,
                   title = "message",
                   delay = 3,
                   parent=NULL,
                   ...
                   ) {
            force(toolkit)
            
            w <- gwindow(title, width=250, height=50, parent = parent)
            g <- ggroup(cont = w)
            l <- glabel("  ", cont = g)
            label <- glabel(message, cont = g, expand=TRUE)
            font(label) <- c("weight"="bold")

            gimage("cancel", dirname="stock", cont = g, handler = function(h,...) dispose(h$action), action=w)
#            gbutton("cancel", cont = g, handler = function(h,...) dispose(w))
#            addHandlerMouseMotion(label, handler = function(h,...) dispose(w))
            ## close after a delay
            timer <- Qt$QTimer()
            timer$setSingleShot(TRUE)
            qconnect(timer, "timeout", function(user.data) dispose(user.data), user.data=w)
            timer$start(as.integer(delay*1000))         # in ms

          })
