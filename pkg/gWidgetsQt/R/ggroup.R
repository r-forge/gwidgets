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


setClass("gGroupQt",
         contains="gContainerQt",
         prototype=prototype(new("gContainerQt"))
         )

## constructor
setMethod(".ggroup",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   horizontal = TRUE, spacing = 5,
                   use.scrollwindow = FALSE, 
                   container = NULL, ... 
                   ) {

            force(toolkit)
            
            spacing <- getWithDefault(spacing, 0)
            theArgs <- list(...)                   # raise.on.dragmotion
            raise.on.dragmotion <- getWithDefault(theArgs$raise.on.dragmotion, FALSE)
            
            if(as.logical(horizontal))
              gp <- Qt$QHBoxLayout()
            else
              gp <- Qt$QVBoxLayout()

            gp$setMargin(0L)
            
            ## put layout into a wudget
            gw <- Qt$QWidget()
            gw$setLayout(gp)

            obj <- new("gGroupQt",block=gw, widget=gp, toolkit=toolkit,            
                       e=new.env(), ID=getNewID()  
                       )

            tag(obj, "default_fill") <- ifelse(horizontal, "y", "x") # orthogonal to expansion

            
            ## implement scrollbars if asked. 
            if(use.scrollwindow == TRUE) {
              ## the size of the object to be scrolled must be pre-allocated and QWidget
              ## has no default size. (The image example with Qt does!). Here we put in some large sizes, but
              ## the user can adjust via width, height hidden arguments
              width <- getWithDefault(theArgs$width, 500L)
              height <- getWithDefault(theArgs$height, 2500L)
              gw$minimumWidth <- width
              gw$minimumHeight <- height


              ## QT Docs: Note that You must add the layout of widget
              ## before you call this function; if you add it later,
              ## the widget will not be visible - regardless of when
              ## you show() the scroll area. In this case, you can
              ## also not show() the widget later.

              ## widgetResizable : bool This property holds whether
              ## the scroll area should resize the view widget. If
              ## this property is set to false (the default), the
              ## scroll area honors the size of its widget. Regardless
              ## of this property, you can programmatically resize the
              ## widget using widget()->resize(), and the scroll area
              ## will automatically adjust itself to the new size. If
              ## this property is set to true, the scroll area will
              ## automatically resize the widget in order to avoid
              ## scroll bars where they can be avoided, or to take
              ## advantage of extra space.

              ## When using a scroll area to display the contents of a
              ## custom widget, it is important to ensure that the
              ## size hint of the child widget is set to a suitable
              ## value. If a standard QWidget is used for the child
              ## widget, it may be necessary to call
              ## QWidget::setMinimumSize() to ensure that the contents
              ## of the widget are shown correctly within the scroll
              ## area.
              ## 
              ## If a scroll area is used to display the
              ## contents of a widget that contains child widgets
              ## arranged in a layout, it is important to realize that
              ## the size policy of the layout will also determine the
              ## size of the widget. This is especially useful to know
              ## if you intend to dynamically change the contents of
              ## the layout. In such cases, setting the layout's size
              ## constraint property to one which provides constraints
              ## on the minimum and/or maximum size of the layout
              ## (e.g., QLayout::SetMinAndMaxSize) will cause the size
              ## of the scroll area to be updated whenever the
              ## contents of the layout changes.
              sb <- Qt$QScrollArea()
              sb$setWidget(gw)


              obj@block <- sb
            }

            tag(obj, "horizontal") <- as.logical(horizontal)

            svalue(obj) <- spacing

            ## raise if we drag across
            if(as.logical(raise.on.dragmotion)) {
              
            }

            if(!is.null(container))
              add(container, obj, ...)
            
            return(obj)
          })


##################################################
## methods
## padding
## Have two choices: setContentsMargin -- like padding in tcltk
## and setSpacing -- like padx in tcltk
setReplaceMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gGroupQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ..., value) {
            lyt <- getWidget(obj)
            lyt$setSpacing(as.integer(value))
            
            return(obj)
          })

