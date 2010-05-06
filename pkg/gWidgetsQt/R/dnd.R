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

## we would need to implement events for each widget to add
## dnd beyond the standard. As such we punt.
setMethod(".adddropsource",
          signature(toolkit="guiWidgetsToolkitQt", obj="gWidgetQt"),
          function(obj, toolkit, targetType="text",
                   handler=NULL, action=NULL, ...) {
            w <- getWidget(obj)
            w$setDragEnabled(TRUE)
          })

setMethod(".adddroptarget",
          signature(toolkit="guiWidgetsToolkitQt", obj="gWidgetQt"),
          function(obj, toolkit, targetType="text",
                   handler=NULL, action=NULL, ...) {
            w <- getWidget(obj)
            w$setAcceptDrops(TRUE)
          })

setMethod(".adddropmotion",
          signature(toolkit="guiWidgetsToolkitQt", obj="gWidgetQt"),
          function(obj, toolkit, 
                   handler=NULL, action=NULL, ...) {
            ## no such
          })
                    
