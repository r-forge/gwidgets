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


## add calendar widget: shoule I have gcalendar, gcalendarbrowser?
## no handler function, can add to entry object with addhandler

setClass("gCalendarQt",
         representation = representation("gComponentQt",
           format="character"),
         contains="gEditQt",
         prototype=prototype(new("gEditQt"))
         )


setMethod(".gcalendar",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   text="",
                   format="%Y-%m-%d",
                   handler = NULL, action=NULL,
                   container=NULL,...) {

            force(toolkit)

            if(text == "" && format != "")
              text = format(Sys.Date(), format)
            text = as.character(text)


            
            cal <- Qt$QCalendarWidget()

            day <- try(as.Date(text), silent=TRUE)
            if(!inherits(day, "try-error")) {
              yr <- as.integer(format(day,"%Y"))
              mo <- as.integer(format(day,"%m"))
              dy <- as.integer(format(day,"%d"))
              cal$setSelectedDate(Qt$QDate(yr, mo, dy))
            }
            

            obj <- new("gCalendarQt", block=cal, widget=cal, toolkit=toolkit,            
            e=new.env(), ID=getNewID()  
            )

            tag(obj, "format") <- format

            if(!is.null(container))
              add(container, obj, ...)
            
            return(obj)
          })


setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gCalendarQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            w <- getWidget(obj)
            date <- w$selectedDate
            day <- c(date$year(),
                     date$month(),
                     date$day())
            val <- format(paste(day, collapse="-"), format=tag(obj, "format"))
            return(val)
          })


## change handler if date selected changes
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitQt",obj="gCalendarQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="selectionChanged", handler, action)
          })

