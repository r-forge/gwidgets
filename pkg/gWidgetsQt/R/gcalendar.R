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
           format="character",
           cal="QCalendarWidget"
           ),
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

            block <- Qt$QWidget()
            edit <- Qt$QLineEdit()
            edit$setSizePolicy(Qt$QSizePolicy$Expanding, Qt$QSizePolicy$Fixed)
            btn <- Qt$QPushButton("select...")

            cal <- Qt$QCalendarWidget()
            

            lyt <- Qt$QHBoxLayout()
            lyt$addWidget(edit)
            lyt$addWidget(btn)
            block$setLayout(lyt)



            
            ## set date uf possible
            day <- try(as.Date(text), silent=TRUE)
            if(!inherits(day, "try-error")) {
              yr <- as.integer(format(day,"%Y"))
              mo <- as.integer(format(day,"%m"))
              dy <- as.integer(format(day,"%d"))
              cal$setSelectedDate(Qt$QDate(yr, mo, dy))
            }
            

            obj <- new("gCalendarQt", block=block, widget=edit,
                       format=format, cal=cal,
                       toolkit=toolkit,            
                       e=new.env(), ID=getNewID()  
            )
            
            ## add handler to btn
            qconnect(btn, "clicked", function() {
              cal$show()
            })
            qconnect(cal, "activated", function(date) {
              yr <- date$year()
              mo <- date$month()
              dy <- date$day()

              svalue(obj) <- format(paste(c(yr, mo, dy), collapse="-"), format=format)
                     
              cal$hide()
            })

            

            
            if(!is.null(container))
              add(container, obj, ...)
            
            return(obj)
          })


## svalue<-
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gCalendarQt"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   w <- getWidget(obj)
                   txt <- format(value, format=obj@format)
                   w$setText(txt)

                   day <- try(as.Date(txt), silent=TRUE)
                   if(!inherits(day, "try-error")) {
                     yr <- as.integer(format(day,"%Y"))
                     mo <- as.integer(format(day,"%m"))
                     dy <- as.integer(format(day,"%d"))
                     cal <- obj@cal
                     cal$setSelectedDate(Qt$QDate(yr, mo, dy))
                   }
                   
                   
                   return(obj)
          })

