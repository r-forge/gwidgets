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

## handle icons

.qtIcons <- new.env()

## make icons from stock icons

## add to stock icons
setMethod(".addStockIcons",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit, iconNames, iconFiles, ...) {
            iconNames <- tolower(iconNames) # normalize
            for(i in seq_along(iconNames)) {
              .qtIcons[[iconNames[i]]] <- Qt$QIcon(iconFiles[[i]])
            }
          })

## get icons
setMethod(".getStockIcons",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit) {
            ## return list from env
            sapply(ls(.qtIcons), function(i) .qtIcons[[i]])
          })

## convenience functions
getStockIconFromName <- function(name) {
  if(!missing(name) && is.character(name) && nchar(name) > 0)
    .qtIcons[[tolower(name[1]), exact=TRUE]]
  else
    NULL
}

addgWidgetsIcons <- function() {
  dir <- system.file("images", package="gWidgets")
  iconFiles <- list.files(path=dir, pattern="gif$", full.names=TRUE)
  iconNames <- basename(iconFiles)
  iconNames <- gsub("\\.gif", "", iconNames)

  for(i in seq_along(iconNames)) {
    .qtIcons[[iconNames[i]]] <- Qt$QIcon(iconFiles[[i]])
  }
}
  
