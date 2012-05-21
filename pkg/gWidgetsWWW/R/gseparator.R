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

## hack to add separator
gseparator <- function(horizontal = TRUE, container = NULL, ...)  {
  if(is.null(container)) {
    obj <- "gseparator stub"
    class(obj) <- c("gSeparator", class(obj))
    return(obj)
  }
    
  if(horizontal)
    return(ghtml("<hr>",container=container))
  else
    return()
}

