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

## creates a notebook interface tohandle plots
setClass("gGraphicsNotebookQt",
         representation=representation(
           width="numeric",height="numeric"
           ),
         contains="gNotebookQt",
         prototype=prototype(new("gNotebookQt"))
         )
setMethod(".ggraphicsnotebook",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   width=dpi*6, height=dpi*6,dpi=75,
                   container = NULL,
                   ...) {
            ## ... passed onto gnotebook


            
            force(toolkit)
            
            return(.glabel(toolkit, "No ggraphics available in gWidgetsQt (yet)", cont=container))
            
          })
          
