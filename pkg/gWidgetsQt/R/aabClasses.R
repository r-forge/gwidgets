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


### these classes need to be defined before their subclasses. Alphabetical doesn't cut
### is so they go here.
## Need a collate function
## for coerce.with
setClassUnion("NULLorFunction",c("NULL","function"))

### this must come after aaaGenerics, as there gComponentQt is defined
setClass("gEditQt",
         contains="gComponentQt",
         prototype=prototype(new("gComponentQt"))
         )
setClass("gGroupQt",
         representation = representation("gContainerQt",
           horizontal="logical"),
         contains="gContainerQt",
         prototype=prototype(new("gContainerQt"))
         )

setClass("gWindowQt",
         representation = representation("gContainerQt"),
         ##           horizontal="logical"),
         contains="gContainerQt",
         prototype=prototype(new("gContainerQt"))
         )

setClass("gNotebookQt",
         representation = representation("gContainerQt",
           closebuttons="logical",
           dontCloseThese="numeric"),
         contains="gContainerQt"
         )

## Class to hold methods common to gdf and gtable
setClass("gTableViewQt",
         contains="gComponentQt",
         prototype=prototype(new("gComponentQt"))
         )


## widget for handling events
setClass("gEventWidgetQt",
         contains="gComponentQt",
         Prototype=prototype(new("gComponentQt"))
         )
