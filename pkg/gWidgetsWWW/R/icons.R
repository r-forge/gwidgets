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

addStockIcons <- function(iconNames, iconFiles) {
  ## Files is a url
  si <- getStockIcons()
  for(i in 1:length(iconNames))
    si[iconNames[i]] <- iconFiles[i]
  .stockicons$si <- si
}

## return list of icons
## Assumes the files in images are installed in base URL
## otherwise paste in prefix.
.stockicons <- proto()
.stockicons$si <- NULL

getStockIcons <- function(icons) {
  gWidgetsWWWimageUrl <- getOption("gWidgetsWWWimageUrl")
  if(is.null(gWidgetsWWWimageUrl))
    gWidgetsWWWimageUrl <- "/custom/gw/images"
  
  if(is.null(.stockicons$si)) {
    files <- list.files(path = system.file(paste("basehtml","images",sep=.Platform$file.sep),
                          package = "gWidgetsWWW"))
    newfiles <- gsub("\\.gif$|\\.jpg$|\\.jpeg$|\\.png$","",files)
    ##XX was <<- below
    si <<- paste(gWidgetsWWWimageUrl,strip_slashes(files), sep="/")
    class(si) <- c("URL",class(si))
    names(si) <- newfiles
    .stockicons$si <- si
  }

  if(missing(icons))
    return(.stockicons$si)
  else
    return(.stockicons$si[icons])
}


