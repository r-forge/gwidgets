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
  if(!exists("gWidgetsWWWimageUrl") || is.null(gWidgetsWWWimageUrl))
    gWidgetsWWWimageUrl <- "/images/"
  
  if(is.null(.stockicons$si)) {
    files <- list.files(path = system.file(paste("basehtml","images",sep=.Platform$file.sep),
                          package = "gWidgetsWWW"))
    newfiles <- gsub("\\.gif$|\\.jpg$|\\.jpeg$|\\.png$","",files)
    si <<- paste(gWidgetsWWWimageUrl,files, sep="")
    class(si) <- c("URL",class(si))
    names(si) <- newfiles
    .stockicons$si <- si
  }

  if(missing(icons))
    return(.stockicons$si)
  else
    return(.stockicons$si[icons])
}


