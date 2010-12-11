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


## glayout
## use TableLayout  -- need algorithm to write out widgets


glayout <- function(homogeneous = FALSE, spacing = 5, # 10 is too big here
                    container = NULL, ...) {



  tbl <- EXTContainer$new(toplevel=container$toplevel,
                          ..spacing = as.numeric(spacing),
                          ..noRows = 0, ..noCols = 0
                         )
  class(tbl) <- c("gLayout",class(tbl))

  ## methods
  tbl$setValues <- function(.,i,j,...,value) {
    ## if character make a glabel object
    if(is.character(value))
      value <- glabel(value, cont = .)
    
    value$..tblLocationRow <- i
    value$..tblLocationCol <- j
    value$..tblSeen <- FALSE
    .$..noRows <- max(i,.$..noRows)
    .$..noCols <- max(j,.$..noCols)

    theArgs <- list(...)
    ## anchor
    if(exists("..style",envir=value,inherits=FALSE))
      style <- value$..style
    else
      style <- c()
    if(!is.null(anchor <- theArgs$anchor)) {
      if(anchor[1] == -1)
        style["text-align"] = "left"
      else if(anchor[1] == 0)
        style["text-align"] = "center"
      else if(anchor[1] == 1)
        style["text-align"] = "right"
      if(anchor[2] == -1)
        style["text-valign"] = "bottom"
      else if(anchor[2] == 0)
        style["text-valign"] = "center"
      else if(anchor[2] == 1)
        style["text-valign"] = "top"
      
    }
    value$..style <- style
  }

  ## css to style alignment
  tbl$footer <- function(.) {
    out <- paste("Ext.util.CSS.createStyleSheet('",
                 ".td-northwest {vertical-align: top; align: left} ",
                 ".td-north {vertical-align: top} ",
                 ".td-northeast {vertical-align: top; align: right} ",
                 ".td-west {align: left} ",
                 ".td-center {} ",
                 ".td-east {align: right} ",
                 ".td-southwest {vertical-align: bottom; align: left} ",
                 ".td-south {vertical-align: bottom} ",
                 ".td-southeast {vertical-align: bottom; align: right} ",
                 "');",
                 sep="")
    return(out)
  }

  tbl$x.hidden <- FALSE
  tbl$ExtConstructor <- "Ext.Panel" ## inherits
  tbl$ExtCfgOptions <- function(.) { ## ih
#    defaults <- String('{') +
#      'bodyStyle:"padding:' + .$..spacing + 'px"}'
    defaults <- list(bodyStyle = sprintf("padding:%spx", .$..spacing))
    layoutConfig <- list(columns=.$..noCols,
                         align="left",
                         valign="top")

      String('{') +
      'columns:' + .$..noCols + '}'
    
    out <- list(layout="table",
                defaults = defaults,
                border=FALSE,
                layoutConfig = layoutConfig
                )
    return(out)
   
  }

  tbl$makeItemsFixedItems <- "border:false,"
  tbl$makeItems <- function(.) {
    ## helper function
    mapAnchorToCSSClass <- function(anchor) {
      if(is.null(anchor))
        return("td-northwest")

      out <-
        if(anchor[2] == 1) {
          if(anchor[1] == -1)
            "td-northwest"
          else if(anchor[1] == 0)
            "td-north"
          else
            "td-northeast"
        } else if(anchor[2] == 0) {
          if(anchor[1] == -1)
            "td-west"
          else if(anchor[1] == 0)
            "td-center"
          else
            "td-east"
        } else if(anchor[2] == -1) {
          if(anchor[1] == -1)
            "td-southwest"
          else if(anchor[1] == 0)
            "td-south"
          else
            "td-southeast"
        }
    }
        


    ## key to this is a simple algorithm which could be optimized
    ## but likely isn't worth the trouble

    children <- .$children; n <- length(children)
    if(n == 0) {
      return("")
    }

    sapply(children, function(i) i$..tblSeen <- FALSE)
    
    allRows <- lapply(children, function(i) i$..tblLocationRow)
    allCols <- lapply(children, function(i) i$..tblLocationCol)

    items <- list(); ctr <- 1
    for(row in 1:.$..noRows) {
      for(col in 1:.$..noCols) {
        gotOne <- FALSE
        for(i in 1:n) {
          if(row %in% allRows[[i]] && col %in% allCols[[i]]) {
            if(children[[i]]$..tblSeen) {
              gotOne <- TRUE
              break
            } else {
              child <- children[[i]]
              items[[ctr]] <- list(rowspan=length(allRows[[i]]),
                                   colspan=length(allCols[[i]]),obj=child)
              ctr <- ctr + 1
              child$..tblSeen <- TRUE
              gotOne <- TRUE
              break
            }
          }
        }
        if(!gotOne) {
          items[[ctr]] <- list(rowspan=1, colspan = 1, obj = NULL)
          ctr <- ctr+1
        }
      }
    }

    tmp <- lapply(items, function(i) {
      if(is.null(i$obj)) {
        contentEl <- String('html:"&nbsp;"')
      } else {
        contentEl <- String('contentEl:') + shQuote(i$obj$ID)
      }

      out <- String(.$makeItemsFixedItems)

      if(i$rowspan > 1 && i$colspan > 1) {
        out <- out +
          'rowspan:' + i$rowspan + ',' +
            'colspan:' + i$colspan + ',' +
              contentEl
      } else if(i$rowspan > 1) {
        out <- out +
          'rowspan:' + i$rowspan + ',' +
            contentEl
      } else if(i$colspan > 1) {
        out <- out +
          'colspan:' + i$colspan + ',' +
            contentEl
      } else {
        out <- out + contentEl
      }
      out <- out + "," + sprintf("cellCls:'%s'", mapAnchorToCSSClass(i$anchor))
      
      return(out)
    })

    out <- paste('{',tmp,'}',sep="",collapse=",")
    return(out)
  }
        
  
  container$add(tbl,...)
  invisible(tbl)
  
}

