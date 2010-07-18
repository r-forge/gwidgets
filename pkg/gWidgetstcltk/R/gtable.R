## TODO:
## * issue with 1 col, space in values
## * use colnames to decide width
##
## NEED to make icons use #0 column, start with column 1 for others
## adjust

## table for selecting values from a data frame
## uses tree to show table

setClass("gTabletcltk",
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )


## some helper functions
.allChildren <- function(obj) {
  unlist(strsplit(tclvalue(tcl(getWidget(obj),"children",""))," "))
}

## covert a dta frame into a character based on
.toCharacter <- function(x,width,...) UseMethod(".toCharacter")
.toCharacter.default <- function(x,width,...) as.character(x)
.toCharacter.integer <- function(x,width,...) {
 if(missing(width)) width <- max(nchar(as.character(x))) + 2  
  format(x, justify="right", width=width)
}
.toCharacter.numeric <- function(x,width,...) {
  if(missing(width)) width <- max(nchar(as.character(x))) + 2
  format(x,trim=FALSE, width=width, justify="right")
}
.toCharacter.factor <- function(x,width,...) {
  if(missing(width)) width <- max(nchar(as.character(x))) + 2
  .toCharacter(as.character(x),width,...)
}
.toCharacter.logical <- function(x,width,...) {
  if(missing(width)) width <- 7
  format(as.character(x), justify="centre", width=width)
}
.toCharacter.data.frame <- function(x,width,...) {
  nms <- names(x)
  df <- as.data.frame(lapply(x,function(i) .toCharacter(i)),
                      stringsAsFactors=FALSE)
  names(df) <- nms
  return(df)
}
.toCharacter.matrix <- function(x, width, ...) {
  .toCharacter(as.data.frame(x), width, ...)
}


.populateTable <- function(tr, items, visible, icons=NULL, nms=names(items),
                           fresh=TRUE, doWidths=TRUE, getSizeFrom) {

  ## we load things row by row -- not by column like others
  ## we leave text value empty, saving spot for icon.
  ## How to adjust width?
  d <- dim(items); m <- d[1]; n <- d[2]


  ## Compute widths
  if(doWidths) {
    ## supposed to set widths based on widget, but isn't working?
    width <- getSizeFrom #as.integer(tclvalue(tkwinfo("width",getSizeFrom)))
    
    widths <- widthOfChar * .computeWidths(items)
    
    ## if(width > 10)
    ##   widths <- as.integer( widths/sum(widths) * width)
    
    
    ## first column
    ##XX  tcl(tr,"column","#0",width=widths[1], stretch=TRUE)
    ##  if(fresh)
    ##    tcl(tr,"configure",column=0)

    ## column #- is for icons

    ## icon column
    if(!is.null(icons)) 
      tcl(tr,"column","#0",width=32, stretch=FALSE)
    else
      tcl(tr,"column","#0",width=0, stretch=FALSE)
#    tcl(tr, "heading","#0",text="")
    
    if(fresh)
      tcl(tr,"column",0,width=1, stretch=FALSE) # override below if needed

    
    ## set widths/names of other columns if present
    for(j in 1:n) {
      tcl(tr,"column", j , width=widths[j], stretch=TRUE, anchor="w") 
      tcl(tr,"heading", j, text=nms[j])
    }
  }
  ## add values
  ## deal with visible
  visible <- rep(visible, length=m)
  items <- items[visible,,drop=FALSE]
  m <- dim(items)[1]

  ## if icons, we create
  if(!is.null(icons)) {
    icons <- sapply(icons,findIcon)
    if(length(icons) < m)
      icons <- c(icons, rep("", m - length(icons)))
  }

  if(m > 0) {
    sapply(1:m, function(i) {
      values <- as.character(unlist(items[i,]))
#      values <- paste("{",values,"}", sep="")  ## JV 9/9/09 -- was needed, why not now?
      if(!is.null(icons)) {
        tcl(tr,"insert","","end",
            values = values,
            image=icons[i] #, 
#            tags = .Tcl.args(list(background="red"))
            )
      } else {
        tcl(tr,"insert","","end",
            values = values)
      }
    })
  }
}

## clear the children. Should also remove row count
.clearColumns <- function(tr) {
  vals <- tcl(tr,"children","")
  tcl(tr,"delete", vals)
}
## compute widths needed from data.frame
.computeWidths <- function(d) {
  d <- as.data.frame(d)
  nms <- names(d)
  n <- dim(d)[2]
  sapply(1:n, function(j) max(10,sapply(c(nms[j],as.character(d[,j,drop=TRUE])), nchar)))
}

 ## constructor for selecting values from a data set -- not meant for editing
setMethod(".gtable",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   items,
                   multiple = FALSE,
                   chosencol = 1,                        # for drag and drop, value
                   icon.FUN = NULL,
                   filter.column = NULL,
                   filter.labels = NULL,
                   filter.FUN = NULL,   # two args gtable instance, filter.labels element
                   handler = NULL,
                   action = NULL,
                   container = NULL,
                   ...) {

            ## NOT IMPLEMENTED
            ## * sorting
            
            force(toolkit)

            if(is(container,"logical") && container)
              container = gwindow()
            if(!is(container,"guiWidget")) {
              warning("Container is not correct. No NULL containers possible\n" )
              return()
            }

            
            theArgs = list(...)

            
            ## we want a data frame for items
            if(missing(items))
              items <- data.frame(x=c(""),stringsAsFactors=FALSE)
            ## coerce items to a data frame
            if(!(inherits(items,"matrix") || inherits(items,"data.frame")))
              items <- data.frame(items=items, stringsAsFactors=FALSE)
            d <- dim(items); m <- d[1]; n <- d[2]
            

            ## if filtering we call a different constructor
            ## we are filtering if filter.FUN or filter.column is
            ## not null *UNLESS* filter.FUN = "manual"
            if((!is.null(filter.FUN) && is.function(filter.FUN ))
              || (is.null(filter.FUN) && !is.null(filter.column))) {
              obj <-
                .gtableWithFilter(toolkit,
                                  items,
                                  multiple,
                                  chosencol,   
                                  icon.FUN,
                                  filter.column,
                                  filter.labels,
                                  filter.FUN,
                                  handler,
                                  action,
                                  container,...)
              return(obj)
            }

            
            ## selectmode
            selectmode = if(multiple) "extended" else "browse"

            ##########
            ## setup widget
            tt <- getWidget(container)
            gp <- ttkframe(tt)
            
            width <- getWithDefault(theArgs$width, 500)
            height <- getWithDefault(theArgs$width, 300)
            tkconfigure(gp, width=width, height=height)
            
            xscr <- ttkscrollbar(gp, orient="horizontal",
                                 command=function(...) tkxview(tr,...))
            yscr <- ttkscrollbar(gp, orient="vertical",
                                 command=function(...) tkyview(tr,...))
            
           
            tr <- ttktreeview(gp, columns = 1:n,
                              displaycolumns=if(is.null(icon.FUN)) 1:n else "#all",
                              show = ifelse(is.null(icon.FUN), "headings", c("tree headings")),
                              selectmode = selectmode,
                              xscrollcommand=function(...) tkset(xscr,...),
                              yscrollcommand=function(...) tkset(yscr,...)
                              )

            ## pack into grid
            ## see tkFAQ 10.1 -- makes for automatic resizing
            tkgrid(tr,   row=0, column=0, sticky="news")
            tkgrid(yscr, row=0, column=1, sticky="ns")
            tkgrid(xscr, row=1, column=0, sticky="ew")
            tkgrid.columnconfigure(gp, 0, weight=1)
            tkgrid.rowconfigure(gp, 0, weight=1)
            tkpack(gp, expand=TRUE, fill="both") #???
            ## call in autoscroll
            do.autoscroll <- getWithDefault(theArgs$do.autoscroll, TRUE)
            if(do.autoscroll) {
              tcl("autoscroll", xscr)
            ##            tcl("autoscroll", yscr)
            }
            ##
            ######################
            
            obj = new("gTabletcltk",block=gp,widget=tr,
              toolkit=toolkit,ID=getNewID(), e = new.env())
            
            tag(obj,"icon.FUN") <- icon.FUN
            tag(obj,"chosencol") <- chosencol
            tag(obj,"color") = if(!is.null(theArgs$color))
              theArgs$color
            else
              "gray90"
            tag(obj,"colnamesColor") = if(!is.null(theArgs$colnamesColor))
              theArgs$colnamesColor
            else
              "red"
            tag(obj,"visible") <- NULL
            

       
            
            ## font -- fixed unless overridden
#            tkconfigure(tr, font="courier") # fixed
            
            
            ## add handler
            if (!is.null(handler)) {
              id = addhandlerchanged(obj,handler,action)
            }

            

            ## load data last to get size after adding
            tag(obj,"items") <- items
            icons <- if(is.null(icon.FUN))
              NULL
            else
              icon.FUN(items)

            size(obj) <- c(width, height)
            
            .populateTable(tr, .toCharacter(items), TRUE, icons,names(items),
                           getSizeFrom=width)

            ## add to container -- do after populating so widths are set
            add(container, obj,...)
            


            return(obj)
            
          })


## incorporate chosenval here
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gTabletcltk"),
          function(obj, toolkit, index=NULL, drop=NULL,...) {

            widget = getWidget(obj)

            sel <- unlist(strsplit(tclvalue(tcl(widget,"selection"))," "))
            if(length(sel) == 0) {
              return(NA)                # check proper return
            }
            theChildren <- .allChildren(widget)
            indices <- sapply(sel, function(i) match(i, theChildren))
            ##which(sel == theChildren)

            inds <- which(visible(obj))[indices]
            if(!is.null(index) && index == TRUE) {
              return(inds)              # oops, had index
            }

            
            ## Now a value
            if(missing(drop) || is.null(drop))
              drop = TRUE               # default is to drop unless asked not to

            chosencol <- tag(obj,"chosencol")

            if(drop)
              return(obj[inds, chosencol,drop=drop])
            else
              return(obj[inds, ])
          })


setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gTabletcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {

                   widget <- getWidget(obj)
                   theChildren <- .allChildren(widget)
                   
                   if(!is.null(index) && index) {
                     ## set by index
                     tcl(widget,"selection","set",theChildren[value])
                   } else {
                     ## set value if present
                     ## need to update this for our hack to handle data frames
                     items = tag(obj,"items")
                     m = match(value,items[,tag(obj,"chosencol"),drop=TRUE])
                     
                     if(!is.na(m)) {    # NA is nomatch
                       tcl(widget,"selection","set",theChildren[m])
                     } 
                   }
                   return(obj)
                 })


## retrieve values
setMethod("[",
          signature(x="gTabletcltk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop) 
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gTabletcltk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            items = tag(x,"items")
            if(missing(j)) j = 1:ncol(items)
            return(items[i,j, drop=drop])
          })
            

## XXX -- harder one
## do [,]; [i,], [,j] (no new row, column); [i,j] no new value
## replace values
setReplaceMethod("[",
                 signature(x="gTabletcltk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })
setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gTabletcltk"),
          function(x, toolkit, i, j, ..., value) {

            widget <- getWidget(x)
            items <- tag(x,"items")
            icon.FUN <- tag(x,"icon.FUN")
            if(is.null(icon.FUN))
              icon.FUN <- function(x) NULL
            theArgs <- list(...)
            if(is.null(theArgs$doVisible))
              tag(x,"visible") <- NULL
            
            ## what to do
            ## main case [,] -- populate
            if(missing(i) && missing(j)) {
              ## replace entire thing
              .clearColumns(widget)
              items <- as.data.frame(value, stringsAsFactors=FALSE)
              tag(x,"items") <- items

              width <- as.integer(tclvalue(tkwinfo("width",widget)))
              .populateTable(widget, .toCharacter(items), visible(x),
                             icon.FUN(items), names(items),fresh=FALSE, doWidths=FALSE,
                             getSizeFrom=size(x)[1])
              return(x)
            }

            d <- dim(x)
            ## error check
            if(missing(i)) {
              if(max(j) > dim(x)[2]) {
                cat(gettext("Can't add columns. Use [,]<-\n"))
                return()
              }
              i <- 1:d[1]
            } else if(missing(j)) {
              if(max(i) > dim(x)[1]) {
                cat(gettext("Can't add rows. Use [,]<-\n"))
                return()
              }
              j <- 1:d[2]
            }

            ## size is okay
            items[i,j] <- value
            tag(x,"items") <- items     # set
            citems <- .toCharacter(items)
            allChildren <- .allChildren(widget)

            ## add row by row (i)
            for(ind in  1:length(i)) {
              ## add one at a time, don't redo icon
              ## might be able to speed up (value=unlist(citems[ind])
              ## This doesn't redo icons!
              sapply(1:length(j), function(k) {
                vals <- citems[ind,j[k],drop=FALSE]
                tcl(widget,"set",allChildren[ind], j[k], unlist(vals))
              })
            }
            
            return(x)
          })


## dim
setMethod(".dim",
          signature(toolkit="guiWidgetsToolkittcltk",x="gTabletcltk"),
          function(x, toolkit) {
            dim(tag(x,"items"))
          })
## length
setMethod(".length",
          signature(toolkit="guiWidgetsToolkittcltk",x="gTabletcltk"),
          function(x, toolkit) {
            length(tag(x,"items"))
          })

setMethod(".visible",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gTabletcltk"),
          function(obj, toolkit, set=TRUE, ...) {
            visible <- tag(obj,"visible")
            if(is.null(visible))
              visible <- rep(TRUE, dim(obj)[1])
            return(visible)
          })

setReplaceMethod(".visible",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gTabletcltk"),
                 function(obj,toolkit, ..., value) {
                   d <- dim(obj)
                   value <- rep(value, length=d[1]) # recycle!
                   tag(obj,"visible") <- value

                   ## now redraw
                   obj[,,doVisible=TRUE] <- tag(obj,"items")
                   return(obj)
                 })

setMethod(".names",
          signature(toolkit="guiWidgetsToolkittcltk",x="gTabletcltk"),
          function(x, toolkit) {
            widget <- getWidget(x)
            d <- dim(x); n <- d[2]
            nms <- sapply(1:n,function(j)
                          tclvalue(tcl(widget,"heading",j,"-text")))
            unlist(nms)
          })

setReplaceMethod(".names",
                 signature(x="gTabletcltk"),
                 function(x,toolkit, value) {
                   widget <- getWidget(x)
                   d <- dim(x); n <- d[2]
                   if(length(value) != n) {
                     cat(gettext("names<- must match length\n"))
                     return(x)
                   }
                   sapply(1:n,function(j) tcl(widget,"heading",j,"text"=value[j]))
                   return(x)
                 })

##' set size
##'
##' Width setting is hacked in if value is a list, 
##' we convert to pixel size so this should be related to the number of characters
##' @param value either a numeric vector with 1 or 2 values to set
##' width [height] or A list with 1 or 2 components. First is a
##' numeric vector of length n to set widths of each column, second is
##' number vector of size 1 to set number of rows shown
setReplaceMethod(".size", 
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gTabletcltk"),
                 function(obj, toolkit, ..., value) {
                   if(!is.list(value)) {
                     ## set width -- value in pixels
                     tkconfigure(getBlock(obj), width=value[1])
                     if(length(value) > 2) {
                       tkconfigure(getBlock(obj), height=value[2])
                     }
                   } else {
                     newWidths <- value[[1]]
                     d <- dim(obj); m <- d[1]; n <- d[2]
                     newWidths <- rep(newWidths, length=n) # recycle
                       
                     widths <- widthOfChar * newWidths
                     sapply(1:n, function(j) {
                       tcl(getWidget(obj), "column", j-1, width=widths[j])
                     })
                     
                     ## set height -- value in pixels for total widget
                     if(length(value) >= 2)
                       tkconfigure(getWidget(obj), height = as.integer(value[2]))
                   }
                   return(obj)
                 })


## handlers

setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gTabletcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandlerdoubleclick(obj, handler, action,...)
          })

## when a selection is changed
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gTabletcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addHandler(obj,toolkit,signal="<<TreeviewSelect>>", handler, action,...)
          })

## pretty print table
.prettyPrintTable = function(x, do.names = TRUE, justify="left") {

  ## the columns, a matrix
  if(is.matrix(x)) x = as.data.frame(x, stringsAsFactors = FALSE)
  
  y = sapply(x, function(i) format(i, justify=justify))

  if(do.names) {
    n = names(x)
    y = rbind(n,y)
    for(j in 1:ncol(y))
      y[,j] = format(y[,j], justify=justify)
  }
  
  z = sapply(1:nrow(y), function(i) paste(y[i,],sep="", collapse=" "))

  return(z)
}


##################################################
##################################################
### for filtering


## table for selecting values
## most methods in gdf.R inherited from gGrid class
setClass("gTableWithFiltertcltk",
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )


setGeneric(".gtableWithFilter",
           function(toolkit,
                    items,
                    multiple = FALSE,
                    chosencol = 1,                        # for drag and drop, value
                    icon.FUN = NULL,
                    filter.column = NULL,
                    filter.labels = NULL,
                    filter.FUN = NULL,   # two args gtable instance, filter.labels element
                    handler = NULL,
                    action = NULL,
                    container = NULL,
                    ...)
           standardGeneric(".gtableWithFilter")
           )

setMethod(".gtableWithFilter",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   items,
                   multiple = FALSE,
                   chosencol = 1,                        # for drag and drop, value
                   icon.FUN = NULL,
                   filter.column = NULL,
                   filter.labels = NULL,
                   filter.FUN = NULL,   # two args gtable instance, filter.labels element
                   handler = NULL,
                   action = NULL,
                   container = NULL,
                   ...) {
            
            ## we only get here *if* we are filtering
 

            g = ggroup(horizontal=FALSE, container=container, ...)

            fg = ggroup(cont=g)
            filterByLabel = glabel("Filter by:", container=fg)
            filterPopup = gdroplist(c(""), container=fg)
            
            tbl = gtable(items,
              multiple=multiple,
              chosencol=chosencol,
              cont=g, expand=TRUE)

            

            
            ## make an object to return
            obj = new("gTableWithFiltertcltk",block=g,widget=tbl,
              toolkit=toolkit,ID=getNewID())

            tag(obj, "allItems") <- items
            tag(obj, "tbl") <- tbl
            tag(obj, "filterPopup") <- filterPopup
            tag(obj, "filterByLabel") <- filterByLabel

            
            ## one of filter.column or filter.fun is non-NULL
            if(is.null(filter.FUN)) {
              ## define filter.FUN
              filter.FUN = function(DF, filterBy) {
                if(filterBy == "") return(rep(TRUE,nrow(DF)))
                inds = as.character(DF[,filter.column]) == filterBy
              }
              
              ## set up droplist
              filterPopup[] <- c("",sort(unique(as.character(items[,filter.column]))))
              svalue(filterByLabel) <- paste("Filter by",names(items)[filter.column],"==",sep=" ", collapse=" ")
            } else {
              ## set up droplist
              filterPopup[] <- c("",filter.labels)
            }

            tag(obj,"filter.FUN") <- filter.FUN

            ## get obj from scoping
            addHandlerChanged(filterPopup,action=obj,
                              handler=function(h,...) {
                                DF = tag(obj, "allItems")
                                tbl = tag(obj,"tbl")
                                filter.fun = tag(obj,"filter.FUN")
                                fval = svalue(h$obj) # popup

                                inds = filter.FUN(DF, fval)
                                ## update  tbl
                                obj[,] <- DF[inds,,drop=FALSE]
                                ## but keep allItems
                                tag(obj,"allItems") <- DF
                              })
            ## add handler to gtable object, but pass in override for methods
            if(!is.null(handler)) 
             ID= addhandlerchanged(tbl,handler,action,actualobj=obj,...)
            
            return(obj)
          })


          

setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gTableWithFiltertcltk"),
          function(obj, toolkit, index=NULL, drop=NULL,...) {

            if(!is.null(index) && index) {
              gwCat("The index refers to the visible data value, not the entire data frame\n")
            }

            return(svalue(obj@widget, toolkit=toolkit, index=index, drop=drop, ...))

          })

## refers to visible
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gTableWithFiltertcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {

                   tbl = tag(obj,"tbl")
                   svalue(tbl, toolkit=toolkit, index=index,  ...) <- value

                   return(obj)
                 })


## retrieve values
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gTableWithFiltertcltk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            tbl = tag(x,"tbl")
                                        # dot function
            .leftBracket(tbl, toolkit, i, j, ..., drop=drop)
          })
            
setMethod("[",
          signature(x="gTableWithFiltertcltk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop) 
          })
## replace values
setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gTableWithFiltertcltk"),
          function(x, toolkit, i, j, ..., value) {
            if(!missing(i) || !missing(j)) {
              gwCat(gettext("[<- only replaces the entire object. Try obj[,]<-value\n"))
              return(x)
            }

            ## underlying gtable object
            tbl = tag(x,"tbl")

            ## We have to a) update allItems, b) update table
            tag(x, "allItems") <- value
            ## tbl needs to be filtered
            DF = value
            fval = svalue(tag(x, "filterPopup"))
            if(fval == "") {
              tbl[,] <- DF
            } else {
              filter.FUN = tag(x,"filter.FUN")
              inds = filter.FUN(DF, fval)
              tbl[,] <- DF[inds,,drop=FALSE]
            }
              

            return(x)
           })

setReplaceMethod("[",
                 signature(x="gTableWithFiltertcltk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

## dim
setMethod(".dim",
          signature(toolkit="guiWidgetsToolkittcltk",x="gTableWithFiltertcltk"),
          function(x, toolkit) {
            tbl = tag(x,"tbl")
            return(dim(tbl))
          })
## length
setMethod(".length",
          signature(toolkit="guiWidgetsToolkittcltk",x="gTableWithFiltertcltk"),
          function(x, toolkit) {
            tbl = tag(x,"tbl")
            return(length(tbl))
          })

## size<- work on tr
setReplaceMethod(".size", 
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gTableWithFiltertcltk"),
                 function(obj, toolkit, ..., value) {
                   tbl = tag(obj,"tbl")
                   size(tbl) <- value
                   return(obj)
                 })

## handlers

setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gTableWithFiltertcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            tbl = tag(obj,"tbl")
            .addhandlerdoubleclick(tbl, toolkit, handler, action,actualobj=obj)
          })

## same as changed
setMethod(".addhandlerdoubleclick",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gTableWithFiltertcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerchanged(obj, toolkit, handler, action,...)
          })

## when a selection is changed
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gTableWithFiltertcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            tbl = tag(obj,"tbl")
            .addHandler(tbl,toolkit,signal="<<ListboxSelect>>", handler, action,
                        actualobj=obj)
          })


         
         
         

