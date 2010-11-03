##################################################
### Gdf
## from Ben Goodrich's fair script who credits:

## This function is modified from tcltk2:::tk2edit, which is
## Copyright 2007 Jeffrey J. Hallman and originally licensed
## under LGPL V3+

## tcltk2:::tk2edit by Jeffrey Hallman


## gGrid cover gDf and gTable
setClass("gGridtcltk",
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )
setClass("gDftcltk",
         contains="gGridtcltk",
         prototype=prototype(new("gComponenttcltk"))
         )


## constructor for editing a data frame
setMethod(".gdf",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   items = NULL,
                   name = deparse(substitute(items)),
                   do.subset = FALSE,
                   container=NULL,...)  {

            force(toolkit)

            ## process ...
            theArgs <- list(...)

            fontsize <- theArgs$fontsize
            if(is.null(fontsize))
              fontsize <- 12

            colors = theArgs$colors
            if(is.null(colors))
              colors = c(
                bg = "navajo white",fg = "black",
                rbg = "white smoke",rfg="red"
                )


            ## check if we can actually use this widget
            if (!inherits(tclRequire("Tktable", warn = FALSE), "tclObj")) {
              return(glabel("Tktable must be installed in tcl (not R).\n tktable.sourceforge.net", cont = container))
            }
            .Tcl(paste("option add *Table.font {courier", 12, "bold}"))
            old <- options(scipen = 7)
            on.exit(options(old))

            ## rebind the Backspace key, which somehow gets messed up
            string <- paste("bind Table <BackSpace> {",
                            "set ::tk::table::Priv(junk) [%W icursor]",
                            "if {[string compare {} $::tk::table::Priv(junk)] && $::tk::table::Priv(junk)} {",
                            "%W delete active [expr {$::tk::table::Priv(junk)-1}]",
                            "}}", sep="\n")
            .Tcl(string)

            ## x
            x = items
            if(!is.data.frame(x))
              x = as.data.frame(x, stringsAsFactors = FALSE)


            ## fix up table
            d = dim(x)

            if(is(container,"logical") && container)
              container = gwindow()
            if(!is(container,"guiWidget")) {
              warning("Container is not correct. No NULL containers possible\n" )
              return()
            }
            
            tt = getWidget(container)
            parent = ttkframe(tt)

            cmat <- toCharacterMatrix(x, rNames=rownames(x), cNames=colnames(x))
            tA <- tclArray()
            fillTclArrayFromCharMat(tA, cmat)

            ## XXX Need to fix widths of columns -- names too
            tktable <- tkwidget(parent, "table", variable = tA,
                                rows = nrow(cmat), cols = ncol(cmat),
                                titlerows = 1, titlecols = 1, # no or rows (cols) for titles
                                selecttitle = 1,    # can edit titles (1 or 0)
                                anchor = "e",
                                multiline = FALSE,   # display newlines with new lines
                                selectmode = "extended", ## not extended
                                rowseparator = dQuote("\n"), # getcontrols display of selection
                                colseparator = dQuote("\t"),
                                colstretchmode="last",
                                background = colors['bg'],
                                foreground = colors['fg']
                                )
            ## pack in scrollbars
            addScrollbarsToWidget(tktable, parent)

            ## add bindings (cf tkTable.tcl in source)
            ## do with .Tcl, otherwise it adds to current binding instead of replacing.
            .Tcl("bind Table <Return>		{::tk::table::MoveCell %W 1 0}" )            
#            tkbind(tktable,"<Return>", function(W) tcl("::tk::table::MoveCell",W,1,0))
            tkbind(tktable,"<Tab>", function(W) tcl("::tk::table::MoveCell",W,0,1))
            
            
            ## new object
            obj = new("gDftcltk",block=parent, widget=tktable,
              toolkit=toolkit, ID=getNewID(), e = new.env())


            tag(obj, "tA") <- tA
            ## set the classes
            tag(obj,"classes") <- sapply(x, function(i) head(class(i)))

            .gDfaddPopupMenu(obj)

            
            ## add to container
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj, ...)
            }

            return(obj)
            
          })


##
####################################################



## gWidget methods
setReplaceMethod(".size",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gGridtcltk"),
          function(obj, toolkit,  ..., value) {
            width = as.integer(value[1])
            height = as.integer(value[2])
            ## size
            tkconfigure(getWidget(obj),
                        maxwidth=width,
                        maxheight=height)
            
            return(obj)
          })



## data frame methods
## get selected value
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gGridtcltk"),
          function(obj, toolkit, index=NULL, drop=NULL,...) {

            tktable = getWidget(obj)

            selPresent <- as.logical(as.numeric(tclvalue(tcl(tktable, "selection","present"))))
            if(!selPresent)
              return(NA)

            ## top col x, bottom col y
            curSel <- sapply(strsplit(as.character(tcl(tktable,"curselection")),","), as.numeric)
            indices <- list(rows=sort(unique(curSel[1,])), columns=sort(unique(curSel[2,])))
            drop <- getWithDefault(drop, TRUE)
            index <- getWithDefault(index, FALSE)
            
            if(index) {
              if(drop) {
                return(indices$rows)
              } else {
                return(indices)
              }
            } else {
              return(obj[indices$rows, indices$columns, drop=drop])
            }
          })
          
          
## set by index value selected value
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gGridtcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   tktable <- getWidget(obj)
                   if(!is.null(index) && index == FALSE) {
                     return(obj)
                   } else if(!is.numeric(value) || !is.list(value)) {
                     ## not a value. Clear
                     tcl(tktable, "selection", "clear", "all")                     
                     return(obj)
                   } else {
                     d <- dim(obj); m <- d[1]; n <- d[2]
                     if(!is.list(value)) {
                       ## assume values is rows
                       value <- list(rows=value, columns=seq_len(n))
                     }
                     tcl(tktable, "selection", "clear", "all")
                     for(i in value[[1]])
                       for(j in value[[2]])
                         if(1 <= i & i <= m & 1 <= j & j <= n)
                           tcl(tktable, "selection", "set", sprintf("%s,%s", i,j))
                   }
                   return(obj)
                 })


## refers to the entire data frame
## index returned by svalue(index=T) works here
setMethod("[",
          signature(x="gGridtcltk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j,..., drop=drop)
          })

setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gGridtcltk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {

            theArgs = list(...)
            

            tktable <- getWidget(x)
            tA <- tag(x, "tA")
            classes <- tag(x, "classes")

            df <- tclArrayToDataFrame(tA, tktable, classes)

            d <- dim(x)
            
            if(missing(i))
              i <- 1:d[1]
            if(missing(j))
              j <- 1:(d[2])
            if(is.logical(i))
              i <- which(i)
            if(is.logical(j))
              j <- which(j)

            
            ## now return
            return(df[i,j, drop=drop])

          })

## [<-
setReplaceMethod("[",
                 signature(x="gGridtcltk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j,...) <- value
                   return(x)
                 })


setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gGridtcltk"),
          function(x, toolkit, i, j, ..., value) {

            tktable <- getWidget(x)
            tA <- tag(x,"tA")
            
            ## change indices
            d = dim(x)
            value <- toCharacterMatrix(value)
            dv = dim(value)

            ## adust indices
            if(missing(i))
              i <- 1:d[1]
            if(missing(j))
              j <- 1:d[2]
            if(is.logical(i)) i = which(i)
            if(is.logical(j)) j = which(j)

            
            ## is value right size?
            if(dv[1] != length(i))
              stop("Value has different number of rows than the replacement area")
            if(dv[2] != length(j))
              stop("Value has different number of columns than the replacement area")

            
            for(l in 1:length(j)) {
              for(k in 1:length(i)) {
                tA[[i[k], j[l] ]] <- as.tclObj(value[k,l], drop = TRUE)
              }
            }
            return(x)
            
          })
                 
## data frame like
setMethod(".dim", 
          signature(toolkit="guiWidgetsToolkittcltk",x="gGridtcltk"),
          function(x,toolkit) {

            tktable <- getWidget(x)
            d <- tkindex(tktable, "end") # get size from tktable
            d <- as.numeric(unlist(strsplit(as.character(d), ",")))

            return(d)
          })

setMethod(".length",
          signature(toolkit="guiWidgetsToolkittcltk",x="gGridtcltk"),
          function(x,toolkit) return(dim(x)[2]))


## no dimnames for gGrid, only names
setMethod(".dimnames",
          signature(toolkit="guiWidgetsToolkittcltk",x="gGridtcltk"),
          function(x,toolkit) {
            tktable <- getWidget(x)

            toVector <- function(i) sapply(i, function(j) paste(j, collapse=" "))
            
            d <- dim(x)
            dimnames <- list(rownames=make.row.names(toVector(sapply(1:d[1], function(i) tktable.get(tktable, i, 0)))),
                             colnames=toVector(sapply(1:d[2], function(j) tktable.get(tktable, 0, j))))
            dimnames
          })
          

setReplaceMethod(".dimnames",
                 signature(toolkit="guiWidgetsToolkittcltk",x="gGridtcltk"),
                 function(x, toolkit,  value) {
                   
                   if(!is.list(value))
                     stop("value is a list with first element the row names, and second the column names")
                   rnames = make.row.names(value[[1]])
                   cnames = value[[2]]
                   d = dim(x)
                   if(is.null(rnames) || length(rnames) != d[1])
                     stop("Row names are the wrong size")
                   if(is.null(cnames) || length(cnames) != (d[2]))
                     stop("Column names are the wrong size")

                   tktable <- getWidget(x)
                   tA <- tag(x, "tA")

                                      
                   ## set column names
                   names(x) <- cnames

                   ## set row names
                   sapply(1:d[1], function(i) tA[[i,0]] <- as.tclObj(rnames[i], drop = TRUE))
                          
                   return(x)
                 })


setMethod(".names",
          signature(toolkit="guiWidgetsToolkittcltk",x="gGridtcltk"),
          function(x, toolkit) {
            dimnames(x)[[2]]
          })


setReplaceMethod(".names",
                 signature(toolkit="guiWidgetsToolkittcltk",x="gGridtcltk"),
                 function(x, toolkit, value) {

                   d <- dim(x)
                   tA <- tag(x, "tA")
                   
                   ## set row names
                   sapply(1:d[1], function(j) tA[[0, j]] <- as.tclObj(value[j], drop = TRUE))

                   return(x)
                 })

##################################################

.gDfaddPopupMenu <- function(obj) {
  ## global variables to record row, column of menu popup
  x0 <- NA; y0 <- NA
  tktable <- getWidget(obj)
  
  menu <- tkmenu(tktable)

  insert <- function(x,ind, y) {
    if(ind == 0) {
      c(y,x)
    } else if(ind >= length(x)) {
      c(x,y)
    } else {
      c(x[1:(ind-1)], y, x[ind:length(x)])
    }
  }
  
  ## return row, column of popup area
  getWhere <-  function() {
    where <- paste("@",x0,",",y0, sep="")
    ind <- tcl(tktable,"index",where)
    ind <- as.numeric(unlist(strsplit(as.character(ind),",")))
    ind
  }

  ## GUI to write expression to evaluate to fill in column
  transformVariable <- function(col) {
    ## obj is main object
    w <- gwindow(gettext("Transform variable"), parent=obj,width=300, height=400)
    g <- ggroup(horizontal=FALSE, cont = w)
    glabel("To transform a variable you define a function body.", cont = g)
    glabel("You can use 'x' for the data frame and the column names.", cont = g)
    glabel("", cont = g)
    glabel("function(x) {", cont=g)
    glabel("\twith(x, {", cont = g)
    out <- gtext("", cont = g); size(out) <- c(300,100)
    glabel("\t})", cont = g)
    glabel("}", cont = g)
##    gseparator(cont =g, expand=TRUE)
    bg <- ggroup(cont = g)
    cancelButton <- gbutton("cancel", cont = bg, handler = function(h,...) dispose(w))
    okButton <- gbutton("ok", cont = bg, handler = function(h,...) {
      str <- paste("x <- obj[,]",
                   "f <- function(x) { with(x,{",
                   svalue(out),
                   "})}",
                   "f(x)",
                   sep="\n", collapse="\n")
      val <- try(eval(parse(text=str)))
      if(!inherits(val,"try-error")) {
        obj[,col] <- val
        dispose(w)
      } else {
        galert(gettext("Error in function body"), parent = w)
      }
    })
    size(w) <- c(300,200)
  }

  columnEmpty <- function(col) {
    val <- obj[,col] ## XXX write me
    return(FALSE)
  }
  rowEmpty <- function(row) {
    val <- obj[,row] ## XXX write me
    return(FALSE)
  }

  ## confirm a delete
  confirmDelete <- function(msg="Really delete? There is non empty data") {
    out <- tkmessageBox(icon="question",
                        message=gettext(msg),
                        type="yesno",
                        parent=tktable)
    ifelse(as.character(out) == "yes",TRUE, FALSE)
  }
  
  formatColumn <- function(col, type) {
    ## use tktable tag to format column to type.
  }

  ## make the menu
  tkadd(menu,"command",label=gettext("Transform Variable"), command = function() {
    ind <- getWhere()
    transformVariable(ind[2])
  })
  tkadd(menu,"separator")
  ##
  tkadd(menu,"command",label=gettext("Insert Variable"), command = function() {
    ind <- getWhere()
    tcl(tktable,"insert", "cols", ind[2])
    classes <- tag(obj, "classes")
    tag(obj,"classes") <- insert(classes, ind[2]+1, "character")

    val <- ginput("New variable name:", parent=obj)
    if(!is.na(val))
      names(obj)[ind[2] + 1] <- val
  })
  tkadd(menu,"command",label=gettext("Delete Variable"), command = function() {
    ind <- getWhere()
    if(columnEmpty(ind[2]) || confirmDelete())
      tcl(tktable,"delete","cols",ind[2])
    tag(obj, "classes") <- tag(obj, "classes")[-ind[2]]
  })

  tkadd(menu,"command",label=gettext("Rename Variable"), command = function() {
    ind <- getWhere()
    j <- ind[2]
    oldName <- names(obj)[j]
    val <- ginput("New variable name:", oldName, icon="question", parent=obj)
    if(!is.na(val))
      names(obj)[j] <- val
  })

  
  tkadd(menu,"command",label=gettext("Insert Case"), command = function() {
    ind <- getWhere()
    tcl(tktable,"insert","rows",ind[1])

    val <- ginput("New case name:", parent=obj)
    if(is.na(val)) 
      val <- "NA"                       # fill in
    rownames(obj)[ind[1] + 1] <- val

  })
  tkadd(menu,"command",label=gettext("Delete Case"), command = function() {
    ind <- getWhere()
    if(rowEmpty(ind[1]) || confirmDelete())
      tcl(tktable,"delete","rows",ind[1])
  })
  tkadd(menu,"command",label=gettext("Rename case"), command = function() {
    ind <- getWhere()
    i <- ind[1] 
    oldName <- rownames(obj)[i]
    val <- ginput("New case name:", oldName, icon="question", parent=obj)
    if(!is.na(val))
      rownames(obj)[i] <- val
  })

  tkadd(menu,"separator")

  setClass <- function(type) {
    ind <- getWhere()
    tclvalue(typeVar) <- type
    classes <- tag(obj,"classes")
    classes[ind[2]] <- type
    tag(obj,"classes") <- classes
    formatColumn(col=ind[2], type=type)
  }
  typeVar <- tclVar("numeric")          # for selecting type via radiobutton
  tkadd(menu, "radiobutton", label="numeric", variable=typeVar, command=function() setClass("numeric"))
  tkadd(menu, "radiobutton", label="integer", variable=typeVar, command=function() setClass("integer"))
  tkadd(menu, "radiobutton", label="factor", variable=typeVar, command=function() setClass("factor"))
  tkadd(menu, "radiobutton", label="character", variable=typeVar, command=function() setClass("character"))
  tkadd(menu, "radiobutton", label="logical", variable=typeVar, command=function() setClass("logical"))
  tkadd(menu, "radiobutton", label="other", variable=typeVar, command=function() {
    ## need to popup dialog to get function name for other.
    galert("other is not written", parent=obj)
    setClass("character")
  })
  
  popupCommand <- function(x,y,X,Y) {
    ## before popping up we have some work to do
    x0 <<- x; y0 <<- y;
    classMenuItems <- 7:12 + 2
    ind <- getWhere() ## row, column
    ## fix menu basd on where
    tkentryconfigure(menu, 0, state=ifelse(ind[2]==0,"disabled","normal"))
    tkentryconfigure(menu, 2, state=ifelse(ind[2]==0,"disabled","normal"))
    tkentryconfigure(menu, 3, state=ifelse(ind[2]==0,"disabled","normal"))
    tkentryconfigure(menu, 4, state=ifelse(ind[2]==0,"disabled","normal"))
    tkentryconfigure(menu, 5, state=ifelse(ind[1]==0,"disabled","normal"))
    tkentryconfigure(menu, 6, state=ifelse(ind[1]==0,"disabled","normal"))
    tkentryconfigure(menu, 7, state=ifelse(ind[1]==0,"disabled","normal"))

    for(i in classMenuItems)
      tkentryconfigure(menu, i, state=ifelse(ind[2]==0,"disabled","normal"))

    ## adjust class depends on which column
    if(ind[2] == 0) {
      tclvalue(typeVar) <- FALSE
    } else {
      theClass <- tag(obj,"classes")[ind[2]]
      if(theClass %in% c("numeric","integer","character","factor","logical"))
        tclvalue(typeVar) <- theClass
      else
        tclvalue(typeVar) <- "other"
    }
    ## popup
    tkpopup(menu,X,Y)
  }
  ## mac binding, just 3 for all
  if( as.character(tcl("tk","windowingsystem")) == "aqua" ) {
    tkbind(tktable, "<2>", popupCommand)
    tkbind(tktable, "<Control-1>", popupCommand)
  }
  tkbind(tktable, "<3>", popupCommand)
}


## getFromIndex -- not using tcl array variable
tktable.get <- function(tktable, i, j) {
  val <- tkget(tktable, paste(i,j, sep=","))
  as.character(val)
}

## set From Index -- not using tcl array variable
tktable.set <- function(tktable, i, j, value) 
  tkset(tktable, paste(i, j, sep=","), as.character(value))



## take a data frame or matrix make a character matrix
## basically sapply(mat,format) but also has dimnames
toCharacterMatrix <- function(x, rNames, cNames) {
  mat <- as.data.frame(x, stringsAsFactors=FALSE)
  mat <- as.data.frame(lapply(mat, format), stringsAsFactors=FALSE)
  if(!missing(rNames)) 
    mat <- cbind(rNames,mat)
  mat[,1] <- as.character(mat[,1])
  
  if(!missing(cNames)) 
    mat <- rbind(c(rep("", !missing(rNames)), cNames), mat)
  return(mat)
}

## fill in a tclArray object from character matrix
## modifies ta in place -- passed through environment
fillTclArrayFromCharMat <- function(ta, cm) {
  ## cm[,1] contains column names, while cm[1,] has rownames
  sapply(2:ncol(cm), function(j)
         ta[[0, j - 1]] <- as.tclObj(cm[1, j], drop = TRUE))
  for(j in 1:ncol(cm)) 
    sapply(2:nrow(cm), function(i) 
      ta[[i - 1, j - 1]] <- as.tclObj(cm[i, j], drop = TRUE))
}

## tclArray -> DataFrame
tclArrayToDataFrame <- function(ta, tktable, classes) {
  d <- tkindex(tktable, "end") # get size from tktable
  d <- as.numeric(unlist(strsplit(as.character(d), ",")))
  l <- list()
  for (j in 1:d[2]) {
    vals <- sapply(1:d[1], function(i) {
      val <- ta[[i,j]]
      ifelse(is.null(val), NA, tclvalue(val))
    })
    l[[j]] <- try(switch(classes[j],
                     factor=factor(vals),
                     as(vals, classes[j])),
                  silent=TRUE)
    if(inherits(l[[j]], "try-error")) l[[j]] <- vals ## character
  }
  ind <- which(classes == "character")
  if(length(ind)) {
    ## convert NA to ""
    for(i in ind) {
      tmp <- l[[i]]
      tmp[is.na(tmp)] <- ""
      l[[i]] <- tmp
    }
  }
  
  df <- as.data.frame(l)
  ## fix character -- turned to factor above through as.data.frame
  if(length(ind)) {
    df[,ind] <- as.character(df[,ind])
  }
  ## dimnames
  getTclValueWithDefault <- function(val, default) {
    if(is.null(val))
      default
    else
      tclvalue(val)
  }
  colnames(df) <- sapply(1:d[2], function(j) getTclValueWithDefault(ta[[0,j]], sprintf("X%s",j)))
  rownames(df) <- make.row.names(sapply(1:d[1], function(i) getTclValueWithDefault(ta[[i,0]], as.character(i))))
  return(df)
}






## helper function here
## unlike make.names this doesn't put "X" prefix
make.row.names <- function(x) {
  dups = duplicated(x)
  if(any(dups))
    x[dups] <- make.unique(x)[dups]
  return(unlist(x))
}
