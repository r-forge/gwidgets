## gGrid cover gDf and gTable
setClass("gGridrJava",
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )
setClass("gDfrJava",
         contains="gGridrJava",
         prototype=prototype(new("gComponentrJava"))
         )

##################################################
### Gdf 

## * Colors not implemented
## * This is **SLOW** for larger data sets (Cars93 large)


## constructor for editing a data frame
setMethod(".gdf",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   items = NULL,
                   name = deparse(substitute(items)),
                   do.subset = FALSE,
                   container=NULL,...)  {

            force(toolkit)

            ## the colors
            theArgs = list(...)
            colors = theArgs$colors
            if(is.null(colors))
              colors = c(
                bg = "navajo white",fg = "black",
                rbg = "white smoke",rfg="red"
                )
            

            x = items

            if(!is.data.frame(x))
              x = as.data.frame(x, stringsAsFactors = FALSE)


            ## fix up table
            ## get dimentions
            d = dim(x)

            ## replace with a clas below
            ##            tbl = jnew("JTable",as.integer(d[1]),as.integer(d[2]+1)) # +1 for rownames
            ##           sp = jnew("JScrollPane", .jcast(tbl,"java/awt/Component"))
            ##            .jcall(tbl,"V","setAutoResizeMode", tbl$AUTO_RESIZE_OFF)

            sp = .jnew("gWidgetsrJava/gDf",as.integer(d[1]),as.integer(d[2]+1))
            tbl = .jcall(sp,"Ljavax/swing/JTable;","getTable")
            

            obj = new("gDfrJava",block=sp,widget=tbl,
              toolkit=toolkit, ID=getNewID(),  e = new.env())

            ## store the data
            obj[1:d[1],1:d[2]] <- x
            
            ## set classes for each column
            ## a bug  here in the java class? Not major, but odd
            theClass = sapply(x,class)
            for(col in 1:dim(x)[2]) {
              ## make a new column in object
              .jcall(sp,"V","setColumnRclass", # sp is the gDf object
                     as.integer(col),
                     .jnew("java/lang/String",switch(theClass[col][[1]],
                                                     "character"="character",
                                                     "numeric"="numeric",
                                                     "integer"="numeric",
                                                     "logical"="logical",
                                                     "factor"="factor",
                                                     "character"))
                     
                     )
            }
            ## set names
            dimnames(obj) <- dimnames(x)
            
            ## fix cell editor for factor, and logical
##             classes = sapply(x,class)
##             for(i in 1:dim(x)[2]) {
##               if(classes[i] %in% c("factor","logical")) {
##                 widget = .jnew("javax/swing/JComboBox")
##                 ## fill in combobox
##                 if(classes[i] %in% c("factor")) {
##                   for(val in levels(x[,i]))
##                     .jcall(widget,"V","addItem",asJobject(val))
##                 } else {
##                   ## logical
##                   .jcall(widget,"V","addItem",asJobject(TRUE)) 
##                   .jcall(widget,"V","addItem",asJobject(FALSE))
##                 }
##                 ## make cell editor
##                 tcm = tbl$getColumnModel()
##                 tm = .jcall(tcm, "Ljavax/swing/table/TableColumn;","getColumn",as.integer(i))
##                 ce = .jnew("javax/swing/DefaultCellEditor",widget)
##                 .jcall(tm,"V","setCellEditor",
##                        .jcast(ce,"javax/swing/table/TableCellEditor"))
##               }
##             }
            

            ## add handler
            ## no handler for this 

            
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
          signature(toolkit="guiWidgetsToolkitrJava",obj="gGridrJava"),
          function(obj, toolkit,  ..., value) {
            width = as.integer(value[1])
            height = as.integer(value[2])
            ## size
            jdim = .jnew("java/awt/Dimension")
            jdim$setSize(width,height)
            .jcall(obj@widget,"V","setPreferredScrollableViewportSize",jdim)

            return(obj)
          })



## data frame methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gGridrJava"),
          function(obj, toolkit, index=NULL, drop=NULL,...) {

            tbl = obj@widget
            lsm = tbl$getSelectionModel()

            if(.jcall(lsm,"Z","isSelectionEmpty"))
              return(NA)                          # nothing selected
            
            indices = sapply(1:dim(obj)[1],function(i) .jcall(lsm,"Z","isSelectedIndex",as.integer(i-1)))

            
            if(!is.null(index) && index == TRUE)
              return(indices)
            
            ## Now a value
            if(missing(drop) || is.null(drop)) drop = FALSE

            return(obj[indices,,drop=drop])

          })
          
          
## set by index value selected value
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gGridrJava"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   if(!is.null(index) && index == FALSE) {
                     ## notthing to do
                   } else {
                     ## set row to be highlighted
                     cat("svalue: not implemented\n")
                   }
                   return(obj)
                 })


## refers to the entire data frame
## index returned by svalue(index=T) works here
setMethod("[",
          signature(x="gGridrJava"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j,..., drop=drop)
          })

setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gGridrJava"),
          function(x, toolkit, i, j, ..., drop=TRUE) {

            theArgs = list(...)
            rownameFudge = doRownameFudge(x)
            

            tbl = x@widget
            gDfobj = x@block
            d = dim(x)
            
            if(missing(i))
              i = 1:d[1]
            if(missing(j))
              j = 1:(d[2])
            if(is.logical(i))
              i = which(i)
            if(is.logical(j))
              j = which(j)

            Rclasses = sapply(j, function(i) # just j, not 1:d[2]
              .jsimplify(.jcall(gDfobj,"Ljava/lang/Object;",
                     "getColumnRclass",as.integer(i))))
            
            lst = lapply(j, function(col) {
              sapply(i, function(row) {
                val = getValueAt(tbl, row - 1, col - 1 + rownameFudge)
                if(is.null(val)) val = "" # make character
                return(val)
              })
            })

            ## lst is a list of character vectors
            ## Rclasses is a list of data types, we need to coerce
            newLst = list()
            for(col in 1:length(lst))
              newLst[[col]] = do.call(paste("as.",Rclasses[col],sep=""), list(lst[[col]]))
            
            ## fix names
            xNames = dimnames(x)
            names(newLst) <- xNames[[2]][j]
            
            ## make a data frame, then add names
            df = data.frame(newLst,
              row.names = xNames[[1]][i],
              check.rows = TRUE,
              check.names=TRUE,
              stringsAsFactors=FALSE) # override default

            
            ## now return
            return(df[,,drop=drop])

          })

## [<-
setReplaceMethod("[",
                 signature(x="gGridrJava"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j,...) <- value
                   return(x)
                 })


setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gGridrJava"),
          function(x, toolkit, i, j, ..., value) {


            theArgs = list(...)
            rownameFudge = doRownameFudge(x)

            tbl = getWidget(x)
            gDfobj = x@block
            
            ## change indices
            d = dim(x)
            dv = dim(value)
            if(missing(i)) {
              i = 1:ifelse(is.null(dv),length(value),dv[1]) # rows the length of value!
              ## delete rows at bottom if value is smaller
              if(max(i) < d[1]) {
                tableModel = .jcall(tbl,"Ljavax/swing/table/TableModel;","getModel")
                print(tableModel)
                for(row in rev((max(i)+1):d[1])) ## revese here to take from bottom
                  try(.jcall(tableModel,"V","removeRow",as.integer(row-1)), silent=TRUE)
              } else if(max(i) > d[1]) {
                ## add rows at bottom if values is bigger
                tableModel = .jcall(tbl,"Ljavax/swing/table/TableModel;","getModel")
                sapply((d[1]+1):max(i),function(row)
                       .jcall(gDfobj,"V","addRow"))

              }
            }
            if(missing(j)) j = 1:d[2]

            if(is.logical(i)) i = which(i)
            if(is.logical(j)) j = which(j)
            

            ## add column by column
            theValue = ""
            for(col in j) {
              for(row in i) {
                theValue = ""
                if (col==0 || is.vector(value)) {
                  theValue = value[row]
                } else {
                  theValue = value[row,col]
                }
                ## we add as characters!
                theValue = as.character(theValue)
                aString = .jnew("java/lang/String",theValue)
                aObject = .jcast(aString,"java/lang/Object")
                .jcall(tbl,"V","setValueAt",
                       aObject,as.integer(row-1),as.integer(col - 1 + rownameFudge))
                
              }
            }

            redrawTable(tbl)
            
            return(x)
            
          })
                 
## first column is the visible row


## data frame like
setMethod(".dim", 
          signature(toolkit="guiWidgetsToolkitrJava",x="gGridrJava"),
          function(x,toolkit) {
            tbl = x@widget
            ## HACK to accomodate rownames. Might have icons here too
            rownameFudge = doRownameFudge(x)
            tm = try(.jcall(tbl,"Ljavax/swing/table/TableModel;","getModel"),
              silent=TRUE)
            if(inherits(tm, "try-error"))
              return(c(NA,NA))

            d = c(
              .jcall(tm,"I","getRowCount"),
              .jcall(tm,"I","getColumnCount")-rownameFudge # rownames in col. 1
              )
            return(d)
          })

## no dimnames for gGrid, only names
setMethod(".dimnames",
          signature(toolkit="guiWidgetsToolkitrJava",x="gGridrJava"),
          function(x,toolkit) {
            tbl = x@widget

            d = dim(x)
            theRowNames = sapply(1:d[1], function(i)
              getValueAt(tbl,i-1,0))

            theRowNames = make.row.names(theRowNames)
            list(rownames=unlist(theRowNames) ,colnames=names(x))  # first column contains names!
          })
          

setReplaceMethod(".dimnames",
                 signature(toolkit="guiWidgetsToolkitrJava",x="gDfrJava"),
                 function(x, toolkit,  value) {
                   if(!is.list(value))
                     stop("value is a list with first element the row names, and second the column names")
                   rnames = value[[1]]
                   cnames = value[[2]]
                   d = dim(x)
                   if(is.null(rnames) || length(rnames) != d[1])
                     stop("Row names are the wrong size")
                   if(is.null(cnames) || length(cnames) != (d[2]))
                     stop("Column names are the wrong size")
                   
                   ## set column names
                   names(x) <- cnames

                   ## set row names
                   rnames = make.row.names(rnames)
                   tbl = x@widget
                   for(i in 1:length(rnames))  {
                     aString = .jnew("java/lang/String",as.character(rnames[i]))
                     aObject = .jcast(aString,"java/lang/Object")
                     .jcall(tbl,"V","setValueAt",
                            aObject,as.integer(i-1),as.integer(0))
                   }
                   redrawTable(x@widget)
                   
                   return(x)
                 })

setMethod(".length",
          signature(toolkit="guiWidgetsToolkitrJava",x="gGridrJava"),
          function(x,toolkit) return(dim(x)[2]))


setMethod(".names",
          signature(toolkit="guiWidgetsToolkitrJava",x="gGridrJava"),
          function(x, toolkit) {

            
            
            tbl = x@widget
            rownameFudge = doRownameFudge(x)
            
            tm = .jcall(tbl,"Ljavax/swing/table/TableColumnModel;","getColumnModel")
            theNames = sapply(1:length(x), function(i) {
              tcl = .jcall(tm,"Ljavax/swing/table/TableColumn;","getColumn",as.integer(i-1+rownameFudge))
              val = .jcall(tcl,"Ljava/lang/Object;","getHeaderValue")
              .jsimplify(val)
            })

            return(theNames)
          })


setReplaceMethod(".names",
                 signature(toolkit="guiWidgetsToolkitrJava",x="gGridrJava"),
                 function(x, toolkit, value) {

                   tbl = x@widget
                   rownameFudge = doRownameFudge(x)
                   
                   tm = .jcall(tbl,"Ljavax/swing/table/TableColumnModel;","getColumnModel")
                   n = length(x)
                   if(length(value) != n)
                     return(x)

                   if(rownameFudge) {
                     value = c("rownames",value) #add in rownames
                   }
                   sapply(0:(length(value)-1), function(i) {
                     str = .jnew("java/lang/String",value[i+1])
                     stro = .jcast(str,"java/lang/Object")
                     tcl = .jcall(tm,"Ljavax/swing/table/TableColumn;","getColumn",as.integer(i))
                     .jcall(tcl,"V","setHeaderValue",stro)
                     })

                   redrawTable(tbl)
                   return(x)
                 })




##################################################
## get single value, not coerced. 0-based
getValueAt = function(tbl,i,j) {
  val = .jsimplify(.jcall(tbl,"Ljava/lang/Object;","getValueAt",
    as.integer(i),as.integer(j)))
  return(val)
}

## access to set bracket at 0-based
setValueAt = function(tbl,i,j,value) {
  aString = .jnew("java/lang/String",as.character(value))
  aObject = .jcast(aString,"java/lang/Object")
  .jcall(tbl,"V","setValueAt",
         aObject,as.integer(i),as.integer(j))
}


redrawTable = function(tbl) {
  ## redraw table
  ## not needed
}

## helper function here
## unlike make.names this doesn't put "X" prefix
make.row.names <- function(x) {
  dups = duplicated(x)
  if(any(dups))
    x[dups] <- make.names(x,unique=TRUE)[dups]
  return(unlist(x))
}

## do we fudge colcount for rownames (or icons)
## 1 for df 0 for table
doRownameFudge = function(obj) {
  if("gDfrJava" %in% class(obj))
    return(1)
  else
    return(0)
}
