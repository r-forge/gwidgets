## table for selecting values
## most methods in gdf.R inherited from gGrid class
setClass("gTablerJava",
         representation = representation("gGridrJava",
           chosencol="numeric"),

         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )


## ## constructor for selecting values from a data set -- not meant for editing
setMethod(".gtable",
          signature(toolkit="guiWidgetsToolkitrJava"),
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
            ## * icon.FUN
            ## * filtering
            ## * sorting
            
            force(toolkit)
            
            ## the colors
            theArgs = list(...)
            colors = theArgs$colors
            if(is.null(colors))
              colors = c(
                bg = "navajo white",fg = "black",
                rbg = "white smoke",rfg="red"
                )
            

            ## do we filter? If so, send to filter function.
            ## this is a hack, but seems easy enough to implement
            if(!is.null(filter.column) || !is.null(filter.FUN)) {
              obj = .gtableWithFilter(toolkit,
                items,
                multiple,
                chosencol,                        # for drag and drop, value
                icon.FUN,
                filter.column,
                filter.labels,
                filter.FUN,   # two args gtable instance, filter.labels element
                handler,
                action,
                container,
                ...)
              return(obj)
            }

            ## Not filtering


            
            x = items
            
            if(is.vector(x))
              x = as.data.frame(x)
            ## get dimensions
            d = dim(x)

            ## Using JTable
##            tbl = jnew("JTable",as.integer(d[1]),as.integer(d[2]))
##            sp = jnew("JScrollPane", .jcast(tbl,"java/awt/Component"))
##            .jcall(tbl,"V","setAutoResizeMode", .jfield(tbl,name="AUTO_RESIZE_OFF"))

            ## with gTable
            sp = .jnew("gWidgetsrJava/gTable",as.integer(d[1]),as.integer(d[2]), as.integer(chosencol-1))
            tbl = .jcall(sp,"Ljavax/swing/JTable;","getTable")
            

            obj = new("gTablerJava",block=sp,widget=tbl,
              toolkit=toolkit,ID=getNewID(), e = new.env(),
              chosencol = as.numeric(chosencol))

            obj[,] <- x

            ## set classes for each column
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
            names(obj) <- names(x)

            ## make look a bit different
            .jcall(tbl,"V","setShowGrid", FALSE)
            .jcall(tbl,"V","setShowHorizontalLines", TRUE)


            ## multiple
            if(!as.logical(multiple)) {
              ## set single, multiple is default
              lsm = tbl$getSelectionModel()
              .jcall(lsm,"V","setSelectionMode",
                     .jfield(lsm,name="SINGLE_SELECTION"))
            }
            
            ## no cell editing
            ## *implement me
            

            ## add handler
            if (!is.null(handler)) {
              id = addhandlerchanged(obj,handler,action)
            }

            
            ## add to container
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj, ...)
            }


            return(obj)
            
          })


## These are defined in gDf for fGrid objects. We only make changes here
## data frame methods

## incorporate chosenval here
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gTablerJava"),
          function(obj, toolkit, index=NULL, drop=NULL,...) {

            tbl = obj@widget
            lsm = tbl$getSelectionModel()
            
            if(.jcall(lsm,"Z","isSelectionEmpty"))
              return(NA)                          # nothing selected
            
            indices = sapply(1:dim(obj)[1],function(i) .jcall(lsm,"Z","isSelectedIndex",as.integer(i-1)))

            
            if(!is.null(index) && index == TRUE)
              return(which(indices))    # the TRUE indices, not all of them
            
            ## Now a value
            if(missing(drop) || is.null(drop))
              drop <- TRUE

            ##
            chosencol = obj@chosencol
            if (is.null(chosencol) || drop==FALSE)
              return(obj[indices,,drop=drop])
            else
              return(obj[indices,chosencol,drop=drop])
          })

## handlers

setMethod(".addhandlerdoubleclick",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gTablerJava"),
          function(obj, toolkit, handler, action=NULL, ...) {

            ## for gTable
            ID = addJHandler(obj,handler, action,
              type="addMouseListener",
              event = "TwoMouseClicked",
              class = "java/awt/event/MouseListener",...)
           return(ID)

##             ## for JTable
##             theTable = getWidget(obj)
##             lsm = theTable$getSelectionModel() ## handler goes here!
##             addJHandler(obj,handler, action,
##                         type="addListSelectionListener",event = "",,
##                         class = "javax/swing/event/ListSelectionListener",
##                         jobj = lsm)

            
          })
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gTablerJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandlerdoubleclick(obj, handler, action)
          })


setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gTablerJava"),
          function(obj, toolkit, handler, action=NULL, ...) {

            ## for gTable
            ID = addJHandler(obj,handler, action,
              type = "addMouseListener",
              event = "MouseClicked",
              class = "java/awt/event/MouseListener",...)
           return(ID)

            
          })


##################################################
##################################################
### for filtering
### This was taken from the tcltk one. If we do another
### put this into gWidgetsANY


## table for selecting values
## most methods in gdf.R inherited from gGrid class
setClass("gTableWithFilterrJava",
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
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
          signature(toolkit="guiWidgetsToolkitrJava"),
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
              handler=handler,
              action=action,
              cont=g, expand=TRUE)

            

            
            ## make an object to return
            obj = new("gTableWithFilterrJava",block=g,widget=tbl,
              toolkit=toolkit,ID=getNewID())

            tag(obj, "allItems") <- items
            
            tag(obj,"filterPopup") <- filterPopup
            tag(obj,"filterByLabel") <- filterByLabel
            
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

            addHandlerChanged(filterPopup, action=obj,
                              handler=function(h,...) {
                                DF = tag(h$action, "allItems")
                                tbl = h$action@widget@widget
                                fval = svalue(h$obj)
                                inds = filter.FUN(DF, fval)
                                ## update tbl, not obj!
                                tbl[,] <- DF[inds,]
                              })
            
            return(obj)
          })


          

setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gTableWithFilterrJava"),
          function(obj, toolkit, index=NULL, drop=NULL,...) {

            if(!is.null(index) && index) {
              cat("The index refers to the visible data value, not the entire data frame\n")
            }

            return(svalue(obj@widget, toolkit=toolkit, index=index, drop=drop, ...))

          })

## refers to visible
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gTableWithFilterrJava"),
                 function(obj, toolkit, index=NULL, ..., value) {

                   tbl = obj@widget
                   svalue(tbl, toolkit=toolkit, index=index,  ...) <- value

                   return(obj)
                 })


## retrieve values
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gTableWithFilterrJava"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            tbl = x@widget@widget       # confusing but we look at a
                                        # dot function
            .leftBracket(tbl, toolkit, i, j, ..., drop=drop)
          })
            
setMethod("[",
          signature(x="gTableWithFilterrJava"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop) 
          })
## replace values
setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gTableWithFilterrJava"),
          function(x, toolkit, i, j, ..., value) {
            if(!missing(i) || !missing(j)) {
              cat("[<- only replaces the entire object. Try obj[,]<-value\n")
              return(x)
            }

            ## underlying gtable object
            tbl = x@widget@widget

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
                 signature(x="gTableWithFilterrJava"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

## dim
setMethod(".dim",
          signature(toolkit="guiWidgetsToolkitrJava",x="gTableWithFilterrJava"),
          function(x, toolkit) {
            tbl = x@widget
            return(dim(tbl))
          })
## length
setMethod(".length",
          signature(toolkit="guiWidgetsToolkitrJava",x="gTableWithFilterrJava"),
          function(x, toolkit) {
            tbl = x@widget
            return(length(tbl))
          })

## size<- work on tl
setReplaceMethod(".size", 
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gTableWithFilterrJava"),
                 function(obj, toolkit, ..., value) {
                   tbl = obj@widget
                   size(tbl) <- value
                   return(obj)
                 })

## handlers

setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gTableWithFilterrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            tbl = obj@widget@widget
            .addhandlerdoubleclick(tbl, toolkit, handler, action)
          })

## same as changed
setMethod(".addhandlerdoubleclick",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gTableWithFilterrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerchanged(obj, toolkit, handler, action)
          })

## when a selection is changed
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gTableWithFilterrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            tbl = obj@widget@widget
            ID = addJHandler(tbl,handler, action,
              type = "addMouseListener",
              event = "MouseClicked",
              class = "java/awt/event/MouseListener",...)
           return(ID)
          })


         
         
         

