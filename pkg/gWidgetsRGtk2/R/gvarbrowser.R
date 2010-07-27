## Use this to filter by type
## knownTypes in common
### Use this for filtering by (gvarbrowser, gvarbrowsertree)

## This is *ugly* -- how to get a reasonable set of values here?
.datasets = c(
  "numeric","logical","factor","character","integer",
  "data.frame","matrix","list",
  "table","xtabs",
  "nfnGroupedData","nffGroupedData","nmGroupedData",
  "POSIXct","POSIXlt","POSIXt"
  )
.models = c("lm","glm","lqs","aov","anova",
  "lme","lmList","gls",
  "ar","arma","arima0","fGARCH","fAPARCH"
    )
.ts = c("ts", "mts", "timeSeries", "its", "zoo","xts")
.functions=c("function")
.plots = c("recordedplot")

knownTypes = list(
  "data sets and models"=c(.datasets, .models, .ts),
  "data sets"= c(.datasets,.ts),
  "model objects" = .models,
  "time series objects" = .ts,
  "functions"=.functions,
  "saved plots" = .plots,
  "all" = NULL
  )

## list of some type
lsType = function(type, envir=.GlobalEnv) {
  x = with(.GlobalEnv, sapply(ls(), function(i) class(get(i))))
  objects = names(x)[sapply(x, function(i) any(i %in% type))]
  return(objects)
}
lsDatasets = function(envir=.GlobalEnv)  lsType(.datasets, envir)
lsModels = function(envir=.GlobalEnv)  lsType(.models, envir)
lsTs = function(envir=.GlobalEnv)  lsType(.ts, envir)
lsFunctions = function(envir=.GlobalEnv)  lsType(.functions, envir)



offspring = function(path=c(), data=NULL) {

  if(!is.null(data) && is.function(data)) data <- data()

  ## data is knownClass value. This checks through inheritance but still the
  ## question of what classes to show is hardcoded -- eh
  .inClass <- function(x,data) {
    if(is.null(data)) return(TRUE)
    any(sapply(1:length(data), function(i) {
      out <- is(x,data[i])
      if(inherits(out,"try-error")) return(FALSE)
      return(out)
    }))
  }
  
  if(length(path) == 0) {
    fullx <- x <- ls(envir=.GlobalEnv)
  } else {
    string <- paste(path,collapse="$")
    obj <- getObjectFromString(string)

    x <- with(obj, ls())
    fullx <- paste(string,x,sep="$")
  }

  if(length(x) == 0) {
    return(data.frame(names="",hasSubTree=FALSE,type=""))
  }

  type <- c(); hasTree <- c(); newNames <- c()
    
  for(i in 1:length(x)) {
    y <-  getObjectFromString(fullx[i])
    if(.inClass(y,data)) {
      j <- length(type)+ 1
      type[j] <- str2(y)
      hasTree[j] <- hasSubTree(y)
      newNames[j] <- x[i]
    }
  }
  
  if(length(type) == 0) {
    return(data.frame(names="",hasSubTree=FALSE,type="", stringsAsFactors=TRUE))
  }

  allValues <-  data.frame(names=I(newNames), hasSubTree=hasTree, type=I(type), stringsAsFactors=FALSE)
  return(allValues)
  
}

hasSubTree = function(x) {
  tmp  = gtktry(is.list(x) &&
    !is.guiWidget(x) && !is.gWidget(x) && !is.RGtkObject(x) &&
    !is.null(names(x)), silent=TRUE)
  if(!inherits(tmp,"try-error") && tmp)
    return(TRUE)
  else
    return(FALSE)
}

## in common.R
## getObjectFromString = function(string, envir=.GlobalEnv) {
##   out = try(eval(parse(text=string), envir=envir), silent=TRUE)
##   if(inherits(out, "try-error"))
##     return(NA)
##   return(out)
## }


setClass("gVarbrowserRGtk",
         representation(filter="guiComponent"),
         contains="gComponentRGtk",
         prototype=prototype(new("gComponentRGtk"))
         )

## THe main object
setMethod(".gvarbrowser",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   handler = NULL,
                   action = "summary",
                   container = NULL,
                   ...) {

            force(toolkit)

            theArgs <- list(...)
            if(!is.null(theArgs$inteval)) theArgs$interval <- theArgs$interval ## typo fix. Remove later
            interval <- ifelse(is.null(theArgs$interval), 2000, theArgs$interval)

            ## fix up known types
            if(!is.null(theArgs$knownTypes))
              knownTypes <- theArgs$knownTypes
            else if(!is.null(getOption("knownTypes"))) {
              knownTypes <- getOption("knownTypes")
            }

            multiple <- getWithDefault(theArgs$multiple, TRUE)
            
            ## fix handler if action is non-null
            if(is.null(handler) && !is.null(action)) {
              handler = function(h, ...) {
                values <- h$obj[]
                value <- paste(values, collapse = "$")
                if (!is.null(action))
                        print(do.call(h$action, list(svalue(value))))
              }
            }

            ## begin
            group <- ggroup(horizontal=FALSE, container=container,...)
            filterGroup <- ggroup(container=group)
            glabel("Filter by:",container=filterGroup)
            filterPopup <- gdroplist(names(knownTypes), container=filterGroup)
            val <- ifelse("data sets" %in% names(knownTypes), "data sets", names(knownTypes)[1])
            svalue(filterPopup) <- val
            
            ## main tree
            tree = gtree(offspring=offspring,
              offspring.data=function() knownTypes[[svalue(filterPopup)]],
              col.types=data.frame(Name="string",Type="string"),
              icon.FUN = function(d,...) {
                ## Allow user to change stock icon
                FUN <- getWithDefault(getOption("gWidgetsStockIconFromClass"), stockIconFromClass)
                byReturnVector(d,function(x) FUN(x[,'type']))
              },
              multiple=multiple,
              container = group, expand=TRUE
              )
            
            ## update the tree this way
            addhandlerclicked(filterPopup,
                              handler = function(h,...) {
                                key = svalue(filterPopup)
                                offspring.data = knownTypes[[key]]
                                update(h$action, offspring.data)
                              },
                              action=tree)
            
            ## add an idle handler for updating tree every  second (or interval)
            idleid = addhandleridle(tree, interval=interval, handler = function(h,...) {
              key = svalue(filterPopup)
              offspring.data = knownTypes[[key]]
              update(h$obj, offspring.data)
            })
            addhandlerunrealize(tree, handler = function(h,...) {
              gSourceRemove(h$action)},
                                action=idleid)
            
            
            
            ## drop handler
            adddropsource(tree,handler=function(h,...) {
              
              values = h$obj[]
              values = sapply(values, untaintName)
              return(paste(values,collapse="$"))
            })
            
            tag(tree,"view")$SetEnableSearch(TRUE)
            tag(tree,"view")$SetHeadersClickable(TRUE)

            obj = new("gVarbrowserRGtk",block=group, widget=tree, filter=filterPopup)


            ## override how we compare items. Default is just by name, here we want
            ## to include class
            tag(tree, "isStillThere") <- function(old, new) {
              out <- (old[1] %in% new[,1, drop=TRUE]) &&
                     (old[2] %in% new[,3, drop=TRUE])
              out
            }
            
            if(!is.null(handler)) {
              id = addhandlerdoubleclick(tree,
                handler=handler, action=action)
            }
            
            ## all done
            return(obj)
          })

### methods
## push methods and handlers down to tree in this case
setMethod(".update",
          signature(toolkit="guiWidgetsToolkitRGtk2",object="gVarbrowserRGtk"),
          function(object, toolkit, ...) {
            update(object@widget, ...)
          })

setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gVarbrowserRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {

            ## check if any selection
            value <- NA
            x <- svalue(obj@widget)
            if(!(is.atomic(x) && length(x) == 1 && is.na(x))) {
              f <- function(x) paste(x, collapse="$")
              values <- obj@widget[]       # from tree
              if(is.list(values))
                value <- sapply(values, f)
              else
                value <- f(values)
            }
            return(value)
          })
setMethod("[",
          signature(x="gVarbrowserRGtk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x,guiToolkit("RGtk2"), i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gVarbrowserRGtk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            if(missing(i))
              x@widget[...]
            else
              x@widget[i,...]
          })




