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


## Use this to filter by type
## knownTypes in common
### Use this for filtering by (gvarbrowser, gvarbrowsertree)
.datasets <- c(
  "numeric","logical","factor","character",
  "data.frame","matrix","list",
  "table","xtabs",
  "nfnGroupedData","nffGroupedData","nmGroupedData"
  )
.models <- c("lm","glm","lqs","aov","anova",
             "lme","lmList","gls",
             "ar","arma","arima0","fGARCH","fAPARCH"
             )
.ts <- c("ts", "mts", "timeSeries", "its", "zoo")
.functions <- c("function")
.plots <- c("recordedplot")

knownTypes = list(
  "data sets and models"=c(.datasets, .models, .ts),
  "data sets"= c(.datasets,.ts),
  "model objects" = .models,
  "time series objects" = .ts,
  "functions"=.functions,
  "saved plots" = .plots,
  "all" = NULL
  )

##' knownTypes determines how filtering will work
##' This allows override by setting an option
knownTypes <- getWithDefault(getOption("gvarbrowserKnownTypes"), knownTypes)
##' return the known types for this key
.getKnownTypes <- function(key=NULL) {
  knownTypes <- getWithDefault(getOption("gvarbrowserKnownTypes"), knownTypes)
  if(is.null(key)) {
    NULL
  } else {
    knownTypes[[key]]
  }
}




## list of some type
lsType = function(type, envir=.GlobalEnv) {
  x = with(envir, sapply(ls(), function(i) class(get(i))))
  objects = names(x)[sapply(x, function(i) any(i %in% type))]
  return(objects)
}


##' Data is not used here at end
##' to filter by types
offspring = function(path=c(), data=NULL) {
  if(length(path) == 0) {
    x = ls(envir=.GlobalEnv)
    if(length(x) == 0) {
      return(data.frame(names="",hasSubTree=FALSE,type=""))
    }

    type = c();hasTree=c()
    for(i in 1:length(x)) {
      y = getObjectFromString(x[i])
      type[i] = str2(y)
      hasTree[i] = hasSubTree(y)
    }
  } else {
    string = paste(path,collapse="$")
    obj = getObjectFromString(string)

    x = with(obj, ls())

    if(length(x) == 0) {
      return(data.frame(names="",hasSubTree=FALSE,type=""))
    }

    type = c();hasTree=c()
    for(i in 1:length(x)) {
      y = getObjectFromString(paste(string,x[i],sep="$"))
      type[i] = str2(y)
      hasTree[i] = hasSubTree(y)
    }
  }

  allValues = data.frame(names=I(x), hasSubTree=hasTree, type=I(type))

  if(!is.null(data)) {
    return(allValues[allValues$type %in% data, ,drop=FALSE])
  } else {
    return(allValues)
  }
}

hasSubTree = function(x) {
  tmp  = try(is.list(x)  && ## !is.guiWidget(x) && !is.gWidget(x) &&
    !is.null(names(x)), silent=TRUE)

  return(!inherits(tmp,"try-error") && tmp)
}

## in common.R
## getObjectFromString = function(string, envir=.GlobalEnv) {
##   out = try(eval(parse(text=string), envir=envir), silent=TRUE)
##   if(inherits(out, "try-error"))
##     return(NA)
##   return(out)
## }


setClass("gVarbrowserQt",
         representation(filter="guiComponent"),
         contains="gComponentQt",
         prototype=prototype(new("gComponentQt"))
         )

## THe main object
setMethod(".gvarbrowser",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   handler = NULL,
                   action = "summary",
                   container = NULL,
                   ...) {

            force(toolkit)
            

            ## fix handler if action is non-null
            if(is.null(handler) && !is.null(action)) {
              handler = function(h, ...) {
                values = h$obj[]
                value = paste(values, collapse = "$")
                if (!is.null(action))
                        print(do.call(h$action, list(svalue(value))))
              }
            }

            ## begin
            group <- ggroup(horizontal=FALSE, container=container,...)
            filterGroup <- ggroup(container=group)
            glabel("Filter by:",container=filterGroup)
            filterPopup <- gcombobox(names(knownTypes), container=filterGroup)
            svalue(filterPopup) <- "data sets"

            icon.FUN <- function(d,...) {
              sapply(d[,'type'], function(i) names(stockIconFromClass(i)))
            }

            
            ## main tree
            tree <- gtree(
                          offspring=offspring,
                          offspring.data=.getKnownTypes(svalue(filterPopup)),
                          col.types=data.frame(Name="string",Type="string"),
                          icon.FUN=icon.FUN,
                          container=group,
                          expand=TRUE
                          )

            

            
            ## update the tree this way
            addhandlerclicked(filterPopup,
                              handler = function(h,...) {
                                tree <- h$action
                                offspring.data <- .getKnownTypes(svalue(h$obj))
                                tag(tree, "offspring.data") <- offspring.data
                                update(tree)
                              },
                              action=tree)
            
            ## add an idle handler for updating tree every  second
            idleid <- addhandleridle(tree, interval=1000, handler = function(h,...) {
              key <- svalue(filterPopup)
              offspring.data <- .getKnownTypes(key)
              ## update must be smart, or this will close open branches
              update(h$obj, offspring.data = offspring.data)
            })

            
##             addhandlerunrealize(tree, handler = function(h,...) {
##               removeHandler(h$obj, h$action)
##             },action=idleid)
            
            
            
            ## drop handler
            adddropsource(tree,handler=function(h,...) {
              values <- h$obj[]
              values <- sapply(values, untaintName)
              return(paste(values,collapse="$"))
            })


            obj <- new("gVarbrowserQt", block=group, widget=tree, filter=filterPopup,
              toolkit=toolkit, e=new.env(), ID=getNewID())

            if(!is.null(handler)) {
              id <- addhandlerdoubleclick(tree, handler=handler, action=action)
              tag(obj, "handler.id") <- id
            }
            
            ## all done
            return(obj)
          })

### methods
## push methods and handlers down to tree in this case

setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gVarbrowserQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            values = obj@widget[]       # from tree
            value = paste(values, collapse = "$")

            return(value)
          })
setMethod("[",
          signature(x="gVarbrowserQt"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x,guiToolkit("Qt"), i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gVarbrowserQt"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            if(missing(i))
              x@widget[...]
            else
              x@widget[i,...]
          })




