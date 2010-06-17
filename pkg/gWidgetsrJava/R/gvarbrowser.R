## Use this to filter by type
## knownTypes in common
### Use this for filtering by (gvarbrowser, gvarbrowsertree)
.datasets = c(
  "numeric","logical","factor","character",
  "data.frame","matrix","list",
  "table","xtabs",
  "nfnGroupedData","nffGroupedData","nmGroupedData"
  )
.models = c("lm","glm","lqs","aov","anova",
    "lme","lmList","gls",
  "ar","arma","arima0","fGARCH","fAPARCH"
    )
.ts = c("ts", "mts", "timeSeries", "its", "zoo")
.functions=c("function")
.plots = c("recordedplot")

knownTypes = list(
  "data sets and models"=c(.datasets, .models, .ts),
  "data sets"= .datasets,
  "model objects" = .models,
  "time series objects" = .ts,
  "functions"=.functions,
  "saved plots" = .plots,
  "all" = NULL
  )

## return data frame with variables
.getVariableDataFrame = function() {

    x = ls(envir=.GlobalEnv)
    theNames = c()
    theClass = c()
    theSummary = c()

    for(i in x) {
      y = getObjectFromString(i)
      theNames = c(theNames,i)
      theClass = c(theClass,str2(y))

      if(is.list(y)) {
        subNames = try(names(y),silent=TRUE)
        if(!inherits(subNames,"try-error")) {
          for(j in subNames) {
            if(length(grep("\\s",j))>0)
              j = paste("\"",j,"\"",sep="",collapse="")
            subName = paste(i,"$",j,sep="",collapse="")
            y1 = getObjectFromString(subName)
            theNames = c(theNames,subName)
            theClass = c(theClass,str2(y1))
          }
        }
      }
    }

    return(data.frame(names=theNames,class=theClass, stringsAsFactors=FALSE))
}

## this will also update
.showVariableDataFrame = function(n, handler=NULL, action=NULL) {

  ### n is a gnotebook instance

  ## lst is for storing widgets for svalue
  lst = list(); i =  1
  ## clear out n
  if(length(n) > 0) 
    for(tmp in 1:length(n)) dispose(n)    # one at a time
  
  ## now get data
  m = .getVariableDataFrame()


  ## datasets
  m1 = m[m[,2] %in% .datasets,]
  if(nrow(m1) > 0) {
    tmp = gtable(m1, handler=handler, action=action)
    lst[[i]] <- tmp; i <- i+1
    add(n,tmp,label="Data")
  }

  ## models
  m1 = m[m[,2] %in% .models,]
  if(nrow(m1) > 0) {
    tmp = gtable(m1, handler=handler, action=action)
    lst[[i]] <- tmp; i <- i+1
    add(n,tmp,label="Models")
  }

  ## ts
  m1 = m[m[,2] %in% .ts,]
  if(nrow(m1) > 0) {
    tmp = gtable(m1, handler=handler, action=action)
    lst[[i]] <- tmp; i <- i+1
    add(n,tmp,label="TimeSeries")
  }


  ## functions
  m1 = m[m[,2] %in% .functions,]
  if(nrow(m1) > 0) {
    tmp = gtable(m1, handler=handler, action=action)
    lst[[i]] <- tmp; i <- i+1
    add(n,tmp,label="functions")
  }


  ## plots
  m1 = m[m[,2] %in% .plots,]
  if(nrow(m1) > 0) {
    tmp = gtable(m1, handler=handler, action=action)
    lst[[i]] <- tmp; i <- i+1
    add(n,tmp,label="plots")
  }
  
  ## all
  if(nrow(m) > 0) {
    tmp = gtable(m, handler=handler, action=action)
    lst[[i]] <- tmp; i <- i+1
    add(n,tmp,label="all")
  }
  
  if(length(n) > 0) svalue(n) <- 1
  return(lst)
}


setClass("gVarbrowserrJava",
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )

## THe main object
setMethod(".gvarbrowser",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   handler = NULL,
                   action = "summary",
                   container = NULL,
                   ...) {
            
            force(toolkit)

            if(is.null(handler) && !is.null(action)) {
              handler = function(h,...) {
                value = svalue(h$obj,drop=TRUE)[1,1]
                if(!is.null(action))
                  print(do.call(h$action,list(svalue(value))))
              }
            }

            
            theHandler=handler
            theAction=action
            g = ggroup(horizontal=FALSE, container=container, ...)
            n = gnotebook(tab.pos=2)

            obj = new("gVarbrowserrJava",block=g, widget=n,
              toolkit=toolkit,ID=getNewID(), e = new.env())


            add(g,n, expand=TRUE)
            g1 = ggroup(cont=g)
            addSpring(g1)
            add(g1,gbutton("refresh",handler=function(h,...) {
              lst = .showVariableDataFrame(n, handler=theHandler,action=theAction)
              tag(obj,"lst") <- lst
            })
                )
            
            lst = .showVariableDataFrame(n, handler=theHandler, action=theAction)
            tag(obj,"lst") <- lst
            
            
            ## all done
            return(obj)
          })

### methods
## push methods and handlers down to tree in this case

setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gVarbrowserrJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            ## return currently selected -- but how?
            notebook = obj@widget
            n = svalue(notebook)
            lst = tag(obj,"lst")        # contains widgets in notebook
            svalue(lst[[n]], drop=TRUE) # need drop to get just variable
          })
setMethod("[",
          signature(x="gVarbrowserrJava"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x,guiToolkit("rJava"), i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gVarbrowserrJava"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            cat("No [ method for varbrowser in rJava\n")
          })




