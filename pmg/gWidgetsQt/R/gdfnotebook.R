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

## TODO
## * need to fix this! what to do with a new data frame. Unlike RGtk2,
##   we don't have ability to add rows/columns (yet)

setClass("gDfNotebookQt",
         contains="gNotebookQt",
         prototype=prototype(new("gNotebookQt"))
         )

setMethod(".gdfnotebook",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   items = NULL,
                   container = NULL,
                   ... # passed to Group, gnotebook = nb,
                       # notebook = nb$notebook)
    ) {

            force(toolkit)
            
            ## set up notebook
            ## put notebook into a group
            mainGroup = ggroup(horizontal=FALSE, container=container, ...)
            buttonGroup = ggroup(spacing = 6, container = mainGroup)
            nb = gnotebook(cont=mainGroup, expand=TRUE)


            
            obj = new("gDfNotebookQt",
              block=getBlock(mainGroup),
              widget = getWidget(nb),       # for inheritance of methods
              closebuttons = TRUE,
              dontCloseThese = 0,
              toolkit = toolkit,
               e = new.env(), ID=getNewID())
            
            ## set up buttons
            openButton = gbutton("open",handler = function(h,...) {
              openPageDfNotebookDialog(h$action)
            }, action=obj, container=buttonGroup)
            saveButton = gbutton("save",handler = function(h,...) {
              savePageDfNotebookDialog(h$action)
            }, action=obj, container=buttonGroup)
            addSpace(buttonGroup, 6)
            closeButton = gbutton("close",handler = function(h,...) {
              dispose(h$action)
            }, action=obj, container=buttonGroup)
            ##   renameButton = gbutton("rename",handler = function(h,...) {
            ##     renamePageDfNotebook(h$action)
            ##   }, action = obj, container=buttonGroup)
            ## add page if non null
            if(!is.null(items))
              add(obj, items)

            
  

  
            return(obj)
          })

##################################################
##
## gWidgetMethods (inherits others from gnotebook
## object is name of R object *or* file
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt",obj="gDfNotebookQt", value="character"),
          function(obj, toolkit, value, ...) {
            df <- get(value, envir=.GlobalEnv)
            .add(obj, toolkit, df, ...)
          })

##' add a matrix
##' Here in case we want to do something before coercion to a data frame
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt",obj="gDfNotebookQt", value="matrix"),
          function(obj, toolkit, value, ...) {
            value <- data.frame(value, stringsAsFactors=FALSE)
            .addDataFrameToNotebook(obj, value, ...)
          })

setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt",obj="gDfNotebookQt", value="data.frame"),
          function(obj, toolkit, value, ...) {
            .addDataFrameToNotebook(obj, value, ...)
          })



## setMethod(".add",
##           signature(toolkit="guiWidgetsToolkitQt",obj="gDfNotebookQt", value=),
##           function(obj, toolkit, value, ...) {
##             theArgs = list(...)

##             ## value is dataframe or available from string
##             if(is.character(value) && length(value) == 1)
##               adf = svalue(value)
##             else
##               adf = value
            
            
##             if(is.dataframelike(adf) || is(adf,"guiContainer") || is(adf,"gGridQt")){

##               if(is(adf,"gGridQt"))
##                 editdf = adf
##               else if (is(adf,"guiContainer"))
##                 editdf = adf@widget
##               else
##                 editdf = gdf(adf, do.subset=TRUE)
    
##               ## The name
##               if(!is.null(theArgs$label))
##                 theName = theArgs$label
##               else if(!is.null(theArgs$name))
##                 theName = theArgs$name
##               else
##                 theName = id(value)
    
##               if(is.null(theName)) theName = "dataset"

##               ## the label
##               label = glabel(theName)
              
##               ## toolbar stuff
##               lst = list()
##               lst$"New scratch  area"$handler = function(h,...) {
##                 newBlankPage(obj@gnotebook)
##               }

##               lst$"Save sheet"$handler = function(h,...) {
##                 savePageDfNotebookDialog(obj)
##               }
##               lst$"Save sheet"$icon = "save"

##               lst$"Close sheet"$handler = function(h,...) {
##                 dispose(obj)
##                 ##      closePageDfNotebook(obj)
##               }
##               lst$"Close sheet"$icon = "close"

##               lst$"Rename sheet"$handler = function(h,...) 
##                 renamePageDfNotebook(obj)
##               lst$"Rename sheet"$icon = "rename"
##               add3rdmousepopupmenu(label, lst)
              
##               ## add to notebook
##               add(obj@gnotebook, editdf, label = label)
##               ## now add in popupmenu to columns. This should be in geditdataframe
##               ## but the singals don't get passed back the way they should
##                                         #    addPopupToPage(editdf, obj)
##             } else {
##               gmessage(Paste("Can't open ",value,": can not be coerced into a data frame.\n"), icon="error")
##               return()
##             }
##           })
          

##################################################
## dialogs
openPageDfNotebookDialog <- function(nb, ...) {
  
  ## dialog for selecting variable to open
  tmp = ls(envir=.GlobalEnv)
  if(length(tmp) == 0) {
    dataframelike = data.frame(Avail.DataSets = "", stringsAsFactors=FALSE)
  } else {
    dataFrameInds = sapply(tmp, function(x) is.dataframelike(svalue(x)))
    if(any(dataFrameInds)) {
      dataframelike = tmp[dataFrameInds]
      dataframelike = data.frame(Avail.DataSets = dataframelike, stringsAsFactors=FALSE)
    } else {
      dataframelike = data.frame(Avail.DataSets = "", stringsAsFactors=FALSE)
    }
  }

  w <- gwindow("Select an available data set, or use New for a blank one")
  g <- ggroup(cont=w, horizontal=FALSE)
  widget <- gtable(items=dataframelike, container=g)
  addDataFrameHandler <- function(h,...) {
    dataname = svalue(widget)
    add(nb, svalue(dataname), label=dataname)
    svalue(nb) <- length(nb)            # focus on new one
    dispose(w)
  }
  addHandlerChanged(widget, handler=addDataFrameHandler)

  bg <- ggroup(cont=g)                  # button group
  addButton <- gbutton("add", cont=bg)
##  newButton <- gbutton("new", cont=bg)
  cancelButton <- gbutton("cancel", cont=bg)

  addHandlerClicked(addButton,  handler=addDataFrameHandler)
  ## addHandlerClicked(newButton, handler=function(h,...) {
  ##   tmp <- cbind(do.call(paste("as.",svalue(theType),sep=""),
  ##                        list(rep(NA, length=svalue(theNoCols)))))
  ##   colnames(tmp)[1] = svalue(theName)
  ##   add(nb, tmp, label=.getScratchName(nb))
  ##   dispose(w)
  ## })
  addHandlerClicked(cancelButton, handler=function(h,...) dispose(w))

}

  
old.openPageDfNotebookDialog = function(nb, ...) {
  ## dialog for selecting variable to open
  tmp = ls(envir=.GlobalEnv)
  if(length(tmp) == 0) {
    dataframelike = data.frame(Avail.DataSets = "", stringsAsFactors=FALSE)
  } else {
    dataFrameInds = sapply(tmp, function(x) is.dataframelike(svalue(x)))
    if(any(dataFrameInds)) {
      dataframelike = tmp[dataFrameInds]
      dataframelike = data.frame(Avail.DataSets = dataframelike, stringsAsFactors=FALSE)
    } else {
      dataframelike = data.frame(Avail.DataSets = "", stringsAsFactors=FALSE)
    }
  }
     

  theTitle = "Double click a data set to select"
  win = gwindow(theTitle, visible=TRUE, parent=nb)
  group = ggroup(horizontal=FALSE, container=win)
  ## define lgroup and lgroup. Later we add to panedgroup
  lgroup = ggroup(horizontal=FALSE)
  glabel(theTitle, container = lgroup)
  widget = gtable(items=dataframelike, handler = function(h,...) {
    dataname = svalue(h$obj)
    add(nb, svalue(dataname), label=dataname)
    svalue(nb) <- length(nb)            # focus on new one
    dispose(win)
  })
  add(lgroup, widget, expand=TRUE)
  
  rgroup = ggroup(horizontal=FALSE)
  glabel("Or fill in the following to add a new sheet", container=rgroup)
  tbl = glayout(); add(rgroup, tbl, expand=TRUE)

  theType <- gcombobox("numeric","character","factor") 
  theNoCols = gspinbutton(from=1,to=100,by=1,value=1)
  tbl[1,1] <- gettext("First variable name:");
  tbl[1,2] <- (theName <- gedit("X1", cont=tbl))
  tbl[2,1] <- gettext("Its type:");
  tbl[2,2] <- (theType <- gcombobox(c("numeric","character","factor"), cont=tbl))
  tbl[3,1] <- gettext("No. rows:");
  tbl[3,2] <- (theNoCols <- gspinbutton(from=1,to=100,by=1,value=1, cont=tbl))

  buttonGroup=ggroup(container=rgroup); addSpring(buttonGroup)
  gbutton("add",container=buttonGroup, handler= function(h,...) {
    tmp <- cbind(do.call(paste("as.",svalue(theType),sep=""),
      list(rep(NA, length=svalue(theNoCols)))))
    colnames(tmp)[1] = svalue(theName)
    add(nb, tmp, label=.getScratchName(nb))
    dispose(win)
  })
  

  gpanedgroup(lgroup,rgroup,container=group)
  gseparator(container=group)
  buttonGroup = ggroup(container=group)
  addSpring(buttonGroup)
  gbutton("cancel",container=buttonGroup,handler = function(h,...) dispose(win))
  
}
### what popup on the buttons do you want
addPopupToPage = function(obj, nb) {    # obj is gdf instance
  ## nb is gdfnotebook instance for adding to...
  f = function(h,...) {
    view.col = h$obj                           # treeview
    obj = h$action

    lst = list()
    lst$"Apply function to column"$handler = function(h,...) {
      win = gwindow("Apply function to column",visible=TRUE)
      group = ggroup(horizontal = FALSE, container=win)
      glabel("<b>Apply function to column</b>", markup=TRUE, container=group)
      tmpGroup = ggroup(container=group)
      glabel("<b>function(x) = {</b>", markup=TRUE,container=tmpGroup)
      addSpring(tmpGroup)
      FUN = gtext(container=group)
        tmpGroup = ggroup(container=group)
        glabel("}", container=tmpGroup)
        addSpring(tmpGroup)
        buttonGroup = ggroup(container=group)
        addSpring(buttonGroup)
        gbutton("ok",container=buttonGroup,handler = function(h,...) {
          FUN = Paste("function(x) {",svalue(FUN),"}")
          f = eval(parse(text=FUN))
          col.no = tag(view.col,"column.number") - 1 # rownames offset
          theNewVals = f(obj[,col.no, drop=FALSE])
          obj[,col.no] = theNewVals
          dispose(win)
        })
        gbutton("cancel",container=buttonGroup, handler = function(h,...)
                dispose(win))
      }
    lst$"Clear column"$handler = function(h,...) {
      col.no = tag(view.col,"column.number") - 1 # rownames offset
      obj[,col.no] = rep(NA, length(view.col))
    }
    lst$"Sort by column (decreasing)"$handler = function(h,...) {
      col.no = tag(view.col,"column.number") - 1 # rownames offset
      newOrder = order(obj[,col.no], decreasing = TRUE)
      obj[,] = obj[newOrder,]
      rownames(obj) = rownames(obj)[newOrder]
    }
    lst$"Sort by column (increasing)"$handler = function(h,...) {
      col.no = tag(view.col,"column.number") - 1 # rownames offset
      newOrder = order(obj[,col.no], decreasing = FALSE)
      obj[,] = obj[newOrder,]
      rownames(obj) = rownames(obj)[newOrder]
    }
    lst$"Rename column"$handler = function(h,...) {
      win = gwindow("Change name", visible=TRUE)
      group = ggroup(horizontal=FALSE, container=win)
      ok.handler = function(h,...) {
        newVal = make.names(svalue(newName))
        id(view.col) <- newVal
        dispose(win)
        return(FALSE)
      }
      newName = gedit(id(view.col),container=group)
      addhandlerchanged(newName, handler=ok.handler, action=newName)
      buttonGroup = ggroup(container=group);addSpring(buttonGroup)
      add(buttonGroup,gbutton("ok", handler = ok.handler, action=newName))
      add(buttonGroup,gbutton("cancel",handler=function(h,...) dispose(win)))

      return(TRUE)
    }
    ## This shows that we can make new pages if desired, as nb is passed in
    ##     lst$testnew$handler = function(h,...)
    ##       add(nb$notebook, glabel("new things"),"delete me")
    ## now make the menu bar, see add3rdbuttonpopup.default


    ## XXX GTK specific -- replace with QT
    ## mb = gmenu(lst, popup=TRUE)
    ## event = gdkEventNew(GdkEventType["button-press"])
    ##   ## do the popup
    ##   mb@widget$PopupHack(button = event$GetButton(), activate.time=event$GetTime())
    }

  ## now add the popup to the buttons. (The widgets are labels, but
  ## signals are not being passed along when the button is clicked,
  ## hence this being here, not in geditdataframe.


  ## XXX Gtk specific, replace with Qt
  ## cols = obj@view$GetColumns()
  ## callbackIDs = sapply(1:length(cols), function(i) {
  ##   gtktry(connectSignal(cols[[i]]$GetWidget()$GetParent()$GetParent()$GetParent(),
  ##                 signal="clicked",
  ##                 f = f,
  ##                 data = list(obj=cols[[i]], action=obj, col=i-1), # 0 base columns
  ##                 user.data.first = TRUE,
  ##                 after = TRUE),
  ##       silent=TRUE)
  ## })
  ## invisible(callbackIDs)
}

##' save current page
##' If a scratch notebook, then saves each column as a vector with the given name
savePageDfNotebookDialog <- function(nb, ...) {
  if(! inherits(nb,"gDfNotebookQt"))
    stop("Must be a dfNotebook to use me")
  
  ## dataframe
  ## nb stores gridobject, and tab is name
  curPage = svalue(nb)
  if(curPage == 0)                      # nothing to save
    return(TRUE)

  ## save it
  gridObj = nb[curPage]                   # widget store
  dfName = names(nb)[curPage]             # for tab label


  df = gridObj[,, drop=FALSE]
  names(df) <- names(gridObj)           # fix names

  
  ## if name match *scratch:no* then we save variables, not as data frame
  if(length(grep("^\\*scratch:[[:digit:]]+\\*$", dfName)) > 0) {

    for(i in names(df)) {
      val = df[,i]

      ind <- which(val != "")
      if(length(ind)) 
        val <- val[1:max(ind)]
      else
        val <- val

      if(is.character(val)) {
        tmpfile = tempfile()
        sink(tmpfile)
        tmp = as.numeric(val)
        if(all(!is.na(tmp)))
          val = tmp
        sink(NULL)
        unlink(tmpfile)
      }
      assign(i, val, envir=.GlobalEnv)
    }
  } else {
    ## save entire data set, only trick is $ possibility
    if(length(grep("\\$",dfName)) > 0) {
      cat(gettext("Can't save with $ in name. Rename data set.\n"))
    } else {
      assign(dfName, df, envir=.GlobalEnv)
    }
  }
}

## rename the page
renamePageDfNotebook = function(nb, ...) {
  old.text = names(nb)[svalue(nb)]
  win = gwindow("Rename data values", visible=TRUE, parent=nb)
  group = ggroup(horizontal = FALSE, container=win)
  glabel("Rename data values", container=group)
  edit = gedit(old.text, container=group)
  buttonGroup = ggroup(horizontal=TRUE, container=group)
  addSpring(buttonGroup)
  gbutton("ok",container=buttonGroup, handler=function(h,...) {
    new.text = make.names(svalue(edit))
    names(nb)[svalue(nb)] = new.text
#    curNames = names(nb)
#    curNames[svalue(nb)] = new.text
#    names(nb) = curNames
    dispose(win)
  })
  gbutton("cancel",container=buttonGroup, handler = function(h,...) {
    dispose(win)
  })
}



########################################
## helpers

.addDataFrameToNotebook <- function(obj, dfname, ...) {
  theArgs <- list(...)
  label <- getWithDefault(theArgs$label, "")
  out <- .gdf(dfname, container=obj, label=label)
  ## add handlers to out
}


.getScratchName = function(nb,...) {
  ## get the proper names
  ## the tab labels
  tabNames = names(nb)
  scratchPads = tabNames[grep("^\\*scratch:[[:digit:]]+\\*$", tabNames)]
  newName =  "df"
  if(length(scratchPads) > 0) {
    scratchPadsNos = as.numeric(gsub("^\\*scratch:([[:digit:]])+\\*$","\\1", scratchPads))
    newName = Paste("*scratch:",1+max(scratchPadsNos),"*") 
  } else {
    newName = "*scratch:1*"
  }
  return(newName)
}

newBlankPage = function(nb, nrow=25, ncol = 10) {
  ## balnk widget
##  editdf = hack.as.data.frame(matrix("",nrow=nrow,ncol=ncol))

  obj = gdf()
  newName = .getScratchName(nb)
  add(nb, obj, label=newName)
}

