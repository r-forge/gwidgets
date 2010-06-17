## interface to variables

## this should live in the gWidgets domain, not the individual toolkits!


##TODO:

## * responsevar-- droplist (update with data), with dnd, close subset dialog, formula editor dialog
## * formulavar -- droplist updated with data, with dnd, also has button for foumulat editor
## * datavar -- droplist updated with dnd, when updated chnages droplist in responsevar and formulavar
## * subsetvar -- updated with dnd, has subsetEditor
## * formulaEditor -- how to do? Must have following
##   adding binary terms: +, * ....
##   grouping via ()
##   wrapping terms: I(), tsvar(), ...
##   how to integrate lm, glm, sspir usages, nlm?


##################################################
## some specific widgets for handling variables




## call this instead of the others
## ivarariables(

gvariables = function(variableType=NULL,..., toolkit=guiToolkit()) {
  if(variableType %in% c("univariate","univariatetable","bivariate","model","lattice")) {
    tmp = switch(variableType,
      "univariate" = gunivariate(...,toolkit=toolkit),
      "univariatetable" = gunivariatetable(...,toolkit=toolkit),
      "fileurl" = gfileurl(...,toolkit=toolkit),
      "bivariate" = gbivariate(...,toolkit=toolkit),
      "model" = gmodel(...,toolkit=toolkit),
      "lattice" = glattice(...,toolkit=toolkit),
      "lmer" = glmer(...,toolkit=toolkit)
      )
    return(tmp)
  } else {
    ## can't do anything here yet
    ## expects something so we give back a box
    return(ggroup(...,toolkit=toolkit))
  }
}



## For these we return a string ready to go.

## take an idwidget, wrap an argument around it, return as a container
## with value method giving "arg=val"

setClass("gAddargrJava",
         representation(argument="character"),
         contains="gComponentrJava"
         )

addArg = function(argument,iwidget,container = NULL, toolkit=guiToolkit()) {
  
  group = ggroup(horizontal=TRUE, container=container)
  label = glabel(text=Paste(argument,"= "), container=group)
  add(group,iwidget)

  obj = new("gAddargrJava",block=group, widget=iwidget,
    toolkit=toolkit,ID=getNewID(), e = new.env(),
    argument=argument)
  return(obj)
}



setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gAddargrJava"),
          function(obj, toolkit, index=NULL, drop=NULL,  ...) {
            val = svalue(obj@widget)
            arg = obj@argument
            val = stripWhiteSpace(val)
            if(val == "") {
              return(NA)
            } else if(
                      is(obj@widget,"gEditListrJava") ||
                      is(obj@widget,"gEditNamedListrJava") ||
                      arg == "...") {
              return(val)
            } else {
              if(is.null(drop) || drop==FALSE)
                return(Paste(arg,"=",val))
              else
                return(val)
            }
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gAddargrJava"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   svalue(obj@widget) <- value
                   return(obj)
                 })

## the most basic drop handler 
basicdrophandler = function(h,...) svalue(h$obj) <- h$dropdata
##################################################
### UNIVARIATE
## returns gedit vaRlue
setClass("gUnivariaterJava",
         representation(widgets="list"),
         contains="gComponentrJava"
         )

gunivariate = function(xlabel="x",container=NULL, ..., toolkit=guiToolkit()) {
  frame = gframe(text = "<b>data</b>",markup=TRUE, horizontal=TRUE, container=container, ...)
  
  xentry = gedit(text="",width=30, container=NULL)
  xarg = addArg(argument=xlabel, xentry, container=frame)

  obj = new("gUnivariaterJava",block=frame, widget=frame,
    toolkit=toolkit, ID=getNewID(),  e = new.env(),
    widgets=list(xarg))
  return(obj)
}

setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gUnivariaterJava"),
          function(obj, toolkit, index=NULL, drop=NULL,  ...) {
            svalue(obj@widgets[[1]])
          })

##################################################
#######################################################################
## returns gedit value, perhaps in table()
setClass("gUnivariateTablerJava",
         representation(widgets="list"),
         contains="gComponentrJava"
         )

gunivariatetable = function(xlabel="x",container=NULL, ..., toolkit=guiToolkit()) {

  frame = gframe(text = "<b>data</b>",markup=TRUE, horizontal=TRUE, container=container, ...)
  xentry = gedit(text="",width=30, container=NULL)
  xarg = addArg(argument=xlabel, xentry, container=frame)
  glabel(" Tabulate data?",container=frame)
  doTable=gdroplist(c(TRUE,FALSE),container=frame)
  obj = new("gUnivariateTablerJava",block=frame, widget=frame,
    toolkit=toolkit,  e = new.env(),
    widgets=list(x=xarg, doTable=doTable))
  return(obj)
}

setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gUnivariateTablerJava"),
          function(obj, toolkit, index=NULL, drop=NULL,  ...) {
            vals = svalue(obj@widgets[[1]], drop=TRUE)
            doTable = as.logical(svalue(obj@widgets[[2]]))
            if(doTable)
              vals = paste("table(",vals,")", sep="", collapse="")
            return(vals)
          })


#################################################
#################################################
## a File browser with a switch for wrapping in url
## returns gedit value, perhaps in table()
setClass("gFileURLrJava",
         representation(widgets="list"),
         contains="gComponentrJava"
         )

gfileurl = function(xlabel="x",container=NULL, ..., toolkit=guiToolkit()) {

  frame = gframe(text = "<b>file</b>",markup=TRUE, horizontal=TRUE, container=container, ...)
  xentry = gfilebrowse(text="",width=40, container=NULL)
  xarg = addArg(argument=xlabel, xentry, container=frame)
  glabel("A url?",container=frame)
  doURL=gdroplist(c(FALSE,TRUE),container=frame)
  obj = new("gFileURLrJava",block=frame, widget=frame, toolkit=toolkit,
     e = new.env(),
    widgets=list(x=xarg, doURL=doURL))
  return(obj)
}

setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gFileURLrJava"),
          function(obj, toolkit, index=NULL, drop=NULL,  ...) {
            vals = svalue(obj@widgets[[1]])
            doURL = as.logical(svalue(obj@widgets[[2]])) # TRUE of FALSE
            if(doURL)
              vals = paste("url(",quoteIfNeeded(vals),")", sep="", collapse="")
            return(vals)
          })

##################################################
## bivariate
setClass("gBivariaterJava",
         representation(widgets="list"),
         contains="gComponentrJava"
         )

gbivariate = function(xlabel = "x", ylabel = "y", container=NULL, ...,
  toolkit=guiToolkit()) {
  
  frame = gframe(text = "<b>data</b>",markup=TRUE, horizontal=TRUE, container=container, ...)

  xentry = gedit(text="",width=30, container=NULL)
#  adddroptarget(xentry, handler = basicdrophandler)
  xarg = addArg(argument=xlabel, xentry, container=frame)
  
  yentry = gedit(text="",width=30, container=NULL)
                                        #  adddroptarget(yentry, handler = basicdrophandler)
  yarg = addArg(argument=ylabel, yentry, container=frame)
  
  obj = new("gBivariaterJava",block=frame, widget=frame,
    toolkit=toolkit, ID=getNewID(),  e = new.env(),
    widgets=list(xarg,yarg))
  return(obj)
}

setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gBivariaterJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            return(PasteWithComma(
                                  svalue(obj@widgets[[1]]), # xvalue
                                  svalue(obj@widgets[[2]])  # yvalue
                                  )
                   )
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gBivariaterJava"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   value = rep(value,2)[1:2]
                   .tmp = sapply(1:2, function(i) svalue(obj@widgets[[i]]) <- value[i])
                   return(obj)
                 })


##################################################
## model formula -- this has drag and drop stuff, and formula editing
setClass("gModelrJava",
         representation(widgets="list"),
         contains="gComponentrJava",
         )

gmodel = function(lattice=FALSE, container=NULL,...,toolkit=guiToolkit()) {
  ## containers
  frame = gframe(text = "<b>data</b>",markup=TRUE,  horizontal=FALSE, container=container, ...)
  line1 =ggroup(horizontal=TRUE, container=frame)
  if(lattice)
    conditionLine = ggroup(horizontal=TRUE, container=frame) # add if there is lattice
  line2 =ggroup(horizontal=TRUE, container=frame)
  line3 =ggroup(horizontal=TRUE, container=frame)
  
  ## we have 4 main things: response, predictor(s), data, subset
  
#  responseDropHandler = basicdrophandler 
#  predictorDropHandler = basicdrophandler
  editPredictorHandler = function(h,...) {
    
    editFormulaDialog(data=h$action$dataEntry,
                      responsewidget=h$action$responseEntry,
                      predictorwidget=h$action$predictorEntry)
  }
  
  dataDropHandler = function(h,...) {
    val.str = h$dropdata
    #    svalue(h$obj) <- val.str ## gets on enter handler?
    names = try(getNamesofObject(val.str))
    if(!inherits(names,"try-error") &&
       !is.null(names)) {
      ## set dropdown values:
      responseEntry[] <- names
      predictorEntry[] <- names
      conditionEntry[] <- names
      ## what else?
    }
    
  }
  dataEnterHandler = function(h,...) {
    val.str = svalue(h$obj)
    names = getNamesofObject(val.str)
    if(!is.null(names)) {
      ## set dropdown values:
      responseEntry[] <- names
      predictorEntry[] <- names
      conditionEntry[] <- names
      ## what else?
    }
  }
    
#  subsetDropHandler = basicdrophandler
  editSubsetHandler = function(h,...) {
    editSubsetDialog(data=h$action$dataEntry,
                     widget=h$action$subsetEntry)
  }
  editConditionHandler = function(h,...) {
    editConditionDialog(data=h$action$dataEntry,
                        widget = h$action$conditionEntry)
  }

  ### Build up the widgets now
  variableNames = c("",getNamesofObject())
  ## Define entries first
  responseEntry = gdroplist(variableNames,editable=TRUE)
  predictorEntry = gdroplist(variableNames,editable=TRUE)
  conditionEntry = gdroplist(variableNames, editable=TRUE)
  dataEntry = gedit("")
  subsetEntry = gedit("")

  

  gaddlabel(responseEntry, text="response", pos=1, container=line1)
#  adddroptarget(responseEntry, handler=responseDropHandler)
  
  ## add ~
  tmpgroup = ggroup(horizontal=FALSE, container=line1)
  glabel(" ~ ", container=line1)
  addSpring(tmpgroup)                 # give space
  
  ## predictor has extra button for editing formula
  predictorGroup = ggroup(container=line1)
  gaddlabel(predictorEntry, text="predictor(s)", pos=1, container=predictorGroup)
                                        #  adddroptarget(predictorEntry, handler=predictorDropHandler)
  tmpgroup = ggroup(horizontal=FALSE, container=line1) # get size correct
  gbutton("edit",container=tmpgroup, handler=editPredictorHandler,
          action=list(dataEntry=dataEntry,
            responseEntry=responseEntry,
            predictorEntry=predictorEntry))
  addSpring(tmpgroup)
  
  ## lattice has a conditioning variable
  if(lattice) {
    glabel(" conditioning variable(s) | ", container=conditionLine)
    add(conditionLine, conditionEntry)
    gbutton("edit",container=conditionLine,handler = editConditionHandler,
            action = list(dataEntry=dataEntry,
              conditionEntry =conditionEntry))
  }
  
  ## data
  table = glayout()
  table[1,1] = glabel("data=")
  table[1,2] = dataEntry
  addhandler(dataEntry,"activate",handler =dataEnterHandler)
  adddroptarget(dataEntry, handler=dataDropHandler)
  
  ## subset has extra button for editing
  table[2,1] = glabel("subset=")
  table[2,2] = subsetEntry

  editSubsetEntryButton = gbutton("edit",  handler=editSubsetHandler,
    action=list(dataEntry=dataEntry,subsetEntry=subsetEntry))
  table[2,3] = editSubsetEntryButton
  
  visible(table) <- TRUE
  add(line2,table)

  obj = new("gModelrJava",block=frame,widget=frame,
    toolkit=toolkit,ID=getNewID(), e = new.env(),
    widgets =
    list(response=responseEntry,
         predictor = predictorEntry,
         data = dataEntry,
         subset = subsetEntry,
         lattice=conditionEntry
         )
    )
  return(obj)
}

setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gModelrJava"),
          function(obj, toolkit,index=NULL, drop=NULL, ...) {
            lst = lapply(1:length(obj@widgets), function(i) svalue(obj@widgets[[i]]))
            names(lst) = c("response","predictor","data","subset","lattice")
            ## return a string of type r ~ p, data=..., subset=...
            str = with(lst, Paste(response," ~ ",predictor))
            if(lst$lattice != "")
              str = Paste(str, " | ", lst$lattice)
            if(lst$data != "") str = PasteWithComma(str,Paste("data=",lst$data))
            if(lst$subset != "") str = PasteWithComma(str,Paste("subset=",lst$subset))
            
            return(str)
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gModelrJava"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   if(is.list(value)) unlist(value)
                   .tmp = sapply(1:4, function(i) svalue(obj@widjets[[i]]) <- value[i])
                   
                   return(obj)
                 })


##################################################
## interface for lattice graphics
glattice = function(..., toolkit=guiToolkit()) return(gmodel(lattice = TRUE, ...,toolkit=toolkit))

##################################################
setClass("gLmerrJava",
         representation(formulaEntry="guiWidget",
                        dataEntry="guiWidget"),
         contains="gComponentrJava")


## interface for linear mixed effects models models
glmer = function(container=NULL, ..., toolkit=guiToolkit()) {
  
  frame = gframe(text = "<b>data</b>",markup=TRUE,  horizontal=FALSE, container=container, ...)
  formulaEntry = gedit("", width=40)
  dataEntry = gedit("")
  
  table = glayout(container=frame)
  table[1,1] = glabel("formula")
  table[1,2] = formulaEntry
  table[2,1] = glabel("data=")
  table[2,2] = dataEntry
  visible(table) <- TRUE
  
  obj = new("gLmerrJava",block=frame,widget=frame,toolkit=toolkit,ID=getNewID(), e = new.env(),
    dataEntry = dataEntry, formulaEntry = formulaEntry)

  return(obj)
}

setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gLmerrJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            formulaValue = svalue(obj@formulaEntry)
            dataValue = svalue(obj@dataEntry)
            
            if (formulaValue == "")
              return("")
            string = formulaValue
            if(dataValue != "")
              string = Paste(string, ", data=",dataValue)
            
            return(string)
          })





## some special edit fields
## this is used to get "..." into generic widget
setClass("gEditListrJava",
         contains="gEditrJava")

geditlist = function(...,toolkit=guiToolkit()) {
  edit = gedit(...,toolkit=toolkit)
  obj = new("gEditListrJava", block=edit@widget@block, widget=edit@widget@widget, toolkit=toolkit,ID=getNewID(),  e = new.env())
  invisible(obj)
}


setClass("gEditNamedListrJava",
         contains="gEditrJava")

geditnamedlist = function(..., toolkit=guiToolkit()) {
  edit = gedit(...,toolkit=toolkit)
  obj = new("gEditNamedListrJava", block=edit@widget@block, widget=edit@widget@widget, toolkit=toolkit,ID=getNewID(),  e = new.env())
  invisible(obj)
}


##################################################
##
## two dialogs for editing


## sample edit Formula dialog, Smilar to one in Splus, but layout is
## not as nice, not quite as feature rich

editFormulaDialog = function(
  data=NULL,                            # within these variables
  responsewidget = NULL,                # response value
  predictorwidget = NULL                # predictor value
  ) {
  ## actually get data, not just a string
  if(is(data,"guiWidget") || is(data,"gComponentrJava")) {
    data = svalue(data)
    dataName = deparse(substitute(data))
  }
  if(is.character(data) && length(data) == 1) {
    dataName = data 
    data = getObjectFromString(data)
  }

  if(is.na(data) || is.null(data)) {
    warning("Can't find data set")
    return(NA)
  }

  ## coerce data if possible
  if(!is.data.frame(data)) {
    tmp = try(as.data.frame(data), silent=TRUE)
    if(inherits(tmp,"try-error")) {
      warning("gtable shows data frames of vectors")
      return(NA)
    }
    data = tmp
  }
  varNames = names(data)
  varNames = data.frame("VariableNames"=varNames)
  varNames[,1] = as.character(varNames[,1])

  ## define key widgets
  variables = gtable(varNames,multiple=TRUE)
  
  addresponse = gbutton("Response")
  addinteraction = gbutton("* (main + interaction)")
  addterm = gbutton("+ (main effect)")
  addsecondpowers= gbutton("^2 (second-order)")
  addwithin = gbutton(": (interaction)")
  subtractintercept= gbutton("remove intercept")

  response = gedit(svalue(responsewidget))
  predictor = gedit(svalue(predictorwidget))

  okbutton = gbutton("ok")
  clearbutton = gbutton("clear")
  cancelbutton = gbutton("cancel")


  ## Set up the window
  ## main window
  win = gwindow("Edit model formula values",v=T)
  
  group = ggroup(horizontal=FALSE, container=win)
  
  datagroup = ggroup(container=group)
  size(datagroup) <- c(300,200)
  glabel("Dataset: ", container=datagroup)
  tmp = glabel(dataName, container=datagroup);
  font(tmp) <- list(style="bold")
  addSpace(datagroup, 10)
  add(datagroup, variables, expand=TRUE)

  
  add(group, gseparator())
  ## buttons

  buttonGroup = ggroup(container=group)
  glabel("Actions:", container=buttonGroup)
  table = glayout()
  add(buttonGroup, table, expand=TRUE)
  table[1,1] = addresponse
  table[1,2] = addterm
  table[2,1] = addwithin
  table[2,2] = addinteraction
  table[3,1] = addsecondpowers
  table[3,2] = subtractintercept
  visible(table) <- TRUE

  add(group, gseparator())
  table = glayout(container=group)
  table[1,1]=gaddlabel(response,"response",pos=1)
  table[1,2] = gaddlabel(glabel(" ~ "),"", pos=1)
  table[1,3:6] = gaddlabel(predictor,"predictor formula",pos=1)
  visible(table) <- TRUE

  buttonbox = ggroup(container=group)
  addSpring(buttonbox)
  add(buttonbox, okbutton)
  addSpace(buttonbox,15)
  add(buttonbox, clearbutton)
  add(buttonbox, cancelbutton)

  ## Now add handlers
  addhandlerclicked(addresponse, handler=function(h,...) {
    vals = svalue(variables)
    if(!is.null(vals)) {
      svalue(response) <- vals[1]
    }
  })
  addhandlerclicked(addinteraction,handler=function(h,...) {
    vars = svalue(variables)
    if(!is.null(vars)) {
      oldval = svalue(predictor)
      if(!is.null(oldval) && oldval !="")
        oldval = Paste(oldval, " + ")
      else
        oldval = ""
      svalue(predictor) <- Paste(oldval, paste(vars, sep="", collapse=" * "))
    }
  })
  addhandlerclicked(addterm,handler=function(h,...) {
    vars = svalue(variables)
    
    if(!is.null(vars)) {
      oldval = svalue(predictor)
      if(!is.null(oldval) && oldval !="")
        oldval = Paste(oldval, " + ")
      else
        oldval = ""
      svalue(predictor) <- Paste(oldval, paste(vars, sep="", collapse=" + "))
    }
  })
  addhandlerclicked(addsecondpowers,handler=function(h,...) {
    vars = svalue(variables)
    if(!is.null(vars)) {
      oldval = svalue(predictor)
      if(!is.null(oldval) && oldval !="")
        oldval = Paste(oldval, " + ")
      else
        oldval = ""
      svalue(predictor) <- Paste(oldval,
                                 " (",
                                 paste(vars, sep="", collapse=" + "),
                                 ")^2")
    }
  })
  addhandlerclicked(addwithin,handler=function(h,...) {
    vars = svalue(variables)
    if(!is.null(vars)) {
      oldval = svalue(predictor)
      if(!is.null(oldval) && oldval !="")
        oldval = Paste(oldval, " + ")
      else
        oldval = ""
      svalue(predictor) <- Paste(oldval,
                                 paste(vars, sep="", collapse=":")
                                 )
    }

  })
  addhandlerclicked(subtractintercept,handler=function(h,...) {
    svalue(predictor) <- Paste(svalue(predictor), " -1")
  })
  addhandlerclicked(okbutton, handler = function(h,...) {
    svalue(responsewidget) <- svalue(response)
    svalue(predictorwidget) <- svalue(predictor)
    dispose(win)
  })
  addhandlerclicked(clearbutton, handler=function(h,...) {
    svalue(response) <- ""
    svalue(predictor) <- ""
  })
  addhandlerclicked(cancelbutton,handler=function(h,...) {
    dispose(win)
  })
}

editSubsetDialog = function(
  data=NULL,
  widget = NULL                         # what to write to, start with
  ) {
  
  ## get data values
  if(is(data,"guiComponent") || is(data,"gComponentrJava")) {
    data = svalue(data)
  }

  
  if(is.character(data)) {
    if(data == "") {
      warning("A data set needs to be set")
      return()
    }
    dataName = data
    data = svalue(data)
  } else {
    dataName = deparse(substitute(data))
  }

  if(!is.data.frame(data)) {
    tmp = try(as.data.frame(data), silent=TRUE)
    if(inherits(tmp,"try-error")) {
      warning("gtable shows data frames of vectors")
      return(NA)
    }
    data = tmp
  }
  varNames = names(data)
  
  
  ## main widgets
  andOrPopup = gdroplist(c("","&","|"))
  notPopup  = gdroplist(c("","!"))
  var1 = gdroplist(c(""), editable=TRUE)
  var1[] <-  varNames
  logicalPopup = gdroplist(c("","<","<=","==","!=",">=",">","%in%"))
  var2 = gdroplist(c(""), editable = TRUE)
  var2[] <- varNames
  
  addButton = gbutton("add")
  clearButton =   gbutton("clear")
  
  output = glabel("")
  font(output) <- list(style="bold")  # make bold
  status = glabel()
  
                                        #  preview = gtable(head(data))
  preview = gtable(data)
  
  enabled(preview) <- FALSE
  
  okButton = gbutton("ok")
  cancelButton = gbutton("cancel")
  
  ## layout
  
  win = gwindow("Edit subset value",v=T)                    # main window
  group = ggroup(horizontal=FALSE, container=win)
  glabel(Paste("Data set:<b>", dataName,"</b>"),markup=TRUE, container=group)
  
  addGroup = ggroup(container=group)
  add(addGroup, gaddlabel(andOrPopup,"join with", pos=1))
  add(addGroup, gaddlabel(notPopup,"negate",pos=1))
  add(addGroup, gaddlabel(var1,"",pos=1))
  add(addGroup, gaddlabel(logicalPopup,"compare with",pos=1))
  add(addGroup, gaddlabel(var2,"",pos=1))
  
  buttonGroup = ggroup(container=group)
  addSpring(buttonGroup)
  add(buttonGroup, addButton)
  addSpace(buttonGroup,10)
  add(buttonGroup, clearButton)

  outputGroup = ggroup(container=group)
  glabel("Subset by",container=outputGroup)
  add(group, output)
  add(group, status)

  
  gseparator(container=group)
  spacingGroup = ggroup(container=group)
  glabel("Preview:", container=spacingGroup)
  addSpring(spacingGroup)

  add(group, preview, expand=TRUE)
  
  buttonGroup = ggroup(container=group)
  addSpring(buttonGroup)
  add(buttonGroup, okButton)
  addSpace(buttonGroup,10)
  add(buttonGroup, cancelButton)

  ## now give some actions
  addHandlerAction = function(h,...) {
    str = Paste(
      svalue(var1),
      svalue(logicalPopup),
      svalue(var2)
      )
    if(!is.null(svalue(notPopup)) && svalue(notPopup) == "!")
      str = Paste("!( ",str," )")
    ## error check
    tmp = try(rpel(Paste("subset(data,subset=",str,")"), envir=environment(NULL)))
    if(! inherits(tmp,"try-error")) {
      combine = svalue(andOrPopup)
      if(is.null(combine)) combine = ""
      previous = svalue(output)
      if(nchar(previous) == 0) combine=""
      if(nchar(previous) > 0 && combine == "") {
        svalue(status) <-"You need an operator to combine terms"
        return(FALSE)
      }
        
      svalue(output) <- Paste(previous," ",combine," ",str)
      svalue(status) <- ""
      ## update preview
      subsetVal = svalue(output)

      tmpVals = rpel(Paste("head(subset(data,subset=",subsetVal,"))"),
        envir=environment(NULL))
      preview[,] = tmpVals
    } else {
      svalue(status) <- Paste(str," failed")
    }
  }

  ## when logical is %in$ and leftside if factor, we can do some  business
  addhandlerchanged(logicalPopup,handler = function(h,...) {
    ## check that all conditions are met
    if(svalue(logicalPopup) == "%in%") {
      varName = svalue(var1)
      var = getObjectFromString(Paste(dataName,"$",varName))
      if(is.factor(var)) {
        varLevels = levels(var)
        selectVectorEntriesDialog(varLevels, var2)
      }
    }
  }
                    )
  
  addhandlerclicked(clearButton, handler = function(h,...) {
    svalue(output) <- ""
    svalue(status) <- ""
    preview[] <- head(data)
  })
  
  addhandlerclicked(addButton, handler=addHandlerAction)
  addhandlerclicked(okButton, handler = function(h,...) {
    outputValue = svalue(output)
    ## strip leading spaces
    svalue(widget) <- sub("^ +","",outputValue)
    dispose(win)
  })
  addhandlerclicked(cancelButton, handler=function(h,...) {
    dispose(win)
  })
}



## add terms 
editConditionDialog =function(data,widget) {

  if(is.na(data) || is.null(data)) {
    warning("empty dataset")
    return()
  } else if(data == "") {
    varNames = ls(envir=.GlobalEnv)
  } else {
    ## get data values
    if(is(data,"gComponentrJava")) {
      data = svalue(data)
    }
    if(is.character(data)) {
      dataName = data
      data = rpel(Paste("get(\"",data,"\")", sep=" "))
    } else {
      dataName = deparse(substitute(data))
    }
    
    if(!is.data.frame(data)) {
      tmp = try(as.data.frame(data), silent=TRUE)
      if(inherits(tmp,"try-error")) {
        warning("gtable shows data frames of vectors")
        return(NA)
      }
      data = tmp
    }
    varNames = names(data)
  }

  win = gwindow("Select variables for conditioning variable",
    visible=TRUE)
  group = ggroup(horizontal=FALSE, container=win)
  vars = gtable(varNames, multiple=TRUE, handler=function(h,...) {
    values = svalue(h$obj)
    if(length(values) > 0) {
      out = paste(values, collapse=" + ")
      svalue(widget) <- out
    }
    dispose(win)
  })
  add(group,vars, expand=TRUE)
  buttonGroup = ggroup(container=group)
  addSpring(buttonGroup)
  gbutton("ok",container=buttonGroup, action=vars,
          handler=function(h,...) {
            values = svalue(h$action)
            if(length(values) > 0) {
              out = paste(values, collapse=" + ")
              svalue(widget) <- out
            }
            dispose(win)
          })
  gbutton("close",container=buttonGroup, handler = function(h,...) dispose(win))
  status = gstatusbar("Select one or more variables to condition by.",
    container=group)

}

## select asks user for which columns to consider
editSelectDialog = function(data,widget) {

    ## get data values
  if(is(data,"gComponentrJava")) {
    data = svalue(data)
    if(data == "") {
      warning("A data set needs to be set")
      return()
    }
  }
  if(is.character(data)) {
    if(data == "") {
      warning("A data set needs to be set")
      return()
    }
    dataName = data
    data = rpel(Paste("get(\"",data,"\")", sep=" "))
  } else {
    dataName = deparse(substitute(data))
  }

  if(!is.data.frame(data)) {
    tmp = try(as.data.frame(data), silent=TRUE)
    if(inherits(tmp,"try-error")) {
      warning("gtable shows data frames of vectors")
      return(NA)
    }
    data = tmp
  }
  varNames = names(data)

  win = gwindow("Select variables", v=TRUE)
  group = ggroup(horizontal=FALSE,container=win)
  frame = gframe("<b>Select all the desired variables</b>",markup=TRUE)
  add(group,frame, expand=TRUE)
  varList = gtable(varNames, multiple=TRUE)
  add(frame, varList, expand=TRUE)

  submitButton = gbutton("submit",handler = function(h,...) {
    values = svalue(varList)
    ## wrap it up
    str = Paste("c(",paste(sapply(values,function(str) paste("'",str,"'",sep="")),sep=" ",collapse=", "),")")
    svalue(widget) <-  str
    ## lclose dialog
    dispose(win)
  })
  buttonGroup = ggroup(container=group)
  addSpring(buttonGroup)
  add(buttonGroup, submitButton)
}
  
  
  ## put values into a vector with paste
  selectVectorEntriesDialog  = function(vals, widget) {
    
    
    
    win=gwindow("Select values", v=T)
    group = ggroup(horizontal = FALSE, container=win)
    varNamesList = gtable(vals, multiple = TRUE)
    add(group,varNamesList, expand=TRUE)

    buttonGroup = ggroup(container=group)
    addSpring(buttonGroup)
    okButton = gbutton("ok",container=buttonGroup)
    cancelButton = gbutton("cancel", container=buttonGroup)

    addhandlerclicked(cancelButton, handler = function(h,...) {
      dispose(win)
    })
    addhandlerclicked(okButton, handler = function(h,...) {
      varNames = svalue(varNamesList)
      varNamesAsString = Paste("c(\"",paste(varNames,sep="'",collapse="\",\""),"\")")
      svalue(widget) <- varNamesAsString
      ## tidy up
      dispose(win)
    })
  }
