## function to take a list and return a dialog

setClass("gGenericWidgetrJava",
         representation = representation("gComponentrJava",
           lst="list"),
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )


## ## helper function, used to cluster list elements with
## ## collect components of list
## glistcollector = function(lst, ncol=1, byrow = FALSE, c.instead = FALSE,
##   container=NULL,...) {

##   n = length(lst)
##   theNames = names(lst)
##   table = glayout()
##   vals = list()
  
##   for(i in 1:n) {
##     if(byrow == TRUE) {
##       trow = floor((i-1)/ncol) + 1
##       tcol = 2*((i-1) %% ncol) + 1
##     } else {
##       nrow = ceiling(n/ncol)
##       trow = (i-1) %% nrow + 1
##       tcol = 2*floor((i-1)/nrow) + 1
##     }
##     lstele = lst[[i]]
##     name = theNames[i]
##     table[trow,tcol] = glabel(Paste("$",name," ="))
##     vals[[name]] <- do.call(lstele$type, lstele[-1])
##     table[trow,tcol + 1] <- vals[[name]]
##   }

##   visible(table) <-TRUE
  
##   obj = list(ref = table$ref, vals = vals, c.instead = c.instead)
##   .class(obj) = c("gListCollector","gComponent","gWidget")
  
##   if (!is.null(container)) {
##     if(is.logical(container) && container == TRUE)
##       container = gwindow(visible=TRUE)
##     add(container, obj)
##   }
##   return(obj)
## }

## ## return lst with entries
## svalue.gListCollector = function(obj, ...) {

##   val.lst = obj$val
##   str =""
##   for(i in names(val.lst)) {
##     value = svalue(val.lst[[i]])
##     if(!is.null(value) && value != "")
##       str = Paste(str, ",",i,"=",value)
##   }
##   ## strip leading '
##   str = sub("^,","",str)
##   if(obj$c.instead) {
##     str = Paste("c(",str,")")
##   } else {
##     str = Paste("list(",str,")")
##   }
##   return(str)
## }


##################################################
## Main widget

setMethod(".ggenericwidget",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   lst,
                   okhandler = ok.cb,
                   cancelhandler = cancel.cb,
                   helphandler = help.cb,
                   cli = NULL,          # iCLi object for output
                   container=NULL,
                   ...
  ) {

            ## ReadParseEvaL -- saves typing
            rpel = function(string, envir=.GlobalEnv) {
              eval(parse(text=string), envir=envir)
            }

            
            ## if lst is not a list, but refers to a function, autogenerate
            if(!is.list(lst) && is.character(lst)) {
              if(rpel(Paste("is.function(",lst,")"))) {
                ## run automenugenerator
                lst = rpel(autogenerategeneric(lst))
              } else if(is.function(lst)) {
                lst = rpel(autogenerategeneric(lst))
              }
            }
            
            ## error check
            if(!is.list(lst)) {
              warning("ggenericwidget needs to be called with a list, a function name or a function to work\n")
              return()
            }

            
            ## store widgets here
            vals = list()
            
            
            ## add to this box, this is what gets returned
            mainGroup = ggroup(horizontal=FALSE, spacing=10,
              container=container, ...)
            
            ## Data
            if(!is.null(lst$variableType)) {
              
              if(lst$variableType == "univariate") {
                vals$Data = gunivariate(...)
              } else if(lst$variableType == "univariatetable") {
                vals$Data = gunivariatetable(...) 
              } else if(lst$variableType == "bivariate") {
                vals$Data = gbivariate(...) 
              } else if(lst$variableType == "model") {
                vals$Data = gmodel(...) 
              } else if(lst$variableType == "lattice") {
                vals$Data = glattice(...) 
              } else {
                cat(Paste("Need to implent variableType",lst$variableType))
                vals$Data = NULL
              }
              if(!is.null(vals$Data))
                add(mainGroup,vals$Data, anchor=c(-1,0))
            }
            ## arguments
            if(!is.null(lst$arguments)) {
              frame = gframe(text="<b>Arguments</b>", markup=TRUE,
                horizontal=FALSE)
              add(mainGroup,frame, anchor=c(-1,0))
              group = ggroup(horizontal=FALSE, container=frame)
              table = glayout(container=group)        # k rows, 4 cols
              trow = 1; tcol= 1
              ## function for recursing down list
              addWidgetToDialog = function(name, lstele) {
                
                if(is.null(lstele$type) || is.list(lstele$type)) {
                  ## recurse after adding seperator
                                        #        gseparator(horizontal=FALSE, container=group)
                  if(name != "arguments") {
                    ## add some indication that this is a seperate group. Not in a frame
                    ## but this might end up looking better if in a frame
                    label = glabel(Paste("<i>",name,"</i>"), markup=TRUE)
                    ##         font(label) <- value=list(weight="italic",style="bold")
                    table[trow,1] <<- label
                    table[trow,2:4] <<- gseparator(horizontal=TRUE)
                    trow <<- trow+1; tcol <<- 1
                  }
                  for(i in names(lstele)) {
                    
                    addWidgetToDialog(i,lstele[[i]])
                  }
                  ## increment rows
                  trow <<- trow+1; tcol <<- 1
                } else {
                  ## the listelement use the argument names so do.call just works.
                  if(!is.list(lstele$type) && lstele$type == "ilabel") {
                    table[trow,2:4] <<- glabel(lstele$text)
                    trow <<- trow+1; tcol <<- 1
                    ##        glabel(lstele$text, container=group)
                  } else {
                    ## not a label
                    table[trow,tcol] <<- glabel(Paste("<span underline='single'>",name,"</span>"),
                                                markup=TRUE,
                                                handler = function(h,...) {
                                                  showHelpAtArgument(name, lst$help)
                                                })
                    vals[[name]] <<- do.call(lstele$type, lstele[-1])
                    table[trow,tcol + 1] <<- vals[[name]]
                    
                    if(tcol == 3) trow <<- trow + 1
                    tcol <<- (tcol+2) %% 4
                  }
                }
              }
              
              
              ## now call function to recurse down list
              addWidgetToDialog("arguments",lst$arguments)
              ## now we need to make table visible
              visible(table) <- TRUE
            }
            
            ## do we assignto?
            assignto = NULL                       # initialize
            if(!is.null(lst$assignto)) {
              frame = gframe("<b>Assign output to:</b>", markup=TRUE)
              add(mainGroup,frame, anchor=c(-1,0))
              assignto = gedit("",container=frame)
              ##    adddroptarget(assignto)
            }
            
            
            ## now add buttons
            ok.cb = function(h,...) {
              vals = h$action$vals            # contains the object
              lst = h$action$lst              # original list
              
              if(!is.null(h$action$assignto) &
                 (is.gWidget(h$action$assignto) ||
                  is.guiWidget(h$action$assignto))
                 ) {
                assignto = svalue(h$action$assignto) #  passed as widget
              } else {
                assignto = h$action$assignto
              }
    
              head = lst$action$beginning
              tail = lst$action$end
              args = c()  
              for(i in names(vals)) {
                val = svalue(vals[[i]])
                if(is.empty(val)) next
                if(i == "..." | i == "Data" ) {
                  args = c(args,val)
                } else {
                  ## combine with name
                  args = c(args,Paste(i,"=",val))
                }
              }
              str = Paste(head, PasteWithComma(args), tail)
              if(!is.empty(assignto))
                names(str) = make.names(assignto)
              else
                assignto = NULL
              ## send to fcommandline instance
              if(is.null(h$action$cli)) {
                cli = gcommandline(command=str, assignto=assignto,
                  useGUI = FALSE, useConsole = TRUE,
                  container=NULL)
#                  container=gwindow(visible=TRUE))
              } else {
                cli = h$action$cli
              }
              command = str; names(command) <- assignto
              svalue(cli) <- command
            }
            
            cancel.cb = function(h,...) {
              action = h$action
              dispose(action)
            }
            
            help.cb = function(h,...) {
              helppage = h$action
              win = gwindow(Paste("Help on ", helppage), v=T)
              size(win) <- c(600, 400)
              group = ggroup(horizontal = FALSE, container=win)
              add(group, ghelp(topic=helppage), expand=TRUE)
              closeGroup = ggroup(container=group)
              addSpring(closeGroup)
              add(closeGroup,gbutton("cancel", handler = function(h,...) dispose(win)))
            }
            
            buttongroup = ggroup(container=mainGroup)
            addSpring(buttongroup)               # push to right
            gbutton("ok", action=list(vals=vals,lst=lst,assignto = assignto,cli=cli),
                    handler = ok.cb, container=buttongroup)
            addSpace(buttongroup, 25)
            if(is.gWindow(container)) {
              gbutton("cancel", action=container, handler = cancel.cb, container=buttongroup)
            }
            gbutton("help",action = lst$help,  handler = help.cb, container=buttongroup)
            
            ## return the container now that it has all the stuff in in.
            obj = new("gGenericWidgetrJava",block=mainGroup, widget=mainGroup,
              toolkit=toolkit,ID=getNewID(), lst = lst)
            invisible(obj)
})

### methods
## return the list
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gGenericWidgetrJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ..) {
            return(obj@lst)
          })


## The call
## ggenericwidget(rpel(autogenerategeneric(f)))

### returns a string that evaluates to a list
autogenerategeneric = function(f,
  help = fName,                             # name of help function
  variableType = NULL   # one ofc(NULL,"univariate","bivariate","model","lattice"),
  ) {
  
  if(is.function(f)) {
    fName = deparse(substitute(f))
  } else{                               # f is a string
    fName = f 
    f = get(f)
  }
  lst = formals(f)

  ## figure out variableType: univariate, bivariate, model or lattice
  if(is.null(variableType)) {
    firstArg = names(lst)[1]

    if(firstArg == "formula") {
      variableType = "model"
      lst[['formula']] <- NULL
      lst[['data']] <- NULL
      lst[['subset']] <- NULL
    } else if(firstArg == "x" & length(names(lst)) > 1) {
      secondArg = names(lst)[2]
      if(secondArg == "y") {
        variableType = "bivariate"
        lst[['x']] <- NULL
        lst[['y']] <- NULL
      } else {
        variableType = "univariate"
        lst[['x']] <- NULL
      }
    } 
  }

  ## make list as a string
  out = Paste(fName,".LIST <- list(", "\n")
  out = Paste(out, "\ttitle = \"",fName,"\",\n")
  out = Paste(out, "\thelp = \"",help,"\",\n")
  out = Paste(out, "\tvariableType = \"",variableType,"\",\n")
  out = Paste(out, "\tassignto = ","TRUE",",\n")
#  out = Paste(out, "\taction = list\(\n")
    out = Paste(out, "\taction = list(\n")
  out = Paste(out, "\t\tbeginning = \"",fName,"(\", \n")
  out = Paste(out, "\t\tending = \")\"),\n")
#  out = Paste(out, "\targuments = list\(\n")
    out = Paste(out, "\targuments = list(\n")
#  out = Paste(out, "\t\tArguments = list\(\n") # add or change these
  

  

  for(i in names(lst)) {
    ## intercept the type
    name = i
    val = lst[[i]]
    type = str1(lst[[i]])$type

#    cat(f,":",i,"is of type",type,"\n")
    
    if(0) {
      ### want to test for empty
      ## what do do here -
      out = Paste(out,"\t\t\t\"","...",'"',"=list(\n")
      out = Paste(out,"\t\t\t\ttype= \"gedit\",\n")
      out = Paste(out,"\t\t\t\ttext=",'""'  ,"),\n")
    } else {
      ## these vals we handle generically
      val1 = "placeholder"
      switch(type,
             "name" = {
               out = Paste(out,"\t\t\t\"",name,'"',"=list(\n")
               out = Paste(out,"\t\t\t\ttype= \"gedit\",\n")
               out = Paste(out,"\t\t\t\ttext=", '""'  ,"),\n")
             },
             "numeric" = {
               out = Paste(out,"\t\t\t\"",name,'"',"=list(\n")
               out = Paste(out,"\t\t\t\ttype= \"gedit\",\n")
               out = Paste(out,"\t\t\t\tcoerce.with= as.numeric,\n")
               out = Paste(out,"\t\t\t\ttext=\"", val  ,"\"),\n")
             },
             "logical" ={
               out = Paste(out,"\t\t\t\"",name,'"',"=list(\n")
               out = Paste(out,"\t\t\t\ttype= \"gradio\",\n")
               out = Paste(out,"\t\t\t\tindex= FALSE,\n")
               if(!is.na(val) && !is.null(val) && is.logical(val) && val == FALSE) {
                 out = Paste(out,"\t\t\t\titems=", 'c(FALSE, TRUE)'  ,"),\n")
               } else {
                 out = Paste(out,"\t\t\t\titems=", 'c(TRUE,FALSE)'  ,"),\n")
               }
             },
             "character" = {
               out = Paste(out,"\t\t\t\"",name,'"',"=list(\n")
               out = Paste(out,"\t\t\t\ttype= \"gedit\",\n")
               out = Paste(out,"\t\t\t\tcoerce.with= as.character,\n")               
               out = Paste(out,"\t\t\t\ttext=\"\'", val  ,"\'\"),\n")
             },
             "call" = {
               ## only handle c(...)
               ## others are just blank gentry
               callVal = as.character(val)
               if(callVal[1] == "c") {
                 items = eval(val)
                 quoteIt = '\"'
                 if(class(items) == "numeric")
                   quoteIt = ""

                 ## combin
 #                theValue = "c("
#                 for(j in 2:length(callVal)) {
#                   theValue=Paste(theValue, quoteIt,callVal[j],quoteIt, ',')
 #                }
#                 theValue=Paste(theValue,'"")') #add emput
                 tmp = paste(quoteIt, items, quoteIt, sep = "", collapse=", ")
                 theValue = paste("c(", tmp, ")")
                 
                 out = Paste(out,"\t\t\t\"",name,'"',"=list(\n")
                 if(length(callVal) >3) {
                   out = Paste(out,"\t\t\t\ttype= \"gdroplist\",\n")
                   if(class(items) == "character")
                     out = Paste(out,"\t\t\t\tdo.quote= TRUE,\n")
                 } else {
                   out = Paste(out,"\t\t\t\ttype= \"gradio\",\n")
                   out = Paste(out,"\t\t\t\tindex= FALSE,\n")
                 }
                 out = Paste(out,"\t\t\t\titems=", theValue  ,"),\n")
               } else {
                 ## just leavl blank
                 out = Paste(out,"\t\t\t\"",name,'"',"=list(\n")
                 out = Paste(out,"\t\t\t\ttype= \"gedit\",\n")
                 out = Paste(out,"\t\t\t\ttext=\"\"),\n")
               }
             },
             "NULL" = {
               out = Paste(out,"\t\t\t\"",name,'"',"=list(\n")
               out = Paste(out,"\t\t\t\ttype= \"gedit\",\n")
               out = Paste(out,"\t\t\t\ttext=", "\"NULL\""  ,"),\n")
             },                           # default
             {
             out = Paste(out,"Don't know what to do\n")
           })
    }
  }
  ## finish up
  out = Paste(out, "))\n")              # trimmed a ) for Arguments


  return(out)
}


