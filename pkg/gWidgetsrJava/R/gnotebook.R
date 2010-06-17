## TODO:
## * drag and drop onto tabs, raise on motion,
## * gracefully add more tabs
## * [] method returns jObject, not gWidget

## class previously defined
setMethod(".gnotebook",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   tab.pos = 3,                          # same as pos= in text
                   closebuttons = FALSE,
                   dontCloseThese = NULL,                 # integer of tabs not to close
                   container=NULL,                           # add to this container
                   ...) {
            
            force(toolkit)

            ## begin
##            notebook = .jnew("javax/swing/JTabbedPane")
            notebook = .jnew("gWidgetsrJava/gNotebook")            
            ## policies
            ## set scrollable
            ## how?
            
            ## tab placement: 1,2,3,4 -> 3,0,2,1
            types = c(
              .jfield(notebook,name="BOTTOM"),
              .jfield(notebook,name="LEFT"),
              .jfield(notebook,name="TOP"),
              .jfield(notebook,name="RIGHT"))
            tabposition = types[tab.pos]
            .jcall(notebook,,"setTabPlacement",as.integer(tabposition))


            
            ## create gnotebook object
            obj = new("gNotebookrJava", block=notebook, widget=notebook,
              toolkit=toolkit,ID=getNewID(),  e = new.env(),
              closebuttons = as.logical(closebuttons),
              dontCloseThese = ifelse(is.null(dontCloseThese),0,dontCloseThese))


            ## add to container
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj, ...)
            }


            invisible(obj)
          })

### methods



## return the current tab
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gNotebookrJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            if(!is.null(index)) {
              warning("No index argument for a gnotebook instance")
            }

            notebook = obj@widget
            return(notebook$getSelectedIndex() + 1)

          })

## set the current tab to value
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gNotebookrJava"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   notebook = obj@widget
                   nPages = notebook$getTabCount()
                   value = max(1,min(value,nPages)) - 1

                   .jcall(notebook, ,"setSelectedIndex", as.integer(value))
                   return(obj)
                 })


## remove the current tab
## this should be called delete -- which is used to remove objects
setMethod(".dispose",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gNotebookrJava"),
          function(obj, toolkit,  ...) {
            theArgs = list(...)
            to.right=ifelse(!is.null(theArgs$to.right), theArgs$to.right,FALSE)
            dontCloseThese = obj@dontCloseThese
            if(dontCloseThese == 0) dontCloseThese = NULL
            deleteOK = function(i) {
              if(is.null(dontCloseThese)) return(TRUE)
              if(i %in% dontCloseThese) return(FALSE)
              return(TRUE)
            }
            cur.page = svalue(obj)
  
            if(to.right) {
              no.pages = length(obj)
              no.right = no.pages - cur.page
    
              if(no.right > 0) {
                ## clear out, must work from last backwards
                for(i in no.right:1) {
                  if(deleteOK(cur.page - 1 + i + 1)) {
                    obj@widget$remove(as.integer(cur.page - 1 +i)) # cur.page 1-based
                    svalue(obj) <- cur.page
                  }
                }
              }
            } else {
              ## just this page
              if(deleteOK(cur.page - 1 + 1)) {
                obj@widget$remove(as.integer(cur.page - 1)) # value is 1 based, not 0
                svalue(obj) <- cur.page
              }
            }
          })

## remove the widget form the notebook
setMethod(".delete",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gNotebookrJava"),
          function(obj, toolkit, widget,  ...) {
            obj@widget$remove(as.jcomponent(getWidget(widget)))
          })


### add() is a workhorse method here. Several args available in ...
#add.gNotebook = functionf(obj, value,
#  label="", markup = FALSE,
#  index = NULL, override.closebutton = FALSE, ...) {
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gNotebookrJava",
                    value="guiWidget"),
          function(obj, toolkit, value,  ...) {
            .add(obj, toolkit, value@widget, ...)
          })

setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gNotebookrJava",
                    value="gWidgetrJava"),
          function(obj, toolkit, value,  ...) {
            tag(value, "parentContainer") <- obj
            
            ## in ... we have many possibilies
            ## label -- for setting label  (also look for name)
            ## index for setting the index of page to add
            ## markup -- markup label
            ## override.closebutton -- to not put closebutton even if set in constructor
            
            ## process ...
            theArgs = list(...)                      # for making generic
            if(!is.null(theArgs$label)) {
              label = theArgs$label
            } else if(!is.null(theArgs$name)) {
              label = theArgs$name
            } else {
              label = id(obj)
              if(is.null(label))
                label = "unnamed"
            }
            
            index = if (is.null(theArgs$index)) NULL else theArgs$index
            if(!is.null(theArgs$pageno)) index = theArgs$pageno # also called paegno
            markup = if (is.null(theArgs$markup)) FALSE  else theArgs$markup
            override.closebutton =
              if (is.null(theArgs$override.closebutton))
                FALSE
              else
                as.logical(theArgs$override.closebutton)

            
            ## let's go
            notebook = obj@widget

            
            ## label -- a string in rJava
            if(!is.character(label))
              label = svalue(label)

            ## need an [icon], label, tooltip
##            icon = .jnull(class = "javax/swing/ImageIcon")
            icon = .jnull(class="javax/swing/Icon")
            tooltip = .jnew("javax/swing/JToolTip")
            
            ## closebutton
            if(!is.null(obj@closebuttons) &&
               as.logical(obj@closebuttons) &&
               !override.closebutton) {
              
##              closeImage = gimage("cancel",dirname="stock",
              ## OVERRIDE cancel here, as thehandler doesn't work. Otherwise,
              ## it looks like one could click and have it close
              closeImage = gimage("open",dirname="stock",              
                handler = function(h,...) {
                  dispose(obj)
                  return(TRUE)
                })
              cat("Making close buttons\n")
              icon = getWidget(closeImage)
              icon = icon$getIcon()     # in a button
              icon = .jcast(icon,"javax/swing/Icon")
            } 


            group =ggroup()
            add(group, value, expand=TRUE)        # get to expand
            ## what to add
            page = getWidget(group)

            ## where to add
            if(is.null(index) | !is.numeric(index)) {
##                if(obj@closebuttons) {
##                  thePage = .jcall(notebook,"V","addTab",
##                    as.jstring(label),
##                    as.jcomponent(page),
##                    as.jstring(system.file("images/",package="gWidgetsrJava")),
##                    .jnew("java/lang/Boolean",TRUE))
##                } else {
              thePage = .jcall(notebook,"V","addTab",
                as.jstring(label),
                icon,
                as.jcomponent(page))
              ##               }
             } else {
                if(index < 0) index = 1
              thePage = .jcall(notebook,"V","insertTab",
                as.jstring(label),
                icon,
                as.jcomponent(page),
                .jnew("java/lang/String",""), ## tooltip
                as.integer(index - 1)
                )
            }
            
            ## Add DND actions for labels
            ## implement me. In RGtk2 used label widget

            ## add drop motion for labels
            
            ## move to newpage
            svalue(obj) <- thePage + 1

            ## pack
            top <- getTopLevel(obj)
            if(!is.null(top)) {
              .jcall(getBlock(top), "V", "pack")
            }

          })
            
## Regular R methods treat gnotebook like a vector

## find out number of pages
setMethod(".length",
          signature(toolkit="guiWidgetsToolkitrJava",x="gNotebookrJava"),
          function(x, toolkit) {
            x@widget$getTabCount()
          })

## return tabnames
setMethod(".names",signature(toolkit="guiWidgetsToolkitrJava",x="gNotebookrJava"),
          function(x, toolkit) {
            notebook = x@widget
            NPages = notebook$getTabCount()
            if(NPages == 0) {
              return(c())
            } else {
              theNames = sapply(1:NPages, function(i) {
                .jcall(notebook,"Ljava/lang/String;",
                       "getTitleAt",as.integer(i-1))
              })
              return(theNames)
            }
          })

## can assigne with names(x) <-x or even names(x)[i] <- "single name"
setReplaceMethod(".names",
                 signature(toolkit="guiWidgetsToolkitrJava",x = "gNotebookrJava"),
                 function(x,toolkit, value) {
                   n = length(x)
                   if(length(value) != n)
                     stop("New names for notebook must have proper length")
                   
                   notebook = x@widget
                   
                   NPages = notebook$getTabCount()
                   if(NPages == 0) {
                     return(c())
                   } else {
                     for(i in 1:NPages)
                       .jcall(notebook,,"setTitleAt",as.integer(i-1),as.jstring(value[i]))
                     }
                   invisible(x)
                 })


## return widget contained in notebook page i as a  list or single widget
setMethod("[",
          signature(x="gNotebookrJava"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gNotebookrJava"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            if(missing(i))
              i = 1:length(x)
            if(length(i) > 1) {
              lst = sapply(i,function(j)
                getNotebookPageWidget(x,pageno = j-1)
                )
              return(lst)
            } else {
              return(getNotebookPageWidget(x, pageno = i-1))
            }
          })


## Puts widget into a position
setReplaceMethod("[",
                 signature(x="gNotebookrJava"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gNotebookrJava"),
          function(x, toolkit, i, j, ..., value) {
            n = length(x)
            if(missing(i)) {
              add(x,value)                        # append!
            } else {
              if(length(i) == 1) {
                add(x, value, index = i)
              } else {
                warning("Can't use '[' method for more than 1 element")
              }
            }
          })


### handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gNotebookrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {

            cat("gnotebook: addhandlerchange not implemented.\n")
            
            addhandler(obj,"switch-page", handler,action)
          })


setMethod(".addhandlerexpose",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gNotebookrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandler(obj,"switch-page",handler, action)
          })

### helpers
## used in [. method
getNotebookPageWidget = function(obj, pageno=.svalue(obj)) {
  ## returning rJava instance.
  ## used setData to retrieve actual value stored in label
  notebook = obj@widget
  .jcall(notebook,"Ljava/awt/Component;",
         "getComponentAt",as.integer(pageno))
}

