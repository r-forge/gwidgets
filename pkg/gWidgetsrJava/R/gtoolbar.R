## gtoolbar, similar to gmenu
## need to incorporate delete/add methods as in imenu


setClass("gToolbarrJava",
         representation = representation("gComponentrJava",
           style="character"),
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )

## turn a list into a uimgr object
setMethod(".gtoolbar",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   toolbarlist,
                   style = c("both","icons","text","both-horiz"),
                   action=NULL,
                   container=NULL, ...) {

            force(toolkit)
            
            style = match.arg(style)


            toolbar = .jnew("javax/swing/JToolBar")


            mapListToToolBar(toolbar, toolbarlist, style)

            obj = new("gToolbarrJava",block=toolbar, widget=toolbar,
              toolkit=toolkit, ID=getNewID(),  e = new.env(),
              style=style)

            tag(obj,"toolbarlist") <- toolbarlist
            
            ## attach to container
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj, expand=TRUE, anchor=c(-1,1))
            }

            invisible(obj)
  
          })


## helpers

addButton = function(tb, style, text=NULL, icon=NULL,handler=NULL, action=NULL) {

  if(style == "icons") {
    button = gimage(icon,dirname="stock",handler=handler,action=action)
  } else if(style == "text") {
    button = gbutton(text,handler=handler,action=action)
  } else if(style == "both" || style == "both-horiz") {
    button = gimage(icon,dirname="stock",handler=handler,action=action)
    button@widget@widget$setText(text)
  }
  
  ## add the button to the toolbar
  .jcall(tb,"Ljava/awt/Component;","add",
         .jcast(button@widget@widget,"java/awt/Component"))
}


mapListToToolBar = function(tb, lst, style) {
  ## list is simple compared to menubar
  for(i in names(lst)) {
    tmp <- lst[[i]]
    label <- i

    if(.isgSeparator(tmp))
      tmp <- list(separator=TRUE)
    
    if(.isgAction(lst[[i]])) {
      tmp <- getToolkitWidget(lst[[i]])
      label <- tmp$label
    }

    if(!is.null(tmp$separator)) {
      ## add separator
      .jcall(tb,"V","addSeparator")
    } else if(!is.null(tmp$handler)) {
      ## how to decide there are no text parts?
      b <- addButton(tb, style, label, tmp$icon, tmp$handler, tmp$action)
      ## store if gaction
      if(.isgAction(lst[[i]])) {
        if(is(lst,"gActionrJava"))
          e <- lst[[i]]@e
        else
          e <- lst[[i]]@widget@e
        l <- e$toolbaritems
        l[[length(l) + 1]] <- b
        e$toolbaritems <- l
      }
    }
  }
}


### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gToolbarrJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            tag(obj, "toolbarlist")
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gToolbarrJava"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   if(!is.list(value)) 
                     stop("A toolbar requires a list to define it.")

                   toolbar = obj@widget
                   ## delete from toolbar
                   n = length(tag(obj,"toolbarlist"))
                   for(i in (n-1):0)
                     toolbar$remove(as.integer(i))
                   
                   mapListToToolBar(toolbar, value, obj@style)

                   tag(obj,"toolbarlist") <- value
                   
                   ##  all done
                   return(obj)
                 })

## returns list, or part of list
setMethod("[",
          signature(x="gToolbarrJava"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gToolbarrJava"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            lst = tag(x,"toolbarlist")
            if(missing(i))
              return(lst)
            else
              return(lst[[i]])
          })

setReplaceMethod("[",
                 signature(x="gToolbarrJava"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gToolbarrJava"),
          function(x, toolkit, i, j, ..., value) {
            if(!is.list(value))
              stop("assignment must be a list defining a (part) of a toolbar.")
            lst = tag(x,"toolbarlist")
            if(missing(i))
              lst = value
            else
              lst[[i]] = value
            
            svalue(x) <- lst
            
            return(x)
          })


## add to a gwindow
## right way is to use border layout to add to a container
## ## Add here is used to add to the bottom of the pane
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava",
                    obj="gWindowrJava", value="gToolbarrJava"),
          function(obj, toolkit,  value, ...) {
            tag(value, "parentContainer") <- obj
            cont = getWidget(obj)
            tb = getBlock(value)
            pane = cont$getContentPane()
            bl <- .jnew("java/awt/BorderLayout")
            where <- .jnew("java/lang/String", .jfield(bl,name="NORTH"))

            .jcall(.jcast(pane,"java/awt/Container"),
                   "V",
                   method = "add",
                   .jcast(tb,  "java/awt/Component"),
                   .jcast(where,"java/lang/Object"))
            ## show up without resizing?
            .jcall(obj@widget,"V", "invalidate")
            .jcall(obj@widget,"V", "validate")
          })



setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gToolbarrJava", value="list"),
          function(obj, toolkit, value,  ...) {
            svalue(obj) <- c(svalue(obj), value)
          })




## (from gmenu)
setMethod(".delete",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gToolbarrJava"),
          function(obj, toolkit, widget,  ...) {
            ## widget can be gToolBar or a list
            if(is.character(widget)) {
              lst = widget                    # else assume its a character
            } else if(is(widget,"gComponentrJava")) {
              lst = svalue(widget)
              lst = names(lst)
            } else if(is.list(widget)) {
              lst = names(widget)
            } else {
              warning("Must be either a vector of names, a list, or a gToolbar instance")
              return()
            }
            
            cur.list = svalue(obj)             
            for(i in lst) {
              ## we delete *last* entry with this name, hence this awkwardness
              theNames = names(cur.list)
              if(i %in% theNames) {
                j = max(which(i == theNames))
                if(!is.null(cur.list[[j]])) cur.list[[j]] <- NULL
              }
            }
            ## now update toolbar
            svalue(obj) <- cur.list
          })

### no method to set style, use tag(obj,"style")<-"theStyle" instead
