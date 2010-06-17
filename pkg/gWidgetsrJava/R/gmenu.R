setClass("gMenurJava",
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )
setClass("gMenuItemrJava",
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )



## menulist is a list of lists with named components. Each named sub
## is a submenu.  a leaf consistis of handler= (required), lab

## put menu in group,
## a menubar is a map from a list into a menubar
## constructor
setMethod(".gmenu",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   menulist, 
                   popup = FALSE,
                   action = NULL,
                   container=NULL, ...) {
            
            force(toolkit)            
            if(popup) {
              mb = .jnew("javax/swing/JPopupMenu")
            } else {
              mb = .jnew("javax/swing/JMenuBar")
            }
            
            ## unlike RGtk2 use removeall to make changes
            obj = new("gMenurJava", block=mb, widget=mb,
              toolkit=toolkit,ID=getNewID(),  e = new.env())
  
            
            tag(obj, "menulist") <- menulist
  
            mapListToMenuBar(mb, menulist, toolkit)
            
            if(!is.null(container)) {
              if(is.logical(container) && container == TRUE) {
                add(gwindow(visible=TRUE), obj)
              } else {
                add(container, obj, ...)
              }
            }
            
            invisible(obj)
          })


### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gMenurJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            tag(obj, "menulist")
          })

## three cases for value: list, gMenurJava, guiWidget push down
## make a menubar, then replace current -- isn't working for popup case
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gMenurJava",
                           value="list"),
                 function(obj, toolkit, index=NULL, ..., value) {

                   menulist = value            # value is a list
                   if(!is.list(menulist))
                     stop("value is not a menubar or a list")
                   
                   
                   mb = obj@widget
                   mb$removeAll()    # remove then replace
                   mapListToMenuBar(mb, menulist, obj@toolkit)

                   parentWidget = mb$getParent()
                   parentWidget$validate()
                   
                   ## store for later?
                   tag(obj,"menulist") <- menulist

                   return(obj)
                 })

## get list, and then call previous
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gMenurJava",
                           value="gMenurJava"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   .svalue(obj,toolkit, index, ...) <- svalue(value)
                   return(obj)
                 })

## call previous after getting list
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gMenurJava",
                           value="guiWidget"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   .svalue(obj,toolkit,index, ...) <- svalue(value@widget)
                   return(obj)
                 })

## this is for adding a menu to a menu
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava", obj="gMenurJava", value="guiWidget"),
          function(obj, toolkit,  value, ...) {
            .add(obj, toolkit, value@widget)
          })

setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava", obj="gMenurJava", value="gMenurJava"),
          function(obj, toolkit,  value, ...) {
            orig.list = svalue(obj)
            add.list = svalue(value)
            new.list = c(orig.list, add.list)
            svalue(obj) <- new.list
          })


setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava",
                    obj="gMenurJava", value="list"),
          function(obj, toolkit,  value, ...) {
            mb = getWidget(obj)
            mapListToMenuBar(mb, value, toolkit)
          })

###  This is for adding a gmenu to a container. In rjava, menus must
### be added to the toplevel frame. We 
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava",
                    obj="gWindowrJava", value="gMenurJava"),
          function(obj, toolkit,  value, ...) {

            tag(value,"parentContainer") <- obj
            
            cont = getWidget(obj)
            mb = getBlock(value)
            topPane = cont$getRootPane()
            topPane$setJMenuBar(mb)
            cont$validate()
          })


setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava",
                    obj="gContainerrJava", value="gMenurJava"),
          function(obj, toolkit,  value, ...) {
            DEBUG("gmenu: obj must be a top-level frame\n")
            tag(value, "parentContainer") <- obj
            
            cont = getWidget(obj)
            mb = getBlock(value)
            topPane = cont$getParent()$getRootPane()
            topPane$setJMenuBar(mb)
            topPane$validate()
            
##            obj@widget$getParent()$setJMenuBar(value@block)
##            .jcall(obj@widget$getParent(),"Ljava/awt/Frame;", "setJMenuBar",
##                   value@block)
            
          })



## "wdget" is either a gMenu, list or just names to delete
setMethod(".delete",
          signature(toolkit="guiWidgetsToolkitrJava", obj="gMenurJava",
                    widget="guiWidget"),
          function(obj, toolkit, widget, ...) {
            .delete(obj,toolkit,widget@widget,...)
          })
setMethod(".delete",
          signature(toolkit="guiWidgetsToolkitrJava", obj="gMenurJava",
                    widget="gWidgetrJava"),
          function(obj, toolkit, widget, ...) {
            .delete(obj,toolkit,widget@widget, ...)
          })
setMethod(".delete",
          signature(toolkit="guiWidgetsToolkitrJava", obj="gMenurJava",
                    widget="gMenurJava"),
          function(obj, toolkit, widget, ...) {
            .delete(obj,toolkit,svalue(widget), ...)
          })

setMethod(".delete",
          signature(toolkit="guiWidgetsToolkitrJava", obj="gMenurJava",
                    widget="list"),
          function(obj, toolkit, widget, ...) {
            lst = widget                    # else assume its a character
            
            cur.list = svalue(obj)
            for(i in lst) {
              ## we delete *last* entry with this name, hence this awkwardness
              theNames = names(cur.list)
              if(i %in% theNames) {
                j = max(which(i == theNames))
                if(!is.null(cur.list[[j]])) cur.list[[j]] <- NULL
              }
            }
            ## now update menubar
            svalue(obj) <- cur.list
          })

## give vector notation
setMethod("[",
          signature(x="gMenurJava"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gMenurJava"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            lst = svalue(x)
            if(missing(i))
              return(lst)
            else
              return(lst[i])
          })

setReplaceMethod("[",
                 signature(x="gMenurJava"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gMenurJava"),
          function(x, toolkit, i, j, ..., value) {
            lst = svalue(obj)
            theNames = names(lst)
            if(is.character(i))
              i = max(which(i %in% theNames))
            lst[[i]] <- value[[1]]
            theNames[i] = names(value)
            names(lst) = theNames
            svalue(obj) <- lst
            return(obj)
          })

##################################################
## helper functions
addNewTopItem = function(mb,text,...) {
  mbh = .jnew("javax/swing/JMenu",text)
  mb$add(mbh)
  return(mbh)
}

addNewItem =function(mbh, text,icon=NULL,handler=NULL, action=NULL, toolkit,...) {
  if(is.null(icon)) {
    mbi = .jnew("javax/swing/JMenuItem",.jnew("java/lang/String",text))
  } else {
    ## deal with icons
    if(file.exists(icon))
      icon = gimage(icon,dirname="")
    else
      icon = gimage(icon,dirname="stock")
    
    mbi = .jnew("javax/swing/JMenuItem",.jnew("java/lang/String",text),
      .jcast(.jnew("javax/swing/ImageIcon",svalue(icon)),"javax/swing/Icon")
      )
  }
  mbh$add(mbi)

  if(!is.null(handler)) {
    obj = new("gMenuItemrJava", block=mbi, widget=mbi,
      toolkit=toolkit,ID=getNewID())

    ID = addJHandler(obj,handler, action,
      type="addActionListener",
      event = "ActionEvent",
      class = "java/awt/event/ActionListener",...)
#    return(ID)
  }
  ## JV change return value from ID or NA to mbi

  return(mbi)
  
}

addSeparator = function(mbh) {
#  mbh$addSeparator()
}

## the takes care of top-level entries which are different as they
## get added to mb. 
mapListToMenuBar = function(mb, lst, toolkit) {
  for(i in names(lst)) {
    mbh = addNewTopItem(mb,i)
    for(j in lst[i]) {
      addListToMenuBarItem(mbh,i,j, toolkit)
    }
  }
}

addListToMenuBarItem = function(mbh,name, lst, toolkit) {
  tmp <- lst
  if(.isgSeparator(tmp))
    tmp <- list(separator=TRUE)

  if(.isgAction(lst)) {
    tmp <- getToolkitWidget(lst)
    name <- tmp$label
  }


  if("handler" %in% names(tmp)) {
    mbi <- addNewItem(mbh, text=name, tmp$icon, tmp$handler, tmp$action, toolkit)
    if(.isgAction(lst)) {
      if(is(lst,"gActionrJava"))
          e <- lst@e
      else
        e <- lst@widget@e
      l <- e$menuitems
      l[[length(l) + 1]] <- mbi
      e$menuitems <- l
    }
    return()
  } else if("separator" %in% names(tmp)) {
    addSeparator(mbh)
    return()
  }

  for(i in names(tmp)) {
    ##    print(i)
    newLst = tmp[[i]]
    if(.isgSeparator(newLst))
      newLst <- list(separator=TRUE)
    if(.isgAction(newLst)) {
      newLst <- getToolkitWidget(newLst)
    }
    
    if("handler" %in% names(newLst)) {
      mbi <- addNewItem(mbh, text=i, newLst$icon, newLst$handler, newLst$action, toolkit)
      if(.isgAction(tmp[[i]])) {
        if(is(tmp[[i]],"gActionrJava"))
          e <- tmp[[i]]@e
        else
          e <- tmp[[i]]@widget@e
        l <- e$menuitems
        l[[length(l) + 1]] <- mbi
        e$menuitems <- l
      }
    } else if ("separator" %in% names(newLst)) {
      addSeparator(mbh)
      return()
    } else {
      ## contains a sublist!
      newMbh = .jnew("javax/swing/JMenu",i)
      .jcall(mbh,"Ljava/awt/Component;","add",
             .jcast(newMbh,"java/awt/Component"))
      addListToMenuBarItem(newMbh, i, tmp[[i]], toolkit)
    }
  }
}

