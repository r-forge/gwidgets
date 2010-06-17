setClass("gTreerJava",
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )

## offspring takes two argument

## map a list to a tree
setMethod(".gtree",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   offspring = NULL,
                   hasOffspring = NULL,                 # for defining offspring. FUN
                                        # of children only.
                   offspring.data = NULL,
                   col.types = NULL, # data frame with logical
                   icon.FUN = NULL,                      # return stock name --called
                                        # on offspring, returns a
                                        # vector of length nrow
                   chosencol = 1,
                   multiple = FALSE,
                   handler = NULL,
                   action=NULL,
                   container=NULL,
                   ...
                   ) {

            force(toolkit)
            cat("gtree not implemented\n")
            return()
##             ## do we have first col. for icons?
##             iconFudge = ifelse(is.null(icon.FUN), 0, 1)
            
##             ## get base offspring
##             children = offspring(c(), offspring.data)
##             lst = getOffSpringIcons(children, hasOffspring, icon.FUN)
##             children = lst$children
##             doExpand = lst$doExpand
            
            
##             if(is.null(col.types))
##               col.types = children[1,]
            
            
            
##             ## get  types -- force first to be character
##             types = c("gchararray",sapply(col.types[-1],RtoGObjectConversion))
##             if(iconFudge == 1)
##               types = c("gchararray", types)       # stores filename of image
            
##             ## define treestore
##             treestore = gtkTreeStoreNew(types)
##             ## define view
##             view = gtkTreeViewNewWithModel(treestore)
##             ##  if(nrow(children) > 15)
##             ##    view$SetFixedHeightMode(TRUE)       # speeds up this. FAILED?
##             view$SetSearchColumn(iconFudge)         # for CTRL-f
            
##             ## define cellrender
##             colHeaders = names(children)
            
##             for(i in (1+iconFudge):ncol(children)) {
##               cellrenderer = gtkCellRendererTextNew()
##               view.col = gtkTreeViewColumnNew()
##               ## properties
##               view.col$SetResizable(TRUE)
##               ## title
##               if(!is.na(colHeaders[i]) && !is.null(colHeaders[i]))
##                 view.col$SetTitle(colHeaders[i])
##               view.col$SetSortColumnId(i-1)
##               view.col$PackStart(cellrenderer, TRUE)
##               view.col$AddAttribute(cellrenderer, "text", i-1)
##               view$InsertColumn(view.col,i-1)
##             }
            
##             if(iconFudge == 1) {
##               cellrenderer = gtkCellRendererPixbufNew()
##               view.col = gtkTreeViewColumnNew()
##               view.col$PackStart(cellrenderer, TRUE)
##               view.col$AddAttribute(cellrenderer, "stock-id", 0)
##               view$InsertColumn(view.col,0)
##             }  
            
##             ## pack into scrolled window
##             group = ggroup()
##             sw <- gtkScrolledWindowNew()
##             sw$SetPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")
##             sw$Add(view)
##             add(group, sw, expand=TRUE)
            
##             ## allow multiple if asked
##             if(multiple) {
##               treeselection = view$GetSelection()
## #              treeselection$SetMode(GtkSelectionMode["multiple"])
##             }
            
            
##             ## turn on alternating shading if more than 1 column
##             if(ncol(children) > 1)
##               view$SetRulesHint(TRUE)
            

##             obj = new("gTreerJava", block=group, widget=view,
##               toolkit=toolkit,ID=getNewID())

##             tag(obj,"store") <- treestore
##             tag(obj,"view") <- view
##             tag(obj,"offspring") =offspring
##             tag(obj,"hasOffspring") = hasOffspring
##             tag(obj,"noffspring.data") = offspring.data
##             tag(obj,"icon.FUN") = icon.FUN
##             tag(obj,"iconFudge") = iconFudge
##             tag(obj,"chosencol") = chosencol
##             tag(obj,"multiple") = multiple
##             tag(obj,"ncols") = length(types)
          
##             ## put in children, handler for exapnd-row
##             addChildren(treestore, children, doExpand, iconFudge, parent.iter=NULL)
            
##             ## now add a handler to row-exapnd
##             addhandler(obj,"row-expanded",action = offspring.data,
##                        handler = function(h,view, iter, path,...) {
##                          children = offspring(.getValuesFromIter(h$obj,iter),h$action)
                         
##                          lst = getOffSpringIcons(children, hasOffspring, icon.FUN)
##                          children = lst$children
##                          doExpand = lst$doExpand
                         
##                          addChildren(treestore, children, doExpand,
##                                      tag(h$obj,"iconFudge"), iter)
##                          ## remove errant offspring
##                          child.iter = treestore$IterChildren(iter)
##                          if(child.iter$retval)
##                            treestore$Remove(child.iter$iter)
##                        })
            
            
##             addhandler(obj,"row-collapsed",
##                        handler = function(h, view, iter, path, ...) {
##                          ## debug
##                          string = treestore$GetPath(iter)$ToString()
##                          ## get children, remove
##                          n = treestore$IterNChildren(iter)
##                          if(n > 1) { ## n=1 gets removed when expanded
##                            for(i in 1:(n-1)) {
##                              child.iter = treestore$IterChildren(iter)
##                              if(child.iter$retval)
##                                treestore$Remove(child.iter$iter)
##                            }
##                          }
##                        })
            
##             if(!is.null(handler)) {
##               id = addhandlerdoubleclick(obj,handler,action)
##               tag(obj, "handler.id") <- id
##             }
            
##             ## attach to container
##             if (!is.null(container)) {
##               if(is.logical(container) && container == TRUE)
##                 container = gwindow(visible=TRUE)
##               add(container, obj)
##             }
            
##             return(obj)
          })

## Take the data frame and massage it to return
## icons if asked, and figure out offspring
getOffSpringIcons = function(children, hasOffspring, icon.FUN) {
  
  ## make icons first column if there
  ## icon.FUN is called on data.frame, returns vector to cbind to children.
  if(!is.null(icon.FUN)) {
    if(nrow(children) > 0) {
      icons = getstockiconname(icon.FUN(children))
      children = data.frame(icons=I(icons), children)
    } else {
      children = data.frame( icons = character(0), children)
    }
  }
  
  ## how to determine if offspring are needed?
  ## default to hasOffspring, then second column, then default to FALSE
  
  if(!is.null(hasOffspring)) {
    doExpand = hasOffspring(children)
  } else {    
    ## ## if second col is not logical, we make it so
    if(ncol(children) == 2 || !is.logical(children[,3])) {
      doExpand = rep(FALSE, nrow(children))
    } else {
      doExpand = children[,3]
      children = children[,-3,drop=FALSE]
    }
  }
  
  return(list(children=children, doExpand=doExpand))
}


## children has label, logical, ...
## used to update tree
addChildren = function(treestore, children, doExpand, iconFudge, parent.iter=NULL) {
  if(nrow(children) == 0)
    return(NULL)
  for(i in 1:nrow(children)) {
    iter = treestore$Append(parent=parent.iter)$iter
    ## set for each column for(j in 1:ncol())...
    ## write label
    for(j in 1:ncol(children)) 
      treestore$SetValue(iter,column=j-1, children[i,j])
    
    ## add branch
    if(!is.na(doExpand[i]) && doExpand[i]) {
      treestore$Append(parent=iter)
    }
  }
}

## use a different signature, should I give this a different name?
setMethod("update",
          signature(object="gTreerJava"),
          function(object) {
            theArgs = list(...)
            offspring.data = theArgs$offspring.data
            if(is.null(offspring.data))
              offspring.data = theArgs[[1]]
            obj = object                          # rename, object from update generic
  ## what should now be in this part of the tree
            newchildren = tag(obj,"offspring")(c(), offspring.data)
            newvalues = as.character(newchildren[,1])
            alreadyThere = c()
            
            ## loop over values in the treestore, if not in newchildren, remove
            i = 0
            
            remove.these = c()
            iter = tag(obj,"store")$GetIterFromString(i)
  while(iter$retval) {
    treeValue = tag(obj,"store")$GetValue(iter$iter,0+tag(obj,"iconFudge"))$value
    if(treeValue %in% newvalues) {
      alreadyThere = c(alreadyThere, treeValue)
    } else {
      ## need to delete
      remove.these = c(remove.these, i)
    }
    i = i + 1
    iter = tag(obj,"store")$GetIterFromString(i)
  }
  if(length(remove.these)>0) {
    for(i in rev(sort(remove.these))) {
      iter = tag(obj,"store")$GetIterFromString(i)
      tag(obj,"store")$Remove(iter$iter)
    }
  }
  
  didThese = newvalues %in% alreadyThere
  newchildren = newchildren[!didThese, , drop=FALSE] # don't drop dimension

  ## add these to end
  if(nrow(newchildren) > 0) {

    lst = getOffSpringIcons(newchildren, tag(obj,"hasOffspring"),
      tag(obj,"icon.FUN"))
    newchildren = lst$children
    doExpand = lst$doExpand
    ## add the children
    addChildren(tag(obj,"store"), newchildren, doExpand, tag(obj,"iconFudge"))
  }
          })
          
## use index for the column to override the column returned
          
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gTreerJava"),
          function(obj, toolkit, index=NULL, drop=NULL,...) {
            theArgs = list(...) 
            if(!is.null(index))
              whichCol = index          ## BAD FORM!!!
            else 
              whichCol = tag(obj,"chosencol")

            ## multiple selection
            if(tag(obj,"multiple")) {
              treeselection = obj@widget$GetSelection()
              out = treeselection$GetSelectedRows() # 2 parts, paths, model
              if(length(out) == 0) {
                return(c())
              } else {
                model = out$model
                tmp = c()
                for(i in out$retval) {
                  iter = model$GetIter(i)$iter
                  value = model$GetValue(iter,
                    tag(obj,"chosencol")-1 + tag(obj,"iconFudge"))$value
                  tmp = c(tmp,value)
                }
                return(tmp)
              }
            } else {
              ## single selection
              iter = obj@widget$GetSelection()$GetSelected()
              if(iter$retval) 
                return( obj@widget$GetModel()$GetValue(iter$iter,whichCol-1 +
                                                       tag(obj,"iconFudge"))$value)
              else
                return(NULL)
            }
          })

### need to figure this out
## return the path in values
setMethod("[",
          signature(x="gTreerJava"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, guiToolkit("rJava"), i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gTreerJava"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            obj = x
            iter = obj@widget$GetSelection()$GetSelected()
            string = tag(obj,"store")$GetPath(iter$iter)$ToString()
            indices = unlist(strsplit(string,":"))
            thePath = c()
            for(j in 1:length(indices)) {
              path = paste(indices[1:j],collapse=":")
              iter = tag(obj,"store")$GetIterFromString(path)
              thePath[j] = tag(obj,"store")$GetValue(iter$iter,0+
                       tag(obj,"iconFudge"))$value
            }
            if(missing(i))
              return(thePath)
            else
              return(thePath[i])
          })



### methods
## row-activated in gtable gives double click
setMethod(".addhandlerdoubleclick",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gTreerJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
             addhandler(obj, "row-activated",handler,action)
           })

## used internally
.getValuesFromIter = function(obj, iter) {
  string = tag(obj,"store")$GetPath(iter)$ToString()
  indices = unlist(strsplit(string,":"))
  thePath = c()
  for(i in 1:length(indices)) {
    path = paste(indices[1:i],collapse=":")
    iter = tag(obj,"store")$GetIterFromString(path)
    ## need to fudge here if necessary
    thePath[i] = tag(obj,"store")$GetValue(iter$iter,0+tag(obj,"iconFudge"))$value
  }
  return(thePath)
}


