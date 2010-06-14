## gnotebook
## when adding, can pass in label= and tooltip= arguments for tooltip on tab
## XXX needs to addJS method
gnotebook <- function(tab.pos = 3, close.buttons = FALSE, container, ...) {

   ## a notebook
   widget <- EXTContainer$new(toplevel=container$toplevel,
                        ..closeButtons=close.buttons,
                        ..tabPos = tab.pos)
   class(widget) <- c("gNotebook",class(widget))

   widget$setValue(value=1)             # current tab
   ## Methods
   ## how to set a value
   widget$setValueJS <- function(.) {
     if(exists("..setValueJS", envir=., inherits=FALSE)) .$..setValueJS(...)
       
     ind <- as.numeric(.$..data - 1)
     
     out <- String() +
       'o' + .$ID + '.setActiveTab(' + ind + ');' 

     return(out)
   }
   widget$disposeJS <- function(.,ind) {
     if(missing(ind)) {
       ind <- length(.) 
     }
     ind <- ind - 1                     # 0 based
     out <- String() +
       'tab = o' + .$ID + '.getComponent(ind);' +
         'o' + .$ID + '.remove(tab);'
     return(out)
   }
   
   ## label names stored in children
   widget$getNames <- function(.) {
     n <- length(.$children)
     val <- character(n)
     for(i in 1:n)
       val[i] <- .$children[[i]]$..label
     return(val)
   }

   ## XXX not updated
   widget$setNames <- function(., value) {
     n <- length(.$children)
     if(length(value) == n) {
       for(i in 1:n) {
         .$children[[i]]$..label <- value[i]
       }
     }
     if(exists("..shown",envir=., inherits=FALSE)) {
       .$setNamesJS()
     }

   }
   widget$setNamesJS <- function(.) {
     ## must reset all the names in a loop
     out <- String()
     n <- length(.$children)
     for(i in 1:n) {
       out <- out +
         'var tab = ' + 'o' + .$ID + '.getTab('+ (i-1) + ');' +
           'tab.get("labelEl").innerHTML =' + shQuote(.$children[[i]]$..label) + ';'
     }
     return(out)
   }


   ## how to add children -- need label, etc
   ## override add
   widget$add <- function(.,child,...) {
     parent <- .$parent                  # to dispatch add method
     theArgs <- list(...)
     ## labels
     if(!is.null(theArgs$label))
       label <- theArgs$label
     else
       label <- "tab"
     child$..label <- label
     ## tooltips
     if(!is.null(theArgs$tooltip))
       child$..tabTooltip <- theArgs$tooltip
     addFUN <- get("add",envir=parent)   # call add for parent widget
     addFUN(.,child)                     # call
   }

   widget$ExtConstructor <- "Ext.TabPanel" ## inherits
   widget$ExtCfgOptions <- function(.) { ## ih
     tabpos <- "top"
     if(.$..tabPos == 1)
       tabpos <- "bottom"
     tabNo <- svalue(.) - 1
     out <- list(frame = TRUE,
                 activeTab = tabNo,
                 enableTabScroll = TRUE,
                 defaults = String("{autoScroll: true}"),
                 tabPosition = tabpos, width=200)
     return(out)
     
   }
   widget$makeItemsFixedItems <- 'border:false,'


   
##   widget$header <- widget$footer <- NULL
##    widget$Show <- function(.,...) {
##      ## override Show
##      children <- .$children

##      out <- String() +
##        '<script>' +
##          'var o' + .$ID +
##            '= new YAHOO.widget.TabView(' + shQuote(.$ID) + ');'  +
##              '</script>' + '\n'
##      out <-  out +
##        '<div id=' + shQuote(.$ID) + ' class="yui-navset"> ' +
##          '<ul class="yui-nav">' + '\n'
##      .$Cat(out)

##      if((n <- length(children)) > 0) {
##       out <- String()
##       for(i in 1:n) {
##         if(i == 1)
##           selected = ' class="selected"'
##         else
##           selected = ""
##         out <- out +
##           '<li' + selected + '><a href="#tab' + i + '"><em>' +
##         children[[i]]$..label +
##         '</em></a></li>\n'
##       }
##       .$Cat(out)
##     }

##     .$Cat('</ul> <div class="yui-content">')

##     if(n >= 1) {
##       for(i in 1:n) {
##         .$Cat('<div><p>')
##         children[[i]]$Show()
##         .$Cat('</p></div>')
##       }
##     }

##     .$Cat("</div></div>")

##      .$..shown <- TRUE                     # set shown     
##   }

##    widget$footer <- function(.) {
##      children <- .$children

##      childIDs <- sapply(children, function(i) i$ID)
##      childLabels <- sapply(children, function(i) i$..label)
     
##      items <- paste(paste('{title:',shQuote(childLabels),',',
##                           'contentEl:',shQuote(childIDs),'}',
##                           sep=""),
##                     collapse=",")

##      tabpos <- "top"
##      if(.$..tabPos == 1)
##        tabpos <- "bottom"

##      out <- String() +
## ##XX       '<script>' +
##          'o' + .$ID + '= new Ext.TabPanel(' +
##            '{' +
##              'renderTo: Ext.getBody(),' +
##                'id:' + shQuote(.$ID) + ',' +
##                  'frame: true,' + ## custom frame
##                    'activeTab:0,' +
##                      'tabPosition:' + shQuote(tabpos) + ',' +
##                      'items:[' + items + ']' +
##                        '});' 
## ##XX     out <- out +'</script>'

##      .$Cat(out)

##    }
   

  ## add after CSS, scripts defined
  container$add(widget,...)
  

##   if(!is.null(handler))
##     widget$addHandlerClicked(handler=handler,action=action)
  
  invisible(widget)
  
}
