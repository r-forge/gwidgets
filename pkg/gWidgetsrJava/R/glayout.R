setClass("gLayoutrJava",
         representation = representation("gComponentrJava",
           homogeneous="logical",
           spacing="numeric"),
         contains="gContainerrJava",
         prototype=prototype(new("gContainerrJava"))
         )

## an gWidget for tables
 
## take two -- this time build up table, then use visible to show
## this way, we don't need to set size initially
## constructor
setMethod(".glayout",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   homogeneous = FALSE,
                   spacing = 10,        # amount (pixels) between row, cols, NULL=0
                   container = NULL, ...
                   ) {

            force(toolkit)
            
            ## how to add in per column adjusments?
            adjust = "center"                             # left or right or center
            
            pane = .jnew("javax/swing/JPanel")
            gbl = .jnew("java/awt/GridBagLayout")
            ## need to cast gbl here
            .jcall(pane,,"setLayout",.jcast(gbl, "java/awt/LayoutManager"))
            
            obj = new("gLayoutrJava",
              block=pane, widget=pane,
              toolkit=toolkit,ID=getNewID(),  e = new.env(),
              homogeneous=homogeneous,
              spacing = spacing
              )

            ## add to container
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj, ...)
            }
            
            
            invisible(obj)
          })

### Add is a stub so that same syntax works with tcltk
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava", obj="gLayoutrJava",
                    value="gWidgetrJava"),
          function(obj, toolkit, value, ...) {
            tag(value,"parentContainer") <- obj
            ## stub
          })


## how we populate the table
setReplaceMethod("[",
                 signature(x="gLayoutrJava"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gLayoutrJava"),
          function(x, toolkit, i, j, ..., value) {

            theArgs = list(...)         # look for expand, anchor

            ## check that widget is correct
            if(is.character(value)) value = glabel(value)
            value = try(getWidget(value),silent=TRUE)
            if(inherits(value,"try-error")) {
              warning("value is not a gwidget or character\n")
              return()
            }


            
            ## anchor is a pair of (x,y) coordinates with
            ## (-1,1) being upper left, (1,-1) lower left
            ## values are -1,0, or 1.

              
            
            c = .jnew("java/awt/GridBagConstraints")
            ins = .jcast(.jfield(c, name="insets"),"java/awt/Insets")

            anchor = theArgs$anchor

            if(is.null(anchor)) anchor = c(-1,1)
            if(length(anchor) !=2 || any(!anchor %in% -1:1)) {
              cat("anchor is a pair of values from -1,0,1\n")
              anchor=c(-1,1)
            }
            
            if(!is.null(anchor)) {
              anchor = anchor+c(1,1)
              anchor = 3*anchor[1]+anchor[2]+1 # from 1 to 10
              possibilities = c(
                .jfield(c,name="LAST_LINE_START"),
                .jfield(c,name="LINE_START"),
                .jfield(c,name="FIRST_LINE_START"),
                .jfield(c,name="PAGE_END"),
                .jfield(c,name="CENTER"),
                .jfield(c,name="PAGE_START"),
                .jfield(c,name="LAST_LINE_END"),
                .jfield(c,name="LINE_END"),
                .jfield(c,name="FIRST_LINE_END"))
              anchor = possibilities[anchor]
            }
                

            
            ## can put in adjustments here
            gridx = as.integer(min(j))
            gridy = as.integer(min(i))
            gridwidth=as.integer(diff(range(j)) + 1)
            gridheight=as.integer(diff(range(i)) + 1)
            gridweightx = .5
            gridweighty = .5


            
            c = .jnew("java/awt/GridBagConstraints",
              gridx, gridy, gridwidth, gridheight,
              gridweightx, gridweighty,
              anchor,
              ifelse(!is.null(theArgs$expand) && theArgs$expand,
                     .jfield(c,name="BOTH"),
                     .jfield(c,name="fill")
                     ), #fill?
              ins,
              .jfield(c,name="ipadx"), .jfield(c,name="ipady"))

            pane = x@widget
            .jcall(pane,"V",
                   "add",
                   as.jcomponent(value),
                   .jcast(c,"java/lang/Object"))
            .jcall(pane, "V", "validate")

            ## pack
            top <- getTopLevel(x)
            if(!is.null(top)) {
              .jcall(getBlock(top), "V", "pack")
            }

            return(x)

          })

## these are here for compatibility with RGtk2

setMethod(".visible",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gLayoutrJava"),
          function(obj, toolkit, set=TRUE,...) {
            cat("visible not defined for glayout\n")
          })

## control expand/close with logical
setReplaceMethod(".visible",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gLayoutrJava"),
                 function(obj, toolkit, ..., value) {
                   ## cat("visible()<- not necessary in rJava\n")
                   return(obj)
                 })
