setClass("gFramerJava",
         contains="gGrouprJava",
         prototype=prototype(new("gGrouprJava"))
         )

## add a frame for packing. subclass of gGroup
setMethod(".gframe",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   text = "", markup=FALSE,
                   pos = 0, ## pos in [0,1] 0 for left, (.01,.99) center, 1 for right
                   horizontal=TRUE,
                   container=NULL,
                   ...) {

            force(toolkit)

            gp = .ggroup(..., horizontal=horizontal, toolkit=toolkit)
            pane = getWidget(gp)

            ## we can't do any markup here. Font() could be used
            if(markup) {
              cat("HTML markup not supported for title. Try font(), it could work.\n")
              text = gsub("<[^>]*>","",text)    # strip off HTML
            }
            
            border = .jnew("javax/swing/BorderFactory")
            ## incorporate position
            titledBorder = border$createTitledBorder(text)
            ## this next is giving an error message rjav0.5

            ## need to figure out pos argument
            ##             titledBorder$setTitleJustification(.jfield(titledBorder,name="RIGHT"),"x")
##             if(pos == 1)
##               try(titledBorder$setTitleJustification(.jfield(titledBorder,name="RIGHT")))
##             else if(.01 < pos && pos < .99)
##               try(titledBorder$setTitleJustification(.jfield(titledBorder,name="CENTER")))
##             else
##               try(titledBorder$setTitleJustification(.jfield(titledBorder,name="LEFT")))

            ## set the border
            .jcall(
                   .jcast(pane,"javax/swing/JComponent"),,
                   "setBorder",
                   .jcast(titledBorder,"javax/swing/border/Border"))
            


            ## make object
            obj = new("gFramerJava",
              block=gp@block, widget=gp@widget,
              toolkit=toolkit,ID=getNewID(),  e = new.env())
            tag(obj,"titledBorder") <- titledBorder
            
            ## add to container if desired
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj)
            }
            return(obj)
          })

### methods -- inherited from ggroup

## set spacing
setReplaceMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gFramerJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ..., value) {
            ## adds some breathing room to object
            ## value is pixels
            .svalue(obj@widget, toolkit=obj@toolkit,
                    index=index, drop=drop, ...) <- value

            return(obj)
          })

## set label
setMethod(".names",signature(toolkit="guiWidgetsToolkitrJava",
                             x="gFramerJava"),
          function(x, toolkit) {
            tb <- tag(x,"titledBorder")
            .jcall(tb,"S","getTitle")
          })


setReplaceMethod(".names",
                 signature(toolkit="guiWidgetsToolkitrJava",x = "gFramerJava"),
                 function(x,toolkit,value) {
                   tb <- tag(x,"titledBorder")
                   .jcall(tb,"V","setTitle",
                          .jnew("java/lang/String",as.character(value))
                          )
                   return(x)
                 })

## use methods
## setBorderPosition
## setTitleFont
## setTitleColor
