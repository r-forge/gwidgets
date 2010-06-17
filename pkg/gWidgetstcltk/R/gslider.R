## FIX up for non-integer values

setClass("gSlidertcltk",
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )

setMethod(".gslider",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   from=0, to=100, by = 1,
                   value=from,
                   horizontal=TRUE,
                   handler=NULL, action=NULL,
                   container=NULL, ...) {
            force(toolkit)


            
            ## if by < 1, call gspinbutton
            if(by < .99) {
              cat(gettext("gslider in tcltk is integer only, using gspinbutton instead\n"))
              obj = gspinbutton(
                from, to, by, value, digits = 1,
                handler, action,container,..., toolkit)
              return(obj@widget)        # return gWidgettcltk; not gWidget
            }


            if(is(container,"logical") && container)
              container = gwindow()
            if(!is(container,"guiWidget")) {
              warning("Container is not correct. No NULL containers possible\n" )
              return()
            }

            
            if(horizontal)
              orientation = "horizontal"
            else
              orientation = "vertical"

            tt <- getWidget(container)
            gp <- ttkframe(tt)
            SliderValue <- tclVar(as.character(value))

            ## missing?
##             ttkscale <- function(parent, ...) tkwidget(parent,"ttk::scale", ...) 
##             slider <- ttkscale(gp, from=from, to=to,
##                                variable = SliderValue,
##                                orient = orientation)
            ## use old school
            slider <- tkscale(gp, from=from, to=to,
                              showvalue=TRUE, variable=SliderValue,
                              resolution=by, orient=orientation)
            
            tkgrid(slider,row=0, column=0, sticky="news")
            tkgrid.columnconfigure(gp,0, weight=1)
            
            obj <- new("gSlidertcltk",block=gp, widget=slider,
              toolkit=toolkit, ID=getNewID(), e = new.env())
            tag(obj,"tclVar") <- SliderValue
            
            add(container, obj,...)
            
            if (!is.null(handler))  {
              id <- addhandlerchanged(obj, handler, action)
            }
            
            invisible(obj)
          })


### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gSlidertcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            rbValue = tag(obj,"tclVar")
            return(as.numeric(tclvalue(rbValue)))
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gSlidertcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   tclvalue(tag(obj,"tclVar")) <- as.character(value)
                   return(obj)
                 })



## Method to replace values of spin button
setReplaceMethod("[",
                 signature(x="gSlidertcltk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gSlidertcltk"),
          function(x, toolkit, i, j, ..., value) {
            obj <- x
            widget <- getWidget(obj)

            ## check that value is a regular sequence
            if(length(value) <=1) {
              warning("Can only assign a vector with equal steps, as produced by seq")
              return(obj)
            }
            if(length(value) > 2 &&
               !all.equal(diff(diff(value)), rep(0, length(value) - 2))) {
              warning("Can only assign a vector with equal steps, as produced by seq")
              return(obj)
            }
            ## get current value, increment
            curValue <- svalue(obj)
            inc <- head(diff(value), n=1)
            tol <- sqrt(.Machine$double.eps) * 10
            if(!all.equal(inc, as.integer(inc + tol))) {
              warning("Increment must be an integer")
            }
            tkconfigure(widget, from=min(value), to =max(value), resolution=inc)
            tcl(widget,"set", curValue)

            ## all done
            return(obj)
          })





### handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gSlidertcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addHandler(obj,toolkit, signal="<ButtonRelease-1>",handler,action)
          })
