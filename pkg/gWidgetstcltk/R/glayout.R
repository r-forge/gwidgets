setClass("gLayouttcltk",
         contains="gContainertcltk",
         prototype=prototype(new("gContainertcltk"))
         )

## an gWidget for tables
 
## take two -- this time build up tale, then use visible to show
## this way, we don't need to set size initially
## constructor
setMethod(".glayout",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   homogeneous = FALSE,
                   spacing = 10,        # amount (pixels) between row, cols, NULL=0
                   container = NULL, ...
                   ) {
            
            force(toolkit)
            
            if(is(container,"logical") && container)
              container = gwindow()
            if(!is(container,"guiWidget")) {
              warning("Container is not correct. No NULL containers possible\n" )
              return()
            }
            
            tt <- getWidget(container)
            gp <- ttkframe(tt)
            tkpack(gp, expand=TRUE, fill="both")

            obj = new("gLayouttcltk",
              block=gp, widget=gp,
              toolkit=toolkit, e = new.env())
            
            
            add(container, obj, ...)


            ## how to add in per column adjusments?
            adjust = "center"                             # left or right or center

            tag(obj,"homogeneous") <- homogeneous
            tag(obj,"spacing") <- as.numeric(spacing)
            tag(obj,"adjust") <- adjust
            
            invisible(obj)
          })

## for adding
setMethod(".add",
          signature(toolkit="guiWidgetsToolkittcltk", obj="gLayouttcltk",
                    value="gWidgettcltk"),
          function(obj, toolkit, value, ...) {
            ## add parent, children
            childComponents <- obj@e$childComponents
            if(is.null(childComponents))
              childComponents <- list()
            obj@e$childComponents <- c(childComponents, value)
            value@e$parentContainer <- obj
            
            ## inherit enabled from parent
            try(enabled(value) <- enabled(obj),silent=TRUE)

            theArgs = list(...)
##            tkpack(getBlock(value), side="left")
          })


## how we populate the table
setReplaceMethod("[",
                 signature(x="gLayouttcltk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gLayouttcltk"),
          function(x, toolkit, i, j, ..., value) {
            ## check that all is good
            if(is.character(value)) {
              value <- glabel(value, cont = x)
            }

            spacing <- tag(x,"spacing")
            
            ## need means to adjust via sticky
            sticky = "n"
            theArgs = list(...)
            if(!is.null(theArgs$anchor)) {
              anchor = theArgs$anchor
              if(anchor[1] == -1)
                sticky = "w"
              else if(anchor[1] == 1)
                sticky = "e"
              else if(anchor[2] == -1)
                sticky = "s"
            }
            if(!is.null(theArgs$expand) && theArgs$expand)
              sticky = "nsew"

            
            tkgrid(getBlock(value),
                   row = min(i) - 1,
                   rowspan = 1 + max(i) - min(i),
                   column = min(j) - 1,
                   columnspan = 1 + max(j) - min(j),
                   sticky = sticky,
                   padx=spacing, pady=spacing
                   )
                   
            return(x)

            ##   if(obj$adjust == "right") {
            ##     group = ggroup()
            ##     addSpring(group)
            ##     add(group,value)
            ##   } else if(obj$adjust = "left") {
            ##     group = ggroup()
            ##     add(group,value)
            ##     addSpring(group)   
            ##   } else {
            ##     group = value
            ##   }
            
          })