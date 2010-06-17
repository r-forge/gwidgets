## class in aaaClasses.R
## constructor
setMethod(".ggroup",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   horizontal = TRUE, spacing = 5,
                   use.scrollwindow = FALSE, 
                   container = NULL, ... 
                   ) {

            force(toolkit)
            
            theArgs = list(...)                   # raise.on.dragmotion
            
            if(is.null(spacing))
              spacing = 0


            if(is.null(container)) {
              cat(gettext("No NULL containers in tcltk. Creating a new window\n"))
              container=gwindow()
            } else if(is.logical(container) && container) {
              container = gwindow()
            }

            if(!is(container,"guiWidget")) {
              container = gwindow()
            }

            tt <- getWidget(container)

            ## implement scrollbars if asked. 
            if(use.scrollwindow == TRUE) {
              ## cf http://mail.python.org/pipermail/python-list/1999-June/005180.html
              block <- ttkframe(tt)
              tkpack(block, expand=TRUE, fill="both")
              
              xscr <- ttkscrollbar(block, orient="horizontal",
                                   command=function(...)tkxview(widget,...))
              yscr <- ttkscrollbar(block, 
                                   command=function(...)tkyview(widget,...))
              
              widget <- tkcanvas(block)
              tkconfigure(widget, xscrollcommand = function(...) tkset(xscr,...))
              tkconfigure(widget, yscrollcommand = function(...) tkset(yscr,...))
              
              ## Pack into a grid
              ## see tkFAQ 10.1 -- makes for automatic resizing
              tkgrid(widget,row=0,column=0, sticky="news")
              tkgrid(yscr,row=0,column=1, sticky="ns")
              tkgrid(xscr, row=1, column=0, sticky="ew")
              tkgrid.columnconfigure(block, 0, weight=1)
              tkgrid.rowconfigure(block, 0, weight=1)

              ## call in autoscroll
              tcl("autoscroll", xscr)
              tcl("autoscroll", yscr)
              
              
              ## Set up frame
              gp <- ttkframe(widget)
              tcl(widget,"create","window",0,0,anchor="nw",window=gp)
                                        #tkgrid(gp, row = 0, column = 0, sticky="news")
              tkgrid.columnconfigure(widget,0,weight=1)
              tkgrid.rowconfigure(widget,0,weight=1)

              tcl("update","idletasks")


              tkbind(widget,"<Configure>",function() {
                bbox <- tcl(widget,"bbox","all")
                tcl(widget,"config",scrollregion=bbox)
              })

              tkbind(gp,"<Configure>",function() {
                bbox <- tcl(widget,"bbox","all")
                tcl(widget,"config",scrollregion=bbox)
              })
            } else {
              gp <- ttkframe(tt)
              tkconfigure(gp, borderwidth=10) # XXX
              block <- gp
              widget <- NULL      # for later
            }

            tkconfigure(gp, padding=spacing)
            

            if(!is.null(theArgs$debug))
              tkconfigure(gp,borderwidth=4, relief="solid")
            
            obj = new("gGrouptcltk",block=block, widget=gp, horizontal=horizontal,
              e = new.env())

            
            ## to move widget when scrolling
            ## if(!is.null(widget <- tag(value,"scrollable.widget"))) {
            ##  tkxview.moveto(widget,1)
            ##  tkyview.moveto(widget,1)
            ## }
##            .tag(obj,toolkit, i="scrollable.widget") <- widget
            obj@e$i <- widget
            ## attach to container if there
            if(!is.null(container)) {
              add(container, obj,...)
            }

            ## raise if we drag across
            if(!is.null(theArgs$raise.on.dragmotion)) {
#              tkbind(gp, "<Motion>", function(W) {
#                tkfocus(W)
#              })
                     
            }
            return(obj)
          })


##################################################
## methods

setReplaceMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gGrouptcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ..., value) {
            ## adds some breathing room to object
            ## value is pixels
            gp <- getWidget(obj)
#            tkcofigure(gp,padx=value,pady=value)
            tkconfigure(gp,padding = value)            

            return(obj)
          })

##################################################
## handlers
