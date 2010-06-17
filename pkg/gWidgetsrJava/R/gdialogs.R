## some dialogs for R
## dialogs don't get windows, they make them
## dialogs are modal
## dialogs return their value -- not an object. so source(gfile()) should work

setMethod(".gmessage",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   message,
                   title = "message",
                   icon = c("info","warning","error","question"),
                   parent = NULL,
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {

            icon = match.arg(icon)
            if(missing(message) || length(message) == 0) message <- ""
            
            frame <- .jnull(class="java/awt/Component")
            if(!is.null(parent))
              frame <- getBlock(parent)
            
            op = .jnew("gWidgetsrJava/gDialog")
            .jcall(op,"V","gMessage",
                   .jcast(frame,"java/awt/Component"),
                   .jnew("java/lang/String",as.character(message)),
                   .jnew("java/lang/String",as.character(title)),
                   .jnew("java/lang/String",icon)
                   )
            
          })
  
## if OK then run handler, else not
setMethod(".gconfirm",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   message,
                   title = "Confirm",
                   icon = c("info", "warning", "error", "question"),
                   parent = NULL,
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {
            
            icon = match.arg(icon)
            if(missing(message) || length(message) == 0) message <- ""
            
            frame <- .jnull(class="java/awt/Component")
            if(!is.null(parent))
              frame <- getBlock(parent)
            

            op = .jnew("gWidgetsrJava/gDialog")
            ans= .jcall(op,"I","gConfirm",
              .jcast(frame,"java/awt/Component"),
              .jnew("java/lang/String",as.character(message)),
              .jnew("java/lang/String",as.character(title)),
              .jnew("java/lang/String",icon)
              )
            
            ## 1 for yes, 0 for no -1 for cancel
            if(ans == 1)
              invisible(TRUE)
            else
              invisible(FALSE)
          })


## Add input to the above
## h,... in handler has componets action, input (for value)
setMethod(".ginput",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   message,
                   text = "",
                   title = "Input",
                   icon = c("info","warning","error","question"),
                   parent = NULL,
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {
            
            icon = match.arg(icon)
            if(missing(message) || length(message) == 0) message <- ""
            
            frame <- .jnull(class="java/awt/Component")
            if(!is.null(parent))
              frame <- getBlock(parent)
            
            op = .jnew("gWidgetsrJava/gDialog")
            ans= .jcall(op,"S","gInput",
              .jcast(frame,"java/awt/Component"),
              .jnew("java/lang/String",as.character(message)),
              .jnew("java/lang/String",as.character(text)),
              .jnew("java/lang/String",as.character(title)),
              .jnew("java/lang/String",icon)
         )
            
            ## call handler if asked
            if(!is.null(handler)) 
              handler(list(obj=NULL, action=action, input=ans))

            if(!is.null(ans))
              return(invisible(ans))
            else
              return(NA)
            
            
          })

## add a widget to the dialog. This is modal
setMethod(".gbasicdialog",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   title = "Dialog",
                   widget,
                   parent = NULL,
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {
  
            g = ggroup()
            add(g,widget)


            frame <- .jnull(class="java/awt/Component")
            if(!is.null(parent))
              frame <- getBlock(parent)
            
            op = .jnew("gWidgetsrJava/gDialog")
            ans= .jcall(op,"I","gBasicDialog",
              .jcast(frame,"java/awt/Component"),
              g@widget@widget,
              .jnew("java/lang/String",as.character(title))
              )
            

            if(ans == 1) {
              ## yes
              if(!is.null(handler)) {
                handler(list(ref=widget,widget=widget,action=action, ...))
              }
              return(invisible(TRUE))
            } else {
              ## no
              return(invisible(FALSE))
            }
            
            return(ans)
          })

## gbasicdialog for tcltk type
setClass("gBasicDialogNoParentrJava",
         contains="gContainerrJava",
         prototype=prototype(new("gContainerrJava"))
         )

setMethod(".gbasicdialognoparent",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   title = "Dialog",
                   parent=NULL,                   
                   handler = NULL,
                   action = NULL,
                   ...
                   ) {
            
            cat("XXX implement me\n")
            return(NA)
          })

setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava",
                    obj="gBasicDialogNoParentrJava", value="guiWidget"),
          function(obj, toolkit, value, ...) {
            .add(obj, toolkit, value@widget, ...)
          })

setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava",
                    obj="gBasicDialogNoParentrJava", value="gWidgetrJava"),
           function(obj, toolkit, value, ...) {
             add(obj@widget, value, ...)
             ## keep these around
             tag(obj,"widget") <- value
          })

setMethod(".visible",
                 signature(toolkit="guiWidgetsToolkitrJava",
                           obj="gBasicDialogNoParentrJava"),
                 function(obj, toolkit, set=NULL, ...) {

                   if(as.logical(set)) {
                   }
                 })



## galert
setMethod(".galert",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   message,
                   title = "message",
                   delay = 3,
                   parent=NULL,
                   ...
                   ) {
            force(toolkit)

            w <- gwindow(title, width=250, height=50, parent = parent)
            g <- ggroup(cont = w)
            l <- glabel("  ", cont = g)
            label <- glabel(message, cont = g, expand=TRUE)
            font(label) <- c("weight"="bold")
            gimage(file="dismiss",dir="stock", cont = g, handler = function(h,...) dispose(w))
            
            addHandlerIdle(label, handler = function(h,...) dispose(w), interval = delay*1000)
          })
