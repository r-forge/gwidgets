## add calendar widget: shoule I have gcalendar, gcalendarbrowser?
## no handler function, can add to entry object with addhandler

## setClass("gCalendartcltk",
##          representation = representation("gComponenttcltk",
##            format="character"),
##          contains="gEdittcltk",
##          prototype=prototype(new("gEdittcltk"))
##          )


setMethod(".gcalendar",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   text="",
                   format="%Y-%m-%d",
                   handler = NULL, action=NULL,
                   container=NULL,...) {

            force(toolkit)

            if(text == "" && format != "")
              text = format(Sys.Date(), format)

            text = as.character(text)
            
            ## use a text widget
            ## format is ignored -- it sets the text when selected via a GUI, but
            ## tcltk has not such GUI
            obj = gedit(text, cont=container, ...)

            theArgs <- list(...)
            tag(obj,"format") <- format
            tag(obj,"coerce.with") <- theArgs$coerce.with
            
            return(obj@widget)          # drop down to tcltk widget
          })

## ## gcalendar is a gedit instance. It inherits those methods.
## setMethod(".svalue",
##           signature(toolkit="guiWidgetsToolkittcltk",obj="gCalendartcltk"),
##           function(obj, toolkit, index=NULL, drop=NULL, ...) {
##             val = tclvalue(tag(obj,"tclVar"))

##             val <- try(as.Date(val, format=tag(obj,"format")), silent=TRUE)
##             if(inherits(val,"try-error")) return(NA)
            
##             coercewith = tag(obj,"coercewith")
##             if(!is.null(coercewith))
##               val = do.call(coercewith, list(val))

##             return(val)
##           })
