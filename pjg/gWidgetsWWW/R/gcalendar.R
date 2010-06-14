gcalendar <- function(text = "", format = "%Y-%m-%d",
                      handler=NULL, action=NULL, container = NULL, ... ) {


    widget <- EXTComponent$new(toplevel=container$toplevel,
                               ..format = format)
    class(widget) <- c("gCalendar",class(widget))

    widget$extDateFormat <- "%a %b %d %Y %H:%M:%S"
    
    if(text != "") {
      tmp <- as.Date(text, widget$..format)
      if(!is.na(tmp))
        text <- format(tmp, widget$extDateFormat)
    }
    
    widget$setValue(value=text)           # no day
    widget$getValueJSMethod <- "getValue"
    widget$transportSignal <- c("change")
    ## coerceValues calls ..format

    widget$coerceValues <- function(., value) {
      ## Wed Jun 11 2008 00:00:00 GMT-0400 (EDT) -- ext format
      theDate = as.Date(value,.$extDateFormat)
      return(format(theDate,.$..format))
    }
      
      
    
    ## override writeConstructor of show method
    widget$writeConstructor <- function(.) {
      lst <- list(xtype = "datefield",
                  id =  as.character(String(.$ID) + "date"))
      if(is.na(.$getValue()) || .$getValue() == "") {
        lst['emptyText'] <- "Select a date..."
      } else {
        lst['emptyText'] <- format(as.Date(.$..data, .$extDateFormat),"%m/%d/%Y")
        lst['value'] <- String('new Date("') + .$..data +'")'#.$..text,
      }


      ## size doesn't work here, as we the style thing isn't
      ## applied to 
      if(exists("..width",envir = .,inherits=FALSE))
        lst[["width"]] <- .$..width
      else
        lst[["width"]] <- "auto"

      if(exists("..height",envir = .,inherits=FALSE))
        lst[["height"]] <- .$..height
      else
        lst[["height"]] <- "auto"
      
      out <- String() +
        'o' + .$ID + 'date = new Ext.Panel({' + # no var -- global
          'id:' + shQuote(.$ID) + ',' +
            'renderTo:Ext.getBody(),' +
              'items: [' +
                .$mapRtoObjectLiteral(lst) +
                  ']\n' +
                    '});'

      out <- out +
        'o' + .$ID + ' = o' + .$ID + 'date' + # no var -- global
          '.getComponent(0);' + '\n'

      return(out)
    }
        

    container$add(widget,...)

      
    if(!is.null(handler))
      widget$addHandlerChanged(handler, action=action)
    
    
    invisible(widget)
  }

