  
## others
## propert ..length is added
gslider <- function(from = 0, to = 100, by = 1, value = from,
                    horizontal = TRUE,
                    handler = NULL, action = NULL, container = NULL, ...) {
  widget <- EXTComponent$new(toplevel=container$toplevel,
                             ..from = from, ..to = to, ..by=by,
                             ..horizontal=horizontal
                             )
  class(widget) <- c("gSlider",class(widget))
  widget$setValue(value=value)
  widget$..coerce.with="as.numeric"
  widget$..length = if(horizontal) 300 else 100
  ## CSS
  
  widget$scripts <- function(.) {
    ## from main example page 
    f <- system.file("javascript","ext.ux.slidertip.js", package="gWidgetsWWW")
    out <- String() + "\n" + paste(readLines(f), collapse="\n")
    return(out)
  }
  
  ## methods
  widget$getValueJSMethod <- "getValue"
  widget$setValueJSMethod <- "setValue"
  ## No methods in extjs to set the values (minValue, maxValue, increment) after construction
  ## so we can't implement [<- method

  widget$transportSignal <- "change"
  widget$transportValue <- function(.,...) {
    out <- String() +
      'var value = newValue;'
    return(out)
  }
  widget$ExtConstructor <- "Ext.Slider"
  widget$ExtCfgOptions <- function(.) {
    out <- list("value"= svalue(.),
                "increment" = .$..by,
                "minValue" =  .$..from,
                "maxValue" = .$..to,
                "enableKeyEvents"=TRUE,
                "vertical"= !.$..horizontal,
                plugins = String("new Ext.ux.SliderTip()")
                )
    if(.$..horizontal)
      out[['width']] <- .$..length
    else
      out[['height']] <- .$..length

    return(out)
  }


  ## add after CSS, scripts defined
  container$add(widget,...)


  if(!is.null(handler))
    widget$addHandler("change",handler=handler,action=action)
  
  invisible(widget)
}



