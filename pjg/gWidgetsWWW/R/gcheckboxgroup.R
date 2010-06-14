gcheckboxgroup = function (items, checked = FALSE, horizontal = FALSE,
  handler = NULL, action = NULL,
  container = NULL, ...) {

    ## use a checkbox if only one item
  if(length(items) == 1) {
    out <- gcheckbox(items, checked = checked, handler=handler, action=action, container = container, ...)
    return(out)
  }

  widget <- EXTComponentWithItems$new(toplevel=container$toplevel,
                                      ..checked = checked,
                                      ..horizontal = horizontal,
                                      ..handler = handler,
                                      ..action = action
                                      )
  class(widget) <- c("gCheckboxgroup",class(widget))

  if(length(checked) != length(items))
    checked <- rep(checked, length=length(items))
  widget$setValue(value= checked) ## store logical vector -- might be string
  widget$setValues(value = items)

  ## we store either a logical vector of character of same to indicate
  ## values
  widget$getValue <- function(.,index=NULL ,drop=NULL,...) {
    ## we need to revers logic from AWidgtet$getValue
    out <- .$..data
    if(exists("..shown",envir=.,inherits=FALSE)) {
      ## get from widget ID
      out <- try(get(.$ID,envir=.),silent=TRUE) ## XXX work in index here?
      if(!inherits(out,"try-error")) {
        if(is.character(out))
          out <- eval(parse(text = out))          # transport ischaracter
      } else {
        out <- .$..data
      }
    }
    ## no index -> index TRUE
    if(is.null(index)) index <- TRUE
    if(index)
      return(out)
    else
      return(.$..values[out])
  }

  widget$setValue <- function(., index=NULL,..., value) {
    ## values can be set by index, logical, or names

    n <- length(.); items <- .$getValues()
    if(is.character(value)) {
      value <- sapply(items, function(i) i %in% value)
    } else if(is.numeric(value) || (!is.null(index) && index)) {
      value <- sapply(1:n, function(i) i %in% value)
    } else if(!is.logical(value)) {
      ## error
      cat("Value should be logical vector, vector of indices, or character vector of names\n")
    }

    .$..data <- value

    if(exists("..shown",envir=., inherits=FALSE))
      cat(.$setValueJS())
  }
  widget$setValueJS <- function(.) {
    out <- String() +
      'var ans = [' + paste(tolower(as.character(.$..data)),collapse=",") +
        '];' +
          'for( var i = 0; i < ' + .$length() + ';i++) {' +
            .$asCharacter() + '.getComponent(i).setValue(ans[i]);' +
              '};'
    return(out)
  }

  widget$xtype <- "checkbox"
  widget$transportSignal <- "check"
  widget$checked <- function(.,i) .$..data[i]

  widget$ExtCfgOptions <- function(.) {
    out <- list(border = FALSE,
                bodyStyle = list(padding = "5px"),
                items = .$makeItems()
                )
    if(.$..horizontal)
      out[['layout']] <- "column"
    
    return(out)
  }

  widget$transportValue <- function(.,...) {
    out <- String() +
      'var ans = new Array();' +
        'var value = "c(";' +
          'for(var i = 0; i < ' + length(.) + '; i++) {' +
            'ans[i] = ' + .$asCharacter() + '.getComponent(i).getValue().toString().toUpperCase();' +
              'value = value + ans[i] + ",";' +
                '};' +
                  're = /,$/;' +
                    'value = value.replace(re,"");' +
                      'value = value + ")";' + '\n'
    return(out)
  }


  container$add(widget, ...)
  if(!is.null(handler))
    id <- widget$addHandler(signal="check", handler, action)
  invisible(widget)

}
