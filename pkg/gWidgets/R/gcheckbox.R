##' @include guiComponents.R

##' Checkbox class
setClass("gCheckbox",
         contains="guiComponent",
         prototype=prototype(new("guiComponent"))
         )

##' constructor for checkbox widget
gcheckbox =function(
  text, checked = FALSE, handler = NULL, action = NULL,      container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .gcheckbox (toolkit,
    text=text, checked=checked, handler=handler, action=action, container=container, ...
    )
  obj = new( 'gCheckbox',widget=widget,toolkit=toolkit) 
  return(obj)
}


##' Generic for toolkit dispatch
setGeneric( '.gcheckbox' , function(toolkit,
                                    text, checked = FALSE, handler = NULL, action = NULL,
                                    container = NULL, ... ) standardGeneric( '.gcheckbox' ))

