##' @include guiComponentWithItems.R

##' Radio button class
setClass("gRadio",
         contains="guiComponentWithItems",
         prototype=prototype(new("guiComponentWithItems"))
         )
##' Constructor for radio button widget
gradio = function(items,selected=1, horizontal=FALSE, handler=NULL,
  action=NULL, container=NULL, ...,
  toolkit=guiToolkit()) {
  radio = .gradio(toolkit,items, selected, horizontal, handler, action, container,...)
  obj = new("guiComponent",widget=radio, toolkit=toolkit)
  return(obj)
}

##' method for dispatch into toolkit
setGeneric(".gradio",function(toolkit,
                              items, selected=1, horizontal=FALSE, handler=NULL, action=NULL,
                              container=NULL,
                              ...) standardGeneric(".gradio"))


