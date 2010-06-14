glabel <- function(text = "", markup = FALSE, editable = FALSE,
                   handler = NULL, action = NULL, container = NULL,...) {

  widget <- EXTComponent$new(toplevel=container$toplevel,
                             ..editable = editable,
                             ..markup = markup
                             )
  
  class(widget) <- c("gLabel",class(widget))
  widget$setValue(value=escapeHTML(text))

  widget$scripts <- function(.) {
    out <- String(sep="\n") +
      'Ext.ux.labelBox = Ext.extend(Ext.BoxComponent, {' + 
	'value: null,' + 
          'initComponent:function() {' + 
	    'Ext.ux.labelBox.superclass.initComponent.call(this);' + 
            '},' + 
              'onRender:function(ct, position) {' + 
                'this.el = document.createElement("span");' + 
                  'this.el.id = this.getId();' + 
                    'this.el.innerHTML = this.value;' +
                      'Ext.ux.labelBox.superclass.onRender.call(this,ct,position);' + 
                      '},' + 
                        'getValue: function() {' + 
                          'return this.value;' + 
                          '},' + 
                            'setValue: function(value) {' + 
                              'this.value = value;' + 
                                'document.getElementById(this.id).innerHTML = value;' + 
                                '}' + 
                                '});' +
                                  'Ext.reg("labelbox", Ext.ux.labelBox);' 
    return(out)
  }
  
  widget$setValueJSMethod = "setValue"
  widget$getValueJSMethod = "setValue"
  widget$ExtConstructor <- "Ext.ux.labelBox"
  widget$ExtCfgOptions <-  function(.) {
    out <- list()
    out[["value"]] = unescapeURL(svalue(.))
    return(out)
  }
  
  
  
  ## add after CSS, scripts defined
  container$add(widget,...)
  invisible(widget)
  
}
