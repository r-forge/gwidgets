##  Copyright (C) 2010 John Verzani
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  A copy of the GNU General Public License is available at
##  http://www.r-project.org/Licenses/

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
