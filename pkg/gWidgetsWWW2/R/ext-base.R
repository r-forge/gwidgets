##      Copyright (C) 2011  John Verzani
##  
##      This program is free software: you can redistribute it and/or modify
##      it under the terms of the GNU General Public License as published by
##      the Free Software Foundation, either version 3 of the License, or
##      (at your option) any later version.
##  
##      This program is distributed in the hope that it will be useful,
##      but WITHOUT ANY WARRANTY; without even the implied warranty of
##      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##      GNU General Public License for more details.
##  
##      You should have received a copy of the GNU General Public License
##      along with this program.  If not, see <http://www.gnu.org/licenses/>.

##' @include gwidgets-app.R
NA

##' a class to handle map between R and arguments for constructor
ExtArgs <- setRefClass("ExtArgs",
                       fields=list(
                         "args"="Array"
                         ),
                       methods=list(
                         initialize = function(...) {
                           args <<- Array$new(...)
                           .self
                         },
                         extend = function(l, overwrite=TRUE) {
                         "Extend argument list by list l."
                         for(i in names(l)) {
                           if(!(i %in% names(args) ) ||
                              overwrite)
                             args$push(l[[i]], i)
                         }
                       },
                         to_js_object = function() {
                           "Convert to string of object literal"
                           toJSObject(args$core())
                         }
                         )
                       )
                         
##' Base class for all Ext objects
ExtObject <- setRefClass("ExtObject",
                         fields=list(
                           "toplevel"="ANY",            # toplevel instance
                           "constructor" = "character", # for write_constructor
                           "args"="ExtArgs",            # for write_constructor
                           ".e"="environment",          # for stashing things (tag, tag<-)
                           "prefix"="character",        # really just "o"
                           "id" = "character",          # the actual id, e.g gWidget_ID2
                           "transport_signal"="character", # if given, the signal to initiate transport
                           "value"="ANY"                   # default property of the object
                           ),
                         methods=list(
                           initialize=function(toplevel = NULL) {
                             "Initialize widget. @param toplevel required or found"
                             ## This invovles a hack for finding the
                             ## toplevel when none is specified, which
                             ## should only happen with the initial
                             ## gwindow() call.
                             
                             args <<- ExtArgs$new() # append with add_args method
                             .e <<- new.env()
                             
                             ## insert a toplevel thingy
                             if(is.null(toplevel)) {
                               ## this variable is created when a new session is and lives in the
                               ## evaluation environment. See gwidgets-session
                               if(exists(".gWidgets_toplevel", inherits=TRUE))
                                  toplevel <<- get(".gWidgets_toplevel", inherits=TRUE)
                               else
                                 toplevel <<- GWidgetsTopLevel$new()
                             } else {
                               toplevel <<- toplevel
                             }
                             
                             ## work on id
                             ## id is used for html DOM id, o+id is used for
                             ## javascript variable and key within toplevel hash
                             ## to refer to this object
                             prefix <<- "o"
                             id <<- sprintf("gWidget_ID%s", .self$toplevel$get_object_id())
                             .self$args$extend(list(id=id))

                             .self$toplevel$add_object(.self, .self$get_id())

                             ## return
                             .self
                           },
                           ##
                           add_args = function(new_args, overwrite=TRUE) {
                             "add new arguments. Will overwrite. @param new_args is a list"
                             args$extend(new_args, overwrite)
                           },
                           ## id of base object (ogWidgetID1)
                           get_id = function() {
                             "ID of object. There is DOM id store in id property and Ext object id returned by this"
                             sprintf("%s%s", prefix, id)
                           },
                           ## attributes
                           get_attr = function(key) {
                             "Persistent attribute. If key missing return names, else return value for key"
                             if(missing(key))
                               ls(.e)
                             else
                               attr(.e, key)
                           },
                           set_attr = function(key, value) {
                             "Set persistent attribute"
                             attr(.e, key) <<- value
                           },
                           ## interface with js queue in toplevel
                           add_js_queue = function(cmd) {
                             "Add command to JavaScript queue"
                             toplevel$js_queue_push(cmd)
                           },
                           flush_js_queue = function() {
                             "Flush commands in JavaScript queue"
                             toplevel$js_queue$flush()
                           },

                           ## method to write out constructor
                           write_constructor = function() {
                             "Write out constructor."
                             cmd <- sprintf("var %s = new %s(%s);",
                                            get_id(),
                                            constructor,
                                            args$to_js_object()
                                            )
                             add_js_queue(cmd)
                           },
                           process_dot_args = function(...) {
                             "Helper function"
                             l <- list(...)
                             out <- sapply(l, coerceToJSString)
                             paste(out, collapse=", ")
                           },
                           ## call a method of ext object
                           ## This converts its arguments to JavaScript strings through coerceToJSString
                           call_Ext = function(meth, ...) {
                             "Write JavaScript of ext method call for this object. The ... values will be coerced to JavaScript stings through coerceToJSString, allowing the call to be as 'R'-like as possible, e.g.: call_Ext('setValue', 'some value'). Here the string will be quoted through ourQuote. To avoid that wrap within the String function, as in call_Ext('setValue', String('some value'))."

                             cmd <- sprintf("%s.%s(%s);",
                                            get_id(),
                                            meth,
                                            process_dot_args(...))
                             cmd
                             add_js_queue(cmd)
                           },
                           ## Ext apply basically merges lists (objects). Where the default comes from is up to you to
                           ## read about....
                           ext_apply = function(value) {
                             "Call ext apply with value a list containing config options. This is called after write-constructor, prior to that call use add_args or arg$append."
                             if(is.null(value))
                               return()
                             cmd <- sprintf("Ext.apply(%s, %s);",
                                            get_id(),
                                            toJSObject(value))
                             add_js_queue(cmd)
                           },

                           ## Transport. Many widgets transport a value from WWW -> R after
                           ## minor changes through an AJAX call. This requires three things.
                           ## 1. a signal that is listened to for an initiation of the transport
                           ## 2. a function to define an object {value: ..., values: ..., others: ...} that
                           ##    is converted to JSON and transported back to R through the param argument
                           ## 3. a process_transport method that is passed the widget id and this param value. It
                           ##    adjusts the state of the R widget and optionally other call, returning the javascript
                           ##    queue when done
                           transport_fun = function() {
                             "javascript function for transport web -> R. Creates an object param.
This is a string to be passed to the javascript queue withing the transport function call
E.g. var param = {value: this.getText()}"
                             ""         # no default
                           },
                           write_transport = function() {
                             "Writes out JavaScript for transport function"
                             cmd <- sprintf("%s.on('%s', function(%s) {%s; transportFun('%s', Ext.util.JSON.encode(param))}, null, {delay:100, buffer:100, single:false});",
                                            get_id(),
                                            transport_signal,
                                            getWithDefault(.ext_callback_arguments[[transport_signal]], ""),
                                            transport_fun(),
                                            get_id()
                                            )
                             add_js_queue(cmd)
                           },
                           process_transport = function(value, ...) {
                             "R Function to process the transport. Typically just sets 'value', but may do more. In the above example, where var param = {value: this.getText()} was from transport_fun we would get the text for value"
                             value <<- value
                           },
                           ## Call back code
                           ##
                           ## add a callback into R for this object
                           get_callback_object = function() {
                             "Return object for callback. Defaults to get_id(), but can be subclassed"
                             get_id()
                           },
                           add_R_callback = function(signal,
                             handler,   # function. First argument, the "h" list, is passed values
                             action=NULL, ...,
                             cb_args=NULL, # optional call back args for the signal, defaults to .ext_callback_arguments
                             param_defn = "" ## define param value her, eg "var param={value:'text'}"
                             ) {
                             "Add a callback into for the Ext signal. Return callback idas a list."
                             
                             cb_args <- getWithDefault(cb_args,
                                                       getWithDefault(.ext_callback_arguments[[signal, exact=TRUE]],
                                                       "")
                                                       )
                             cb_args <- paste(cb_args, collapse=", ")
                             
                             cbid <- toplevel$add_R_handler(.self, handler, action=action, ...)

                             ## actions don't write this JavaScript out, it is done in gaction
                             if(signal == "action")
                               return(list(cbid=cbid))

                             ## The value of param_defn is used to put in JavaScript that defines the value for
                             ## param that is passed back to the handler. For example, var param=newText 
                             if(is.null(param_defn) || param_defn == "") {
                               ## no extra
                               fn <- sprintf("var %s_%s_handler = function(%s) {callRhandler(%s)};",
                                             get_id(), cbid, cb_args, cbid)
                             } else {
                               fn <- sprintf("var %s_%s_handler =  function(%s) {%s;callRhandler(%s, param)}, null, {delay:100, buffer:100, single:false});",
                                             get_id(), cbid,
                                             cb_args, # e.g 'e,w'
                                             param_defn, # e.g. 'var param = this.getText();'
                                             cbid)
                             }
                             cmd <- paste(fn,
                                          sprintf("%s.on('%s', %s_%s_handler, null, {delay:100, buffer:100, single:false});",
                                                  get_callback_object(),
                                                  signal,
                                                  get_id(), cbid),
                                          sep="")
                             add_js_queue(cmd)
                             invisible(list(
                                             cbid=cbid,       # return callback id in list
                                             signal=signal,
                                             fn = sprintf("%s_%s_handler", get_id(), cbid)
                                        ))
                           },
                           remove_R_callback = function(cbid) {
                             if(is(cbid, "idleHandler")) {
                               remove_idle_handler(cbid)
                             } else {
                               unblock_R_callback(cbid)
                               toplevel$rHandlers$remove_by_name(cbid$cbid)

                               cmd <- sprintf("%s.removeListener('%s', %s);",
                                              get_callback_object(),
                                              cbid$signal,
                                              cbid$fn)
                               add_js_queue(cmd)
                             }
                           },
                           block_R_callback = function(cbid) {
                             "block callback temporarily. Remove through unblock"
                             if(toplevel$rHandlers$contains(cbid$cbid))
                               toplevel$blockedHandlers$push("", cbid$cbid)
                           },
                           unblock_R_callback = function(cbid) {
                             "remove block on callback"
                             if(toplevel$blockedHandlers$contains(cbid$cbid))
                               toplevel$blockedHandlers$remove_by_name(cbid$cbid)
                           },
                           ## some generic handlers
                           add_handler_blur = function(...) {
                             "Call when widget loses focus"
                             add_R_callback("blur", ...)
                           },
                           add_handler_change = function(...) {
                             "A 'change' handler' -- not add_handler_changed which maps to a generic"
                             add_R_callback("change", ...)
                           },
                           add_handler_clicked = function(...) {
                             "When object is clicked"
                             add_R_callback("click", ...)
                           },
                           add_handler_doubleclick = function(...) {
                             add_R_callback("celldblclick", ...)
                           },
                           add_handler_idle = function(interval, handler, action=NULL, ...) {
                             "Add a callback after an interval"
                             cbid <- toplevel$add_R_handler(.self, handler, action=action, ...)
                             task <- sprintf("%s_%s_task", get_id(), cbid)
                             cmd <- paste(sprintf("var %s {", task),
                                          sprintf("run: function() callRhandler(%s),", cbid),
                                          sprintf("interval : %s", interval),
                                                  "};",
                                          sprintf("(new Ext.util.TaskRunner()).start(%s);",
                                                  task),
                                          sep="")
                             add_js_queue(cmd)
                             out <- list(cbid=cbid, task=task)
                             class(out) <- c("idleHandler", class(out))
                             out
                           },
                           remove_idle_handler = function(cbid) {
                             cmd <- sprintf("(new Ext.util.TaskRunner()).stop(%s);", cbid$task)
                             add_js_queue(cmd)
                           },
                           ## Some javascript code
                           ## Used to add a javascript callback -- that is not call into R. Th
                           add_js_callback = function(signal, callback, ...) {
                             "Add a javascript callback. The value of 'this' refers to the object this is called from"
                             cmd <- sprintf("%s.on('%s', %s);",
                                            get_id(),
                                            signal,
                                            callback)
                             add_js_queue(cmd)
                           }


                           ))

## idea

## a <- ExtObject$new(list(renderTo="replaceme", x=1))
## a$write_constructor(name="Ext.Panel")
## a$add_R_callback("click", function(h,...) assign("test", "test", envir=.GlobalEnv))
## a$toplevel$js_queue$core()

##' List containing callback argument for ExtJs Events
.ext_callback_arguments <- list(afteredit = "e",                 # for gdf cell editing
                                beforechange="tb, params",       # for paging toolbar
                                blur="w",                        # w = "this"
                                bodyresize = "w, width, height",
                                bodyscroll = "scrollLeft, scrollRight",
                                cellcontextmenu = "w, rowIndex, cellIndex, e",
                                cellclick = "w, rowIndex, columnIndex, e", # grid
                                celldblclick = "w, rowIndex, columnIndex, e", # grid
                                cellmousedown = "w, rowIndex, columnIndex, e", # grid
                                change="w, newValue, oldValue", beforechange = "w, newValue, oldValue",
                                check = "w, checked",
                                click = "w, e", # gtree?
                                collapse = "w",                  # combobox
                                columnmove = "oldIndex, newIndex",
                                columnresize = "columnIndex, newSize",
                                dblclick = "e",                  # grid -- not celldblclick
                                destroy="w", beforedestroy = "w",
                                destroyed = "w, c", # child component destroyed
                                disable="w",
                                drag = "w, e", dragend = "w,e", dragstart = "w,e",
                                enable = "w",
                                expand = "w",                    # combobox
                                fileselected = "w, s",               # FileUploadField
                                focus = "w",
                                headerclick = "w, columnIndex, e", # grid
                                headercontextmenu = "w, columnIndex, e", # grid
                                headerdblclick = "w, columnIndex, e", # grid
                                headermousedown = "w, columnIndex, e", # grid       
                                hide = "w", beforehide = "w",
                                invalid = "w",
                                keydown = "w,e",                 # e Ext.EventObject
                                keypress = "w,e",
                                keyup = "w,e",
                                mousedown = "e",
                                mouseover = "e", 
                                mousemove = "e", 
                                move = "w, x, y",
                                render = "w", beforerender = "w",
                                resize = "w, adjWidth, adjHeight, rawWidth, rawHeight",
                                rowclick = "w, rowIndex, e", # grid
                                rowcontextmenu = "w, rowIndex, e", # grid
                                rowdblclick = "w, rowIndex, e", # grid
                                rowmousedown = "w, rowIndex, e", # grid       
                                select = "w,record,index", beforeselect = "w, record, index",
                                selectionchange = "selModel",    # gcheckboxgrouptable
                                show = "w", beforeshow = "w", 
                                specialkey = "w, e",
                                tabchange = "w, tab", # notebook
                                toggle = "w, value",             # gtogglebutton
                                valid = "w")
