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

##' @include ext-base.R
NA

## Need a toplevel gWidgetsApp to store session, ...
## called from GWindow

##' Class for toplevel instances (one per script)
GWidgetsTopLevel <- setRefClass("GWidgetsTopLevel",
                                fields=list(
                                  "e" = "environment", # evaluation environment
                                  "rHandlers"="Array", # handlers
                                  "blockedHandlers"="Array",
                                  "objects"="Array",   # objects on page
                                  "js_queue"="Array",   # queue to flush
                                  "do_layout_cmd" = "character"
                                  ),
                                methods=list(
                                  initialize=function(...) {
                                    rHandlers <<- Array$new()
                                    objects <<- Array$new()
                                    js_queue <<- Array$new()
                                    callSuper(...)
                                  },
                                  set_e = function(e) {
                                    e <<- e
                                  },
                                  ## js_stuff
                                  js_queue_push = function(x) {
                                    ## push command (string) to queue. No name needed here
                                    js_queue$push(x)
                                  },
                                  ## return queue, clear
                                  flush_queue = function() {
                                    "Clear js_queue"
                                    js_queue$flush()
                                  },

                                  ## R handler stuff
                                  add_R_handler = function(obj, handler, ...) {
                                    ## Add R handler, return ID
                                    l <- list(object=obj,
                                              handler = handler)
                                    l <- merge(l, list(...))
                                    id <- rHandlers$push(l)
                                    id
                                  },
                                  call_handler = function(cbid, value) {
                                    if(blockedHandlers$contains(cbid) || !rHandlers$contains(cbid))
                                      return()
                                    h <- rHandlers$get_by_name(cbid)
                                    
                                    handler <- h$handler
                                    h$handler <- NULL
                                    h$toplevel <- .self
                                    ## add values if present to h, e.g. x, y, key, ...
                                    if(!is.null(value))
                                      h <- merge(h, value, overwrite=FALSE)
                                    do.call(handler, list(h), envir=e)

                                    do_layout()
                                  },
                                  do_layout = function() {
                                    "Update layout from toplevel"
                                    if(length(nchar(do_layout_cmd))) # update layout
                                      js_queue_push(do_layout_cmd)
                                  },
                                  ## transport add javascript
                                  call_transport = function(id, param) {
                                    "Run transport"
                                    obj <- objects$get_by_name(id)
                                    if(!is.list(param)) {
                                      l <- list()
                                      for(i in names(param))
                                        l[[i]] <- param[i]
                                      param <- l
                                    }

                                    do.call(obj$process_transport, param)
                                  },
                                  ## proxy call
                                  call_proxy = function(id, param) {
                                    "Run proxy, return JSON encoded object"
                                    obj <- objects$get_by_name(id)
                                    if(!is.list(param))
                                      param <- list(param)
                                    ## obj should be ExtProxy
                                    out <- ""
                                    if(is(obj, "ExtProxy"))
                                       out <- do.call(obj$get_json_data, param)
                                    return(out)
                                  },
                                  ## upload a file
                                  call_upload = function(id, param, req) {
                                    "Run proxy, return empty string or throw error"
                                    obj <- objects$get_by_name(id)
                                    key <- sprintf("%s_upload", id)
                                    theData <- req$POST()[[key]]
                                    if(!is.null(theData)) {
                                      ## place local file name 
                                      svalue(obj) <- theData$tempfile
                                      out <- list(success=TRUE,
                                                  responseText="File uploaded"
                                                  )
                                    } else {
                                      out <- list(success=FALSE,
                                                  responseText="File uploaded",
                                                  errors=list(
                                                    portOfLoading="No file transmitted"
                                                    )
                                                  )
                                      
                                    }
                                    out
                                  },
                                  ## add gWidgets object
                                  get_object_id = function() {
                                    objects$get_id()
                                  },
                                  ## add the object
                                  add_object = function(obj, id) {
                                    "Add object to objects list"
                                    objects$push(obj, id)
                                  }
                                  ))


