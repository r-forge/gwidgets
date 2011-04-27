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

##' @include ext-widget.R
NA

##' File selection function
##'
##' @param text Instructional text. 
##' @param type only "open" implemented
##' @param filter ignored
##' @param handler Ignored. called when file is selected
##' @param action Ignored. passed to handler
##' @param container parent container
##' @param ... passed to add method of parent container
##' @param width width in pixels
##' @param height ignored
##' @param ext.args extra args to pass to constructor
##' @return an ExtWidget
##' @note the \code{svalue} method returns the temporary filename of the uploaded file, or a value of \code{NA}.
##' @export
##' @examples
##' w <- gwindow()
##' gstatusbar("Powered by gWidgetsWWW2 and Rook", cont=w)
##' f <- gfile("Choose a file for upload", cont=w, handler=function(h,...) {
##'   galert(paste("You uploaded", svalue(h$obj)), parent=w)
##' })
gfile <- function(text="Choose a file",
                  type = c("open"),
                  filter = NULL, 
                  handler = NULL, action = NULL, container = NULL, ...,
                  width=NULL, height=NULL, ext.args=NULL
                  ) {
  f <- GFile$new(container$toplevel)
  f$init(text, type, filter, handler, action, container, ...,
         width=width, height=height, ext.args=ext.args)
  f
}

##' base class for gfile
##' @name gfile-class
GFile <- setRefClass("GFile",
                     contains="ExtWidget",
                     fields=list(
                       stub="ANY"
                       ),
                     method=list(
                       init=function(text, type, filter, handler, action, container, ...,
                         width=width, height=height, ext.args=ext.args) {

                         if(!missing(handler))
                           cbid <- toplevel$add_R_handler(.self, handler, action=action)
                         else
                           cbid <- NULL

                         
                         constructor <<- "Ext.form.FormPanel"
                         arg_list <- list(
                                          width=width,
                                          height=height,
                                          frame=TRUE,
                                          fileUpload=TRUE,
                                          bodyPadding="2 2 0",
                                          url= String("file_url"),
                                          method="POST",
                                          defaults = list(
                                            anchor="100%",
                                            allowBlank=FALSE
                                            ),
                                          items=String(sprintf("[%s]",
                                            toJSObject(list(xtype="fileuploadfield",
                                                            id=sprintf("%s_upload", get_id()),
                                                            emptyText =text,
                                                            hideLabel = TRUE,
                                                            buttonText="Browse...",
                                                            buttonConfig=list(iconClass="update-icon")
                                                            )))),
                                          buttons = String(sprintf("[%s]",
                                            toJSObject(list(text="Save",
                                                            handler= String(paste("function () {",
                                                              sprintf("var form = %s.getForm();",get_id()),
                                                              "if(form.isValid()){",
                                                              "form.submit(",
                                                              toJSObject(list(
                                                                              waitMsg="Uploading...",
                                                                              success=if(!is.null(cbid)) {
                                                                                String(sprintf("function(fp,o) {callRhandler('%s')}", cbid)) } else {
                                                                                  NULL},
                                                                              failure=String("function(fp,o) {alert('Failed')}"),
                                                                              params=list(
                                                                                id=get_id(),
                                                                                session_id=String("session_id")
                                                                                )
                                                                              )),
                                                              ")}}",
                                                              sep=" "))))
                                                                                
                                                                      
                                            ))
                                          )
                
                         add_args(arg_list)

                         setup(container, NULL, NULL, ext.args, ...)

                         set_value(NA)
                       },
                       set_value = function(value) {
                         "Set local file name. NA if not"
                         value <<- value
                       }

                       ))
                         
                                          
