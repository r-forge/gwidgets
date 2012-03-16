##' @include manipulate.R
NULL

## Class for holding controls
ManipulateControls <- setRefClass("ManipulateControls",
                                  fields=list(
                                    l="list",
                                    widget = "ANY",
                                    label="character",
                                    initial="ANY"
                                    ),
                                  methods = list(
                                    initialize=function(l=list(), label="", initial=NULL) {
                                      initFields(l=l, label=label, initial=initial)
                                      callSuper()
                                    },
                                    validate_inputs = function(...) {
                                      "Validate input code"
                                    },
                                    get_value=function(...) {
                                      "Get value of widget"
                                      svalue(widget)
                                    },
                                    make_gui=function(cont, handler) {
                                      "Create widget, then add to table"
                                      w <- widget
                                      svalue(w) <- initial
                                      addHandlerChanged(w, handler=handler)
                                    }
                                    ))




