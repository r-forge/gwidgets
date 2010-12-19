## class defined in aaaClasses for inheritance
library(tcltk)


##' AutoComplete widget
##'
##' An ttkentry box with an accompanying set of words that are matched for when the entry box is filled out.
##' 
AC <- setRefClass("AutoComplete",
                  fields=list(
                    v = "tclVar",       # text variable holding entry value
                    e = "tkwin",        # text entry widget
                    m = "tkwin",        # toplevel transient window
                    l = "tkwin",        # selection wi1dget
                    lindex = "integer",  # index of selection widget
                    no.wds = "integer",  # track number of possible wds to choose from
                    words = "character",
                    max.words = "numeric" # maximum words in a display
                    ),
                  methods=list(
                    ##' @param parent parent container
                    ##' @param text Initial text for widget
                    initialize = function(parent, text="", words, max.words = 20, ...) {
                      "Initialize widget. parent is parent widget, text is intial text"
                      v <<- tclVar(text)
                      e <<- ttkentry(parent, textvariable=v)
                      tclServiceMode(FALSE)
                      m <<- tktoplevel()
                      tkwm.overrideredirect(m, TRUE)
                      tkwm.withdraw(m)
                      tclServiceMode(TRUE)                      
                      l <<- tktext(m); tkpack(l)
                      lindex <<- 0      # index of selected
                      max.words <<- max.words
                      if(!missing(words))
                        setWords(words)
                      addBindings()
                      .self
                    },
                    widget = function() {
                      "Return entry widget"
                      e
                    },
                    ##' Set the words
                    ##' @param words character vector of auto completion words
                    setWords = function(words) {
                      words <<- unique(as.character(words))
                    },
                    ##' Get text value
                    getValue = function() as.character(tclvalue(v)),
                    ##' set text value
                    ##' @param text value to set
                    setValue = function(text) {
                      tclvalue(v) <- text
                      lindex <<- 0
                      tcl(e, "icursor", "end")
                      tcl("event","generate", e, "<<Changed>>")
                    },                                               
                    ## find match in word list
                    findMatch = function(x) {
                      ind <- grepl(sprintf("^%s", tolower(x)), tolower(words))
                      words[ind]
                    },
                    ##' show the word list
                    ##' @param str a string. If
                    ##' missing do nothing, otherwise match against
                    ##' string to generate word list. Popup menu
                    ##' depending on length
                    showWordList = function(str) {
                      ## put m into right place,
                      if(missing(str))
                        return()

                      char.height <- 16 ## or compute from font metrics
                      wds <- findMatch(str)
                      if(length(wds) == 0) {
                        no.wds <<- 0
                        hideWordList()
                        return()
                      }

                      ## compute max.height -- number of words that can be shown
                      screenheight <- as.numeric(tkwinfo("screenheight", e))
                      y <- as.numeric(tclvalue(tkwinfo("rooty",e)))
                      max.words <- min(max.words, floor((screenheight - y)/char.height))
                      if(length(wds) > max.words)
                        wds <- c(wds[1:max.words], "...")
                      tkdelete(l, "0.0", "end")
                      tkinsert(l, "end", paste(wds, collapse="\n"))
                      lindex <<- 1; no.wds <<- length(wds)

                      ## set geometry
                      x <- as.numeric(tclvalue(tkwinfo("rootx",e)))
                      y <- as.numeric(tclvalue(tkwinfo("rooty",e)))
                      geo <- as.character(tkwinfo("geometry",e))
                      geo <- as.numeric(strsplit(geo, "[x+]")[[1]])
                      tkwm.geometry(m, sprintf("%sx%s+%s+%s", geo[1], 10 + char.height*length(wds), x, y + geo[2]))
                      ## popup
                      tcl("wm","attributes", m, "topmost"=TRUE) 
                      tcl("wm","attributes", m, "alpha"=0.8)
                      tkwm.deiconify(m)
                      tcl("raise", m)
                      highlightWordList()
                    },
                    ## hide the word list
                    hideWordList = function() {
                      tcl("wm","attributes", m, "topmost"=FALSE) # not working!
                      tkwm.withdraw(m)
                    },
                    ## highlight word on lindex
                    highlightWordList = function() {
                      if(lindex > 0) {
                         tktag.remove(l, "selectedWord", "0.0", "end")
                         tktag.add(l,"selectedWord",sprintf("%s.0", lindex), sprintf("%s.end", lindex))
                         tktag.configure(l, "selectedWord", font="bold")
                       }
                    },
                    ## get current word. From lineindex if applicable, or from entry widget itself
                    getCurrentWord = function() {
                      if(no.wds > 0)
                        if(lindex > 0) {
                          tclvalue(tkget(l, sprintf("%s.0", lindex), sprintf("%s.end", lindex)))
                        } else {
                          ""
                        }
                      else
                        tclvalue(v)
                    },
                    ##' Add bindings to entry box
                    addBindings = function() {
                      tkbind(e, "<KeyRelease>", function(W, K) {
                        ## set out virtual event, as otherwise we can;t have addHandlerKeystrike
                        tcl("event","generate", e, "<<KeyRelease>>")#, "keysym"=K) ## can't send in keysymbol here
                        ## Main bindings
                        if(nchar(K) == 1 || K == "BackSpace") {
                          ## single letter, popup menu
                          val <- tclvalue(tcl(W, "get"))
                          showWordList(val)
                        } else if(K == "Down") {
                          ## down arrow. Open if empty, but also scroll down list
                          if(nchar(val <- getCurrentWord()) == 0) {
                            showWordList(".")
                            lindex <<- 0
                          }
                          lindex <<- min(lindex + 1, no.wds)
                          highlightWordList()
                        } else if(K == "Up") {
                          ## move up list
                          lindex <<- max(lindex - 1, 1)
                          highlightWordList()
                        } else if(K == "Return") {
                          ## get value and put into e widget
                          hideWordList()
                          if(lindex > 0) {
                            setValue(getCurrentWord())
                          } else {
                            tcl("event","generate", e, "<<Changed>>")
                          }
                        } else if(K == "Escape") {
                          ## close the word list
                          hideWordList()
                          lindex <<- 0
                        }
                      })
                      ## show or hide, depending
                      tkbind(e, "<Map>", showWordList)
                      tkbind(tcl("winfo", "toplevel", e), "<Configure>", hideWordList)
                      tkbind(e, "<Destroy>", hideWordList)
                      tkbind(e, "<FocusOut>", hideWordList())
                      tkbind(e, "<Unmap>", hideWordList)

                      tkbind(l, "<Motion>", function(x, y) {
                        tmp <- as.character(tcl(l, "index", sprintf("@%s,%s", x, y)))
                        lindex <<- as.numeric(strsplit(tmp, "\\.")[[1]][1])
                        highlightWordList()
                      })
                      
                      ## bind to text widget
                      tkbind(l, "<Button-1>", function(x,y) {
                        wd <- getCurrentWord()
                        hideWordList()
                        if(wd != "...") {
                          setValue(getCurrentWord())
                        }
                      })
                      ## we don't want focus on l
                      tkbind(l, "<FocusIn>", function() {
                        tkfocus(e)
                      })
                    }
                  )
            )

                
                

## constructor
setClass("gEdittcltk",
         representation = representation("gComponenttcltk",
           coercewith="NULLorFunction"),
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )


setMethod(".gedit",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   text="", width=25,
                   coerce.with = NULL, 
                   handler=NULL, action=NULL,
                   container=NULL,
                   ...
                   ) {

           force(toolkit)
            
            if(is(container,"logical") && container)
              container = gwindow()
            if(!is(container,"guiWidget")) {
              warning("Container is not correct. No NULL containers possible\n" )
              return()
            }


           if (is.null(text)) text<-""

            ## check that coerce.with is a function
            if(is.null(coerce.with) || is.function(coerce.with)) {
              ## okay
            } else {
              if(is.character(coerce.with)) {
                coerce.with = get(coerce.with)
              }
            }

           tt <- getWidget(container)

           e <- getRefClass("AutoComplete")$new(tt)
           obj <- new("gEdittcltk", block=e$widget(), widget = e$widget(),
                      toolkit=toolkit,ID=getNewID(), e = new.env(),
                      coercewith=coerce.with)
           tag(obj, "widget") <- e
           
           svalue(obj) <- text
           
           ## entryValue = tclVar("")
           ## entry = ttkentry(tt, width=as.character(width),
           ##   textvariable=entryValue)

           ## ## what gives with this, was causing an error. No reason to manage  here
           ## ##           tkgrid(entry)
           
           ##  obj <- new("gEdittcltk",block=entry, widget=entry,
           ##    toolkit=toolkit,ID=getNewID(), e = new.env(),
           ##    coercewith=coerce.with)
           ## tag(obj,"tclVar") <- entryValue

           ## ## set the initial text
           ## svalue(obj) <- text
           

           ## ## type ahead support
           ## tag(obj,"typeAhead") <- c()
           ## tkbind(entry, "<KeyRelease>", function(W, K) {
           ##   if(K == "BackSpace")
           ##     return()
           ##   eVar <- tag(obj,"tclVar")
           ##   x <- obj[]
           ##   cur <- tclvalue(eVar)
           ##   ind <- which(cur == substr(x, 1, nchar(cur)))
           ##   if(length(ind) == 1) {
           ##     ## replace
           ##     tclvalue(eVar) <- x[ind]
           ##     ## set selection
           ##     tcl(W,"selection","range", nchar(cur), nchar(x[ind]))
           ##   }
           ## })
           

           ## Drag and drop
           ## addDropSource(obj)
           ## addDropTarget(obj)
           
           add(container, obj,...)
           
           if (!is.null(handler)) 
             tag(obj, "handler.id") <- addhandlerchanged(obj,handler,action)
           
           
           invisible(obj)
            
            
          })

## methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gEdittcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            widget <- tag(obj, "widget")
            val <- widget$getValue()
            ## val = tclvalue(tag(obj,"tclVar"))
            if(val == "<NA>")
              val <- NA
            coercewith = obj@coercewith
            if(!is.null(coercewith))
              val = do.call(coercewith, list(val))

            return(val)
          })

## svalue<-
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gEdittcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   if(is.na(value))
                     value <- "<NA>"

                   widget <- tag(obj, "widget")
                   widget$setValue(value)
                   ## tclvalue(tag(obj, "tclVar")) <- value
                   return(obj)
          })


## left bracket implement completion
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gEdittcltk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            widget <- tag(x, "widget")
            vals <- widget$words
            if(missing(i))
              vals
            else
              vals[i]
          })
            
setMethod("[",
          signature(x="gEdittcltk"),
          function(x, i, j, ..., drop=TRUE) {
            if(missing(i))
              .leftBracket(x,x@toolkit, ...)
            else
              .leftBracket(x,x@toolkit, i, ...)
          })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gEdittcltk"),
          function(x, toolkit, i, j, ..., value) {
            widget <- tag(x, "widget")
            vals <- widget$words
            # vals <- tag(x, "typeAhead")
            
            if(missing(i))
              vals <- value
            else
              vals[i] <- value
            widget$setWords(vals)
            ## tag(x, "typeAhead") <- vals
            return(x)
          })

setReplaceMethod("[",
                 signature(x="gEdittcltk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })


setReplaceMethod(".size", 
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gEdittcltk"),
                 function(obj, toolkit, ..., value) {
                   if(is.numeric(value))
                     tkconfigure(obj@widget,width=floor(value[1]/5)) 
                                        # convert pixels to chars
                   else
                     cat(gettext("size needs a numeric vector c(width,...)\n"))
                   return(obj)
                 })


##' visible<- if FALSE, for password usage
setReplaceMethod(".visible",signature(toolkit="guiWidgetsToolkittcltk", obj="gEdittcltk"),
          function(obj, toolkit, ..., value) {
            widget <- getWidget(obj)
            if(as.logical(value))
              tkconfigure(widget, show="")
            else
              tkconfigure(widget, show="*")
            return(obj)
          })


##################################################
## handlers

## changed is called after a commit (svalue, Return key in widget -- not drop down menu)
## keystroke is called when widget display changes

## Use Virtual Event for KeyRelease, as other one is used by class above

setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gEdittcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            widget <- tag(obj, "widget")
            .addHandler(widget$e, toolkit, signal="<<Changed>>", handler, action, actualobj=obj)
          })


setMethod(".addhandlerkeystroke",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gEdittcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            widget <- tag(obj, "widget")
            .addHandler(widget$e, toolkit, signal="<<KeyRelease>>", handler, action, actualobj=obj)
          })

