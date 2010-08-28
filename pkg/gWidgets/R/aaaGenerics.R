##################################################
### Code to handle toolkits
### A new toolkit should have:
### * a name gWidgetsXXXXX
### * a subclass of guiWidgetsToolkit
### * methods for .functions (.glabel, .svalue, ...)
### * optionally, methods for glabel(obj, ...) dispatching to .glabel(obj,toolkit, ...)

### toolkit code comes *before* others

##################################################
## Toolkit
## define a classes for toolkit, base class


setClass("guiWidgetsToolkit",
         representation(toolkit="character"),
         prototype(toolkit="")
         )

## RGtk2
setClass("guiWidgetsToolkitRGtk2",
         contains="guiWidgetsToolkit",
         prototype=prototype(new("guiWidgetsToolkit"))
         )

## rJava
setClass("guiWidgetsToolkitrJava",
         contains="guiWidgetsToolkit",
         prototype=prototype(new("guiWidgetsToolkit"))
         )

## SJava
setClass("guiWidgetsToolkitSJava",
         contains="guiWidgetsToolkit",
         prototype=prototype(new("guiWidgetsToolkit"))
         )
## tcltk
setClass("guiWidgetsToolkittcltk",
         contains="guiWidgetsToolkit",
         prototype=prototype(new("guiWidgetsToolkit"))
         )

## RwxWidgets
setClass("guiWidgetsToolkitRwxWidgets",
         contains="guiWidgetsToolkit",
         prototype=prototype(new("guiWidgetsToolkit"))
         )


## Qt
setClass("guiWidgetsToolkitQt",
         contains="guiWidgetsToolkit",
         prototype=prototype(new("guiWidgetsToolkit"))
         )


##################################################


##' all packages that are registered with gWidgets. Used if guiToolkit not specified
registered_packages <- c("gWidgetsRGtk2", "gWidgetstcltk", "gWidgetsQt", "gWidgetsrJava",
                         "gWidgetsRwxWidgets")

##' set or get the current toolkit for gWidgets
guiToolkit = function(name=NULL) {
  ## plan, if name is NULL, and options("guiToolkit") NULL then we popup a menu
  ## with choices coming from all installed packages named gWidgetsXXXX
  ## when a name is selected, we require the package gWidgets+name

  if(is.null(name)) {
    ## try to get from inheritance, then get from option

    x = try(get("toolkit", inherits=TRUE), silent=TRUE)
    if(!inherits(x,"try-error")) {
      ## check that toolkit is of guiWidgets type
      x = try("x@toolkit", silent=TRUE)
      if(!inherits(x,"try-error"))
        name = x
      else
        name = getOption("guiToolkit")
    } else {
      name = getOption("guiToolkit")
    }
  }
  if(!is.null(name) && is.na(name)) return(NULL)          # use NA to override choice
  ## no if it is null, we have to find the possible choices
  if(is.null(name)) {
    
    choices <- registered_packages[sapply(registered_packages, function(i) system.file(i) != "")]

    
    if(interactive()) {
      if(length(choices) == 0) {
        warning("No toolkits installed")
        return(NULL)
      } else if(length(choices) == 1) {
        theChoice = choices
      } else {
        theChoice = menu(choices, title="Select a GUI toolkit")
        if(theChoice == 0) {
          warning("No toolkit loaded")
          return(NULL)
        } else {
          theChoice = choices[theChoice]
        }
      }
      ## go with theChoice
      name = gsub("^gWidgets","",theChoice)
      options("guiToolkit"=name)
    } else {
      ## not interactive 
      return(NULL)
    }
  }

  ## require the package
  require(paste("gWidgets",name,sep=""), character.only=TRUE)
  ## we return an instance of the toolkit class
  obj = new(paste("guiWidgetsToolkit",name,sep=""), toolkit = name)
  return(obj)
}

##
##
##################################################


##################################################
##
## The basic classes
setClass("guiWidget",
         representation(
                        toolkit="guiWidgetsToolkit",
                        widget="ANY"  # could be RGtkObject, TclObject,....
                        ),
         prototype(
                   toolkit =guiToolkit(),
                   widget=NULL
                   )
         ) 
           

## define a subclass
setClass("guiComponent",
         contains="guiWidget",
         prototype=prototype(new("guiWidget"))
         )
## define a subclass
setClass("guiContainer",
         contains="guiWidget",
         prototype=prototype(new("guiWidget"))
         )
## define useless subclass
setClass("guiDialog",
         contains="guiWidget",
         prototype=prototype(new("guiWidget"))
         )


## useful later on.
setClassUnion("guiWidgetOrNULL",
              c("NULL","guiWidget"))

##################################################
### Show to control printing
setMethod("show",signature(object="guiWidget"),
          function(object) {
            cat("guiWidget of type:",
                class(object@widget),
                "for toolkit:",
                class(object@toolkit),
                "\n")
          })
                



##################################################
### Components

### glabel
## constructor
glabel = function(
  text= "", markup = FALSE, editable = FALSE, handler = NULL, 
    action = NULL, container = NULL, 
  ..., toolkit=guiToolkit()) {
  widget = .glabel(toolkit,
    text= text, markup = markup, editable = editable, handler = handler, 
    action = action, container = container, 
    ...)
  obj = new("guiComponent",widget=widget,toolkit=toolkit)
  return(obj)
}
setGeneric(".glabel",function(toolkit,
                              text= "", markup = FALSE, editable = FALSE, handler = NULL, 
                              action = NULL, container = NULL, 
                              ...) standardGeneric(".glabel"))



## gbutton
gbutton =function(
  text = "", border=TRUE, handler = NULL, action = NULL, container = NULL,      ...,
  toolkit=guiToolkit()){
  force(toolkit)                        # load package
  widget =  .gbutton (toolkit,
    text, border, handler, action, container,...)  
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}
 
setGeneric( '.gbutton' , function(toolkit,
                                  text = "", border=TRUE, handler = NULL, action = NULL, container = NULL,...)
           standardGeneric( '.gbutton' ))
 
## gcheckbox
## the constructor
gcheckbox =function(
  text, checked = FALSE, handler = NULL, action = NULL,      container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .gcheckbox (toolkit,
    text=text, checked=checked, handler=handler, action=action, container=container 
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}


## generic for toolkit dispatch
setGeneric( '.gcheckbox' , function(toolkit,
                                    text, checked = FALSE, handler = NULL, action = NULL,      container = NULL, ... ) standardGeneric( '.gcheckbox' ))


## gradio
## constructor
gradio = function(items,selected=1, horizontal=FALSE, handler=NULL,
  action=NULL, container=NULL, ...,
  toolkit=guiToolkit()) {
  radio = .gradio(toolkit,items, selected, horizontal, handler, action, container,...)
  obj = new("guiComponent",widget=radio, toolkit=toolkit)
  return(obj)
}
setGeneric(".gradio",function(toolkit,
                              items,selected=1, horizontal=FALSE, handler=NULL, action=NULL,
                              container=NULL,
                              ...) standardGeneric(".gradio"))


## gdroplist
## the class
## the constructor
gdroplist =function(
  items, selected = 1, editable = FALSE, coerce.with=NULL, handler = NULL,      action = NULL, container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .gdroplist (toolkit,
    items=items, selected=selected, editable=editable, coerce.with=coerce.with, handler=handler, action=action, container=container, ...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}


 ## generic for toolkit dispatch
setGeneric( '.gdroplist' , function(toolkit,
                                    items, selected = 1, editable = FALSE, coerce.with = NULL, handler = NULL,      action = NULL, container = NULL, ... ) standardGeneric( '.gdroplist' ))

## give alias for gdroplist
gcombobox <- gdroplist

## gcheckboxgroup
## the constructor
gcheckboxgroup =function(
  items, checked = FALSE, horizontal = FALSE,
  use.table=FALSE, handler = NULL,
  action = NULL, container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .gcheckboxgroup (toolkit,
    items=items, checked=checked, horizontal=horizontal, use.table=use.table,
    handler=handler, action=action, container=container, ...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}
 

## generic for toolkit dispatch
setGeneric( '.gcheckboxgroup' ,
           function(toolkit,
                    items, checked = FALSE, horizontal = FALSE,
                    handler = NULL, action = NULL,
                    container = NULL, ... ) standardGeneric( '.gcheckboxgroup' )) 

## gspinbutton
## the constructor
gspinbutton =function(
  from = 0, to = 10, by = 1, value = from, digits = 0,
  handler = NULL, action = NULL, container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .gspinbutton (toolkit,
    from=from, to=to, by=by, value=value, digits=digits,
    handler=handler, action=action, container=container ,...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}


## generic for toolkit dispatch
setGeneric( '.gspinbutton' ,
           function(toolkit,
                    from = 0, to = 10, by = 1, value = from, digits = 0,
                    handler = NULL, action = NULL, container = NULL, ... ) standardGeneric( '.gspinbutton' ))

## gslider
## the constructor
gslider =function(
  from = 0, to = 100, by = 1, value = from, horizontal = TRUE,
  handler = NULL, action = NULL, container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .gslider (toolkit,
    from=from, to=to, by=by, value=value, horizontal=horizontal,
    handler=handler, action=action, container=container ,...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}
 
## generic for toolkit dispatch
setGeneric( '.gslider' ,
           function(toolkit,
                    from = 0, to = 100, by = 1, value = from, horizontal = TRUE,
                    handler = NULL, action = NULL, container = NULL, ... ) standardGeneric( '.gslider' ))

## gedit
## the constructor
gedit =function(
  text = "", width = 25, coerce.with = NULL,
  handler = NULL, action = NULL, container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .gedit(toolkit,
    text=text, width=width, coerce.with=coerce.with,
    handler=handler, action=action, container=container ,...
 )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
 }
 

## generic for toolkit dispatch
setGeneric( '.gedit' ,
           function(toolkit,
                    text = "", width = 25, coerce.with = NULL,
                    handler = NULL,action = NULL, container = NULL, ... )
           standardGeneric( '.gedit' ))


## gtext
## the constructor
gtext =function(
  text = NULL, width = NULL, height = 300, font.attr = NULL,
  wrap = TRUE,
  handler = NULL, action = NULL, container = NULL,      ... ,
  toolkit=guiToolkit()){
  widget = .gtext(toolkit,
    text=text, width=width, height=height, font.attr=font.attr, wrap=wrap,
    handler=handler, action=action, container=container ,...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}
 

## generic for toolkit dispatch
setGeneric( '.gtext' ,
           function(toolkit,
                    text = NULL, width = NULL, height = 300, font.attr = NULL,
                    wrap = TRUE,
                    handler = NULL, action = NULL, container = NULL,... )
           standardGeneric( '.gtext' ))


## gaction
gaction =function(
  label, tooltip=NULL, icon = NULL, key.accel = NULL,
  handler = NULL, action = NULL, ...,
  toolkit=guiToolkit()) {
  widget =  .gaction (toolkit,
    label, tooltip, icon, key.accel, handler, action, ...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}


## generic for toolkit dispatch
setGeneric( '.gaction' ,
           function(toolkit,
                    label, tooltip = NULL, icon = NULL, key.accel=NULL,
                    handler = NULL, action = NULL,
                    ... )
           standardGeneric( '.gaction' ))



## gmenu
## the constructor
gmenu =function(
  menulist, popup = FALSE, action = NULL, container = NULL,      ... ,
  toolkit=guiToolkit()){
  widget =  .gmenu (toolkit,
    menulist=menulist, popup=popup, action=action, container=container ,...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}


## generic for toolkit dispatch
setGeneric( '.gmenu' ,
           function(toolkit,
                    menulist, popup = FALSE, action = NULL, container = NULL,
                    ... )
           standardGeneric( '.gmenu' ))

## gtoolbar
## the constructor
gtoolbar =function(
  toolbarlist, style = c("both", "icons", "text", "both-horiz"),
  action = NULL, container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .gtoolbar (toolkit,
    toolbarlist=toolbarlist, style=style, action=action, container=container ,...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}


 ## generic for toolkit dispatch
setGeneric( '.gtoolbar' ,
           function(toolkit,
                    toolbarlist, style = c("both", "icons", "text", "both-horiz"),
                    action = NULL, container = NULL, ... )
           standardGeneric( '.gtoolbar' ))


## gtable

## the constructor
gtable =function(
  items, multiple = FALSE, chosencol = 1, icon.FUN = NULL,
  filter.column = NULL, filter.labels = NULL, filter.FUN = NULL,
  handler = NULL, action = NULL, container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .gtable (toolkit,
    items=items, multiple=multiple, chosencol=chosencol,
    icon.FUN=icon.FUN,
    filter.column=filter.column, filter.labels=filter.labels, filter.FUN=filter.FUN,
    handler=handler, action=action, container=container ,...
 )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}


## generic for toolkit dispatch
setGeneric( '.gtable' ,
           function(toolkit,
                    items, multiple = FALSE, chosencol = 1,
                    icon.FUN = NULL,
                    filter.column = NULL, filter.labels = NULL, filter.FUN = NULL,
                    handler = NULL, action = NULL, container = NULL, ... )
           standardGeneric( '.gtable' ))


## gdf
## the constructor
gdf =function(
  items = NULL, name = deparse(substitute(items)), do.subset = FALSE,
  container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .gdf (toolkit,
    items=items, name=name, do.subset=do.subset, container=container ,...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}


## generic for toolkit dispatch
setGeneric( '.gdf' ,
           function(toolkit,
                    items = NULL, name = deparse(substitute(items)),
                    do.subset = FALSE,      container = NULL, ... )
           standardGeneric( '.gdf' ))


## gdfnotebook
## the constructor
gdfnotebook =function(
  items = NULL, container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .gdfnotebook (toolkit,
    items=items, container=container ,...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}


## generic for toolkit dispatch
setGeneric( '.gdfnotebook' ,
           function(toolkit,
                    items = NULL, container = NULL, ... )
           standardGeneric( '.gdfnotebook' ))

## gtree
## the constructor
gtree =function(
  offspring = NULL, hasOffspring = NULL, offspring.data = NULL,
  col.types = NULL, icon.FUN = NULL, chosencol = 1, multiple = FALSE,
  handler = NULL, action = NULL, container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .gtree (toolkit,
    offspring=offspring, hasOffspring=hasOffspring,
    offspring.data=offspring.data, col.types=col.types, icon.FUN=icon.FUN,
    chosencol=chosencol, multiple=multiple,
    handler=handler, action=action, container=container ,...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}
 

## generic for toolkit dispatch
setGeneric( '.gtree' ,
           function(toolkit,
                    offspring = NULL, hasOffspring = NULL,
                    offspring.data = NULL,
                    col.types = NULL, icon.FUN = NULL, chosencol = 1,
                    multiple = FALSE,
                    handler = NULL, action = NULL, container = NULL, ... )
           standardGeneric( '.gtree' ))


## gfile (gfilebrowse in base)
## the constructor
gfile <- function(
                  text = "", type = c("open", "save", "selectdir"),
                  initialfilename = NULL,
                  filter = list("All files" = list(patterns = c("*")), "R files" = list(patterns = c("*.R",          "*.Rdata")), "text files" = list(mime.types = c("text/plain"))          ),
                  multi=FALSE,
                  handler = NULL, action = NULL, ... ,
                  toolkit=guiToolkit()){
  widget =  .gfile (toolkit,
    text=text, type=type, initialfilename=initialfilename,
    filter=filter, multi=multi, handler=handler, action=action ,...
    )
}


## generic for toolkit dispatch
setGeneric( '.gfile' ,
           function(toolkit,
                    text = "", type = c("open", "save", "selectdir"),
                    initialfilename = NULL,
                    filter = list("All files" = list(patterns = c("*")), "R files" = list(patterns = c("*.R",          "*.Rdata")), "text files" = list(mime.types = c("text/plain"))
                      ),
                    handler = NULL, action = NULL, ... )
           standardGeneric( '.gfile' ))


## gfilebrowse
gfilebrowse = function (
  text = "Select a file...", type = "open", quote = TRUE, 
    container = NULL, ..., toolkit = guiToolkit()) {
  widget =  .gfilebrowse (toolkit,
    text=text, type=type, quote=quote, container=container, ...)
  obj = new('guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}
setGeneric(".gfilebrowse",
           function(toolkit,
                    text = "Select a file...", type = "open", quote = TRUE, 
                    container = NULL, ...)
           standardGeneric( '.gfilebrowse' ))

## gcalendar
## the constructor
gcalendar =function(
  text = "", format = "%Y-%m-%d", 
  handler = NULL, action=NULL, container = NULL,...,
  toolkit=guiToolkit()){
  widget =  .gcalendar (toolkit,
    text=text, format=format, handler=handler,action=action,
    container=container , ...
    )
  obj = new( 'guiWidget',widget=widget,toolkit=toolkit) 
 return(obj)
}


## generic for toolkit dispatch
setGeneric( '.gcalendar' ,
           function(toolkit,
                    text = "", format = "%Y-%m-%d", 
                    handler=NULL, action=NULL, container = NULL,
                    ... )
           standardGeneric( '.gcalendar' ))

## ggraphics

## the constructor
ggraphics =function(
  width = dpi * 6, height = dpi * 6, dpi = 75, ps = 12,      container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .ggraphics (toolkit,
    width=width, height=height, dpi=dpi, ps=ps, container=container ,...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}


 ## generic for toolkit dispatch
setGeneric( '.ggraphics' ,
           function(toolkit,
                    width = dpi * 6, height = dpi * 6, dpi = 75, ps = 12,
                    container = NULL, ... )
           standardGeneric( '.ggraphics' ))


## gplotnotebook
## the constructor
ggraphicsnotebook =function(
  width = dpi * 6, height = dpi * 6, dpi = 75, container = NULL,      ... ,
  toolkit=guiToolkit()){
  widget =  .ggraphicsnotebook (toolkit,
    width=width, height=height, dpi=dpi, container=container ,...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}


## generic for toolkit dispatch
setGeneric( '.ggraphicsnotebook' ,
           function(toolkit,
                    width = dpi * 6, height = dpi * 6, dpi = 75,
                    container = NULL,      ... )
           standardGeneric( '.ggraphicsnotebook' ))

## gimage

## the constructor
gimage =function(
  filename = "", dirname = "", size = "", handler = NULL,
  action = NULL, container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .gimage (toolkit,
    filename=filename, dirname=dirname, size=size,
    handler=handler, action=action, container=container ,...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}

## generic for toolkit dispatch
setGeneric( '.gimage' ,
           function(toolkit,
                    filename = "", dirname = "", size = "",
                    handler = NULL,action = NULL, container = NULL, ... )
           standardGeneric( '.gimage' ))


## gsvg

## the constructor
gsvg <- function(
                 filename="", width=480, height=480,
                 handler=NULL, action=NULL,
                 container = NULL, ... ,
                 toolkit=guiToolkit()){
  widget =  .gsvg (toolkit,
    filename=filename, width=width, height=height,
    handler=handler, action=action, container=container ,...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}

## generic for toolkit dispatch
setGeneric( '.gsvg' ,
           function(toolkit,
                    filename = "", width=480,  height=480,
                    handler=NULL, action=NULL,
                    container = NULL, ... )
           standardGeneric( '.gsvg' ))


## ANY constructor
setMethod(".gsvg",
          signature(toolkit="ANY"),
          function(toolkit,
                   filename = "", width=480, height=480,
                   handler=NULL, action=NULL,
                   container = NULL,
                   ...) {
            cat(gettext("gsvg widget not imlemented"))
            return(glabel(container=container, ...))
          })


## gstatusbar

## the constructor
gstatusbar =function(
  text = "", container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .gstatusbar (toolkit,
    text=text, container=container ,...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}


## generic for toolkit dispatch
setGeneric( '.gstatusbar' ,
           function(toolkit,
                    text = "", container = NULL, ... )
           standardGeneric( '.gstatusbar' ))

## ghtml function
ghtml = function(
  x, handler = NULL, 
    action = NULL, container = NULL, 
  ..., toolkit=guiToolkit()) {
  widget = .ghtml(toolkit,
    x,
    handler = handler, 
    action = action, container = container, 
    ...)
  obj = new("guiComponent",widget=widget,toolkit=toolkit)
  return(obj)
}
setGeneric(".ghtml",function(toolkit,
                             x, handler = NULL, 
                             action = NULL, container = NULL, 
                             ...) standardGeneric(".ghtml"))




## gseparator
## the constructor
gseparator =function(
  horizontal = TRUE, container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .gseparator (toolkit,
    horizontal=horizontal, container=container ,...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}


## generic for toolkit dispatch
setGeneric( '.gseparator' ,
           function(toolkit,
                    horizontal = TRUE, container = NULL, ... )
           standardGeneric( '.gseparator' ))

##################################################
## these should be in gWidgets, based on others no toolkit specific code

## the constructor
gcommandline =function(
  command = "", assignto = NULL, useGUI = TRUE, useConsole = FALSE,
  prompt = getOption("prompt"), width = 500, height = 0.6 * width,
  container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .gcommandline (toolkit,
    command=command, assignto=assignto,
    useGUI = useGUI, useConsole=useConsole,
    prompt=prompt, width=width, height=height, container=container, ...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}


## generic for toolkit dispatch
setGeneric( '.gcommandline' ,
           function(toolkit,
                    command = "", assignto = NULL,
                    useGUI = TRUE, useConsole = FALSE,
                    prompt = getOption("prompt"), width = 500,
                    height = 0.6 * width, container = NULL, ... )
           standardGeneric( '.gcommandline' ))

## ghelp, ghelpbrowser

## the constructor
ghelp =function(
  topic = NULL, package = NULL, container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .ghelp (toolkit,
    topic=topic, package=package, container=container ,...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}


## generic for toolkit dispatch
setGeneric( '.ghelp' ,
           function(toolkit,
                    topic = NULL, package = NULL, container = NULL, ... )
           standardGeneric( '.ghelp' ))
## ghelpbrowser

## the constructor
ghelpbrowser =function(
  title = "Help browser", maxTerms = 100, width = 550,      height = 600 ,
  ...,
  toolkit=guiToolkit()) {
  widget =  .ghelpbrowser(toolkit,
    title=title, maxTerms=maxTerms, width=width
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}


## generic for toolkit dispatch
setGeneric( '.ghelpbrowser' ,
           function(toolkit,
                    title = "Help browser", maxTerms = 100,
                    width = 550, height = 600 )
           standardGeneric( '.ghelpbrowser' ))

## ggenericwidget

## the constructor
ggenericwidget =function(
  lst,  cli = NULL, container = NULL,
  ... ,
  toolkit=guiToolkit()){

  ## if lst is a function, we much fix
  fName <- NULL
  if(is.function(lst)) 
    fName <- deparse(substitute(lst))

  widget =  .ggenericwidget (toolkit,
    lst=lst, cli=cli, container=container , fName=fName, ...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}


## generic for toolkit dispatch
setGeneric( '.ggenericwidget' ,
           function(toolkit,
                    lst, cli = NULL,
                    container = NULL, ... )
           standardGeneric( '.ggenericwidget' ))

## gformlayout

## the constructor
gformlayout =function(
  lst, container = NULL,
  ... ,
  toolkit=guiToolkit()){
  widget =  .gformlayout(toolkit,
    lst=lst, container=container ,...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}


## generic for toolkit dispatch
setGeneric( '.gformlayout' ,
           function(toolkit,
                    lst, 
                    container = NULL, ... )
           standardGeneric( '.gformlayout' ))

## gvarbrowser

## the constructor
gvarbrowser =function(
  handler = NULL,
  action = "summary",
  container = NULL ,...,
  toolkit=guiToolkit()){
  widget =  .gvarbrowser (toolkit,
    handler=handler, action=action, container = container, ...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}
 

## generic for toolkit dispatch
setGeneric( '.gvarbrowser' ,
           function(toolkit,
                    handler = NULL,
                    action = "summary", container = NULL,... )
           standardGeneric( '.gvarbrowser' ))



##################################################
### Containers

## define a gWidget constructor
## not a generice          
gwindow = function(title="Window" ,visible=TRUE, name=title,
  width = NULL, height = NULL, parent = NULL,
  handler = NULL, action = NULL,
  ...,
  toolkit=guiToolkit()
  ) {
  theArgs <- list(...)
  if(!is.null(theArgs$location)) {
    parent <- theArgs$location
    cat(gettext("location argument is renamed to 'parent'\n"))
  }

  ## THe visible=TRUE default is not the best. I'd change it if I could go back in time, but
  ## c'est la vie. Anyways, for those that it really bugs there is this check
  if(!is.null(getOption("gWidgets:gwindow-default-visible-is-false")))
    visible <- FALSE
  
      
  win = .gwindow(toolkit,title, visible,width, height, parent, handler, action, ...)
  obj = new("guiContainer",widget=win,toolkit=toolkit)
  return(obj)
}

## define a toolkit constructor, dispatch on toolkit
setGeneric(".gwindow",function(toolkit, title, visible, width, height, parent, handler, action,...) standardGeneric(".gwindow"))

## ggroup
 
## the constructor
ggroup =function(
  horizontal = TRUE, spacing = 5, use.scrollwindow = FALSE, container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .ggroup (toolkit,
    horizontal=horizontal, spacing=spacing,
    use.scrollwindow = use.scrollwindow, 
    container=container,...
    )
  obj = new( 'guiContainer',widget=widget,toolkit=toolkit) 
  return(obj)
}
 

## generic for toolkit dispatch
setGeneric( '.ggroup' ,
           function(toolkit,
                    horizontal = TRUE, spacing = 5,  use.scrollwindow = FALSE, container = NULL,  ... )
           standardGeneric( '.ggroup' ))


## gframe
## the constructor
gframe =function(
  text = "", markup = FALSE, pos = 0, horizontal=TRUE, container = NULL,
   ... ,
  toolkit=guiToolkit()){
  widget =  .gframe (toolkit,
    text=text, markup=markup, pos=pos, horizontal=horizontal, container=container ,
    ...
    )
  obj = new( 'guiContainer',widget=widget,toolkit=toolkit) 
  return(obj)
}


## generic for toolkit dispatch
setGeneric( '.gframe' ,
           function(toolkit,
                    text = "", markup = FALSE, pos = 0, horizontal=TRUE,
                    container = NULL,      ... )
           standardGeneric( '.gframe' ))

## gexpandgroup
## the constructor
gexpandgroup =function(
  text = "", markup = FALSE, horizontal=TRUE, handler = NULL, action = NULL,      container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .gexpandgroup (toolkit,
    text=text, markup=markup, horizontal=horizontal, handler=handler, action=action, container=container ,...
    )
  obj = new( 'guiContainer',widget=widget,toolkit=toolkit) 
  return(obj)
}


## generic for toolkit dispatch
setGeneric( '.gexpandgroup' ,
           function(toolkit,
                    text = "", markup = FALSE,horizontal=TRUE,
                    handler = NULL, action = NULL,
                    container = NULL, ... )
standardGeneric( '.gexpandgroup' ))



## gnotebook
## the constructor
gnotebook =function(
  tab.pos = 3, closebuttons = FALSE, dontCloseThese = NULL,
  container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .gnotebook (toolkit,
    tab.pos=tab.pos, closebuttons=closebuttons, dontCloseThese=dontCloseThese,
    container=container ,...
    )
  obj = new( 'guiContainer', widget=widget,toolkit=toolkit)
  return(obj)
}


## generic for toolkit dispatch
setGeneric( '.gnotebook' ,
           function(toolkit,
                    tab.pos = 3, closebuttons = FALSE, dontCloseThese = NULL,
                    container = NULL, ... )
           standardGeneric( '.gnotebook' ))

## glayout

## the constructor
glayout =function(
  homogeneous = FALSE, spacing = 10, container = NULL,      ... ,
  toolkit=guiToolkit()){
  widget =  .glayout (toolkit,
    homogeneous=homogeneous, spacing=spacing, container=container ,...
    )
  obj = new( 'guiContainer',widget=widget,toolkit=toolkit) 
  return(obj)
}
## generic for toolkit dispatch
setGeneric( '.glayout' ,
           function(toolkit,
                    homogeneous = FALSE, spacing = 10, container = NULL,
                    ... )
           standardGeneric( '.glayout' ))


## gpanedgroup
## the constructor
gpanedgroup =function(
  widget1=NULL, widget2=NULL, horizontal = TRUE, container = NULL , ...,
  toolkit=guiToolkit()){
  widget = .gpanedgroup (toolkit,
    widget1, widget2, horizontal=horizontal,
    container=container, ...
    )
  obj = new( 'guiContainer',widget=widget,toolkit=toolkit) 
  return(obj)
}


## generic for toolkit dispatch
setGeneric( '.gpanedgroup' ,
           function(toolkit,
                    widget1, widget2, horizontal = TRUE, container = NULL, ... )
           standardGeneric( '.gpanedgroup' ))

## icons
addStockIcons = function(iconNames,iconFiles, ..., toolkit = guiToolkit()) {
  out =  .addStockIcons (toolkit, iconNames, iconFiles, ...)
  return(out)
}
## generic for dispath
setGeneric( '.addStockIcons' ,
           function(toolkit, iconNames, iconFiles,... )
           standardGeneric( '.addStockIcons' ))

getStockIcons = function( ..., toolkit = guiToolkit()) {
  out =  .getStockIcons (toolkit,...)
  return(out)
}

setGeneric( '.getStockIcons' ,
           function(toolkit,...)
           standardGeneric( '.getStockIcons' ))


stockIconFromClass = function(theClass, ..., toolkit = guiToolkit()) {
  out =  .stockIconFromClass (toolkit, theClass, ...)
  return(out)
}
## generic for dispath
setGeneric( '.stockIconFromClass' ,
           function(toolkit, theClass,... )
           standardGeneric( '.stockIconFromClass' ))

stockIconFromObject = function(obj, ..., toolkit = guiToolkit()) {
  out =  .stockIconFromClass (toolkit, obj, ...)
  return(out)
}
## generic for dispath
setGeneric( '.stockIconFromObject' ,
           function(toolkit, obj,... )
           standardGeneric( '.stockIconFromObject' ))

##################################################
##
## Methods

## svalue
setGeneric("svalue",function(obj, index=NULL, drop=NULL,...) standardGeneric("svalue"))
setMethod("svalue",signature(obj="guiWidget"),
          function(obj, index=NULL, drop=NULL, ... ) {
            toolkit = obj@toolkit
            .svalue(obj@widget, toolkit, ...,index=index, drop=drop)
          })
## svalue has a few pass throughs
setMethod("svalue",signature(obj="numeric"),
          function(obj, index=NULL, drop=NULL, ... ) {
            return(obj)
          })
setMethod("svalue",signature(obj="character"),
          function(obj, index=NULL, drop=NULL, ... ) {
            if(length(obj) == 1)
              return(get(obj, envir=.GlobalEnv))
            else
              return(obj)
          })
## package generic has toolkit, object to dispatch on
setGeneric(".svalue",function(obj, toolkit, index=NULL, drop=NULL,  ...)
           standardGeneric(".svalue"))

## svalue<-
setGeneric("svalue<-",function(obj, index=NULL, ...,value) standardGeneric("svalue<-"))
setReplaceMethod("svalue",signature(obj="guiWidget"),
          function(obj, index=NULL,  ...,value) {
            toolkit = obj@toolkit
            .svalue(obj@widget, toolkit, index=index,  ...) <- value
            obj
          })
setGeneric(".svalue<-",function(obj, toolkit, index=NULL, ..., value)
           standardGeneric(".svalue<-"))

## [ -- this is tricky, hopefully it works
setMethod("[",
          signature(x="guiWidget"),
          function(x,i,j,...,drop=TRUE) {
            if(missing(drop)) drop <- TRUE
            return(.leftBracket(x@widget, x@toolkit,i,j,...,drop=drop))
          })
setGeneric(".leftBracket",function(x, toolkit, i,j, ..., drop=TRUE)
           standardGeneric(".leftBracket"))

## toolkit specific 
## [<-
setReplaceMethod("[",signature(x="guiWidget"),
          function(x,i,j,...,value) {
            toolkit = x@toolkit
            if(missing(i) && missing(j))
              .leftBracket(x@widget, toolkit,...) <- value
            else if(missing(j))
              .leftBracket(x@widget, toolkit,i,...) <- value
            else 
              .leftBracket(x@widget, toolkit,i,j,...) <- value
            return(x)
          })
setGeneric(".leftBracket<-",function(x, toolkit, i,j, ..., value)
           standardGeneric(".leftBracket<-"))



## add
setGeneric("add",function(obj,value, ...) standardGeneric("add"))
## add generic for Containers and sometimes widgets
setMethod("add",signature(obj="guiWidget"),
          function(obj, value, ...) {
            toolkit = obj@toolkit
            .add(obj@widget, toolkit,value,...)
          })
setMethod("add",signature(obj="guiContainer"),
          function(obj, value, ...) {
            toolkit = obj@toolkit
            ladd <- function(obj, value, ...,do.newline, font.attr, where)
              .add(obj@widget, obj@toolkit, value, ...)
            ladd(obj, value,...)
          })
setMethod("add",signature(obj="guiComponent"),
          function(obj, value, ...) {
            toolkit = obj@toolkit
            ladd <- function(obj,value, ..., label, name, override.closebuttons, index, pageno, markup, anchor, expand) .add(obj@widget,obj@toolkit,value,...)
             ladd(obj,value,...)
          })


## dispatch with toolkit
setGeneric(".add",function(obj, toolkit,value,...) standardGeneric(".add"))


## Insert is new add for components (gtext)
setGeneric("insert",function(obj,value,
                             where = c("end","beginning","at.cursor"),
                             font.attr=NULL,
                             do.newline=TRUE,
                             ...) standardGeneric("insert"))

setMethod("insert",signature(obj="guiComponent"),
          function(obj, value, where = c("end","beginning","at.cursor"), font.attr = NULL,
                   do.newline = TRUE, ...) {
            toolkit = obj@toolkit
            where = match.arg(where)
            .insert(obj@widget, toolkit, value, where, font.attr,do.newline,...)
          })

## dispatch with toolkit
setGeneric(".insert",function(obj, toolkit,value, where=c("end","beginning","at.cursor"),
                              font.attr=NULL, do.newline=TRUE,...)
           standardGeneric(".insert"))




## addSpace
setGeneric("addSpace",function(obj,value, ...) standardGeneric("addSpace"))
## add generic for Containers and sometimes widgets
setMethod("addSpace",signature(obj="guiWidget"),
          function(obj, value, ...) {
            toolkit = obj@toolkit
            .addSpace(obj@widget,toolkit,value,...)
          })
## dispatch with toolkit
setGeneric(".addSpace",function(obj,toolkit,value,...) standardGeneric(".addSpace"))



## addSpring
setGeneric("addSpring",function(obj,...) standardGeneric("addSpring"))
## add generic for Containers and sometimes widgets
setMethod("addSpring",signature(obj="guiWidget"),
          function(obj, ...) {
            toolkit = obj@toolkit
            .addSpring(obj@widget, toolkit,...)
          })
## dispatch with toolkit
setGeneric(".addSpring",function(obj, toolkit,...) standardGeneric(".addSpring"))



## delete
setGeneric("delete",function(obj,widget, ...) standardGeneric("delete"))
## add generic for Containers and sometimes widgets
setMethod("delete",signature(obj="guiWidget"),
          function(obj, widget, ...) {
            toolkit = obj@toolkit
            .delete(obj@widget,toolkit,widget,...)
          })
## dispatch with toolkit
setGeneric(".delete",function(obj, toolkit,widget,...) standardGeneric(".delete"))

## dispose
setGeneric("dispose",function(obj, ...) standardGeneric("dispose"))
## add generic for Containers and sometimes widgets
setMethod("dispose",signature(obj="guiWidget"),
          function(obj, ...) {
            toolkit = obj@toolkit
            .dispose(obj@widget, toolkit,...)
          })
## dispatch with toolkit
setGeneric(".dispose",function(obj,toolkit,...) standardGeneric(".dispose"))


## visible
setGeneric("visible",function(obj, set=NULL, ...) standardGeneric("visible"))
## add generic for Containers and sometimes widgets
setMethod("visible",signature(obj="guiWidget"),
          function(obj, set=NULL, ...) {
            toolkit = obj@toolkit
            .visible(obj@widget,toolkit, set=set, ...)
          })
## dispatch with toolkit
setGeneric(".visible",function(obj, toolkit, set=NULL, ...) standardGeneric(".visible"))

## visible<-
setGeneric("visible<-",function(obj, ..., value) standardGeneric("visible<-"))
## add generic for Containers and sometimes widgets
setReplaceMethod("visible",signature(obj="guiWidget"),
          function(obj, ..., value) {
            toolkit = obj@toolkit
            .visible(obj@widget, toolkit, ...) <- value
            return(obj)
          })
## dispatch with toolkit
setGeneric(".visible<-",function(obj, toolkit,...,value)
           standardGeneric(".visible<-"))

## enabled
setGeneric("enabled",function(obj, ...) standardGeneric("enabled"))
## add generic for Containers and sometimes widgets
setMethod("enabled",signature(obj="guiWidget"),
          function(obj, ...) {
            toolkit = obj@toolkit
            .enabled(obj@widget, toolkit,...)
          })
## dispatch with toolkit
setGeneric(".enabled",function(obj, toolkit,...) standardGeneric(".enabled"))

## enabled<-
setGeneric("enabled<-",function(obj, ..., value) standardGeneric("enabled<-"))
## add generic for Containers and sometimes widgets
setReplaceMethod("enabled",signature(obj="guiWidget"),
          function(obj, ..., value) {
            toolkit = obj@toolkit
            .enabled(obj@widget, toolkit,...) <- value
            return(obj)
          })
## dispatch with toolkit
setGeneric(".enabled<-",function(obj, toolkit,...,value)
           standardGeneric(".enabled<-"))

## size
setGeneric("size",function(obj, ...) standardGeneric("size"))
## add generic for Containers and sometimes widgets
setMethod("size",signature(obj="guiWidget"),
          function(obj, ...) {
            toolkit = obj@toolkit
            .size(obj@widget, toolkit,...)
          })
## dispatch with toolkit
setGeneric(".size",function(obj, toolkit,...) standardGeneric(".size"))

## size<-
setGeneric("size<-",function(obj, ..., value) standardGeneric("size<-"))
## add generic for Containers and sometimes widgets
setMethod("size<-",signature(obj="guiWidget"),
          function(obj, ..., value) {
            toolkit = obj@toolkit
            .size(obj@widget, toolkit,...) <- value
            return(obj)
          })
## dispatch with toolkit
setGeneric(".size<-",function(obj, toolkit,...,value)
           standardGeneric(".size<-"))



## focus
setGeneric("focus",function(obj, ...) standardGeneric("focus"))
## add generic for Containers and sometimes widgets
setMethod("focus",signature(obj="guiWidget"),
          function(obj, ...) {
            toolkit = obj@toolkit
            .focus(obj@widget, toolkit,...)
          })
## dispatch with toolkit
setGeneric(".focus",function(obj, toolkit,...) standardGeneric(".focus"))

## focus<-
setGeneric("focus<-",function(obj, ..., value) standardGeneric("focus<-"))
## add generic for Containers and sometimes widgets
setMethod("focus<-",signature(obj="guiWidget"),
          function(obj, ..., value) {
            toolkit = obj@toolkit
            .focus(obj@widget, toolkit,...) <- value
            return(obj)
          })
## dispatch with toolkit
setGeneric(".focus<-",function(obj, toolkit,...,value)
           standardGeneric(".focus<-"))


## tooltip
setGeneric("tooltip<-",function(obj, ..., value) standardGeneric("tooltip<-"))
## add generic for Containers and sometimes widgets
setMethod("tooltip<-",signature(obj="guiWidget"),
          function(obj, ..., value) {
            toolkit = obj@toolkit
            .tooltip(obj@widget, toolkit,...) <- value
            return(obj)
          })
## dispatch with toolkit
setGeneric(".tooltip<-",function(obj, toolkit,...,value)
           standardGeneric(".tooltip<-"))


## defaultWidget
setGeneric("defaultWidget",function(obj, ...) standardGeneric("defaultWidget"))
## add generic for Containers and sometimes widgets
setMethod("defaultWidget",signature(obj="guiWidget"),
          function(obj, ...) {
            toolkit = obj@toolkit
            .defaultWidget(obj@widget, toolkit,...)
          })
## dispatch with toolkit
setGeneric(".defaultWidget",function(obj, toolkit,...) standardGeneric(".defaultWidget"))

## defaultWidget<-
setGeneric("defaultWidget<-",function(obj, ..., value) standardGeneric("defaultWidget<-"))
## add generic for Containers and sometimes widgets
setMethod("defaultWidget<-",signature(obj="guiWidget"),
          function(obj, ..., value) {
            toolkit = obj@toolkit
            .defaultWidget(obj@widget, toolkit,...) <- value
            return(obj)
          })
## dispatch with toolkit
setGeneric(".defaultWidget<-",function(obj, toolkit,...,value)
           standardGeneric(".defaultWidget<-"))


## font
setGeneric("font",function(obj, ...) standardGeneric("font"))
## add generic for Containers and sometimes widgets
setMethod("font",signature(obj="guiWidget"),
          function(obj, ...) {
            toolkit = obj@toolkit
            .font(obj@widget, toolkit,...)
          })
## dispatch with toolkit
setGeneric(".font",function(obj,toolkit,...) standardGeneric(".font"))

## font<-
setGeneric("font<-",function(obj, ..., value) standardGeneric("font<-"))
## add generic for Containers and sometimes widgets
setMethod("font<-",signature(obj="guiWidget"),
          function(obj, ..., value) {
            toolkit = obj@toolkit
            .font(obj@widget, toolkit,...) <- value ## DEPRECATED.fixFontMessUp(value)
            return(obj)
          })
## dispatch with toolkit
setGeneric(".font<-",function(obj, toolkit,...,value)
           standardGeneric(".font<-"))


## undo
setGeneric("undo",function(obj, ...) standardGeneric("undo"))
## add generic for Containers and sometimes widgets
setMethod("undo",signature(obj="guiWidget"),
          function(obj, ...) {
            toolkit = obj@toolkit
            .undo(obj@widget, toolkit,...)
          })
## dispatch with toolkit
setGeneric(".undo",function(obj,toolkit,...) standardGeneric(".undo"))

## redo
setGeneric("redo",function(obj, ...) standardGeneric("redo"))
## add generic for Containers and sometimes widgets
setMethod("redo",signature(obj="guiWidget"),
          function(obj, ...) {
            toolkit = obj@toolkit
            .redo(obj@widget, toolkit,...)
          })
## dispatch with toolkit
setGeneric(".redo",function(obj,toolkit,...) standardGeneric(".redo"))


## tag
setGeneric("tag",function(obj,i, drop=TRUE, ...) standardGeneric("tag"))
## add generic for Containers and sometimes widgets
setMethod("tag",signature(obj="guiWidget"),
          function(obj,i,drop=TRUE, ...) {
            toolkit = obj@toolkit
            .tag(obj@widget, toolkit,i, drop=drop,...)
          })
## dispatch with toolkit
setGeneric(".tag",function(obj, toolkit,i, drop=TRUE,...) standardGeneric(".tag"))

## tag<-
setGeneric("tag<-",function(obj, i, replace=TRUE, ..., value) standardGeneric("tag<-"))
## add generic for Containers and sometimes widgets
setMethod("tag<-",signature(obj="guiWidget"),
          function(obj, i, replace=TRUE, ..., value) {
            toolkit = obj@toolkit
            .tag(obj@widget, toolkit,i, replace, ...) <- value
            return(obj)
          })
## dispatch with toolkit
setGeneric(".tag<-",function(obj, toolkit,i, replace=TRUE,...,value)
           standardGeneric(".tag<-"))


## id
setGeneric("id",function(obj, ...) standardGeneric("id"))
## add generic for Containers and sometimes widgets
setMethod("id",signature(obj="guiWidget"),
          function(obj, ...) {
            toolkit = obj@toolkit
            .id(obj@widget, toolkit,...)
          })
## dispatch with toolkit
setGeneric(".id",function(obj, toolkit,...) standardGeneric(".id"))

## id<-
setGeneric("id<-",function(obj, ..., value) standardGeneric("id<-"))
## add generic for Containers and sometimes widgets
setMethod("id<-",signature(obj="guiWidget"),
          function(obj, ..., value) {
            toolkit = obj@toolkit
            .id(obj@widget,toolkit,...) <- value
            return(obj)
          })
## dispatch with toolkit
setGeneric(".id<-",function(obj, toolkit,...,value)
           standardGeneric(".id<-"))


## isExtant -- is gwindow extant?
setGeneric("isExtant",function(obj, ...) standardGeneric("isExtant"))
## add generic for Containers and sometimes widgets
setMethod("isExtant",signature(obj="guiWidget"),
          function(obj, ...) {
            toolkit = obj@toolkit
            .isExtant(obj@widget,toolkit, ...)
          })
## dispatch with toolkit
setGeneric(".isExtant",function(obj, toolkit, ...) standardGeneric(".isExtant"))


## toOlkitprovidesgwidgetsdg
## Does thie toolkit provide the widget
## the constructor
toolkitProvidesWidget <- function(
                                  widgetname,
                                  toolkit=guiToolkit()){
  .toolkitProvidesWidget (toolkit, widgetname)
}

## generic for toolkit dispatch
setGeneric( '.toolkitProvidesWidget' ,
           function(toolkit,
                    widgetname)
           standardGeneric( '.toolkitProvidesWidget' ))


## ANY constructor
setMethod(".toolkitProvidesWidget",
          signature(toolkit="ANY"),
          function(toolkit,
                   widgetname) {
            notThere <- list(guiWidgetsToolkitQt=c("ggraphics","ggraphicsnotebook"),
                             guiWidgestToolkitRGtk2=c("gsvg", "ghtml"),
                             guiWidgetsrToolkitJava=c("gsvg", "ghtml", "ggraphics", "ggraphicsnotebook"),
                             guiWidgetsToolkittcltk=c("gsvg", "ghtml", "ggraphics", "ggraphicsnotebook",
                               "gdfnotebook")
                             )

            notThere <- notThere[[class(toolkit)]]
            return(!widgetname %in% notThere)
          })




## handlers
setGeneric("removehandler",function(obj, ID=NULL, ...)
           standardGeneric("removehandler"))
setMethod("removehandler", signature("guiWidget"),
          function(obj, ID=NULL, ...) {
            .removehandler(obj@widget, obj@toolkit, ID, ...)
          })
setGeneric(".removehandler",function(obj, toolkit, ID=NULL, ...)
           standardGeneric(".removehandler"))
## caps
setGeneric("removeHandler",function(obj, ID=NULL, ...)
           standardGeneric("removeHandler"))
setMethod("removeHandler", signature("guiWidget"),
          function(obj, ID=NULL, ...) {
            .removehandler(obj@widget, obj@toolkit, ID, ...)
          })

## block -- use unblock to remove block
setGeneric("blockhandler",function(obj, ID=NULL, ...)
           standardGeneric("blockhandler"))
setMethod("blockhandler", signature("guiWidget"),
          function(obj, ID=NULL, ...) {
            .blockhandler(obj@widget, obj@toolkit, ID, ...)
          })
setGeneric(".blockhandler",function(obj, toolkit, ID=NULL, ...)
           standardGeneric(".blockhandler"))
## caps
setGeneric("blockHandler",function(obj, ID=NULL, ...)
           standardGeneric("blockHandler"))
setMethod("blockHandler", signature("guiWidget"),
          function(obj, ID=NULL, ...) {
            .blockhandler(obj@widget, obj@toolkit, ID, ...)
          })

## unblock
setGeneric("unblockhandler",function(obj, ID=NULL, ...)
           standardGeneric("unblockhandler"))
setMethod("unblockhandler", signature("guiWidget"),
          function(obj, ID=NULL, ...) {
            .unblockhandler(obj@widget, obj@toolkit, ID, ...)
          })
setGeneric(".unblockhandler",function(obj, toolkit, ID=NULL, ...)
           standardGeneric(".unblockhandler"))
## caps
setGeneric("unblockHandler",function(obj, ID=NULL, ...)
           standardGeneric("unblockHandler"))
setMethod("unblockHandler", signature("guiWidget"),
          function(obj, ID=NULL, ...) {
            .unblockhandler(obj@widget, obj@toolkit, ID, ...)
          })


## addhandler is now exported
setGeneric("addhandler",function(obj, signal, handler, action=NULL, ...) standardGeneric("addhandler"))
setMethod("addhandler",signature(obj="guiWidget"),
          function(obj, signal, handler, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandler(obj@widget, toolkit, handler, action, ...)
          })
## dispatch with toolkit
setGeneric(".addhandler",function(obj,  toolkit, signal, handler, action=NULL,...) standardGeneric(".addhandler"))
## caps
setGeneric("addHandler",function(obj, signal, handler, action=NULL, ...) standardGeneric("addHandler"))
setMethod("addHandler",signature(obj="guiWidget"),
          function(obj, signal, handler, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandler(obj@widget, toolkit, signal, handler, action, ...)
          })

           

## addhandlerchanged
setGeneric("addhandlerchanged",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addhandlerchanged"))
setMethod("addhandlerchanged",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlerchanged(obj@widget, toolkit, handler, action, ...)
          })
## dispatch with toolkit
setGeneric(".addhandlerchanged",function(obj, toolkit,...) standardGeneric(".addhandlerchanged"))
## caps
setGeneric("addHandlerChanged",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addHandlerChanged"))
setMethod("addHandlerChanged",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlerchanged(obj@widget, toolkit, handler, action, ...)
          })



## addhandlerkeystroke
setGeneric("addhandlerkeystroke",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addhandlerkeystroke"))
setMethod("addhandlerkeystroke",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlerkeystroke(obj@widget, toolkit,handler, action, ...)
          })
## dispatch with toolkit
setGeneric(".addhandlerkeystroke",function(obj, toolkit,...) standardGeneric(".addhandlerkeystroke"))
#caps
setGeneric("addHandlerKeystroke",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addHandlerKeystroke"))
setMethod("addHandlerKeystroke",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlerkeystroke(obj@widget, toolkit,handler, action, ...)
          })



## addhandlerclicked
setGeneric("addhandlerclicked",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addhandlerclicked"))
setMethod("addhandlerclicked",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlerclicked(obj@widget, toolkit,handler, action, ...)
          })
## dispatch with toolkit
setGeneric(".addhandlerclicked",function(obj, toolkit,...) standardGeneric(".addhandlerclicked"))
## caps
setGeneric("addHandlerClicked",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addHandlerClicked"))
setMethod("addHandlerClicked",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlerclicked(obj@widget, toolkit,handler, action, ...)
          })



## addhandlerdoubleclick
setGeneric("addhandlerdoubleclick",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addhandlerdoubleclick"))
setMethod("addhandlerdoubleclick",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlerdoubleclick(obj@widget,toolkit,handler, action, ...)
          })
## dispatch with toolkit
setGeneric(".addhandlerdoubleclick",function(obj, toolkit,...) standardGeneric(".addhandlerdoubleclick"))
## caps
setGeneric("addHandlerDoubleclick",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addHandlerDoubleclick"))
setMethod("addHandlerDoubleclick",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlerdoubleclick(obj@widget,toolkit,handler, action, ...)
          })



## addhandlerrightclick
setGeneric("addhandlerrightclick",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addhandlerrightclick"))
setMethod("addhandlerrightclick",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlerrightclick(obj@widget,toolkit,handler, action, ...)
          })
## dispatch with toolkit
setGeneric(".addhandlerrightclick",function(obj,toolkit,...) standardGeneric(".addhandlerrightclick"))
## caps
setGeneric("addHandlerRightclick",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addHandlerRightclick"))
setMethod("addHandlerRightclick",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlerrightclick(obj@widget,toolkit,handler, action, ...)
          })


###
## Column clicks
## clicked
setGeneric("addhandlercolumnclicked",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addhandlercolumnclicked"))
setMethod("addhandlercolumnclicked",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlercolumnclicked(obj@widget, toolkit,handler, action, ...)
          })
## dispatch with toolkit
setGeneric(".addhandlercolumnclicked",function(obj, toolkit,...) standardGeneric(".addhandlercolumnclicked"))
## caps
setGeneric("addHandlerColumnClicked",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addHandlerColumnClicked"))
setMethod("addHandlerColumnClicked",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlercolumnclicked(obj@widget, toolkit,handler, action, ...)
          })



## addhandlerCOLUMNdoubleclick
setGeneric("addhandlercolumndoubleclick",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addhandlercolumndoubleclick"))
setMethod("addhandlercolumndoubleclick",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlercolumndoubleclick(obj@widget,toolkit,handler, action, ...)
          })
## dispatch with toolkit
setGeneric(".addhandlercolumndoubleclick",function(obj, toolkit,...) standardGeneric(".addhandlercolumndoubleclick"))
## caps
setGeneric("addHandlerColumnDoubleclick",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addHandlerColumnDoubleclick"))
setMethod("addHandlerColumnDoubleclick",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlercolumndoubleclick(obj@widget,toolkit,handler, action, ...)
          })

## columnrightclick


## addhandlerCOLUMNdoubleclick
setGeneric("addhandlercolumnrightclick",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addhandlercolumnrightclick"))
setMethod("addhandlercolumnrightclick",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlercolumnrightclick(obj@widget,toolkit,handler, action, ...)
          })
## dispatch with toolkit
setGeneric(".addhandlercolumnrightclick",function(obj, toolkit,...) standardGeneric(".addhandlercolumnrightclick"))
## caps
setGeneric("addHandlerColumnRightclick",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addHandlerColumnRightclick"))
setMethod("addHandlerColumnRightclick",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlercolumnrightclick(obj@widget,toolkit,handler, action, ...)
          })



## Selections
setGeneric("addhandlerselect",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addhandlerselect"))
setMethod("addhandlerselect",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlerselect(obj@widget,toolkit,handler, action, ...)
          })
## dispatch with toolkit
setGeneric(".addhandlerselect",function(obj,toolkit,...) standardGeneric(".addhandlerselect"))
## caps
setGeneric("addHandlerSelect",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addHandlerSelect"))
setMethod("addHandlerSelect",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlerselect(obj@widget, toolkit, handler, action, ...)
          })





## addhandlerFocus
setGeneric("addhandlerfocus",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addhandlerfocus"))
setMethod("addhandlerfocus",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlerfocus(obj@widget,toolkit,handler, action, ...)
          })
## dispatch with toolkit
setGeneric(".addhandlerfocus",function(obj,toolkit,...) standardGeneric(".addhandlerfocus"))
## caps
setGeneric("addHandlerFocus",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addHandlerFocus"))
setMethod("addHandlerFocus",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlerfocus(obj@widget,toolkit,handler, action, ...)
          })



## addhandlerblur
setGeneric("addhandlerblur",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addhandlerblur"))
setMethod("addhandlerblur",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlerblur(obj@widget,toolkit,handler, action, ...)
          })
## dispatch with toolkit
setGeneric(".addhandlerblur",function(obj,toolkit,...) standardGeneric(".addhandlerblur"))
## caps
setGeneric("addHandlerBlur",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addHandlerBlur"))
setMethod("addHandlerBlur",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlerblur(obj@widget,toolkit,handler, action, ...)
          })




## addhandlerdestroy
setGeneric("addhandlerdestroy",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addhandlerdestroy"))
setMethod("addhandlerdestroy",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlerdestroy(obj@widget, toolkit,handler, action, ...)
          })
## dispatch with toolkit
setGeneric(".addhandlerdestroy",function(obj,toolkit,...) standardGeneric(".addhandlerdestroy"))
##caps
setGeneric("addHandlerDestroy",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addHandlerDestroy"))
setMethod("addHandlerDestroy",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlerdestroy(obj@widget, toolkit,handler, action, ...)
          })


# addhandlerexpose
setGeneric("addhandlerexpose",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addhandlerexpose"))
setMethod("addhandlerexpose",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlerexpose(obj@widget,toolkit,handler, action, ...)
          })
## dispatch with toolkit
setGeneric(".addhandlerexpose",function(obj, toolkit,...) standardGeneric(".addhandlerexpose"))
## caps
setGeneric("addHandlerExpose",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addHandlerExpose"))
setMethod("addHandlerExpose",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlerexpose(obj@widget,toolkit,handler, action, ...)
          })

# addhandlerunrealize
setGeneric("addhandlerunrealize",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addhandlerunrealize"))
setMethod("addhandlerunrealize",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlerunrealize(obj@widget,toolkit,handler, action, ...)
          })
## dispatch with toolkit
setGeneric(".addhandlerunrealize",function(obj, toolkit,...) standardGeneric(".addhandlerunrealize"))
## caps
setGeneric("addHandlerUnrealize",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addHandlerUnrealize"))
setMethod("addHandlerUnrealize",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlerunrealize(obj@widget,toolkit,handler, action, ...)
          })

## mousemotion
setGeneric("addhandlermousemotion",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addhandlermousemotion"))
setMethod("addhandlermousemotion",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlermousemotion(obj@widget,toolkit,handler, action, ...)
          })
## dispatch with toolkit
setGeneric(".addhandlermousemotion",function(obj, toolkit,...) standardGeneric(".addhandlermousemotion"))
## caps
setGeneric("addHandlerMouseMotion",function(obj, handler=NULL, action=NULL, ...) standardGeneric("addHandlerMouseMotion"))
setMethod("addHandlerMouseMotion",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .addhandlermousemotion(obj@widget,toolkit,handler, action, ...)
          })




# addhandleridle
setGeneric("addhandleridle",function(obj, handler=NULL, action=NULL, interval=1000, ...) standardGeneric("addhandleridle"))
setMethod("addhandleridle",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, interval=1000, ...) {
            toolkit = obj@toolkit
            .addhandleridle(obj@widget, toolkit, handler=handler, action=action, interval=interval, ...)
          })
## dispatch with toolkit
setGeneric(".addhandleridle",function(obj, toolkit,handler=NULL,action=NULL,
                                      interval=1000,...) standardGeneric(".addhandleridle"))
## caps
setGeneric("addHandlerIdle",function(obj, handler=NULL, action=NULL, interval=1000, ...) standardGeneric("addHandlerIdle"))
setMethod("addHandlerIdle",signature(obj="guiWidget"),
          function(obj, handler=NULL, action=NULL, interval=1000, ...) {
            toolkit = obj@toolkit
            .addhandleridle(obj@widget, toolkit, handler=handler, action=action, interval=interval, ...)
          })

## addpopupmenu
setGeneric("addpopupmenu",function(obj, menulist, action=NULL, ...) standardGeneric("addpopupmenu"))
setMethod("addpopupmenu",signature(obj="guiWidget"),
          function(obj, menulist, action=NULL, ...) {
            toolkit = obj@toolkit
            .addpopupmenu(obj@widget, toolkit,menulist, action, ...)
          })
## dispatch with toolkit
setGeneric(".addpopupmenu",function(obj, toolkit, menulist, action=NULL, ...) standardGeneric(".addpopupmenu"))
## caps
setGeneric("addPopupmenu",function(obj, menulist, action=NULL, ...) standardGeneric("addPopupmenu"))
setMethod("addPopupmenu",signature(obj="guiWidget"),
          function(obj, menulist, action=NULL, ...) {
            toolkit = obj@toolkit
            .addpopupmenu(obj@widget, toolkit,menulist, action, ...)
          })

## add3rdmousepopupmenu
setGeneric("add3rdmousepopupmenu",function(obj, menulist, action=NULL,  ...) standardGeneric("add3rdmousepopupmenu"))
setMethod("add3rdmousepopupmenu",signature(obj="guiWidget"),
          function(obj, menulist, action=NULL,  ...) {
            .add3rdmousepopupmenu(obj@widget, obj@toolkit, menulist,
                                  action=action,  ...)
          })
## dispatch with toolkit
setGeneric(".add3rdmousepopupmenu",function(obj, toolkit,menulist, action=NULL, ...) standardGeneric(".add3rdmousepopupmenu"))
## caps
setGeneric("add3rdMousePopupmenu",function(obj, menulist, action=NULL,  ...) standardGeneric("add3rdMousePopupmenu"))
setMethod("add3rdMousePopupmenu",signature(obj="guiWidget"),
          function(obj, menulist, action=NULL,  ...) {
            .add3rdmousepopupmenu(obj@widget, obj@toolkit, menulist,
                                  action=action,  ...)
          })

## adddropsource
setGeneric("adddropsource",function(obj, targetType="text",
                                    handler=NULL, action=NULL, ...) standardGeneric("adddropsource"))
setMethod("adddropsource",signature(obj="guiWidget"),
          function(obj, targetType="text", handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .adddropsource(obj@widget, toolkit,targetType=targetType,
                           handler=handler, action=action, ...)
          })
## dispatch with toolkit
setGeneric(".adddropsource",function(obj, toolkit,targetType="text", handler=NULL, action=NULL, ...) standardGeneric(".adddropsource"))
## caps
setGeneric("addDropSource",function(obj, targetType="text",
                                    handler=NULL, action=NULL, ...) standardGeneric("addDropSource"))
setMethod("addDropSource",signature(obj="guiWidget"),
          function(obj, targetType="text", handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .adddropsource(obj@widget, toolkit,targetType=targetType,
                           handler=handler, action=action, ...)
          })


## adddropmotion
setGeneric("adddropmotion",function(obj, handler=NULL, action=NULL, ...)
           standardGeneric("adddropmotion"))
setMethod("adddropmotion",signature(obj="guiWidget"),
          function(obj,  handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .adddropmotion(obj@widget, toolkit,
                           handler=handler, action=action, ...)
          })
## dispatch with toolkit
setGeneric(".adddropmotion",function(obj, toolkit, handler=NULL, action=NULL, ...) standardGeneric(".adddropmotion"))
## caps
setGeneric("addDropMotion",function(obj, handler=NULL, action=NULL, ...)
           standardGeneric("addDropMotion"))

setMethod("addDropMotion",signature(obj="guiWidget"),
          function(obj,  handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .adddropmotion(obj@widget, toolkit,
                           handler=handler, action=action, ...)
          })

## adddroptarget
setGeneric("adddroptarget",function(obj, targetType="text",
                                    handler=NULL, action=NULL, ...) standardGeneric("adddroptarget"))
setMethod("adddroptarget",signature(obj="guiWidget"),
          function(obj, targetType="text", handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .adddroptarget(obj@widget, toolkit,targetType=targetType,
                           handler=handler, action=action, ...)
          })
## dispatch with toolkit
setGeneric(".adddroptarget",function(obj, toolkit,targetType="text", handler=NULL, action=NULL, ...) standardGeneric(".adddroptarget"))
## caps
setGeneric("addDropTarget",function(obj, targetType="text",
                                    handler=NULL, action=NULL, ...) standardGeneric("addDropTarget"))
setMethod("addDropTarget",signature(obj="guiWidget"),
          function(obj, targetType="text", handler=NULL, action=NULL, ...) {
            toolkit = obj@toolkit
            .adddroptarget(obj@widget, toolkit,targetType=targetType,
                           handler=handler, action=action, ...)
          })

##################################################


## dialogs

## galert
## like gmessage, only for transient messages -- not modal
galert = function(
  message,
  title = "message",
  delay = 3,
  parent = NULL, 
  ..., toolkit=guiToolkit()) {
  .galert(toolkit,message, title, 
            ...)
}
setGeneric(".galert",
           function(toolkit,
                    message, title="message", delay=3, parent=NULL, ...)
           standardGeneric(".galert"))


## gmessage
gmessage = function(
  message,
  title = "message",
  icon = c("info", "warning", "error", "question"),
  parent=NULL,
  handler = NULL, action = NULL,
  ..., toolkit=guiToolkit()) {
  .gmessage(toolkit,message, title, icon, parent, handler, action,
            ...)
}
setGeneric(".gmessage",
           function(toolkit,
                    message=message, title=title, icon=icon,
                    parent = parent,
                    handler=handler, action=action, ...)
           standardGeneric(".gmessage"))



## ginput
ginput = function(
  message,text="",
  title = "Input",
  icon = c("info", "warning", "error", "question"),
  parent = NULL,
  handler = NULL, action = NULL,
  ..., toolkit=guiToolkit()) {
  .ginput(toolkit,
          message, text=text, title=title, icon, parent, handler, action, 
          ...)
}
setGeneric(".ginput",
           function(toolkit,
                    message=message, text=text, title=title, icon=icon,
                    parent = parent,
                    handler=handler, action=action, ...)
           standardGeneric(".ginput"))


## gconfirm
gconfirm = function(
  message,
  title = "Confirm",
  icon = c("info", "warning", "error", "question"),
  parent=NULL,
  handler = NULL, action = NULL,
  ..., toolkit=guiToolkit()) {
  .gconfirm(toolkit,message=message, icon=icon, parent=parent, handler=handler, action=action,
            ...)
}
setGeneric(".gconfirm",
           function(toolkit,
                    message=message, title=title, icon=icon,
                    parent = parent,
                    handler=handler, action=action, ...)
           standardGeneric(".gconfirm"))

## gbasicdialog
## when no widget we dispath different
gbasicdialog = function(
  title = "Dialog", widget,
  parent = NULL,
  handler = NULL, action = NULL,
  ..., toolkit=guiToolkit()) {
  if(missing(widget)) {
    obj <- .gbasicdialognoparent(toolkit, title, parent, handler, action,...)
    obj = new( 'guiDialog',widget=obj,toolkit=toolkit) 
  } else {
    obj <- .gbasicdialog(toolkit,
                  title=title, widget=widget,parent=parent,
                  handler=handler, action=action,
                         ...)
  }
  return(obj)
}
setGeneric(".gbasicdialog",
           function(toolkit,
                    title = "Dialog", widget, parent,
                    handler = NULL, action = NULL,
                    ...)
           standardGeneric(".gbasicdialog"))

setGeneric(".gbasicdialognoparent",
           function(toolkit,
                    title = "Dialog",  parent,
                    handler = NULL, action = NULL,
                    ...)
           standardGeneric(".gbasicdialognoparent"))



##################################################
##
## Usual R methods made into methods for dispatch
## update
setGeneric("update")
setMethod("update",signature(object="guiWidget"),
          function(object, ...) {
            .update(object@widget, object@toolkit, ...)
          })
setGeneric(".update",function(object, toolkit,  ...)
           standardGeneric(".update"))

## length
##setGeneric("length")
setMethod("length",signature(x="guiWidget"),
          function(x) {
            .length(x@widget, x@toolkit)
          })
setGeneric(".length",function(x, toolkit)
           standardGeneric(".length"))

## dim
#setGeneric("dim")
setMethod("dim",signature(x="guiWidget"),
          function(x) {
            .dim(x@widget, x@toolkit)
          })
setGeneric(".dim",function(x, toolkit)
           standardGeneric(".dim"))
## dimnames
#setGeneric("dimnames")
setMethod("dimnames",signature(x="guiWidget"),
          function(x) {
            .dimnames(x@widget, x@toolkit)
          })
setGeneric(".dimnames",function(x, toolkit)
           standardGeneric(".dimnames"))
## dimnames<-
#setGeneric("dimnames<-")
setReplaceMethod("dimnames",signature(x="guiWidget"),
                 function(x,value) {
                   .dimnames(x@widget, x@toolkit) <- value
                   return(x)
                 })
setGeneric(".dimnames<-",function(x, toolkit, value) {
  standardGeneric(".dimnames<-")
})
## names
## as of 2.5.0 this became primiive
if(as.numeric(R.Version()$major) <= 2 &
   as.numeric(R.Version()$minor) <= 4.1) {
  setGeneric("names")
  setGeneric("names<-")
}


setMethod("names",signature(x="guiWidget"),
          function(x) {
            .names(x@widget, x@toolkit)
          })
setGeneric(".names",function(x, toolkit)
           standardGeneric(".names"))
## names<-
setReplaceMethod("names",signature(x="guiWidget"),
                 function(x,value) {
                   .names(x@widget, x@toolkit) <- value
                   return(x)
                 })
setGeneric(".names<-",function(x, toolkit, value) {
  standardGeneric(".names<-")
})

## getWidget to return toolkit widget
setGeneric("getToolkitWidget",function(obj) standardGeneric("getToolkitWidget"))
setMethod("getToolkitWidget",signature(obj="guiWidget"),
          function(obj) {
            .getToolkitWidget(obj@widget, obj@toolkit)
          })
## dispatch with toolkit
setGeneric(".getToolkitWidget",function(obj, toolkit)
           standardGeneric(".getToolkitWidget"))


## ####
## put into RGtk2 only
## ## S3 class for coercing to gWidget
## as.gWidget <- function(obj,...) UseMethod("as.gWidget")
## as.gWidget.default <- function(obj,...) {
##   print(sprintf("No coercion to gWidget available for object of class %s",class(obj)))
##   return(obj)
## }
