\name{gWidgetsWWW-package}
\alias{gWidgetsWWW-package}
\alias{gWidgetsWWW}
\docType{package}
\title{
Toolkit implementation of gWidgets for WWW
}
\description{
Port of gWidgets API to WWW using RApache and the ExtJS
javascript libraries.
}
\details{
  
  This package ports the gWidgets API to WWW, allowing the authoring of
  interactive webpages within R commands.
  \cr
  
  The webpages can be seen locally through a local server or viewed
  through the internet:The local version is installed with the package
  and uses R's internal web server for serving help pages. 
  The server version requires the \pkg{RApache} module for Apache to be
  installed and configured. (More later.)
  \cr
  
 The webpages themselves use the excellent Extjs toolkit (\url{www.sencha.com}) to
 render the widgets. This toolkit is installed with the package.
 \cr
 
  The \pkg{gWidgets} API is meant to be multi-toolkit. For its other
  implementations (\pkg{gWidgetsRGtk2}, \pkg{gWidgetsQt},
  \pkg{gWidgetstcltk}) the main methods are defined in the
  \pkg{gWidgets} package, with toolkit packages providing the link
  between the graphical widget library and R. The \pkg{gWidgetsWWW} is
  different, it is a more-or-less faithful implementation of the API,
  but this is not enforced by using the \pkg{gWidgets} pacakge. This was
  chosen, as there are some significant differences between an
  interactive desktop GUI and a webpage.
  \cr
  

  As the differences are not great, this file documents differences from
  the API. We defer to the \pkg{gWidgets} documentation for more
  detail. (However, this package is not compatible with the
  \pkg{gWidgets} package so reading that documentation is best done with
  a separate R process.)
  \cr
  
  \bold{Overview:}\cr

  The \pkg{gWidgetsWWW} package at one level maps \R\/ commands to
  javascript commands. Basically it just pastes together a bunch of
  javascript commands to send to the browser. A more sophisticated
  treatment (like \code{pyjamas} for python) would compile the R code
  into javascript, but that does not happen here. This package is a
  simple mapping.
  \cr

  The communication between \R\/ and the browser works as follows. Before
  a page is rendered, it can be constructed using the gWidget
  constructors and methods. When the top-level window is shown (via its
  \code{print} method), the javascript code to produce the page is
  created. This javascript code takes advantage of the freely available
  Ext libraries which greatly enhance the base javascript language.
  This code should be contained in or sourced into a web page so that it
  can be rendered in a browser. The \pkg{gWidgetsWWW} package provides
  several examples of how this works.

 
  The interactivity of a GUI is created through httpxmlrequests back to
  the RApache server. Each page load creates a unique session to store
  the state. This allows data to be passed from the web page back to an
  R process and back without needed to reload an entire page. In fact,
  reloading a page creates a new session. What is missing is a way to initiate a request from \R\/
  to the web page and then back to \R. As such, the state of the web
  page is constantly passed back to the \R\/ session. That is, when a
  control is updated, such as when a radio button is clicked, there is a
  call back into \R\/ to set that value. This allows handlers in R to be
  aware of the state of the web page. On the downside, this may cause a
  lag in performance.

  


  \bold{Installation:}\cr
  
  To run the local version of \pkg{gWidgetsWWW} no installation is
  necessary beyond installing the package. The command
  \code{localServerStart} can be used to load a script.
  \cr


  Installation for the server requires several steps: a) installing RApache, b)
  installing gWidgetsWWW c) configuring RApache for gWidgetsWWW, and
  d) creating some webpages
  \cr
  
  Installing RApache is documented at the RApache website. Installing
  gWidgetsWWW is straightforward.
  \cr

  To configure RApache for gWidgetsWWW, a sample configuration is
  included in the templates directory. There are several variables to
  configure. The defaults work with a recent version of Ubuntu, so it
  can be as easy as simply copying the file into the appropriate place
  for the web server to read it.
  \cr
  
  The \code{Ext} libraries are included in the \code{basehtml/ext}
  directory. Ext has a dual-license model, offering an open
  source LGPL license along with a per-developer, royalty-free
  commercial license. The standard configuration reads these files from
  where the R package is installed. (This may need adjustment), so no
  installation is necessary.
  \cr

  To complete the installation, the \pkg{gWidgetsWWW} package provides
  some stock icons in the \code{basehtml/images} directory.. As with the
  extjs libraries, these can be installed whereever, but the default
  configuration finds them where R installs the package.
  The author would very much appreciate that a coherent set of icons be contributed.
  \cr

  Web pages can be made in two ways. Once can use a brew template and
  call back into R to generate the page. However, it is much easier to
  write a standalone R script and call that.
  \cr

  For local installs, the function \code{localServerOpen} is used to
  open a file. For server installs, the script need only be put into the
  proper directory. (This is \code{/var/www/gWidgetsWWW} in the default
  configuration.) The script is loaded by calling the proper URL, which
  takes the form: \code{servername/gWidgetsWWWrun/scriptname}, where
  \code{servername} is the \code{http:///www.yourhost.com} bit,
  \code{gWidgetsWWWrun} is the \emph{default} name for the invocation
  and \code{scriptname} the file in the directory (or directories)
  specified to search through.
  \cr

  The scripts themselves have a pretty standard structure: there must be
  a global \code{gwindow} instance (corresponding to the main web page,
  or which there can only be on), \pkg{gWidgetsWWW} commands for layout,
  and finally a call to the \code{visible<-} method of \code{gwindow} to
  print out the javascript commands generated by the package. The
  Examples directory has many examples. (Try
  \code{localServerOpen("Examples/ex-index.R", pacakge="gWidgetsWWW")}
  to see.)


  
  \bold{Sessions:}

  \pkg{gWidgetsWWW} uses sessions to store data when a web page is
  loaded. Sessions are simply R environments. HTML itself is stateless,
  so some means is necessary to keep track of data between
  interactions. The use of AJAX allows pieces of a page to be updated
  without the entire page, making pages appear much more
  responsive. This involves callbacks into a R process that are executed
  within the environment stored for the session.  \cr
  
  The environments do not remember which packages are loaded, so if your
  callback requires a package beyond the base and \pkg{gWidgetsWWW} you
  will need to include the appropriate \code{require} command in the
  callback.  \cr
  
  Sometimes the database driver will complain. Likely this is due to the
  database not being locked while a transaction is being completed.  \cr
  
  The sessions are stored by default in files under a directory using
  filehash. This has the advantage of being easy to remove using regular
  cron jobs, but may be slower than other forms of storage.  \cr
  
  This is all a bit of a hack, so any report of bugs is appreciated.
  \cr

  

\bold{Containers:}
  \cr
  We now describe implementation of the basic widgets in
  \pkg{gWidgetsWWW} beginning with the containers.
  \cr

  A primary difference in \pkg{gWidgetsWWW} over \pkg{gWidgets} for
  other toolkits is the restriction to only one top-level window, which
  maps to the webpage.  Subwindows are possible though. These are
  constructed with the same constructor, but one passes in the top-level
  window to the \code{parent} argument. The window object is a bit
  different than other \code{gWidgets} implementations, as the object
  is not rendered until the web page is ready to display. This rendering
  is done through the \code{visible<-} method. (In particular, the
  constructor's \code{visible} argument is ignored.) After rendering,
  new widgets can be added but it doesn't happen by the same process and
  may not work as desired. To be on the safe side, you can add child
  widgets and make them not visible to begin.  The default handler is
  assigned to the \code{unload} signal which is called when a page is
  left. One thing to remember, is that web pages in \pkg{gWidgetsWWW}
  will lose state when reloaded. For windows, only the \code{svalue<-}
  method, for adjusting the title, is defined.  \cr \cr

  There are a few \pkg{proto} properties to adjust the page. The
  property \code{doLoadingText} can be set to \code{FALSE} to suppress
  the "loading" message whenever a handler is being called.
  \cr

  The default means of each window being a webpage can be overridden
  by specifying a value of \code{..renderTo}, as in \code{w =
  gwindow(); w$..renderTo = "someId"} for some HTML object with
  appropriate id.
  \cr

  The \code{ggroup} container accepts the \code{horizontal} and
  \code{spacing} arguments, but does not use the \code{use.scrollwindow}
  argument (spacing can not be done after rendering with the
  \code{svalue<-} method though). When adding a child component (which
  is done when a \code{ggroup} instance is used as a parent container)
  the \code{expand} argument is not used, nor is the \code{anchor}
  argument of \pkg{gWidgets}. These are useful for alignment.
  Additionally, the \code{addSpring} and \code{addSpace} methods are not
  defined.   
  \cr

  Nested horizontal groups will expand to fill all the space to the
  right. To stop this behaviour -- which is desired as you can't
  display other widgets -- set the width of the container via the
  \code{size<-} method.


  The \code{gframe} container simply adds a title to a \code{ggroup}
  instance. This can be adjusted with the \code{svalue<-} method.
  \cr
  
  The \code{gexpandgroup} container extends the \code{gframe} container
  by making the title clickable. When clicked the container will toggle
  between a hidden and visible state. When hidden only the title is
  visible. This can be done programatically through the
  \code{visible<-} method.
  \cr
  
  The \code{glayout} container lays out its children in a grid. There is
  no ability to adjust how a child is layed out in the parent cell
  through the \code{anchor} arugment. When adding a child to this
  container one specifies the location using the \code{[} method and
  specifies the container to the constructor as in this example:
  \code{f <- glayout(cont = g); f[1,1] <- (b <- gbutton("hi", cont=f))}
  \cr

  
  The \code{gnotebook} container does not implement all the features
  expected. The  \code{tab.pos} argument only accepts values of 1 and 3
  for top and bottom placement of tabs. The \code{close.buttons}
  argument is not yet implemented. Adding child components is done by
  specifying the notebook object to the constructor's \code{container}
  argument. When done, the extra argument \code{label} is used to
  specify the tab label. The \code{svalue<-} method can be used to set
  the tab by index, the \code{names} and \code{names<-} method set the
  tab names. The \code{dispose} method deletes the current tab.
    \cr
  
  The \code{gformlayout} widget, used to layout widgets using
  \code{glayout}, is imlemented.

  \bold{Widgets:}\cr

  The \code{glabel} widget  has no handler, and is not editable. HTML
  markup is allowed, but may not work if not escaped. The
  \code{font<-} method can be used to adjust font properties. This
  method takes a vector of values with corresponding names being the CSS
  property.



  The \code{gbutton} widget: Can accept a \code{gaction} object to
  define the click handler. Pass in the action to the \code{action}
  component and \code{NULL} for the \code{handler} argument. 
  \cr


  The \code{ghtml} widget shows urls in the window. The url should point
  to a basic web page or be HTML formatted text. Some browsers will
  process this text before it gets to R, so it may be best to source in
  the file that contains HTML formatted text.
  \cr


  The \code{gcheckbox} widget has no method to change the label (\code{[<-},
  \code{names<-})
  \cr


  The \code{gcheckboxgroup} widget has no \code{[<-} method.
  \cr


  The \code{gradio} widget has no \code{[<-} method.
  \cr

  The \code{gslider} widget has a tooltip that indicates the value of
  the widget. This tooltip can be customized. Pass a string, such as
  \code{"<b>{0} percent complete</b>"} to the constructor. (The 0 in braces
  is substituted into the HTML fragment.) The \code{[<-} method to set
  the values to choose from is not implemented.
  \cr


  The \code{gspinbutton} widget is implemented but the trigger icons may
  not appear. The widget is responsive to arrow keys to cycle through
  the specified values. The \code{[<-} method to set the values to
  choose from is not implemented.  \cr


  The \code{gedit} widget: The transport occurs on the "blur" signal
  -- when the focus leaves the widget basically. The \code{gedit}
  widget does not have the \code{[<-} method. However, if this is
  desired (for type ahead) the \code{gcombobox} widget can be used. By
  default, this widget has a "trigger" icon indicating to the user
  that it looks like a combobox, but this will not be drawn if the
  property \code{..hideTrigger} is set to \code{TRUE}, as in \code{obj
  = gcombobox(...); obj$..hideTrigger <- TRUE}. In a similar way, one
  can specify "typeAhead" by setting \code{obj$..typeAhead <-
  TRUE}. The handler \code{addHandlerKeystroke} returns the numeric
  value of the key that is pressed in the \code{key} component of the
  first argument, \code{h}. This is supposed to be a UNICODE value. If
  one adds the arguments "key='e.ENTER'" (say ENTER could be
  BACKSPACE, ..) or "charCode=65", where 65 is "A" or "a" then a check
  is made at the browser level before calling back into R.  \cr


  

  The \code{gtext} widget: The transport occurs on the "blur" signal --
  when the focus leaves the widget basically.
  \cr


  The \code{gimage} widget \code{filename} and \code{dirname} referring
  to url's not to local files. There is no \code{handler} defined for
  this widget.
  \cr


  The \code{gcombobox} widget has an \code{items} argument which can be
  a data vector, or a data frame with one, two, three, or perhaps more
  columns. This should have two or more values. If a data vector or
  one-column data frame, then it should be a vector of values. If a two
  or more column data frame, then the first column will be the value,
  the second column (if present) will be used to generate an image. If
  the values are identified as URL, or have that class the image will be
  downloaded, otherwise it will be assumed to be a character string
  identifying a stock icon. Finally, the third column (if present) will
  be used for a tooltip. For the real ambitious, the \code{$tpl()}
  method of the widget can be used to markup the combobox text and
  (perhaps) include other columns of the \code{items} argument. The
  template should use the names "value","text","iconurl" and "qtip" for
  the first four columns, where the "text" column is the "value" column
  repeated.\cr


  The \code{gcalendar} widget formatting may be unrealiable. The
  conversion from Ext to R format for dates may be locale dependent and
  this hasn't been investigated.
  \cr

  
  The \code{ggraphics} widget is a link to the \pkg{gWidgetsWWW}
  \code{gcanvas} widget. This is used slightly differently than an
  interactive graphics device -- how \code{ggraphics} is meant to be
  used, but is similar to how a non-interactive device is used. The
  \pkg{canvas} grahics device allows graphics to be written using
  JavaScript, so they render natively in a canvas widget in a
  browser. Not all browsers implement this relatively modern widget.
  \cr
  
  The \code{gsvg} widget creates a space in a page to use the		
  \pkg{RSVGTipsDevice} package to display SVG graphics within a web page.
  \cr

  The \code{gfilebrowse} widget is written only for local
  installs. The handler does not work. The basic idea is that gfile is
  like an edit box with a button (like gfilebrowse in gWidgets) and a
  \code{svalue} method.  However, the behaviour is limited. No path
  information is included. Even more, on Chrome that path has an extra
  windows-specific bit added called "fakepath". In short, this widget
  needs work.\cr


  The \code{gtable} widget: when using \code{[<-} to replace values,
  indices don't work and when replacing data frame the replacement must
  have same number of columns, each with the same type; has no
  \code{names<-} method to set column names. Only \code{addHandlerClicked} and
  \code{addHandlerDoubleclick} are working. 

  The filtering feature (specified through \code{filter.FUN} say) is
  not implemented. However, a proto method \code{filter} is available
  to filter by a column name and regular expression after the widget
  is shown. Call as in: \code{tbl=gtable(mtcars, ...);
  tbl\$filter("cyl","6")}. The \code{ex-gtable} example has a sample usage.

  The \code{icon.FUN} should return either url's or stock icon names.
  \cr

  For tables the \code{size<-} method can be, as usual, a vector of
  width and height values (in pixels). However, it ma also be a list
  with components \code{width}, \code{height} and
  \code{columnWidths}. The latter allows one to specify the widths of
  each column in pixels. Otherwise these are intuited from the maxmim
  number of characters in each column at time of construction.

  The formatting of the columns is controlled by a few javascript
  functions. For example the formatting of numeric variables by the
  javascript function \code{gtableNumeric}. This can be overridden. For
  example if \code{tbl} is a table widget, then before the GUI is drawn,
  defing a new function in the \code{..scripts} method can override the default.


% \code{tbl$..scripts <- function(.) \{}\cr
% \code{  paste("gtableNumeric = function(val) \{ ",}\cr
% \code{        "return '<span  style=\"text-align:right; color:blue\"><h1>' + val + '</h1></span>';",}\cr
% \code{        "\}",}\cr
% \code{        sep="\n")}\cr
% \code{\}}\cr
   




  The \code{gbigtable} widget allows very large tables to be displayed,
  at it only shows part of the table at once. The number of lines
  displayed is specified by the \code{pageSize} argument, with default
  of 25.


  The \code{gdf} widget is implemented but limited. The widget
  recognizes certain data types and provides different means to edit the
  values. However, there is no capacity to add rows or columns. (One
  could start with more rows and columns than needed and then trim.) As
  the column types come from the \code{items} argument, this must be
  provided. The widget does not gracefully handle \code{NA} values
  (\code{items[complete.cases(items),]} is used). This can be worked
  around, but isn't automated. For numeric values \code{NaN} is
  recongized. For character, logical or factors vectors, one could use
  \code{"NA"} or \code{""}.  The widget does not show the data frames
  row names. One could add these to \code{items} first as a character
  vector to edit. and then take them off again. \cr

  The \code{visible<-} method is use to restrict the rows that are shown
  by a specification of a logical vector. In addition, a proto method
  \code{filter} is available to filter by a column name and regular
  expression after the widget is shown. The \code{ex-gtable} example has
  a sample usage. The \code{visible<-} method uses \code{filter} in an
  inefficient manner, so using filter is suggested for large data
  sets. (Although it is not portable across gWidgets toolkits).\cr


  The \code{gtree} widget works, but only for single columns. (No grid).
  \cr


  The \code{gaction} widget creates "actions" which can be used in
  menubars and attached to buttons. Unlike the \code{gaction}
  constructor of other \pkg{gWidgets} implementations, here one must
  include a \code{parent} argument. The methods \code{svalue<-} and
  \code{enabled<-} can be used to set the label and the en/dis able the
  items. 
  \cr
  
  The \code{gmenu} widget has no methods to replace or access the
  elements of the menu. However, the leaves of the menu items should be
  \code{gaction} elements, rather than lists as with the other gwidgets
  implementations. This allows one to call \code{enabled<-} to disable,
  and \code{svalue} to change the text.
  The icons are not working.
  \cr

  At this time, support for popup menus via \code{add3rdMousePopupMenu}
  and \code{addPopupMenu} is not implemented.
  \cr


  The \code{gtoolbar} widget is subsumed in the \code{gmenu} widget. The
  \code{gaction} constructor can be used to create menubar elements.
  \cr


  The \code{gstatusbar} widget is slightly different from the
  \pkg{gWidgets} API, where there is a stack of status messages. The
  intial text stays until overridden via an svalue call. Subsequent
  svalue calls are transient, lasting for about 10 seconds.  \cr

  The \code{gseparator} widget only works for horizontal lines, and
  simply uses the \code{HR} tag within the \code{ghtml} widget.


  The \code{\link{ggooglemaps}} widget allows one to integrate google
  maps into an application. It is not working. This constructor is
  \pkg{gWidgetsWWW} specific and is documented in its own help file.
  
  \bold{Dialogs:}\cr  

  
  The \code{galert(title = "", message = "", delay=3, parent=NULL)}
  dialog is specific to \pkg{gWidgetsWWW}. It creates a quick alert
  that pops down from the top of the web page. The \code{parent}
  should be the top-level window object. Otherwise, this constructor
  has a \code{title} and \code{message} argument for the message and
  \code{delay} in seconds to specify the time the dialog appears.  \cr
  
  The other dialogs, as well, have a \code{parent} argument that must be
  specified. This allows handlers, etc. to be registered. Visually, the
  dialogs will appear to "pop-up" from the component specified.
  \cr
  
  The \code{gmessage} dialog produces a modal dialog similar to the
  javascript \code{alert} dialog.
  \cr

  The \code{gconfirm} widget works. The handler is called on the
  "ok" selection.
  \cr

  The \code{ginput} works. The handler is called on okay. The value of
  the input component contains the text typed into the box.  \cr

  As an alternative to \code{gconfirm} and \code{ginput} a subwindow can
  be used. These are created by the \code{gwindow} constructor when the
  \code{parent} argument is specified. (The dialogs are modal, the
  subwindow is not, although it sits on top of the browser window.)


  \bold{icons:}\cr

  
  The function \code{getStockIcons} returns a vector of these icons. The
  \code{names} of this vector may be used to define icons in some of the
  widgets. The return values are url's of the stock icons.
  
  The function \code{addStockIcons} can be used to add to this list. These
  should be added \emph{before} the GUI is rendered so that they will be
  available to the GUI's components.
  


  
  \bold{Methods:}\cr


  The \code{svalue} method should work as with \pkg{gWidgets}.
  \cr

  
  The \code{svalue<-} method should work as with \pkg{gWidgets}.
  \cr

  
  The \code{add} method for containers (implicitly called by
  constructors) is implemented (but may behave differently) for
  containers after they are rendered.
  \cr

  
  The \code{enabled<-} method is used to disable/enable a widget for
  user input.
  \cr

  
  The \code{delete} and \code{dispose<-} methods only hide the widget, rather than
  destroy the widget. 
  \cr

  
  The \code{focus<-} method should work as expected to set the focus
  to the widget.
  \cr

  
  The \code{visible<-} method should unhide a child widget that was
  hidden via \code{dispose<-}. There is no \code{visible=FALSE} state
  for widgets when the GUI is initially drawn.
  \cr

  
  The \code{[} method method for many constructors is not fully implemented.
  \cr

  The \code{[<-} method method likely won't use the i,j indexing arguments
  \cr

  The \code{names<-} method method is only partially implemented.
  \cr

  The \code{size<-} method can be called with either \code{c(width)}
  or \code{c(width, height)}. The default sizing for many widgets is
  "auto", which will fill the width of the page.  This will make
  \code{horizontal=TRUE} not behave as expected.
  \cr
  
  The \code{print} method for windows (gwindow) will print out the
  javascript to produce the page. This is how a page gets rendered to
  the web browser, but can also help in debugging (somewhat).
  \cr
  
  The \code{tooltip<-} method will set a tooltip for the widget. The
  value may be a string; a character vector; a URL which returns a
  string (URLs are
  identified by the function \code{isURL}); or a list with components
  \code{title} and \code{message}, where the title component is a string
  and the message component a character vector.  \cr

  \bold{Handlers:}\cr

  The interactivity between the web browser GUI and R is provided by
  handlers which respond to GUI signals sent when an event occurs. For
  instance, when the mouse clicks on a button, a mousedown signal is
  sent. The \code{gWidgetsWWW} code allows \R\/ functions to be used as
  handlers. These functions create JavaScript code to update the the web
  browser. The \pkg{gWidgetsWWW} package takes the methods of
  \pkg{gWidgetsWWW} and produces the JavaScript code, so the \R\/
  programmer need only know how to program in \R.
  
  Handlers in \pkg{gWidgetsWWW} are slightly different than in other
  implementations of gWidgets. As with others, the first argument to the
  handler, traditionally \code{h}, is a list with components \code{obj},
  referring to the widget; \code{action}, referring to the value passed
  into the \code{action} argument; and occasionally others.

  Handlers are added to the widget through the \code{handler} argument
  of the constructor, or using one of the \code{addHandlerXXX}
  methods. The constructor assigns a default signal (or more) to bind
  the handler to. More control is given by the \code{addHandlerXXX}
  methods. If you want to bind to a different signal (after having read
  the Ext documentation say) the method \code{addHandler} allows the
  specification of a signal.

  Handlers may not evaluate within the scope of the function, as one
  would expect. This should be addressed. If not fixed, then one can use
  global variables within the handler.



  \bold{Drag and Drop:}\cr

  The \code{Ext} library has some built in drag and drop
  support. Otherwise, drag and drop is not currently supported.
  
  \bold{Compound widgets:}\cr

  The \code{gcommandline} widget is implemented to run locally only. It
  provides a simple notebook like interface. The implementation does not
  follow the API of \pkg{gWidgets}, the only arguments are the
  container, a \code{width} and a \code{graphic_size} (to specify the
  size for a gsvg widget).


  At this time the constructors of the compound widgets
  \code{gdfnotebook}, \code{gvarbrowser}, \code{ggraphicsnotebook},
  \code{ggenericwidget} are not implemented.
  
  \bold{proto:}\cr

  %% proto
  The \pkg{proto} package is used as the backend for this package, rather
  than the \pkg{gWidgets} package. This was done as much for variety than
  any other reason. The user of \pkg{gWidgetsWWW} need not be aware of
  this, but it does allow some undocumented access to the underlying widgets.
  
  A few examples. First, many of the widgets have properties that are
  set during the construction of the widget, but are not specified by
  the arugments to the constructor. This is due to the EXT libraries
  having more options, than are given by \pkg{gWidgets}. For instance,
  in the \code{gcombobox} function the properties \code{..hideTrigger}
  and \code{..emptyText} can be set. The \code{..hideTrigger} values is
  a logical value, and it \code{TRUE} will render the widget without an
  arrow indicating to the user this is a combobox. This can be useful
  for setting values that will appear as the user types. In the API for
  \code{gWidgets} the \code{[<-} method for \code{gedit} should cause
  something similar to happen, but it doesn't. The \code{..emptyText}
  argument is there to set the initial text of the box in gray to
  suggest to the user what to do.

  The double dots are reserved for properties of methods of the widget
  that are instance specific, hence the funny names.
  
  Another way to modify a widget is to set the method
  \code{..ExtCfgOptions}. This should be a function which returns a
  list. The named components of the list map to the properties of the
  EXT constructor. (See that API for things you may want). These values
  will override any others set by \pkg{gWidgetsWWW}. The conversion will
  quote values of type \code{character}. If this is undesired, use the
  class \code{String} returned by the \code{\link{String}} function.

  
  The above two examples set properties of the object. These only effect
  the rendering if  set prior to rendering. To affect the widget after
  rendering, one can create JavaScript to be sent back to the
  browser. This can only occur during a handler. Most of the
  \code{gWidgetsWWW} methods eventually call a function ending in
  \code{JS}. For instance, \code{svalue<-} calls the proto method
  \code{setValue} which in turn calls \code{setValueJS}. The latter can
  be overridden, or can be added to by setting a method for
  \code{..setValueJS}. 

  If you do this, and think your code should be integrated into the main
  code, please share it.
}
\author{ John Verzani <jverzani@gmail.com> }

% \references{}
\keyword{ package }
\seealso{gWidgets}
