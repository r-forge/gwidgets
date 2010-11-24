w <- gwindow("gWidgetsWWW")
g <- ggroup(cont = w, horizontal=FALSE)
ghtml("<h1>gWidgetsWWW</h1>", cont = g)
f <- gexpandgroup("About", cont = g)
con <- textConnection("The gWidgets package provides an API to abstract the interface to a
few of the available GUI toolkits avaiilable through R. The
gWidgetsWWW package provides an implementation of the gWidgets
API for use through a web browser. That is, using just R commands interactive
GUIs can be produced quite easily.
<p><br>
The current status of the project is still experimental. The package
does not have much testing as of yet. As of version 0-0.16 Firefox, Safari, IE and Opera
basically work, although some widgets -- gcanvas, gsvg -- are browser dependent.
<p><br>
To create an interactive web GUI three things are done:
<ul>
<li>* A web page is layed out. This is done with gWidgets components</li>
<li>* A means to call back to the web server is needed. This is done using gWidgets handlers<li>
<li>* A means for the R process in the web server to manipulate the components of the web page.
This is done using JavaScript commands that are created by the gWidgets methods.</li>
</ul>
<p><br>
The package uses the dynamic help server for development and stand alone use.
<p><br>
To serve web pages to a wider community, the
RApache package
<A href=http://biostat.mc.vanderbilt.edu/rapache/>url</A>, which embeds
an R process within the Apache web server, is used so that callbacks
from the browser to the web server can be processed through R.
<p><br>
The javascript code is simplified by using the ext
javascript libraries 
<a href=http://www.extjs.com>extjs.com</a>. These are included with
the package.
<p><br>To make an interactive GUI in gWidgets can be
as easy as creating the following script:
<br>
<p>
<code>
w <- gwindow('simple interactive GUI with one button', visible=FALSE)<br />
g <- ggroup(cont=w)<br />
b <- gbutton('click me', cont=g, handler=function(h,...) {<br />
  &nbsp;gmessage('hello world', parent=b)<br />
})<br />
visible(w) <- TRUE<br />
</code>
</p>")
msg <- readLines(con, warn=FALSE)
close(con)
ghtml(paste(msg, collapse=" "), cont=f)



f <- gexpandgroup("Examples", cont = g, horizontal=FALSE)
dir <- system.file("Examples",package="gWidgetsWWW")
files <- list.files(path=dir, pattern="R$")

ghtml("Several examples accompany the package:", cont = f)
makeLinks <- function(i,f) {
  g1 <- ggroup(cont = f)
  b <- gbutton("Source", cont = g1, handler = function(h,...) {
    i <- h$action
    galert(i, parent=w)
    w1 <- gwindow("Source", parent = w)
    g1 <- ggroup(cont = w1, horizontal=FALSE)
    Rfile <- paste(dir,i, sep=.Platform$file.sep)
    ghtml(paste("<pre>",
                paste(readLines(Rfile), collapse="<br />"),
                "</pre>", sep=""), cont = g1)
    gseparator(cont = g1)
    gbutton("dismiss", cont = g1, handler = function(h,...) {
      dispose(w1)
    })
    visible(w1) <- TRUE
  }, action=i)
  if(gWidgetsWWWIsLocal()) {
    b <- gbutton(sprintf("Run %s",i), cont=g1, handler=function(h,...) {
      localServerOpen(sprintf("Examples/%s", i), package="gWidgetsWWW")
    })
  } else {
    url <- sprintf("%s/%s", w$..gWidgetsWWWrunUrl, i)
    ghtml(sprintf("&nbsp;<a href=%s target='_blank'>%s</a>",url, i), cont = g1)
  }
}
for(i in files) makeLinks(i,f)


f <- gexpandgroup("More information", cont = g)
ghtml(paste("The gWidgetsWWW package is installed from CRAN. After installation,",
            "read the package vignette for information on installation",
            "of external packages, javascript libraries",
            "etc.", sep=" "),
      cont = f)

f <- gexpandgroup("Security", cont = g)
ghtml("Security is a big issue with server installs. The use of RApache reduces the risk dramatically. Read the package vignette for more detail.", cont = f)

f <- gexpandgroup("Recent NEWS", cont = g)
newsfile <- system.file("NEWS", package="gWidgetsWWW")
eol <- "\\n"                            # had local/server issue
ghtml(paste("<pre>", paste(readLines(newsfile, n=50)[-(1:2)], collapse=eol), eol, "... %< snip >% ...", "</pre>"), cont=f)


gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE

