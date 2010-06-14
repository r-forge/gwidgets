## Simple file to create a directory listing
w <- gwindow("gWidgetsWWW")
g <- ggroup(cont = w)
files <- list.files(path=getwd(), pattern="R$")

ghtml("R files in working directory.", cont = g)
makeLinks <- function(i,f) {
  g1 <- ggroup(cont = f)
  b <- gbutton("Source", cont = g1, handler = function(h,...) {
    w1 <- gwindow("Source", parent = w)
    g1 <- ggroup(cont = w1, horizontal=FALSE)
    Rfile <- paste(getwd(),i, sep=.Platform$file.sep)
    ghtml(paste("<pre>",
                paste(readLines(Rfile), collapse="\\n"),
                "</pre>", sep=""), cont = g1)
    gseparator(cont = g1)
    gbutton("dismiss", cont = g1, handler = function(h,...) {
      dispose(w1)
    })
    visible(w1) <- TRUE
  })
  url <- paste("http://127.0.0.1:",get("RpadPort", envir = .RpadEnv),"/gWidgetsWWWrun/", i, sep="")
  ghtml(paste("&nbsp;<A href=", url," target=\"_blank\">",i,"</A>", sep=""), cont = g1)
}
for(i in files) makeLinks(i,g)

visible(w) <- TRUE

