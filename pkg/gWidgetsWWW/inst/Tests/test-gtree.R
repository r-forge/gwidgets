fn <- "gtree"
##################################################
listCRANMirrors <- function() {
  ## con <- url("http://cran.r-project.org/CRAN_mirrors.csv")
  ## m <- try(open(con, "r"), silent = TRUE)
  ## if (!inherits(m, "try-error")) 
  ##   m <- try(read.csv(con, as.is = TRUE))
  ## else
    m <- read.csv(file.path(R.home("doc"), "CRAN_mirrors.csv"), 
                  as.is = TRUE)
  m
}
m.cran <- listCRANMirrors()
l.cran <- split(m.cran$Name, m.cran$Country)

offspring <- function(path, l.cran, ...) {
  assign("path", path, envir=.GlobalEnv)
  assign("l.c", l.cran, envir=.GlobalEnv)
  if(missing(path) || length(path) == 0) {
    x <- names(l.cran)
    o <- rep(TRUE, length(x))
  } else {
    x <- l.cran[[path]]
    o <- rep(FALSE, length(x))
  }
  data.frame(items=x, has.offspring=o, stringsAsFactors=FALSE)
}
##################################################
w <- gwindow(sprintf("test %s", fn))

g <- ggroup(cont=w, horizontal=FALSE)
widget <- gtree(offspring=offspring, offspring.data=l.cran, cont=g)
size(widget) <- c(300,300)

addHandlerChanged(widget, handler=function(h,...) {
  galert(svalue(widget), parent=w)
})

## svalue
gbutton("click to get svalue", cont=g, handler=function(h,..) {
  galert(svalue(widget), parent=w)
})


## ##################################################                  
gstatusbar(sprintf("Tests for %s", fn), cont=w)
visible(w) <- TRUE
