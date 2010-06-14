## Build a t-test gui
##require(Cairo) ## no X11 dependence

w <- gwindow("t-test example")
g <- ggroup(cont = w, horizontal = FALSE)
data(Cars93, package="MASS")


dataSets <- c("mtcars") #,"Cars93")
alternatives <- data.frame(value=c("two.sided","less","greater"),
                           label = c("two sided", "less than", "greater than"))

tbl <- glayout(cont=g)
tbl[1,1] <- glabel("Selecte a data frame:", cont=tbl)
tbl[1,2] <- (selData <- gcombobox(dataSets, selected=0, cont=tbl, editable=TRUE))

tbl[2,1] <- glabel("Select a variable", cont=tbl)
tbl[2,2] <- (selVariable <- gcombobox(c("",""),cont = tbl, editable = TRUE))

tbl[3,1] <- glabel("mu:", cont=tbl)
tbl[3,2] <- (selMu <- gedit(0, cont=tbl))

tbl[4,1] <- glabel("alternative:", cont=tbl)
tbl[4,2] <- (selAlt <- gcombobox(alternatives, cont=tbl))

## doesn't work
tbl[5,2] <- (buttonGroup <- ggroup(cont=tbl))

doButton <- gbutton("run t-test", cont=buttonGroup)
doGraph <- gbutton("EDA plot", cont=buttonGroup)

outputArea <- gtext("", cont = g); size(outputArea) <- c(width=500,height=200)


## blank out until ready
enabled(selVariable) <- FALSE
enabled(selMu) <- FALSE
enabled(selAlt) <- FALSE
enabled(buttonGroup) <- FALSE

## handlers
addHandlerChanged(selData, handler = function(h,...) {
  enabled(selVariable) <- TRUE
  df <- svalue(selData)
  df <- get(df, inherits=TRUE)                # string to object
  selVariable[] <- data.frame(names(df),names(df), stringsAsFactors=FALSE)
})

addHandlerChanged(selVariable, handler = function(h,...) {
  enabled(selMu) <- TRUE
  enabled(selAlt) <- TRUE
  enabled(buttonGroup) <- TRUE
})
                  
addHandlerClicked(doButton, handler = function(h,...) {
   df <- svalue(selData)
   var <- svalue(selVariable)
   mu <- svalue(selMu)
   alt <- svalue(selAlt)

   if(df != "") {
     df <- try(get(df, inherits=TRUE), silent=TRUE)
     val <- capture.output({
       t.test(df[,var], mu=as.numeric(mu), alt=as.character(alt))
     })
     
     svalue(outputArea) <- val
   }
})

addHandlerClicked(doGraph, handler = function(h,...) {
  df <- svalue(selData)
  var <- svalue(selVariable)
  width <- 400; height <- 300



  w1 <- gwindow("EDA", parent = w, width=width, height=height+2*25 + 20 + 30)
  g1 <- ggroup(horizontal=FALSE, cont=w1, use.scrollwindow=TRUE)
  cv <- gsvg(cont = g1, width=width, height=height)
  gseparator(cont = g1)
  gbutton("dismiss", cont=g1, handler = function(h,...) dispose(w1))

  require(RSVGTipsDevice, quietly=TRUE, warn=FALSE)
  f <- getStaticTmpFile(ext=".svg")
  devSVGTips(f)
  df <- get(df, inherits=TRUE)
  hist(df[, var], main=var)
  dev.off()
  svalue(cv) <- convertStaticFileToUrl(f)
  
  ## show page
  visible(w1) <- TRUE
})
  
## show w
gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE
                  
