## sample on possible way to have data base editing

w <- gwindow("Edit a data base", visible=FALSE)

as.character.matrix <- function(d) {
  if(is.matrix(d))
    sapply(1:ncol(m), function(i) as.character(m[,i]))
  else
    sapply(d, as.character)
}

## read from disk
## dfFile <- "/var/www/Rpad/tmp/ex-datafile.txt")
## m <- read.table(dfFile, header=TRUE)
m <- data.frame(key=c("smith","jones"),
                name = c("John","John"),
                age = c(44, 12))
g <- ggroup(horizontal=FALSE, cont = w)

tbl <- glayout(cont = g)
n <- ncol(m)
nms <- names(m)
m <- as.character.matrix(m); colnames(m) <- nms
tbl[1,1, align=c(1,0)] <- nms[1]
tbl[1,2, align=c(-1,0)] <- (keyCB <- gcombobox(m[,1], selected=0, editable=TRUE, cont=tbl))
keyCB$..hideTrigger <- TRUE
## populate if selected from drop down
keyCB$addHandler(signal="select", handler = function(h,...) {
  key <- svalue(h$obj)
  vals <- m[m[,1] == key,]

  for(j in 2:length(vals))
    svalue(widgets[[j]]) <- vals[j]
})

widgets <- list()
widgets[[1]] <- keyCB
if(n > 1) {
  for(i in 2:n) {
    tbl[i,1, align=c(1,0)] <- nms[i]
    tbl[i,2, align=c(-1,0)] <- (widgets[[i]] <- gedit("",cont = tbl))
  }
}

gseparator(cont = g)
saveButton <- gbutton("save", cont = g, handler = function(h,...) {
  i <- nrow(m) + 1
  vals <- sapply(1:n, function(j) svalue(widgets[[j]]))
  ## if not a repeat ...
  m <- rbind(m, vals)
  ## write to disk
  ## write.table(m, file=dfFile)
})


gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE

