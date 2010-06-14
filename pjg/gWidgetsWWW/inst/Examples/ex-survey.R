## script to do a survey and store result between sessions
## we use a file in the /tmp directory. This could be a database, ...

f <- "/tmp/survey.R"
if(!file.exists(f)) {
  ## initialize
  d <- data.frame(data=character(), response=character(), stringsAsFactors=FALSE)
  dump("d", f, append=FALSE)
}

question <- "Have you ever programmed a GUI in R before?"
answers <- c("yes","no","kind of")

w <- gwindow("Survey")
g <- ggroup(cont =w)
ghtml(paste("This example illustrates a simple way to keep data persistent",
            "between page loads. It uses a simple strategy of dump/source ",
            "to keep the data. More sophisticated uses could include a data base.",
            sep=" "), cont = g)
g <- ggroup(cont = w, horizontal=TRUE)
g1 <- gframe("Please take our survey", cont = g, horizontal=FALSE, width=300, height=400)
ghtml(question, cont = g1)
gseparator(cont = g1)
rb <- gradio(answers, horizontal=FALSE, cont = g1)
gseparator(cont = g1)
voteButton <- gbutton("Vote", cont = g1)
addHandlerClicked(voteButton, handler = function(h,...) {
  source(f)                         # loads d
  ## store date and response. More??
  ## Could store values from SERVER value
  ## seee http://biostat.mc.vanderbilt.edu/rapache/manual.html
  d[nrow(d)+1,1] <- date()
  d[nrow(d),2] <- svalue(rb)
  dump("d", f, append=FALSE)
  rm("d")>
  makeGraphic()
})

## make graphic
g2 <- gframe("survey responses", cont = g, horizontal=FALSE, width=300, height=400)
responseLabel <- glabel("", cont = g2)
makeGraphic <- function() {
  source(f)
  if(nrow(d) == 0) {
    svalue(responseLabel) <- "No responses recorded"
  } else {
    library(hwriter)                    # to make a table
    tbl <- table(d$response)
    m <- matrix(tbl, nrow=1)
    colnames(m) <- names(tbl)
    svalue(responseLabel) <- gsub("\n","",hwrite(m, border=1,row.bgcolor='#ffdc98'))
  }
  rm("d")                                 # don't store in environment
}
makeGraphic()

gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE
