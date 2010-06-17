### A help widget and a more complicated browser does not use notebook
### interface, as the tcltk one is a bit of a hack.

setClass("gHelpANY",
         contains="gComponentANY",
         prototype=prototype(new("gComponentANY"))
         )

setMethod(".ghelp",
          signature(toolkit="ANY"),
          function(toolkit,
                   topic=NULL, package=NULL,
                   container = NULL,
                   ...) {                                # passed to gnotebook
            force(toolkit)

            ## check if newversion of R, if so, we con't do a thing but return a label
            if(getRversion() >= "2.10.0" && getRversion() < "2.11.0") {
              l <- glabel("ghelp doesn't work with R version 2.10.0 or 2.10.1. Sorry.", cont=container)
              return(l)
            }

            
            lggroup <- function(horizontal, container, width, height,...)
              ggroup(horizontal=horizontal, container=container, ...)
            group <- lggroup(horizontal=FALSE, container = container, ...)

            theArgs = list(...)
            ## width height also adjustable via size<-
            width = theArgs$width; height = theArgs$height
            if(is.null(width)) width=375
            if(is.null(height)) height=400

            topGroup = ggroup(cont=group, expand=TRUE); addSpring(topGroup)
            glabel("Help on:", cont=topGroup)

            selectPage = gcombobox(c(""),width=50, editable=TRUE,cont=topGroup)
            showPage = gtext("", width=width,height=height, cont=group, expand=TRUE)
              
            obj = new("gHelpANY", block=group, widget=showPage,
              toolkit=toolkit)

            tag(obj,"selectPage") <- selectPage # update this

            if(!is.null(topic))
              .add(obj, toolkit, value = list(topic=topic, package=package))

            addhandlerchanged(selectPage, handler = function(h,...) {
              value = svalue(selectPage)
              add(obj, value)
            })

            invisible(obj)

          })

##################################################
## gHelp methods
## workhorse is add -- value is either
## just a topic (not a list), or a list with components topic, package
setMethod(".add",
          signature(toolkit="ANY",obj="gHelpANY", value="character"),
          function(obj, toolkit, value, ...) {

            if(length(grep(":",value)) > 0) { # "stats:::t.test" works here
              tmp = unlist(strsplit(value, ":+"))
              package = tmp[1]
              topic = tmp[2]
            } else {
              topic = value
              package = NULL
            }
            .add(obj, toolkit, list(topic=topic, package=package))
          })

setMethod(".add",
          signature(toolkit="ANY",obj="gHelpANY", value="list"),
          function(obj, toolkit, value, ...) {
            topic <- value$topic
            package <- value$package
            
            ## error check
            if(!is.character(topic) || length(topic) > 1 || length(topic) == 0) {
              warning("Adios, adding to ghelp needs a valid topic. You tried",topic,"\n")
              return()
            }


            if(getRversion() < "2.10.0") {
              ## if package is NULL, we find them
              if(is.null(package)) {
                possiblePackages <- getPossiblePackages(topic)
                if(length(possiblePackages) > 0) {
                  package = possiblePackages
                } else {
                  galert(sprintf("Can't find a package containing ", topic,"\n"))
                  return()
                }
              }
              ## add a page for each package
              for(pkg in package) {
                ## make page
                makeHelpPage(obj, topic, pkg) # obj@widget is gtext
                ## update pages selection
                selectPage <- tag(obj,"selectPage")
                curItems <- selectPage[]
                newPage <- paste(pkg,":",topic,sep="")
                selectPage[] <- unique(c(newPage, curItems))
              }
            } else if(getRversion() >= "2.11.0") {
              ## add a page for each package
              l <- list(topic=topic)
              if(!is.null(package))
                l$package <- package
              out <- do.call("help", l)
              pkgname <-  basename(dirname(dirname(out)))
              temp <- tools::Rd2txt(utils:::.getHelpFile(out), out = tempfile("Rtxt"), package=pkgname)
              x <- readLines(temp)
              unlink(temp)
              dispose(obj@widget)       # clear
              ## add text to gtext widget
              ## we want to add with bold, but this causes tcltk to stall out, not sure why.
              ## try to fix that bug
              isTclTk <- obj@widget@toolkit@toolkit == "tcltk"
              out <- c()
              for(i in x) {
                if(grepl("^_\b",i)) {
                  if(isTclTk)
                    out <- c(out, gsub("_\b","",i))
                  else
                    insert(obj@widget, gsub("_\b","",i), font.attr=c(weight="bold"))
                } else {
                  if(isTclTk)
                    out <- c(out,i)
                  else
                    insert(obj@widget, i)
                }
              }
              if(isTclTk)
                svalue(obj@widget) <- out
              else
                insert(obj@widget, "", do.newline=FALSE, where="beginning")              
            }
            return()
            })

## value returns the topic of the current page or the one give by index
setMethod(".svalue",
          signature(toolkit="ANY",obj="gHelpANY"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            value = svalue( tag(obj,"selectPage"))
            if(length(grep(":",value)) > 0) { # "stats:::t.test" works here
              tmp = unlist(strsplit(value, ":+"))
              package = tmp[1]
              topic = tmp[2]
            } else {
              topic = value
              package = NULL
            }
            return(list(topic=topic, package=package))
          })

setMethod(".length",
          signature(toolkit="ANY",x="gHelpANY"),
          function(x, toolkit) {
            length(tag(x,"selectPage"))
          })

setMethod(".dispose",
          signature(toolkit="ANY",obj="gHelpANY"),
          function(obj, toolkit, ...) {
            cat("not implemented")
          })

##################################################
## helpers

## Return gtext widget with help page
## Only called if R Version <= 2.10.0
makeHelpPage = function(obj, topic, pkg) {
  helpPage <- obj@widget
  l <- list(topic, package=force(pkg), verbose=TRUE,
         chmhelp=FALSE, htmlhelp=FALSE) # avoid warning
  helpFile = try(do.call("help", l)[1], silent=TRUE)

  ## if as.character(helpFile) == character(0) then no good
  if(length(as.character(helpFile) != 0)) {
    dispose(helpPage)                   # clean out old one!
    text = readLines(helpFile)
    text = sapply(text, function(i) gsub("\\_\\\b","",i))
    insert(helpPage, text[2])
    insert(helpPage, text[3], font.attr=c(weight="bold",size="large",color="blue"))
##    add(helpPage, text[-(1:3)])
    ## This gave troubles when there were more than a few pages open!
    sapply(text[-(1:3)], function(x) {
      if( length(grep("^\\w+:", x)) > 0) {
        tmp = unlist(strsplit(x,":"))
        insert(helpPage,Paste(tmp[1],":"),font.attr=c(color="blue"), do.newline=FALSE)
        insert(helpPage,paste(tmp[-1], sep="", collapse=":"))
      } else {
        insert(helpPage,x)
      }
    })
    insert(helpPage,"",where="beginning")
  } else {
    add(helpPage,paste("Page for ",topic," in package ",pkg," was not found.",collapse=" "))
  }
}

getPossiblePackages = function(topic) {
  possiblePackages = c()
  ## find all packages
  lib.loc <- .libPaths()
  packages <- .packages(all.available = TRUE, lib.loc = lib.loc)
  for (lib in lib.loc) {
    for (pkg in packages) {
      dir <- system.file(package = pkg, lib.loc = lib)
      path = utils:::index.search(topic, dir, "AnIndex", "help")
      if(path != "")
        possiblePackages = c(possiblePackages, pkg)
    }
  }
  
  if(length(possiblePackages) == 0) {
    warning("Adios, can't find a package to match ",topic,"\n")
    return()
  }
  return(possiblePackages)
}


##################################################
## This just pops up a window to show the argument from a help page


## Hack to open up help page to the argument
showHelpAtArgument = function(argument, topic, package=NULL,
  width=600, height=250) {
  if(missing(argument) || missing(topic))
    return()

  if(is.null(package)) {
    possiblePackages = getPossiblePackages(topic)
    if(length(possiblePackages) > 0) {
      package = possiblePackages
    } else {
      warning(Paste("Can't find a package containing", topic,"\n"))
      return()
    }
  }

  ## the widget
  win=gwindow(Paste("Help on argument: ",topic), visible=FALSE) # set to visible if one is found
  group = ggroup(horizontal=FALSE, container=win)
  textwindow = gtext("", cont=group, expand=TRUE)
  size(textwindow) <- c(width,height)

  for(pkg in package) {
##    helpFile = system.file("help",topic,package=pkg)
    helpFile = help(topic, package=force(pkg), verbose=TRUE)[1]
    if(helpFile != "") {
      text = readLines(helpFile)
      text = sapply(text, function(i) gsub("\\_\\\b","",i))
      argPosition = grep(Paste(argument,": "), text)
      if(length(argPosition) == 0) {
        next
      } else {
        argPosition = argPosition[1] - 1
        ##Found one
        visible(win) <- TRUE            # show window
      }

      add(textwindow,Paste("From package:",pkg), font.attr=c(weight="bold"))
      ## add first line (it has a :)
      add(textwindow,text[argPosition+1],font.attr=c(weight="bold",color="blue"))
      ## add until a :
      i = 2; n = length(text)
      while(length(grep(":",text[argPosition+i])) == 0 &&
            (argPosition + i) <= n
            ) {
        add(textwindow,text[argPosition+i],font.attr=c(weight="bold",color="blue"))
        i = i + 1
      }
      add(textwindow,"\n")
    }
  }
  ## close button
  buttonGroup = ggroup(container=group)
  addSpring(buttonGroup)
  gbutton("cancel", container=buttonGroup,
          handler = function(h,...) dispose(h$obj))
  
}




##################################################
## build on ghelp widget to make a browser with search,
## simpler than old pmg.helpBrowser. Break that into components

## a notebook for holding help pages
setClass("gHelpbrowserANY",
         contains="gComponentANY",
         prototype=prototype(new("gComponentANY"))
         )


##################################################
## build on ghelp widget to make a browser with search,
## simpler than old pmg.helpBrowser. Break that into components

## a notebook for holding help pages
setClass("gHelpbrowserANY",
         contains="gComponentANY",
         prototype=prototype(new("gComponentANY"))
         )


setMethod(".ghelpbrowser",
          signature(toolkit="ANY"),
          function(toolkit,
                   title = "Help browser", maxTerms=100,
                   width=550, height=600) {

            force(toolkit)
            
            win = gwindow("Help browser")
            size(win) <-  c(width,height)

            obj=new("gHelpbrowserANY",block=win,widget=win,toolkit=toolkit)
  
            gp = ggroup(horizontal = FALSE, container = win)
            toolbarGroup = ggroup(container = gp)

            quitHandler = function(h,...) dispose(win)
            quitButton = ggroup(container=toolbarGroup)
            gimage("quit",dirname="stock",handler=quitHandler, cont=quitButton)
            glabel("Quit",handler = quitHandler, cont=quitButton)

            runExamples = function(h,...) {
              lst = svalue(help.notebook)
              if(!is.null(lst$topic))
                do.call("example",lst)
            }

            examplesButton = ggroup(container=toolbarGroup)
            gimage("evaluate",dirname="stock",handler=runExamples,
                   cont=examplesButton)
            glabel("run examples",handler = runExamples, cont=examplesButton)

            addSpring(toolbarGroup)
            
            ## others?
            searchOptionsList = list(
              "Help on function:" = function(...) NULL,
              "help.search: apropos"=function(...) searchResultsApropos(...),
              "help.search: pattern"=function(...) searchResultsHelpSearch(...)
              )
            searchOptions = gdroplist(names(searchOptionsList), container = toolbarGroup)
            searchBox = gedit("", container = toolbarGroup)
            
            ## search through packages
            expgp = gexpandgroup("Browse package help pages:",container = gp,
              expand=TRUE)
            addSpring(gp)
            visible(expgp) <- FALSE
            
            packageNotebook = gnotebook(container=expgp, expand=TRUE)
#            size(packageNotebook) <- c(400,300)


#            addhandlerchanged(packageNotebook,function(h,...) {
#              dispose(h$obj, to.right=TRUE)
#            })                  # delete to right, when changed

            allPackages = .packages(all=TRUE)
            packageList = gtable(
              data.frame("Package names"=allPackages,stringsAsFactors=FALSE),
              container = packageNotebook, label="All packages",
              expand=TRUE
              )

            addHandlerDoubleclick(packageList, handler = function(h,...) {
              ## get contents, show with filter
              package = svalue(h$obj)
              contents = getContentsOfPackage(package)

              ## delete page 2 if present
              if(length(packageNotebook) == 2) {
                svalue(packageNotebook) <-2
                dispose(packageNotebook)
              }
              
              page = ggroup(horizontal=FALSE,
                cont = packageNotebook, label=Paste("Objects in ",package) )
              ## objectList
              objectList = gtable(contents, ## filter.column didn't work here
                cont = page, expand=TRUE)

              addhandlerdoubleclick(objectList,action=package,
                                    handler=function(h,...) {
                                      topic = svalue(h$obj)
                                      package = h$action
                                      svalue(statusBar) <- Paste("Getting help page for ",topic)
                                      add(help.notebook,list(topic=topic, package=package))
                                      svalue(statusBar)
                                      svalue(nb) <- 1 # help page
                                      visible(expgp) <- FALSE
                                      return(FALSE)
                                    })
              return(FALSE)             # doubleclick return for no more propogation
            })
            
##################################################
            nb = gnotebook(tab.pos=3,
              container=gp, expand=TRUE)

            
            help.notebook  = ghelp(tab.pos=1,closebuttons=TRUE,
              container=nb, label="Help pages", expand=TRUE)     # bottom tab

            emptyDataFrame = data.frame(Title=c(""), Package=c(""),Descr=c(""))
            for(j in 1:3) emptyDataFrame[,j] <- as.character(emptyDataFrame[,j])

            search.results = gtable(emptyDataFrame, ## filter.column=2,
              container=nb, label="Search results", expand=TRUE)
#            size(search.results) <- c(400,250)
            svalue(nb) <-1                # help page first
            
            statusBar = gstatusbar(container=win)
            svalue(statusBar) <- "Enter search term in box, click ENTER to begin"
            ## actions
            ## double click on search results
            addhandlerdoubleclick(search.results,
                                  handler = function(h,...) {
                                    vals = svalue(h$obj, drop=FALSE) # a data frame
                                    vals <- as.data.frame(vals)      # may be list
                                    topic = as.character(vals[,1,drop=TRUE])
                                    package = as.character(vals[,2,drop=TRUE])

                                    svalue(statusBar) <-
                                      Paste("Getting help page for ",topic)
                                    add(help.notebook, list(topic=topic, package=package))
                                    svalue(statusBar) <- ""
                                    ## swap tabs
                                    svalue(nb) <- 1
                                    return(FALSE) # no mas
                                  })
            ## make search resuslts -- return dataframe with title, package, description
            ## as character vectors
            searchResultsApropos = function(query) {
              out = help.search(apropos=query, ignore.case = TRUE)
              out = out$matches
              if(nrow(out) > 0) {
                out = out[1:min(nrow(out),maxTerms),c(1,3,2), drop=FALSE]
              } else {
                out = c("no matches","","")
              }
              colnames(out) = c("topic","Package","title")
              out = as.data.frame(out)
              for(j in 1:3) out[,j] <- as.character(out[,j]) # avoid factors
              return(out)
            }
            searchResultsHelpSearch = function(query) {
              out = help.search(pattern=query, ignore.case = TRUE)
              out = out$matches
              if(nrow(out) > 0) {
                out = out[1:min(nrow(out),maxTerms),c(1,3,2), drop=FALSE]
              } else {
                out = c("no matches","","")
              }
              colnames(out) = c("topic","Package","title")
              out = as.data.frame(out)    
              for(j in 1:3) out[,j] <- as.character(out[,j]) # avoid factors
              
              return(out)
            }
            
            addhandlerchanged(searchBox, handler = function(h,...) {
              searchType = svalue(searchOptions, index=TRUE)
              svalue(statusBar) <- "Getting to work"
              if(searchType == 1) {
                ## first one is show help page
                topic = svalue(h$obj)
                add(help.notebook,topic)
              } else {
                df = searchOptionsList[[searchType]](svalue(h$obj))
                ## set value in widget
                search.results[,] <- df
                ## raise search box
                svalue(nb) <-2
                svalue(statusBar) <-"Double click line to show help page"
              }
              svalue(statusBar)                   # pops
            })
            
            
            return(obj)
          })


##################################################
## these are from old version
## contents a matrix with entry, keywords, description and URL
getContentsOfPackage = function(package=NULL) {
  if(getRversion() <= "2.10.0") {  
    if(is.null(package)) {
      warning("Empty package name")
      return(NA)
    }
    contents = read.dcf(system.file("CONTENTS",package=package))
    
    return(data.frame(Entry=contents[,1],Keywords=contents[,3],
                      Description=contents[,4],
                      stringsAsFactors = FALSE))
  } else {
    ## return a data frame with entry keywords description
    path <- system.file("help", package = package)
    contents <- .readRDS(sub("/help", "/Meta/Rd.rds", path, fixed = TRUE))    
    return(data.frame(Entry=contents[,'Name'],Keywords=contents[,'Keywords'],
                      Description=contents[,'Title'],
                      stringsAsFactors = FALSE))
  }
}

