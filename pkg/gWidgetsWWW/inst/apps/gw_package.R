## Load a gWidgets package

## globals
availPackages <- NULL            # cache
newPackages <- NULL              # new ones

##' ##' do we have a cran site set?
##'
##' @return logical. TRUE If cran is set
hasCRAN <- function() {
  !getOption("repos")[1] == "@CRAN@"
}
##' Return matrix of CRAN mirrors
##'
##' From getCRANmirrors
listCRANMirrors <- function() {
    m <- read.csv(file.path(R.home("doc"), "CRAN_mirrors.csv"), 
                  as.is = TRUE)
  m
}

##' return uninstalled packages
getNewPackages <- function() {
  m.new <- data.frame(Package="", Version="",
                             stringsAsFactors=FALSE)
  if(hasCRAN()) {
    m.new <- new.packages()                 # a character
    if(is.null(availPackages))
      availPackages <<- available.packages()             # a matrix
    m.new <- availPackages[m.new, ]               # a matrix now.
    newPackages <<- m.new                         # save
  }
  return(data.frame(Package=m.new[,1], Version=m.new[,2], stringsAsFactors=FALSE))
}

##' return vector of installed package names
getInstalledPackages <- function() {
  m.installed <- installed.packages()     # a matrix  
  m.installed[,1]
}


##' select a CRAN site
makeCRANSelectionPage <- function(cont) {
  g <- ggroup(cont=cont, horizontal=FALSE)
  glabel("Select a CRAN site by double clicking", cont=g)
  mirrors <- listCRANMirrors()
  tbl <- gtable(mirrors[,1:2], cont=g); size(tbl) <- c(600,500)
  addHandlerDoubleclick(tbl, handler=function(h,...) {
    ind <- svalue(h$obj, index=TRUE)
    url <- mirrors[ind, "URL"]
    options(repos=list(CRAN=url))
    galert("Fetching available packages....", parent=w)
    update(w)
  })
}


##' make page for new packages
makeNewPage <- function(cont, width=600, height=500) {

  g <- ggroup(cont=cont, horizontal=TRUE)
  lg <- ggroup(cont=g, horizontal=FALSE)
  bg <- ggroup(cont=lg); size(bg) <- width
  filter.FUN <- function(h,...) {
    pkgs <- getNewPackages()
    if(nchar(svalue(fltr))) {
      reg <- svalue(fltr)
      ind <- grepl(reg, pkgs[,1, drop=TRUE])
    } else {
      ind <- 1:nrow(newPackages)
    }
    if(any(ind))
      packageTable[] <- pkgs[ind,]
   else
     galert(sprintf("No match for %s", svalue(fltr)), parent=w)
  }
  fbutton <- gbutton("Filter by:", cont=bg, handler=filter.FUN) ## called on blur
  fltr <- gedit("", cont=bg, expand=TRUE, handler=filter.FUN) 
  packageTable <- gbigtable(getNewPackages(), cont=lg, expand=TRUE, filter.FUN="manual")
  size(packageTable) <- c(width, height)
  
  addHandlerDoubleclick(packageTable, handler=function(h,...) {
   pkgname <- svalue(h$obj)
   type <- svalue(package_type)
   depends <- svalue(package_depends)
   gconfirm(sprintf("Install package %s? It may take some time.", pkgname), parent=w,
            handler=function(h,...) {
              out <- try(do.call("install.packages", h$action), silent=TRUE)
              if(inherits(out, "try-error"))
                galert(sprintf("Error trying to install %s", pkgname), parent=w)
              else
                galert(sprintf("Installed package %s", pkgname), parent=w)
            },
            action=list(pkgs=pkgname, type=type, dependencies=depends)
            )
 })

  
  lyt <- glayout(cont=g)
  lyt[1,1:2] <- "Double click a package to install. Options for install.packages:"
  lyt[2,1] <- "Type:"
  lyt[2,2] <- (package_type <- gcombobox(c("source", "mac.binary", "mac.binary.leopard", "win.binary", "win64.binary"),
                        cont=lyt))

  lyt[3,1] <- "Dependencies"
  lyt[3,2] <- (package_depends <- gcheckboxgroup(c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"),
                              checked=c(TRUE, TRUE, FALSE, TRUE, FALSE),
                              cont=lyt,
                              horizontal=FALSE))
  
}







##' show installed packages and options
makeInstalledPage <- function(cont, width=250, height=400) {
  g1 <- ggroup(cont=cont, horizontal=FALSE)
  ghtml("<h2>Click on an installed package name to see more detail</h2>", cont=g1)
  g <- ggroup(cont=g1, horizontal=TRUE)
  installedPackagesTable <- gtable(getInstalledPackages(), cont=g)
  size(installedPackagesTable) <- c(width, height)
  addHandlerClicked(installedPackagesTable, handler=function(h,...) {
    pkgname <- svalue(h$obj)
    makePackageDescription(ig, pkgname)
  })
  ig <- ggroup(cont=g, horizontal=FALSE)
  makePackageDescription(ig)
}


##' Make a package Description
##' 
##' @param g container
##' @param pkgname package name. If missing makes an invisible template to fill in
makePackageDescription <- function(g, pkgname) {
  ## workaround for layout issue with deleting/adding containers
  ## two phases, first sets up widgets and then leaves not visible
  ## second (and subsequent) fills in values and makes visible
  if(missing(pkgname)) {
    e$.tbl <- tbl <- glayout(cont=g, expand=TRUE)
    tbl[1,1] <- "Title"
    tbl[1,2] <- (e$.title <- ghtml("", cont=tbl))

    tbl[2,1] <- "Author:"
    tbl[2,2] <- (e$.author <- ghtml("", cont=tbl))

    tbl[3,1] <- "Installed Version:"
    tbl[3,2] <- (e$.current <- ghtml("", cont=tbl))

    tbl[4,1] <- "Latest CRAN Version:"
    tbl[4,2] <- (e$.latest <- ghtml("", cont=tbl))
    tbl[5,2] <- (e$.buttonGroup <- ggroup(cont=tbl))

    e$.updateButton <- gbutton("Update package", cont=e$.buttonGroup, handler=function(h,...) {
      galert(sprintf("update package %s", e$.pkgname), parent=w)
    })
    enabled(e$.updateButton) <- FALSE

    e$.loadButton <- gbutton("Load package", cont=e$.buttonGroup, handler=function(h,...) {
      ret <- do.call("require", list(e$.pkgname))
      if(ret)
        msg <- sprintf("Package %s loaded", e$.pkgname)
      else
        msg <- sprintf("Package %s failed to load", e$.pkgname)
      galert(msg, parent=w)
    })
    enabled(e$.loadButton) <- FALSE

    tbl[6,1, anchor=c(-1,1)] <- "Description:"
    tbl[6,2] <- (e$.desc <- gtext("", cont=tbl, expand=TRUE))
    size(e$.desc) <- c(8*80, 16*8)
    visible(tbl) <- FALSE
  } else {
    e$.pkgname <- pkgname
    desc <- read.dcf(system.file("DESCRIPTION", package=pkgname))
    curVersion <- desc[1, "Version"]

    assign("l", list(pkgname, availPackages[,"Package"]), .GlobalEnv)
    
    latestVersion <-  if(pkgname %in% availPackages[,"Package"]) {
      availPackages[pkgname, "Version"]
    } else {
      "Package not on CRAN"
    }
    
    loadedPackages <- c(names(sessionInfo()$otherPkgs), names(sessionInfo()$basePkgs))
    isLoaded <- pkgname %in% loadedPackages

    svalue(e$.title) <- desc[1,"Title"]
    svalue(e$.author) <- gWidgetsWWW:::escapeHTML(desc[1, "Author"])
    svalue(e$.current) <- curVersion
    svalue(e$.latest) <- latestVersion
    svalue(e$.desc) <- paste(strwrap(desc[1,"Description"]), collapse=" ")

    enabled(e$.updateButton) <- curVersion != latestVersion
    enabled(e$.loadButton) <- !isLoaded

    visible(e$.tbl) <- TRUE
  }
}







##################################################
## Make GUI
w <- gwindow(gettext("Package manager"))
g <- ggroup(cont=w, horizontal = FALSE)

if(!hasCRAN()) {
  makeCRANSelectionPage(g)
} else {
  height <- 574
  
  nb <- gnotebook(cont=g)
  f <- ggroup(cont=nb, label="Packages to install")
  makeNewPage(f, width=600, height=height)
  
  f <- ggroup(cont=nb, label="Installed packages")
  makeInstalledPage(f, width=300, height=height)

  svalue(nb) <- 2
}

gstatusbar("Powered by gWidgetsWWW", cont=w)

visible(w) <- TRUE

