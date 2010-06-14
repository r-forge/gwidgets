## Simple interface to googlemaps

## Can show a map with some programmatic markup possible
## The API key is set in the RApache configuration.
## the key comes from google : http://code.google.com/apis/maps

## XXX The handler code is not currently working. XXX

## see the footer for ideas on how to access GMap2 API
## Use obj[,] <- data.frame(lat,long,title) to set markers
## * markers: [geoCodeAddr], [geoCodeAddr, title], [lat, long], [lat,long,title]

## center and markers set *prior* to rendering. No methods after (possible, not done)



ggooglemaps <- function(x, title = "",  type = c("map","panorama"),
                        key="ABQIAAAAYpRTbDoR3NFWvhN4JrY1ahS5eHnalTx_x--TpGz1e2ncErJceBS7FrNBqzV5DPxkpbheIzZ9nTJPsQ", # for 127.0.0.1:8079; only for local. For server, set in RApache.conf
                        container, ...) {
  widget <- EXTComponent$new(toplevel=container$toplevel,
                             ..title = title,
                             ..key = key,
                             ..gmapType = match.arg(type))
  class(widget) <- c("gGoogleMap",class(widget))
  widget$setValue(value = x)
  ## default is 0-row set of marks
  widget$setValues(value = data.frame(lat=0,long=0,title="")[0,])
  widget$..runCmds <- c()
  

  widget$scripts <- function(.) {
    ## we run this on creation. We also set key here:
    ## works for local
    ## server has key set in RApache.conf
    options(gWidgetsWWWGoogleAPI=.$..key)

    
    f <- system.file("javascript","GMapPanel.js", package="gWidgetsWWW")
    out <- paste(readLines(f), collapse="\n")
    
    return(out)
  }

  ## svalue -- location of map
  ## [] -- extra markers specified by lat and long vector. Names of vector gives
  ##       title attribute


  ## markers are set with a data frame.
  ## [geoCodeAddr]
  ## [geoCodeAddr, title] ## first col is non-numeric
  ## [lat, long]
  ## [ lat, long, title]
 
widget$makeMarkers <- function(.) {
    values <- .$getValues()
    out <- String("")
    if(nrow(values) > 0) {
      if(ncol(values) == 1) {
        ## geoCodeAddr
        out <- paste("{ geoCodeAddr:",shQuoteEsc(values[,1]),
                     "}",
                     collapse=",")
      } else if(ncol(values) == 2) {
        if(!is.numeric(values[,1])) {
          ## geoCodeAddr, title
          out <- paste("{ geoCodeAddr:",shQuoteEsc(values[,1]),",",
                       "marker:{title:", shQuoteEsc(values[,2]),"}",
                       "}",
                       collapse=",")
        } else {
          ## no title, lat and long
          out <- paste("{ lat:",values[,1],",'long':",values[,2],
                       '}',
                       collapse=",")
        }
      } else {
        out <- paste("{ lat:",values[,1],",'long':",values[,2],",",
                     "marker:{title:", shQuoteEsc(values[,3]),"}",
                     "}",
                     collapse=",")
      }
      
    }
    return(String("[") + out + "]")
  }

  ## XXX set defaults for width and height -- doesn't like auto
  widget$..width <- 600
  widget$..height <- 400
  ## can override
  widget$..zoomLevel <- 14

  
  widget$makeMapCommands <- function(.) {
    lst <- list(xtype = "gmappanel",
                region = "center",
                zoomLevel = .$..zoomLevel,
                gmapType = .$..gmapType,
                width = .$..width,
                height = .$..height #,
#                addControl = String("new GSmallMapControl()")
                )
    val <- svalue(.)
    if(length(val) == 1) {
      lst[["setCenter"]] = list(
           geoCodeAddr = val,
           marker = list(title = .$..title)
           )
    } else {
      lst[["setCenter"]] = list(
           lat = val[1],
           long = val[2],
           marker = list(title = .$..title)
           )
    }

    if(length(.$getValues()) > 0)
      lst[["markers"]] <- .$makeMarkers()

    return(.$mapRtoObjectLiteral(lst))

  }
  widget$ExtConstructor <- "Ext.Panel"
  widget$ExtCfgOptions <- function(.) {
    ## out <- list(autoLoad=String('http://www.google.com/jsapi?key=' +
    ##               getOption("gWidgetsWWWGoogleAPI" + '"></script>' +
    ##       '<script type="text/javascript">  google.load("maps", "2"); </script>' + '\n'

    out <- list(items = .$makeMapCommands())
                  
    return(out)
  }
  ## this is an exampe of a footer
  ## more API at http://code.google.com/apis/maps/documentation/reference.html#GMap2
  widget$footer <- function(.) {
    out <- String() +
      ## how to get the map
      .$setgmapID() +
        .$gmapID() + '.enableGoogleBar();' +
          .$gmapID() + '.enableScrollWheelZoom();'

    if(length(.$..runCmds)) {
      for(i in .$..runCmds) out <- out + i
    }

    return(out)
  }

  widget$setValueJS <- function(.) {
    value <- svalue(.)
    if(length(value) == 2)
      .$panTo(value)
  }
  widget$setValuesJS <- function(.,...) {
    if(exists("..setValuesJS", envir=., inherits=FALSE)) .$..setValuesJS(...)
  
    values <- .$getValues()
    for(i in 1:nrow(values))
      widget$addMarker(values[i,3:4])
    
  }

  
  ## some non-gWidgets methods to access *some* of the google maps API

  ## return bounds of map
  ## write bounds in a transport function
  widget$setgmapID <- function(.) {
    out <- String() +
      'gmap' + .$ID +' = ' + 'o' + .$ID + '.getComponent(0).gmap;'
    return(out)
  }
  widget$gmapID <- function(.) {
    out <- String() +
      'gmap' + .$ID
    return(out)
  }

  ## bounds
  ## javascript transport to write bounds
  widget$transportBounds <- function(.) {
    out <- String() +
      .$setgmapID() +
        'var bounds = ' + .$gmapID() + '.getBounds();' +
          '_transportToR("' + .$ID + '.SouthWest",' +
            '{value:bounds.getSouthWest().toString()});' +
              '_transportToR("' + .$ID + '.NorthEast",' +
                '{value:bounds.getNorthEast().toString()});'
    return(out)
  }

  widget$getBounds <- function(.) {
    pat.sw = String(.$ID) + '.SouthWest'
    pat.ne = String(.$ID) + '.NorthEast'

    sw <- unlist(pat.sw[3:4])
    ne <- unlist(pat.ne[3:4])

    ## return
    list(southwest = sw, northeast = ne)
  }

  ## set center
  ## latlng <- c(lat=xxx, lng = yyy)
  widget$panTo <- function(., latlng) {
    out <- String() +
      .$setgmapID() +
        .$gmapID() + '.panTo(' +
          'new GLatLng(' + latlng[1] + ',' + latlng[2] + '));'

    if(exists("..shown", envir=., inherits = FALSE))
      cat(out)
    else
      .$setValue(value = latlng)
  }
  ## zoom in or out
  widget$setZoom <- function(., zoom=14) {
    out <- String() +
      .$setgmapID() +
        .$gmapID() + '.setZoom(' + zoom + ');'

    if(exists("..shown", envir=., inherits = FALSE))
      cat(out)
    else
      .$..zoomLevel <- zoom

  }

  ## popup a message at a point
  widget$openInfoWindow <- function(., latlng, myHTML) {
    out <- String() +
      .$setgmapID() +
        'var point = new GLatLng(' + latlng[1] + ',' + latlng[2] + ');' +
          .$gmapID() + '.openInfoWindow(point,' +
            shQuoteEsc(myHTML) + ');'

    if(exists("..shown", envir=., inherits = FALSE))
      cat(out)
    else
      .$..runCmds <- c(.$..runCmds, out)
  }

  ## methods to add to map: marker, Polyline, Polygon
  
  ## addMarker
  widget$addMarker <- function(., latlng, title="", draggable = FALSE) {

    ## append to markers
    marks <- .$getValues()
    if(nrow(marks) == 0) {
      marks <- data.frame(latlng[1], latlng[2], latlng[3], latlng[4], title)
    } else {
      n <- nrow(marks)
      marks[n+1, 1:4] <- unlist(latlng)
      if(ncol(marks) == 5)
        marks[n+1, 5] <- title
    }
    .$..values <- marks                 # bypass setValues, as it would recurse

    ## make JS
    lst <- list(draggable = draggable)
    if(title != "")
      lst[["title"]] <- title
    out <- String() +
      .$setgmapID() +
        'var point = new GLatLng(' + latlng[1] + ',' + latlng[2] + ');' +
          'var marker = new GMarker(point,' +
            .$mapRtoObjectLiteral(lst) +
              ');'
    if(draggable) {
      ##     ## add handlers
      out <- out +
        'GEvent.addListener(marker, "dragstart", function() {' +
          .$gmapID() + '.closeInfoWindow();' +
            '});'
      
      ## XXX dragend should also update marks position
      out <- out +
        'GEvent.addListener(marker, "dragend", function() {' +
          'myHtml = "new latitude and longitude:<br>" + this.getLatLng().toString();' +
            'this.openInfoWindowHtml(myHtml);' +
              '});'
    }

    out <- out +
      .$gmapID() + '.addOverlay(marker);'
    
    if(exists("..shown", envir=., inherits = FALSE))
      cat(out)
    else
      .$..runCmds <- c(.$..runCmds, out)
  }

  ## polyLine
  ## latlng matrix or data frame of lat and lng
  widget$addPolyline <- function(., latlng,
                                 color="#ff0000", pixel.width = 5, opacity=1) {
    if(missing(latlng))
      latlng <- .$getValues()
    if(! (is.matrix(latlng) || is.data.frame(latlng))) return()
    if(nrow(latlng) == 0) return()
    
    out <- String() +
      .$setgmapID() +
        'var polyline = new GPolyline(['
    tmp <- c()
    for(i in 1:nrow(latlng))
      tmp[i] <- String("new GLatLng(") +
        latlng[i,1] + ',' + latlng[i,2] + ')'

    out <- out + paste(tmp, collapse=", ") +
      '], ' + shQuote(color) + ',' + pixel.width + ',' + opacity +
        ', {clickable: true, geodesic: true}' +
        ');'
    ## add a handler to show length
     out <- out +
       'GEvent.addListener(polyline, "click", function(latlng) {' +
         'var dist = (this.getLength()/1000).toFixed(2);' +
           'myHtml = "length (meters):<br>" + dist.toString();' +
             .$gmapID() + '.openInfoWindowHtml(latlng, myHtml);' +
              '});'
    
    out <- out +
      .$gmapID() + '.addOverlay(polyline);'

    if(exists("..shown", envir=., inherits = FALSE))
      cat(out)
    else
      .$..runCmds <- c(.$..runCmds, out)
  }
    
        

  ## drawPolygon
  widget$addPolygon <- function(., latlng,
                                border.color="#ff0000", border.pixel.width = 5,
                                border.opacity = 1,
                                region.color = "#000000", region.opacity = .1
                                ) {

    if(missing(latlng))
      latlng <- .$getValues()
    if(! (is.matrix(latlng) || is.data.frame(latlng))) return()
    if(nrow(latlng) == 0) return()
    

    out <- String() +
      .$setgmapID() +
        'var polygon = new GPolygon(['
    tmp <- c()
    for(i in 1:nrow(latlng))
      tmp[i] <- String("new GLatLng(") +
        latlng[i,1] + ',' + latlng[i,2] + ')'
    ## terminate
    tmp[nrow(latlng) + 1] <- String("new GLatLng(") +
        latlng[1,1] + ',' + latlng[1,2] + ')'

    out <- out + paste(tmp, collapse=", ") +
      '], ' +
        shQuote(border.color) + ',' + border.pixel.width + ',' +
          border.opacity +
            ',' + shQuote(region.color) + ',' + region.opacity +
              ');'

    ## add a handler to show area
     out <- out +
       'GEvent.addListener(polygon, "click", function(latlng) {' +
         'var area = (this.getArea()/(1000*1000)).toFixed(2);' +
           'myHtml = "Area (sq. kms):<br>" + area.toString();' +
             .$gmapID() + '.openInfoWindowHtml(latlng, myHtml);' +
               '});'

    
    out <- out +
      .$gmapID() + '.addOverlay(polygon);'

    if(exists("..shown", envir=., inherits = FALSE))
      cat(out)
    else
      .$..runCmds <- c(.$..runCmds, out)
  }
    

  ## ???

  ## handlers -- these call back into R.
  ## this should work for click and dblclick
  widget$writeHandlerJS <- function(., signal, handler=NULL) {
    out <- String() +
      .$setgmapID() +
        'GEvent.addListener(' + .$gmapID() + ',' + shQuote(signal) +
          ',function(overlay, point) {' +
            ## transport bounds
            'var bounds = ' + .$gmapID() + '.getBounds();' +
              'var SW = bounds.getSouthWest();' +
                'var NE = bounds.getNorthEast();' +
                  '_transportToR("' + .$ID + '.SouthWest",Ext.util.JSON.encode({value:SW}));' +
                    '_transportToR("' + .$ID + '.NorthEast",Ext.util.JSON.encode({value:NE}));' +
                      'runHandlerJS(' + handler$handlerID + ',Ext.util.JSON.encode({latlng:point}))' +
                        '});'
    return(out)
  }

  
  container$add(widget, ...)
  invisible(widget)

}
