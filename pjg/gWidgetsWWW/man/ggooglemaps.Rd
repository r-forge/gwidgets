\name{ggooglemaps}
\alias{ggooglemaps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Contructor for Google maps widget}
\description{

  This creates a widget to show a map provided by Google.  One can
  specify the center of the map, markers on the map and add line
  semgemnts and polygons. This extension to \pkg{gWidgets} is
  \pkg{gWidgetsWWW} specific.

}
\usage{
ggooglemaps(x, title = "", type = c("map", "panorama"), 
key = "ABQIAAAAYpRTbDoR3NFWvhN4JrY1ahS5eHnalTx_x--TpGz1e2ncErJceBS7FrNBqzV5DPxkpbheIzZ9nTJPsQ",
container, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Location of center of map. Given as a string containing an
    address, or as a vector of latitude and longitude values. }
  \item{title}{The center is marked. This sets the text for the tooltip.}
  \item{type}{Determines type of map. One of "map" or "panorama".}
  \item{key}{A google API key. The default is for localhost 127.0.0.1:8079, which hopefully just works.}
  \item{container}{A \pkg{gWidgetsWWW} container object}
  \item{\dots}{ignored}
}
\details{
  This widget was created using the code posted to the ExtJS blog by
  Shea Frederick
  \url{http://extjs.com/blog/2008/07/01/integrating-google-maps-api-with-extjs/}
  . 
  
  This widget requires the HTML page that will render it have the DOCTYPE of the page set according to
  these instructions:
  \url{http://code.google.com/apis/maps/documentation/index.html#XHTML_and_VML}

  Next, the website the page will be listed on must register to have an
  API key: \url{http://code.google.com/apis/maps/signup.html}. NOT WORKING:This key is
  specifies through the \code{key} argument. The default will work for the domain \code{127.0.0.1:8079}.

    The key can be specified in the header of the html page that will render the gWidgets code. The following will work for the domain 127.0.0.1:8079:

\code{<script
src="http://maps.google.com/maps?file=api&v=2.x&key=ABQIAAAAYpRTbDoR3NFWvhN4JrY1ahS5eHnalTx_x--TpGz1e2ncErJceBS7FrNBqzV5DPxkpbheIzZ9nTJPsQ"
type="text/javascript"></script>}
  
  This widget is experimental. Its API is still not settled. As of
  now, the  method \code{svalue<-} is used to set the center of the
  map. After rendering this is done using a data vector of longitude
  and latitude. Prior to rendering an address can be used.
  
  The \code{[} method will return the markers locations, but does not
  reflect their position after if they are moved with the mouse. 
  
  The \code{[$<$-} method can be used to set markers. The value can
  These are specified in a 2 or 3 column data frame with columns of
  latitude, longitude and the optional title. After the widget has
  rendered, the \pkg{proto} method \code{widget$addMarker} is more efficient.
  
  
  The handlers \code{addHandlerClicked} and \code{addHandlerDoubleclick}
  should work. The first argument of the handler, \code{h}, is, as
  usual, a list. The component \code{latlng} contains a vector with
  the latitude and longitude of the location the mouse click occurred at.
  
  More of the Google maps API is availble through some \pkg{proto}
  methods.  These methods are called using the dollar sign notation, as
  in \code{widget\$methodname(methodargs)}.
  
  The available \pkg{proto} methods and their signatures are:

  \describe{
  \item{widget\$getBounds()}{Returns a list with the lower and upper
    coordinates of the current map.}
  
  \item{widget\$panTo(latlng)}{Sets map center to new coordinates
    specified using a numeric vector of latitude and longitude.}
  
  \item{widget\$setZoom}{Sets new zoom value. Default is 14}
  
  \item{widget\$openInfoWindow(latlng, myHTML)}{Pops up message at point
    specified by \code{latlng}}
  
  \item{widget\$addMarker(latlng, title, draggable=FALSE)}{Add a new marker at the
    specified point with tooltip. These markers may be draggable. A popup
    shows the new coordinates after dragging. However, the values
    returned by \code{[} are not aware of the new coordinates.}
  
  \item{widget\$addPolyline(latlng, color = "\#ff0000", pixel.width = 5,
    opacity = 1}{Like the \code{\link{lines}} graphic function, this
    method adds line segments to the map. The points are specified in
    \code{latlng}, which is a matrix or data frame with columns of
    latitude and longitude values. If this is missing, the values set
    via \code{[$<$-} and \code{addMarker} are used. The color is set
    using hexadecimal RGB notation, eg.  "\#ff0000". Opacity levels less
    than 1 allow the map to bleed through. When the line is clicked, its
    length in meters is given.}
  
  \item{widget\$addPolygon(latlng,border.color="\#ff0000",
    border.pixel.width = 5,border.opacity = 1,region.color =
    "\#000000",region.opacity = .1)}{ Like the \code{\link{polygon}}
    graphics function, this method draws a polygon, rendering it on the
    map. The points are specified by a matrix (or data.frame) to
    \code{latlng}. If missing, the values set via \code{[$<$-} or
    \code{addMarker} are used.. When the polygon is clicked its area in
    square meters is presented.}
  }   

  Adding additional methods from the Google API is certainly possible. 
  
}
\value{
  Returns a \pkg{gWidgetsWWW} object.
}
\references{
	\url{http://extjs.com/blog/2008/07/01/integrating-google-maps-api-with-extjs/},
	\url{http://code.google.com/apis/maps/}
}
\author{John Verzani }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}

