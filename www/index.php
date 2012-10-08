
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<h3>About</h3>
  A brief description of the packages available from this r-forge project:
<ul>
  <li>gWidgets: an API for writing graphical user interfaces (GUIs)
  within R. The package is cross-platform (Linux/Mac/Windows) and
  cross-toolkit (RGtk2, qtbase, tcltk). An RNews article is
       <a href=http://www.r-project.org/doc/Rnews/Rnews_2007-3.pdf>here</a>. The
  gWidgets package needs a further "gWidgetsXXX" package to provide the connection
  to the graphical toolkit.
</li>
<li>(The gWidgets package is being replaced by gWidgets2 which is hosted on github. See the files here: <a href='https://github.com/jverzani'>https://github.com/jverzani</a>
</li>

  <li>gWidgetsRGtk2: needed to use the <a href=http://www.ggobi.org/rgtk2/>RGtk2</a> package for the graphical toolkit</li>

  <li>gWidgetstcltk: needed to use the tcltk package for the graphical toolkit</li>

  <li>gWidgetsQt: needed to use the <a href=https://r-forge.r-project.org/R/?group_id=454>qtbase</a> package for the graphical toolkit</li>

  <li>gWidgetsrjava: needed to use the rjava package for the graphical
  toolkit. <em>This package is not being maintained. For Java-based GUIs please see the deducer project.</em></li>
  <li>gWidgetsWWW: A standalone implementation of the gWidgets API
  that allows one to easily make dynamic web sites, either locally or,
     with rapache, to be served to a remote user. See <a href='www.jstatsoft.org/v49/i10/paper'>a JSS article</a> for some details. (This package is replaced by the
						   gWidgetsWWW2 and gWidgetsWWW2.rapache packages on the github site). 						   
</li>
  <li>pmg: This is a GUI for R written using gWidgets, primarily aimed around its use in the
  classroom. 

     The package is described <a href=http://www.amstat.org/publications/jse/v16n1/verzani.pdf>here</a>.

</li>
  <li>traitr: an alternate interface for programming GUIs which uses
  gWidgets for the graphical presentation</li>
</ul>


<h3>Examples</h3>
A basic hello world application can be made as follows:
<pre>
library(gWidgets)
options(guiToolkit="RGtk2")                     # avoid question if more than one is installed
w <- gwindow("Hello world example")             # top level window
g <- ggroup(cont=w, horizontal=FALSE)           # a box container, added to w
b <- gbutton("Click me for a message", cont=g)  # add button to container g
addHandlerClicked(b, handler=function(h,...) {  # add interactivity through a handler
      galert("Hello world", parent=h$obj)
})
</pre>

     <h3>More documentation/examples</h3>
<p>
     The gWidgets vignette is a source of further examples. The vignette for gWidgets2 is shortened, but still has some exmaples. (See <a href='https://github.com/jverzani/gWidgets2/blob/master/vignettes/gWidgets2.pdf'>a copy here</a>.)


<p>
The text <em>Programming Graphical User Interfaces in R</em> has a part on gWidgets
     



<!-- end of project description -->


<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

<p>Old web site <a href=http://wiener.math.csi.cuny.edu/pmg/>here</a>.</p>
</body>
</html>
