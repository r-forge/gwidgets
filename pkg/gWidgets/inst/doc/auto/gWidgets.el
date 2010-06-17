(TeX-add-style-hook "gWidgets"
 (lambda ()
    (LaTeX-add-environments
     "RArgs")
    (LaTeX-add-labels
     "fig:basic-widgets"
     "fig:hello-world"
     "fig:confirmDialog"
     "sec:browseEnv"
     "sec:repeating-plot"
     "fig:gtkdensity"
     "fig:Rmail"
     "fig:doPlot"
     "fig:notebook"
     "fig:filebrowser"
     "sec:using-ggenericwidget"
     "sec:an-altern-ggenericwidget"
     "fig:gfunction")
    (TeX-add-symbols
     '("RListel" 1)
     '("RArg" 1)
     '("RPackage" 1)
     '("RFunc" 1)
     '("RCode" 1)
     "VERSION")
    (TeX-run-style-hooks
     "fancyhdr"
     "fancyvrb"
     "color"
     "hyperref"
     "url"
     "amsfonts"
     "amsmath"
     "relsize"
     "floatflt"
     "graphicx"
     "mathptm"
     "geometry"
     "times"
     "latex2e"
     "art12"
     "article"
     "12pt")))

