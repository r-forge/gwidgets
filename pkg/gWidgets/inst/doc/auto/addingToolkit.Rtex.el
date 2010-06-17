(TeX-add-style-hook "addingToolkit.Rtex"
 (lambda ()
    (LaTeX-add-environments
     "RArgs")
    (LaTeX-add-labels
     "fig:hello-world")
    (TeX-add-symbols
     '("RListel" 1)
     '("RArg" 1)
     '("RPackage" 1)
     '("RFunc" 1)
     '("RCode" 1)
     "VERSION")
    (TeX-run-style-hooks
     "jvfloatstyle"
     "fancyhdr"
     "fancyvrb"
     "color"
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

