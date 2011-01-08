\name{gWidgets-classes}
\alias{gWidgets-classes}

\alias{gAction-class}
 \alias{gButton-class}
 \alias{gCalendar-class}
 \alias{gCheckbox-class}
 \alias{gCheckboxGroup-class}
 \alias{gCombobox-class}
 \alias{gCommandline-class}
 \alias{gDf-class}
 \alias{gDfNotebook-class}
 \alias{gEdit-class}
 \alias{gExpandGroup-class}
 \alias{gFilebrowse-class}
 \alias{gFormLayout-class}
 \alias{gFrame-class}
 \alias{gGenericWidget-class}
 \alias{gGraphics-class}
 \alias{gGraphicsNotebook-class}
 \alias{gGridComponent-class}
 \alias{gGroup-class}
 \alias{gHelp-class}
 \alias{gHelpBrowser-class}
 \alias{gHtml-class}
 \alias{gImage-class}
 \alias{gLabel-class}
 \alias{gLayout-class}
 \alias{gMenu-class}
 \alias{gNotebook-class}
 \alias{gPanedGroup-class}
 \alias{gRadio-class}
 \alias{gSeparator-class}
 \alias{gSlider-class}
 \alias{gSpinbutton-class}
 \alias{gStatusbar-class}
 \alias{gSvg-class}
 \alias{gTable-class}
 \alias{gText-class}
 \alias{gToolbar-class}
 \alias{gTree-class}
 \alias{guiComponentRangeSelector-class}
 \alias{guiComponentWithItems-class}
 \alias{gVarBrowser-class}
 \alias{gWindow-class}
 \alias{gCommandlineANY-class}
 \alias{gGenericWidgetANY-class}
 \alias{gFormLayoutANY-class}
 \alias{gHelpbrowserANY-class}
 \alias{gHelpANY-class}

  
\title{Classes for gWidgets instances}
\description{
  Classes for gWidgets objects
}

\details{	
  The gWidgets API is designed to have a double dispatch. The gWidgets
  package calling methods in the gWidgetsXXX packages. As such, the real
  class structure sits inside the toolkit packages. These classes are
  merely here for organization and some day documentation.

  The "ANY" classes are a means to write compound widgets in gWidgets
  that work across the toolkits.

  gAction gButton gCalendar gCheckbox gCheckboxGroup gCombobox
  gCommandline  gDf gDfNotebook gEdit gExpandGroup
  gFilebrowse gFormLayout gFrame gGenericWidget
  gGraphics gGraphicsNotebook gGroup gHelp 
  gHelpBrowser  gHtml gImage gLabel gLayout gMenu
  gNotebook gPanedGroup gRadio gSeparator gSlider gSpinbutton
  gStatusbar gSvg gTable gText gToolbar gTree guiComponentRangeSelector
  guiComponentWithItems gVarBrowser gWindow

  gCommandlineANY
  gGenericWidgetANY
  gFormLayoutANY
  gHelpbrowserANY
  gHelpANY

}
\keyword{interface}% at least one, from doc/KEYWORDS
