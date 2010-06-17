\name{gWidgets-package}
\alias{gWidgets-package}
\alias{gWidgets}
\alias{installing_gWidgets_toolkits}
\docType{package}
\title{
gWidgets API for building toolkit-independent, interactive GUIs
}
\description{
gWidgets provides a toolkit-independent API for building
interactive GUIs
}
\details{

  The gWidgets package creates an API for interacting with GUI
  toolkits. This package is toolkit neutral. The interactions is
  provided by a further package, such as gWidgetsRGtk2, or gWidgetsrJava.

  Some details on the installation of toolkits is provided by
  \code{installing_gWidgets_toolkits}.
  
Index:
\preformatted{
gWidgets-dialogs        Basic dialog constructors
gWidgets-dnd            Functions to add drag and drop ability to
                        widgets
gWidgets-handlers       Methods to add event handlers to objects
gWidgets-icons          Functions for adding icons
gWidgets-methods        Methods for gWidgets instances
gWidgets-undocumented.Rd
gaction			Reusable objects for buttons, menus, toolbars
                        Undocumented, but exported, functions
gcheckbox               Constructor of widget to indicate whether a
                        value is desired or not
gcommandline            A command line interface
gdf                     Constructor of widget to edit a data frame
gedit                   Constructors for widgets to handle text input
gfile                   Dialogs for file and date selection
ggenericwidget          A constructor to create widgets for evaluating
                        functions
ggraphics               Constructor for a toolkit specific plot device
                        and a notebook to wrap plots in
ggroup                  Containers for packing in subsequent widgets
ghelp                   Widget to interface with help pages
ghtml			widget to show html. (not implemented)
glabel                  Constructors for widgets that show text or
                        images
glayout                 A container for aligning widgets in a table
gmenu                   Constructors to make menubar or toolbars
gnotebook               constructor for notebook widget
gpanedgroup             A paned group holds two widgets with a handle
                        between them to adjust the amount of space
                        allocated to each
gradio                  Widgets to allow selection from a vector of
                        items
gseparator              Widget to draw separator line
gslider                 Constructors for widgets to select a value from
                        a sequence.
gstatusbar              Constructor of status bar widget
gtable                  Constructor for widget to display tabular data
gtree                   Constructor for widget to display heirarchical
                        dta
guiToolkit              Function to select the GUI toolkit used by
                        gWidgets
guiWidget-class         Class "guiWidget" ~~~
gvarbrowser             Widget for browsing environment
gwindow                 Constructor for base container
}
}
\author{
Philippe Grosjean, Michael Lawrence, Simon Urbanek, John
Verzani

Maintainer: John Verzani <gwidgetsrgtk@gmail.com>
}
%\references{}

\keyword{ package }
