\name{Item}
\alias{Item}
\title{Base Trait for an Item...}
\description{Base Trait for an Item}
\details{An Item combines a model, view and controller interface into one convenient package.
Items may be combined into an ItemGroup or a Dialog to be shown.

The \code{make_ui} method creates the user interface, initializes the model and the controller.
The \code{init_model}, \code{init_controller} and \code{init_ui} do the work.

The model may be shared with different items. See \code{set_model_from_item} or
the \code{instance} proto method.

Items implement the observer interface, so one can add observers to listen for changes to
the properties. (Properties are listed in the property "\code{properties}".)

Items use the Adpater interface to link the model with the view (an Editor). The "\code{properties}"
property lists the names of model properties. One should use "value" for the special one to be
returned by the method \code{to_R}. (This method gathers values from the items after coercion)

When an item's user interface is made, the method \code{on_realized} is called.}
