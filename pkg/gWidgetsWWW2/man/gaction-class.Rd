% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/gaction.R
\docType{class}
\name{GAction-class}
\alias{GAction}
\alias{GAction-class}
\title{Class for gaction}
\description{
Class for gaction
}
\section{Methods}{

\describe{
\item{\code{add_keybinding(key)}}{Add keybinding to document for this action. Key is value for Ext.EventObject: http://docs.sencha.com/ext-js/4-1/#!/api/Ext.EventObject. Use Ctrl-X, Alt-X of Shift-X indicate keys}

\item{\code{connect_to_toolkit_signal(signal, ...)}}{Connect signal of toolkit to notify observer}

\item{\code{get_value(...)}}{Get main property, Can't query widget, so we store here}

\item{\code{set_value(value, ...)}}{Set main property, invoke change handler on change}
}}

