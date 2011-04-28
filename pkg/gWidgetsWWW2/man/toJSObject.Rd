\name{toJSObject}
\alias{toJSObject}
\title{map an R list object into a string containing javascript code representation of an object...}
\usage{toJSObject(x)
}
\description{map an R list object into a string containing javascript code representation of an object}
\value{a string with the list formatted as code to produce a JavaScript object}
\arguments{\item{x}{a list that will map to an object literal. E.g., list(a=1, b="a") goes to {a:1, b:'a'}}
}
\examples{arg_list <- list(
tooltip = "some tooltip",
width = 200,
height = "auto",
id = String("noquote")
)
toJSObject(arg_list)}

