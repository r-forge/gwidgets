### these classes need to be defined before their subclasses. Alphabetical doesn't cut
### it so they go here.

## for coerce.with
setClassUnion("NULLorFunction",c("NULL","function"))

### this must come after aaaGenerics, as there gComponentrJava is defined
setClass("gEditrJava",
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )
setClass("gGrouprJava",
         representation = representation("gComponentrJava",
           horizontal="logical"),
         contains="gContainerrJava",
         prototype=prototype(new("gContainerrJava"))
         )

setClass("gNotebookrJava",
         representation = representation("gComponentrJava",
           closebuttons="logical",
           dontCloseThese="numeric"),
         contains="gComponentrJava"
         )
setClass("gWindowrJava",
         contains="gContainerrJava",
         prototype=prototype(new("gContainerrJava"))
         )

