## hack to add separator
gseparator <- function(horizontal = TRUE, container = NULL, ...)  {
  if(is.null(container)) {
    obj <- "gseparator stub"
    class(obj) <- c("gSeparator", class(obj))
    return(obj)
  }
    
  if(horizontal)
    return(ghtml("<hr>",cont=container))
  else
    return()
}

