#' Image pixel coordinates
#' 
#' Returns image pixel coordinates.
#' 
#' @param x Matrix or RBG image
#' @param expand Logical value specifying whether to expand the pixel coordinate values to cover the size of the image.
#' 
#' @return Returns a list of x and y coordinates.
#' @export coordinates

coordinates <- function(x, expand = FALSE){
   xx <- as.numeric(dimnames(x)$x)
   yy <- as.numeric(dimnames(x)$y)
   if (expand){
      xx <- repvec(as.numeric(dimnames(z)$x), nrow = length(dimnames(z)$y))
      yy <- repvec(as.numeric(dimnames(z)$y), ncol = length(dimnames(z)$x))
   }
   
   return(list(x = xx, y = yy))
}
