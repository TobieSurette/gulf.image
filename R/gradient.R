#' Image gradient
#' 
#' @description Calculate the gradient for an image.
#' 
#' @param x Matrix or RBG image
#' @param type Character string indicating the type of grdient kernel to use ('sobel', 'scharr')
#' @param kernel Character string indicating the dimension of the kernel to be used (default = 3).
#' @param polar Logical value specifying whether the output is in polar coordinates.
#' 
#' @return Returns a list of image matrices. Regular output returns x and y gradients while polar coordinates return magnitude and angles.
#' 
#' @export gradient
#' 
gradient <- function(x, type = "sobel", kernel = "3x3", polar = FALSE, slope = FALSE, intercept = FALSE){
   if (kernel == "3x3"){
       if (type == "sobel") M <- c(1,2,1)
       if (type == "scharr") M <- c(47, 162, 47)
       M <- cbind(M, 0, -M)   
   }

   if (kernel == "5x5"){
       if (type == "sobel") M <- c(1,1,2,1,1)
       M <- rbind(2*M, M, 0, -M, -2*M)   
   }   
   
   d <- round(nrow(M)/2) 
   
   # Calculate x and y gradients:
   if (length(dim(x)) == 2) dim(x) <- c(dim(x), 1)
   Ax <- rep(0, length(x))
   dim(Ax) <- dim(x)
   Ay <- Ax
   pp <- d:(dim(Ax)[1]-d+1) # Pixels being calculated.
   
   # Calculate gradient:
   for (i in 1:nrow(M)){
      for (j in 1:ncol(M)){
         for (k in 1:dim(x)[3]){
            ii <- i:(dim(x)[1]-nrow(M)+i)
            jj <- j:(dim(x)[2]-ncol(M)+j)
            Ax[pp,pp,k] <- Ax[pp,pp,1] + M[i,j] * x[ii,jj,k] 
            Ay[pp,pp,k] <- Ay[pp,pp,1] + M[j,i] * x[ii,jj,k]
         }
      }
   }

   # Label dimensions:
   dimnames(Ax) <- dimnames(x)
   dimnames(Ay) <- dimnames(x)
   
   v <- list(dx = Ax, dy = Ay)
   
   if (polar){
      # Calculate gradient magnitude:
      v$magnitude <- sqrt(Ax^2 + Ay^2)
      
      # Calculate gradient angle:
      v$angle <- atan2(Ay, Ax)
   }
   
   # Calculate the y slope the gradient at each coordinate:
   if (slope) v$slope <- v$dy / v$dx
   
    # Calculate the y intercept the gradient at each coordinate:
   if (intercept){
      xx <- repvec(as.numeric(dimnames(z)$x), nrow = length(dimnames(z)$y))
      yy <- repvec(as.numeric(dimnames(z)$y), ncol = length(dimnames(z)$x))

      if ("slope" %in% names(v)) v$intercept <- yy - v$dy / m$dx * xx
   } 
  
   return(v)
}
