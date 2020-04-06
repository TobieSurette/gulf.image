#' Label groups of connected elements
#'
#' @param z Numerical or logical vector or matrix.
#' 
#' @examples 
#' # Create simple image:
#'  z = rbind(c(1,1,1,0,0,0,0,1,0,0),
#'            c(0,1,1,0,0,0,0,1,0,0),
#'            c(0,0,0,1,0,0,0,0,0,0),
#'            c(0,0,0,1,1,0,1,1,0,0),
#'            c(0,0,1,1,0,0,0,1,0,0),
#'            c(0,0,0,0,0,0,0,0,0,0),
#'            c(1,1,1,0,0,0,0,0,1,0))
#' z = cbind(0, rbind(0, z, 0), 0)
#' 
#' v <- label(z)
#' image(v) 
#' 
#' # Label objects for a large image:
#' z <- matrix((runif(1000000) > 0.6)+1-1, nrow = 1000)
#' v <- label(z)
#' image(v)
#' 
#' @export label

label_1d <- function(x){
   v <- cumsum((x-c(0, x[-length(x)])) == 1) 
   v[x == 0] <- 0
   return(v)
}

label_2d <- function(x){
   image <- rbind(0, cbind(0, x, 0), 0) # Define image and add border.
   m <- nrow(image)
   n <- ncol(image) 
   connected <- matrix(0, nrow = m, ncol = n)

   mark <- 1 # Object label counter.
   for (i in 1:m){
      for (j in 1:n){
         index <- (j-1) * m + i
         if (image[index] == 1){
            connected[i,j] <- mark
            while (length(index) > 0){
               image[index] <- 0 
               
               # Find surrounding object pixels:
               neighbours <- unique(c(index + m, index - m, index + 1, index - 1))
               neighbours <- neighbours[neighbours > 0 & (neighbours <= (m*n))]
               index <- neighbours[image[neighbours] > 0]
               
               # Mark neighbours as connected:
               connected[index] <- mark
            }
            mark <- mark + 1
         }
      }
   }
   
   connected <- connected[c(-1, -m), c(-1, -n)]
   
   return(connected)
}

label <- function(z){
   # Check input parameters:
   if (is.factor(z))       z <- as.numeric(z)
   if (!is.numeric(z))     stop("'z' must be numeric or logical.")
   if (length(dim(z)) > 2) stop("'z' must be a vector or matrix.")
  
   if (is.null(dim(z))) v <- label_1d(z) else v <- label_2d(z)
   
   return(v)
}

z <- matrix((runif(1000000) > 0.6)+1-1, nrow = 1000)
