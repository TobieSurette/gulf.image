# Calculates coordinates corresponding to where image gradient maximally converges.
# 'g' Image gradient
nucleus <- function(g){
   # Calculate other gradient values:
   g$magnitude <- sqrt(g$dx^2 + g$dy^2)
   g$slope <- g$dy / g$dx

   # Define minimum point-to-line distance function:
   obj.nucleus <- function(theta){    
      x0 <- theta[["x0"]]
      y0 <- theta[["y0"]]
      
      g$distance <- NA * g$dx
   
      for (i in 1:dim(g$dx)[1]){
         for (j in 1:dim(g$dx)[2]){
             xx <- as.numeric(dimnames(g$dx)$x[j])
             yy <- as.numeric(dimnames(g$dx)$y[i])

             slope <- g$slope[i,j,1]
             intercept <- yy - slope * xx
      
             g$distance[i,j,1] <- abs(intercept + slope * x0 + -y0) / sqrt(1+slope^2)
             if (g$dx[i,j,1] == 0) g$distance[i,j,1] <- abs(y0 - yy)
         }
      }
   
      v <- sum(g$magnitude[,,1] * g$distance[,,1]) / sum (g$magnitude[,,1])
      
      return(v)
   }
   
   # Initialize nucleus coordinate values.   
   theta = c(x0 = mean(as.numeric(dimnames(g$dx)$x)), 
             y0 = mean(as.numeric(dimnames(g$dx)$y)))
   
   theta <- optim(theta, obj.nucleus, control = list(trace = 3))$par
   
   return(theta)
}
