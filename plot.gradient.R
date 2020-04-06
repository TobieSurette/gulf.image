# Plot various image gradients
plot.gradient <- function(x, type = "vector"){

   for (i in 1:dim(z)[1]){
      for (j in 1:dim(z)[2]){
         points(as.numeric(dimnames(z)$x[j]), as.numeric(dimnames(z)$y[i]), cex = 0.1)
         xx <- as.numeric(dimnames(z)$x[j])
         yy <- as.numeric(dimnames(z)$y[i])

         m$slope[i,j,k] <- m$dy[i,j,k] / m$dx[i,j,k]
         m$intercept[i,j,k] <- yy - m$slope[i,j,k] * xx
      
         xx <- c(xx - m$dx[i,j,k], xx + m$dx[i,j,k])
         yy <- c(yy - m$dy[i,j,k], yy + m$dy[i,j,k])
         lines(xx, yy)
         arrows(xx[1], yy[1], xx[2], yy[2], length = 0.1 * m$magnitude[i,j,1] / max(m$magnitude[,,1]))
      }
   }

}
   
   # Do image vector field gradient plot:
   plot(c(xlim[1]-1, xlim[2]+1), c(ylim[1]-1, ylim[2]+1), type = "n", 
        xaxs = "i", yaxs = "i", xlab = "", ylab = "")
   
   rasterImage(z[dim(z)[1]:1,,], xlim[1]-0.5, ylim[1]-0.5, xlim[2]+0.5, ylim[2]+0.5, interpolate = FALSE)

   
}
   
