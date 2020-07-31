#' Plot a \code{grt_wind_fit} object
#' 
#' Plot the object returned by \code{\link{grt_wind_fit}}
#' 
#' @param model A \code{grt_wind_fit} object
#' @param labels Optional names for the labels of dimensions A and B
#' @param ellipse_width Parameter controlling the width of the drawn ellipses
#' 
#' @export
plot.grt_wind_fit_CDL <- function(model, bnd_type="means", labels=c("dim A", "dim B"), ellipse_width=0.8, main = NULL,
    line_width=1, lgnd_vals=c('A1B1', 'A2B1', 'A1B2', 'A2B2'), lgnd_symbols=3,
    lgnd_position='topleft', marginals=TRUE, hideticks=FALSE){
  
  plot.new()
  title(main = main)
  plot_size=3 # used in par(pin=)) settings; size of plot in inches - see ?par 
    # didn't include as argument to avoid alignment problems with marginals
  
  # first plot the main panel
  par(mar=c(4,4,2,2)+0.1,fig=c(0.2,1,0.2,1), lty=1, new=TRUE, lwd=line_width, pin=c(plot_size,plot_size))

  # get range of values
  ranx <- c(min(model$means[,1]-2), max(model$means,1)+2)
  rany <- c(min(model$means[,2]-2), max(model$means,2)+2)

  # draw model$means of distributions
  plot(model$means[,1], model$means[,2], pch=lgnd_symbols, lwd=line_width,
       xlim=ranx,
       ylim=rany,
       xlab=NA,
       ylab=NA,
       xaxt=ifelse(hideticks==TRUE,"n", "s"), 
       yaxt=ifelse(hideticks==TRUE,"n", "s"))
  
  mtext(side=1, text=labels[1], line=0.5)
  mtext(side=2, text=labels[2], line=0.5)
  
    # draw contours of distributions
    ellipse <- function(s,t) {u<-c(s,t)-center; u %*% sigma.inv %*% u / 2}
    n <- 200
    x<-1:200/10-10
    y<-1:200/10-10
    for (i in 1:4){
      center <- model$means[i,]
      sigma.inv <- solve(model$covmat[[i]])
      z <- mapply(ellipse, as.vector(rep(x,n)), as.vector(outer(rep(0,n), y, `+`)))
      contour(x,y,matrix(z,n,n), levels=ellipse_width,drawlabels=F, add=T, lwd=line_width)
    }
  
    
    
    # draw bounds

    if (bnd_type=="means") {
      # Draw mean bounds
      # x-axis
      slope_1 <- -mean(model$indpar$bx1)/mean(model$indpar$by1) # dim 1 slope
      cept_1 <- -mean(model$indpar$a1)/mean(model$indpar$by1) # dim 1 cept
      abline(a = cept_1,
             b = slope_1,
             col=rgb(0,0,0,alpha=1), lwd=line_width)
      # y-axis
      slope_2 <- -mean(model$indpar$bx2)/mean(model$indpar$by2)
      cept_2 <- -mean(model$indpar$a2)/mean(model$indpar$by2)
      abline( a = cept_2,
              b = slope_2,
              col=rgb(0,0,0,alpha=1), lwd=line_width)
      
    } else if (bnd_type=="medians") {
      # Draw median bounds
      # x-axis
      slope_1 <- -median(model$indpar$bx1)/median(model$indpar$by1) # dim 1 slope
      cept_1 <- -median(model$indpar$a1)/median(model$indpar$by1) # dim 1 cept
      abline(a = cept_1,
             b = slope_1,
             col=rgb(0,0,0,alpha=1), lwd=line_width)
      # y-axis
      slope_2 <- -median(model$indpar$bx2)/median(model$indpar$by2)
      cept_2 <- -median(model$indpar$a2)/median(model$indpar$by2)
      abline( a = cept_2,
              b = slope_2,
              col=rgb(0,0,0,alpha=1), lwd=line_width)

    } else if (bnd_type=="allbounds") {
      # Draw all bounds
      # x-axis
      slope_1 <- -model$indpar$bx1/model$indpar$by1 # dim 1 slope
      cept_1 <- -model$indpar$a1/model$indpar$by1 # dim 1 cept
      # y-axis
      slope_2 <- -model$indpar$bx2/model$indpar$by2
      cept_2 <- -model$indpar$a2/model$indpar$by2
      
      for (i in 1:nrow(model$indpar)) {
        abline(a = cept_1[i],
               b = slope_1[i],
               col=rgb(1,0,0,alpha=1), lwd=line_width)
        abline( a = cept_2[i],
                b = slope_2[i],
                col=rgb(0,1,0,alpha=1), lwd=line_width)
      } # for (i in 1:nrow(model$indpar)) 

    } else if (bnd_type=="xbounds") {
      # x-axis
      slope_1 <- -model$indpar$bx1/model$indpar$by1 # dim 1 slope
      cept_1 <- -model$indpar$a1/model$indpar$by1 # dim 1 cept
      for (i in 1:nrow(model$indpar)) {
        abline(a = cept_1[i],
               b = slope_1[i],
               col=rgb(1,0,0,alpha=1), lwd=line_width)
      } # for (i in 1:nrow(model$indpar))
        
    } else if (bnd_type=="ybounds") {  
      # y-axis
      slope_2 <- -model$indpar$bx2/model$indpar$by2
      cept_2 <- -model$indpar$a2/model$indpar$by2
      for (i in 1:nrow(model$indpar)) {
        abline( a = cept_2[i],
                b = slope_2[i],
                col=rgb(0,1,0,alpha=1), lwd=line_width)
      } # for (i in 1:nrow(model$indpar)) 

    } # if (bnd_type=="means") {

    legend(lgnd_position, legend = lgnd_vals, pch = lgnd_symbols)


  if (marginals == TRUE) {
    # add marginal distributions at the bottom
    par(mar=c(1,3.7,1,1.7)+0.1,fig=c(0.2,1,0,0.2), new=T)
    for (i in 1:4){
      x <- 1:100*(ranx[2]-ranx[1])/100+ranx[1]
      y <- dnorm(x,mean=model$means[i,1],sd=sqrt(model$covmat[[i]][1,1]))
      if (i>2){par(new=T,lty=2)} else {par(new=T,lty=1)}
      plot(x,y,type="l", axes=F, ylab="", xlab="", xlim=ranx)
    }
    par(new=T)
    Axis(side=1)
    
    # add marginal distributions to the left
    par(mar=c(3.7,1,1.7,1)+0.1, fig=c(0,0.2,0.2,1), new=T)
    for (i in 1:4){
      x <- 1:100*(rany[2]-rany[1])/100+rany[1]
      y <- dnorm(x,mean=model$means[i,2],sd=sqrt(model$covmat[[i]][2,2]))
      if (i==2 | i==4){par(new=T,lty=2)} else {par(new=T,lty=1)}
      plot(y,x,type="l", axes=F, ylab="", xlab="", ylim=rany)
    }
    par(new=T)
    Axis(side=2)
    
    # add scatterplot if there are predicted and observed values
    if (any(names(model)=="predicted") & any(names(model)=="observed")) {
      par(mar=c(1.5,1.5,1,1), fig=c(0,0.33,0,0.33), new=T)
      plot(model$predicted,model$observed,pch=21,cex=.3,col='gray40',bg='gray40',bty='n', axes=F)
      abline(a=0, b=1, lty=1)
      axis(side=1, at=c(0,1), mgp=c(3,0.5,0))
      axis(side=2, at=c(0,1), mgp=c(3,0.5,0))
    }
  } # if (marginals == TRUE)
  
}