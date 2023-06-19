AHBOS <- function(x=x,br="FD"){
  order_back <- order(order(x))      #sort
  n <- length(x)

  hist_inf<-hist(x,breaks=br,plot=F) # histogram
  denss <- hist_inf$density          # density obtained from histogram
  dens_ma <- c(0,denss,0)            # dummy bins to left and right
  ma <- filter(dens_ma, rep(1/3,3))
  
  l <- length(ma)
  ma2<- ma[c(-1,-(l))]               # remove dummy bins after calculations
  dens<-rep(ma2,hist_inf$counts)     # new densities for observations
  
  if (length(unique(dens))==1){
    d2<- (1/n)
  } else {
    d2 <- dens/max(dens)             # normalization (max density 1)
    
  }
  score<-log10(1/d2)[order_back]   # final score (ordered back)
  return(score)
}  #AHBOS Function

dyn_hist <- function(x,plt=F){
  
  order_back <- order(order(x))  #sort
  y <- sort(x)  
  
  n <- length(x)
  k <- round(sqrt(n))            #bin number for dynamic histogram
  wid <- n%/%k                   #remainder       
  r <- n-wid*k
  br1 <- (1:k)*wid
  r2 <- ifelse((k %% 2) == 0,k/2+1,(k+1)/2+1) ## put remainder obs to bin where median is located
  br1[r2:k] <- br1[r2:k]+r 
  bin_dyn <- unique(y[c(1,br1)])              
  
  hist_inf2<- hist(x,breaks=bin_dyn,plot = plt) # dynamic histogram
  max.den <- max(hist_inf2$density)
  scale.den <- hist_inf2$density/max.den        # normalization (max density 1)
  dens2<-rep(scale.den,hist_inf2$counts)  
  score2<-log10(1/dens2)[order_back]            # final score (ordered back)
  return(score2)
}   #Dynamic HBOS function
###################################################