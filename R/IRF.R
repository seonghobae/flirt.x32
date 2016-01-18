    
    
# Item resonse curves for unidimensional models for binary items 

IRF <- function(alpha=NULL, beta, guess=NULL , inside=NULL, ylim=NULL) {

    inside <- if (is.null(inside)) 0 else if (inside==FALSE) 0 else if (inside==TRUE) 1 # if 1, a(th+b)
    ylim <- if (is.null(ylim)) 0 else ylim 
   
    type <- if (!is.null(beta) && is.null(alpha) && is.null(guess))  "1PL" 
            else if (!is.null(beta) && is.null(guess) && !is.null(alpha)) "2PL"  
            else if (!is.null(beta) && !is.null(guess) && !is.null(alpha)) "3PL" 
        
    nitem <- length(beta)
    if (nitem==1 ) {

    # one item     

        theta <- matrix(seq(-5,5,0.01), ncol=1)
 
        b <- beta    
        
        if (type=="1PL") { a <- 1 ; c <- 0} else if (type=="2PL"){ a<- alpha;  c <- 0 } else if (type=="3PL"){ a<- alpha;   c <- guess }
                
        pr <- apply(theta, 1, prob, a=a, b=b, c=c, inside=inside)

        if (ylim==0) {
            plot(theta, pr, type="l", lty=1, col=1, xlab=expression(theta), ylab="Probability", ylim=c(0,1), xlim=c(-5,5))
            abline(h=0.5, lty=3) 

        } else {
            plot(theta, pr, type="l", lty=1, col=1, xlab=expression(theta), ylab="Probability", ylim=ylim, xlim=c(-5,5))
            abline(h=0.5, lty=3) 
          
        }
        
    } else {
    
    # several items 
        theta <- matrix(seq(-5,5,0.01), ncol=1)

        b <- beta[1]
        if (type=="1PL") { a <- 1 ; c <- 0} else if (type=="2PL"){ a <- alpha[1]; c <- 0 } else if (type=="3PL"){ a <- alpha[1];  c <- guess[1] }        
        
        pr <- apply(theta, 1, prob, a=a, b=b, c=c, inside=inside) 
  
        if (ylim==0) {
            plot(theta, pr, type="l", lty=1, col=1, xlab=expression(theta), ylab="Probability", ylim=c(0,1), xlim=c(-5,5))
            abline(h=0.5, lty=3) 

        } else {
            plot(theta, pr, type="l", lty=1, col=1, xlab=expression(theta), ylab="Probability", ylim=ylim, xlim=c(-5,5))
            abline(h=0.5, lty=3) 
          
        }  
        
        
        for (i in 2:nitem) {
    
            b <- beta[i] ; 
            if (type=="1PL") { a <- 1 ; c <- 0} else if (type=="2PL"){ a <- alpha[i]; c <- 0 } else if (type=="3PL"){ a <- alpha[i];  c <- guess[i] } 
            pr <- apply(theta, 1, prob, a=a, b=b,c=c, inside=inside) 
            
            points(theta, pr, type="l", lty=i, col=i, xlab=expression(theta), ylab="Probability" )
            
        }
    
        legend("topleft", paste("i", 1:nitem, sep=""), col=1:nitem, lty=1:nitem)



    }
        
    
}


# polytomous items based on graded resonse model 
IRF_pol <- function(alpha=NULL, beta, guess=NULL, inside=NULL, ylim=NULL) {

    inside <- if (is.null(inside)) 0 else if (inside==FALSE) 0 else if (inside==TRUE) 1 # if 1, a(th+b)
    ylim <- if (is.null(ylim)) 0 else ylim 
   
    type <- if (!is.null(beta) && is.null(alpha) && is.null(guess))  "1PL" 
            else if (!is.null(beta) && is.null(guess) && !is.null(alpha)) "2PL"  
            else if (!is.null(beta) && !is.null(guess) && !is.null(alpha)) "3PL" 


    # several categories 
        theta <- matrix(seq(-5,5,0.01), ncol=1)
        ncat <- length(beta)
        
        if (type=="1PL") { b <- beta; a <- rep(1, ncat) ; c <- rep(0, ncat)} 
        else if (type=="2PL"){ b <- beta;  a <- alpha; c <- rep(0, ncat) } 
        else if (type=="3PL"){ b <- beta; a <- alpha;  c <- guess }    


        if (ncat==2) {  

            pr0 <- 1- apply(theta, 1, prob, a=a[1], b=b[1],c=c[1], inside=inside)
            #pr1 <- apply(theta, 1, prob, a=a[2], b=b[2],c=c[2], inside=inside) -apply(theta, 1, prob, a=a[3], b=b[3],c=c[3], inside=inside)
            pr1 <-  apply(theta,1, prob, a=a[2], b=b[2],c=c[2], inside=inside  )
    
            if (ylim==0) {
                plot(theta, pr0, type="l", lty=1, col=1, xlab=expression(theta), ylab="Probability", ylim=c(0,1), xlim=c(-5,5))
                abline(h=0.5, lty=3) 
    
            } else {
                plot(theta, pr0, type="l", lty=1, col=1, xlab=expression(theta), ylab="Probability", ylim=ylim, xlim=c(-5,5))
                abline(h=0.5, lty=3) 
              
            }              
               
            points(theta, pr1, type="l", lty=2, col=2, xlab=expression(theta), ylab="Probability")
            #points(theta, pr2, type="l", lty=3, col=3, xlab=expression(theta), ylab="Probability")            
            legend("topleft", c("cat1","cat2"), col=1:2, lty=1:2)


       }else if (ncat==3) {    
             
            pr0 <- 1- apply(theta, 1, prob, a=a[1], b=b[1],c=c[1], inside=inside)
            pr1 <- apply(theta, 1, prob, a=a[2], b=b[2],c=c[2], inside=inside) -apply(theta, 1, prob, a=a[3], b=b[3],c=c[3], inside=inside)
            pr2 <-  apply(theta,1, prob, a=a[3], b=b[3],c=c[3], inside=inside  )
    
            if (ylim==0) {
                plot(theta, pr0, type="l", lty=1, col=1, xlab=expression(theta), ylab="Probability", ylim=c(0,1), xlim=c(-5,5))
                abline(h=0.5, lty=3) 
    
            } else {
                plot(theta, pr0, type="l", lty=1, col=1, xlab=expression(theta), ylab="Probability", ylim=ylim, xlim=c(-5,5))
                abline(h=0.5, lty=3) 
              
            }              
               
            points(theta, pr1, type="l", lty=2, col=2, xlab=expression(theta), ylab="Probability")
            points(theta, pr2, type="l", lty=3, col=3, xlab=expression(theta), ylab="Probability")            
            legend("topleft", c("cat1","cat2","cat3"), col=1:3, lty=1:3)
        
        } else if (ncat==4) {        
        
            pr0 <- 1- apply(theta, 1, prob, a=a[1], b=b[1],c=c[1], inside=inside)
            pr1 <- apply(theta, 1, prob, a=a[2], b=b[2],c=c[2], inside=inside) -apply(theta, 1, prob, a=a[3], b=b[3],c=c[3], inside=inside)
            pr2 <- apply(theta, 1, prob, a=a[3], b=b[3],c=c[3], inside=inside) -apply(theta, 1, prob, a=a[4], b=b[4],c=c[4], inside=inside)
            pr3 <-  apply(theta,1, prob, a=a[4], b=b[4],c=c[4], inside=inside  )
    
            if (ylim==0) {
                plot(theta, pr0, type="l", lty=1, col=1, xlab=expression(theta), ylab="Probability", ylim=c(0,1), xlim=c(-5,5))
                abline(h=0.5, lty=3) 
    
            } else {
                plot(theta, pr0, type="l", lty=1, col=1, xlab=expression(theta), ylab="Probability", ylim=ylim, xlim=c(-5,5))
                abline(h=0.5, lty=3) 
              
            }   
            
            points(theta, pr1, type="l", lty=2, col=2, xlab=expression(theta), ylab="Probability")
            points(theta, pr2, type="l", lty=3, col=3, xlab=expression(theta), ylab="Probability")            
            points(theta, pr3, type="l", lty=4, col=4, xlab=expression(theta), ylab="Probability")      
            legend("topleft", c("cat1","cat2","cat3","cat4"), col=1:4, lty=1:4)
 
 
        } else if (ncat==5) {         

            pr0 <- 1- apply(theta, 1, prob, a=a[1], b=b[1],c=c[1], inside=inside)
            pr1 <- apply(theta, 1, prob, a=a[2], b=b[2],c=c[2], inside=inside) -apply(theta, 1, prob, a=a[3], b=b[3],c=c[3], inside=inside)
            pr2 <- apply(theta, 1, prob, a=a[3], b=b[3],c=c[3], inside=inside) -apply(theta, 1, prob, a=a[4], b=b[4],c=c[4], inside=inside)
            pr3 <- apply(theta, 1, prob, a=a[4], b=b[4],c=c[4], inside=inside) -apply(theta, 1, prob, a=a[5], b=b[5],c=c[5], inside=inside)
            pr4 <-  apply(theta,1, prob, a=a[5], b=b[5],c=c[5], inside=inside  )
    
            
            if (ylim==0) {
                plot(theta, pr0, type="l", lty=1, col=1, xlab=expression(theta), ylab="Probability", ylim=c(0,1), xlim=c(-5,5))
                abline(h=0.5, lty=3) 
    
            } else {
                plot(theta, pr0, type="l", lty=1, col=1, xlab=expression(theta), ylab="Probability", ylim=ylim, xlim=c(-5,5))
                abline(h=0.5, lty=3) 
              
            }  
            points(theta, pr1, type="l", lty=2, col=2, xlab=expression(theta), ylab="Probability")
            points(theta, pr2, type="l", lty=3, col=3, xlab=expression(theta), ylab="Probability")            
            points(theta, pr3, type="l", lty=4, col=4, xlab=expression(theta), ylab="Probability")     
            points(theta, pr4, type="l", lty=4, col=4, xlab=expression(theta), ylab="Probability")     

            legend("topleft", c("cat1","cat2","cat3","cat4","cat5"), col=1:5, lty=1:5)

        }
     
}
