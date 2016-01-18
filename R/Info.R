

# Item information function for unidimensional models and binary items 

Item_info <- function(alpha=NULL, beta, guess=NULL, inside=NULL, ylim=NULL) {

    inside <- if (is.null(inside)) 0 else if (inside==FALSE) 0 else if (inside==TRUE) 1 # if 1, a(th+b)
    ylim <- if (is.null(ylim)) NULL else ylim 
   
    type <- if (!is.null(beta) && is.null(alpha) && is.null(guess))  "1PL" 
            else if (!is.null(beta) && is.null(guess) && !is.null(alpha)) "2PL"  
            else if (!is.null(beta) && !is.null(guess) && !is.null(alpha)) "3PL" 
   
   
   nitem <- length(beta) 
   
   if (nitem == 1 ) {
   
   # one item 
        theta <- matrix(seq(-5,5,0.01), ncol=1)
        
        b <- beta
        
        if (type=="1PL") { a <- 1 ; c <- 0} else if (type=="2PL"){ a <- alpha;  c <- 0 } else if (type=="3PL"){ a <- alpha; c <- guess }
         
        pr <- apply(theta, 1, prob, a=a, b=b, c=c, inside=inside)
        
        inf <-(a^2 * (pr - c )^2  * (1-pr)) / ( (1-c)^2 * pr) 
        
        if (is.null(ylim)) {
            plot(theta, inf, type="l", lty=1, col=1, xlab=expression(theta), ylab=expression(I(theta)))

        } else {
            plot(theta, inf, type="l", lty=1, col=1, xlab=expression(theta), ylab=expression(I(theta)), ylim=ylim)
        }  
   
   }else {
    
    # several items 
        theta <- matrix(seq(-5,5,0.01), ncol=1)
        
        b <- beta[1]
        
        if (type=="1PL") { a <- 1 ; c <- 0} else if (type=="2PL"){ a<- alpha[1];  c <- 0 } else if (type=="3PL"){  a<- alpha[1]; c <- guess[1] }
         
        pr <- apply(theta, 1, prob, a=a, b=b, c=c, inside=inside)
        
        inf <-(a^2 * (pr - c )^2  * (1-pr)) / ( (1-c)^2 * pr) 
        
        if (is.null(ylim)) {
            plot(theta, inf, type="l", lty=1, col=1, xlab=expression(theta), ylab=expression(I(theta)))

        } else {
            plot(theta, inf, type="l", lty=1, col=1, xlab=expression(theta), ylab=expression(I(theta)), ylim=ylim)
          
        }
        
        for (i in 2: nitem) {
    
            b <- beta[i]
            if (type=="1PL") { a <- 1 ; c <- 0} else if (type=="2PL"){ a<- alpha[i];  c <- 0 } else if (type=="3PL"){  a<- alpha[i]; c <- guess[i] }
        
            pr <- apply(theta, 1, prob, a=a, b=b,c=c, inside=inside)
            inf <- (a^2 * (pr - c )^2  * (1-pr)) / ( (1-c)^2 * pr) 

            points(theta, inf, type="l", lty=i, col=i, xlab=expression(theta), ylab=expression(I(theta)))
        
        }
    
        legend("topleft", paste("i", 1:nitem, sep=""), col=1:nitem, lty=1:nitem)
 
    
    } 
    
}


# Test information function for unidimensional models and binary items 

Test_info <- function(alpha=NULL, beta, guess=NULL, col=NULL, lty=NULL, inside=NULL, ylim=NULL) {


    lty <- if (is.null(lty)) 1 else lty 
    col <- if (is.null(col)) 2 else col 
    inside <- if (is.null(inside)) 0 else if (inside==FALSE) 0 else if (inside==TRUE) 1 # if 1, a(th+b)
    ylim <- if (is.null(ylim)) NULL else ylim 
   
    type <- if (!is.null(beta) && is.null(alpha) && is.null(guess))  "1PL" 
            else if (!is.null(beta) && is.null(guess) && !is.null(alpha)) "2PL"  
            else if (!is.null(beta) && !is.null(guess) && !is.null(alpha)) "3PL" 
   
   
    theta <- matrix(seq(-5,5,0.01), ncol=1)
    nitem <- length(beta) 

    inf <- matrix(NA, length(theta), nitem)
    for (i in 1: nitem ) {
        b <- beta[i]
        if (type=="1PL") { a <- 1 ; c <- 0} else if (type=="2PL"){ a<- alpha[i];  c <- 0 } else if (type=="3PL"){  a<- alpha[i]; c <- guess[i] }
         
        pr <- apply(theta, 1, prob, a=a, b=b, c=c, inside=inside)
        
        inf[,i] <-(a^2 * (pr - c )^2  * (1-pr)) / ( (1-c)^2 * pr) 
    
    }
    
    test_inf <- apply(inf,1,sum, na.rm=T)
    
    if (is.null(ylim)) {
        plot(theta, test_inf, type="l", lty=lty, col=col, xlab=expression(theta), ylab=expression(I(theta)))

    } else {
        plot(theta, test_inf, type="l", lty=lty, col=col, xlab=expression(theta), ylab=expression(I(theta)), ylim=ylim)
      
    }
    
    
}
