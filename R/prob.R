
# compute probability 
prob <- function(theta, a, b, c, inside) {
    
    if (inside==0) {
        pr <- c + { (1-c) / (1 + exp(-1 * (a*theta+b)) )   }  #exp(a*theta+b) / (1 + exp(a*theta+b) )
    } else {
        pr <- c + { (1-c) / (1 + exp(-1 * a * (theta+b)) )   } 
    }
    return(pr)
    
}
    
# count the number of categories 
count <- function(data) {
    data[data==-1] <- NA 
    con <- length(table(data))
    return(con)

}
