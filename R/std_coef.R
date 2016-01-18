
## Function for standardized loadings and covariance matrix 
## For 2PL and bifactor models 

std_coef <- function(est, dim_info, cov_matrix) {

    # est: a vector of parameter estiamtes (loading/slope)
    # dim_info: a list of item numbers for each dimension 
    # cov_matrix: a square numeric matrix of variance_covariance
     
    if(length(unlist(dim_info)) != length(est)) 
    stop("dimension information shoul be provided for all estimates")
    
    # n of dimensions 
    n_dim <- length(dim_info)
    d <- dim(cov_matrix)
    
    if (!is.numeric(cov_matrix) || length(d) != 2L || n_dim != d[2L]) 
        stop("cov_matrix is not a square numeric matrix")
        
    Is <- sqrt(1/diag(cov_matrix))
    
    if (any(!is.finite(Is))) 
        warning("diag(.) had 0 or NA entries; non-finite result is doubtful")    
        
    std_est <- rep(NA, length(est)) 
    rev_sd_vec <- rep(NA, n_dim)
    for (i in 1: n_dim) {  
        std_est[dim_info[[i]]] <- est[dim_info[[i]]] *  sqrt(cov_matrix[i,i])
        rev_sd_vec[i] <- 1/sqrt(cov_matrix[i,i])
    }

    
    cor_mat <- rev_sd_vec * cov_matrix * rep(rev_sd_vec, each=n_dim)

    return(list("std_est"=std_est, "cor_mat"=cor_mat)) 

}

## correlation matrix 
## For 1PL models 

std_cov <- function(dim_info, cov_matrix) {

    # dim_info: a list of item numbers for each dimension 
    # cov_matrix: a square numeric matrix of variance_covariance
         
    # n of dimensions 
    n_dim <- length(dim_info)
    d <- dim(cov_matrix)
    
    if (!is.numeric(cov_matrix) || length(d) != 2L || n_dim != d[2L]) 
        stop("cov_matrix is not a square numeric matrix")
        
    Is <- sqrt(1/diag(cov_matrix))
    
    if (any(!is.finite(Is))) 
        warning("diag(.) had 0 or NA entries; non-finite result is doubtful")    
         
    cor_mat <- Is* cov_matrix * rep(Is, each=n_dim)

    return("cor_mat"=cor_mat) 

}
