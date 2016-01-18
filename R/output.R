# output class 
setClassUnion("mat.null",c("matrix","NULL"))  
setClassUnion("mat.vec.null",c("matrix","vector","NULL"))
setClassUnion("num.null",c("numeric","NULL"))
setClassUnion("list.null",c("list","NULL"))
setClassUnion("char.null",c("character","NULL"))

setClass(
        Class ='flirtClass',
        representation = representation( direct = 'character',
            pars='mat.vec.null',  se = 'num.null', se_emp='num.null', parms='num.null', out_cov='list.null', 
            first_order='num.null', second_order='num.null', variances_first='num.null', stand_first_order='num.null', cor_first_second='num.null', 
            info_num ='mat.null', info_emp='mat.null',
            loglik='numeric', AIC='numeric',BIC='numeric',npar='numeric' ,
            post = 'list.null', 
            data_inf = 'list.null', dim_inf = 'list.null', 
            inside ='char.null', model = 'num.null', 
            dif_beta='num.null', dif_alpha = 'num.null', 
            est_inf ='list.null', model_name = 'character', Call='call'),
        validity = function(object) return(TRUE)
)

# output file 


output <- function(model, names, direct, show) {


#############################################
#    1. Run and read from  the input file 
#############################################

    if (model==1) {
        if (show==0) system("uniRasch", show.output.on.console = F) else system("uniRasch", show.output.on.console = T)
        model_name <- "Unidimensional 1PL Model Family"
        file_name <- "input_uni.txt"
    }
    if (model==2) {
        if (show==0) system("uni_2pl", show.output.on.console = F) else system("uni_2pl", show.output.on.console = T)
        model_name <- "Unidimensional 2PL Model Family"
        file_name <- "input_2pl.txt"
    }
    if (model==3) {
        if (show==0) system("mulRasch", show.output.on.console = F) else system("mulRasch", show.output.on.console = T)
        model_name <- "Multidimensional 1PL Model Family"
        file_name <- "input_mul.txt"
    }    
    if (model==4) {
        if (show==0) system("mul2pl", show.output.on.console = F) else system("mul2pl", show.output.on.console = T)
        model_name <- "Multidimensional 2PL Model Family"
        file_name <- "input_2pl_mul.txt"
    }
    if (model==5) {
        if (show==0) system("bifactor", show.output.on.console = F) else system("bifactor", show.output.on.console = T) 
        model_name <- "Bifactor Model Family"
        file_name <- "input_bifactor.txt"
    }

    if (model==6) {
        if (show==0) system("second", show.output.on.console = F) else system("second", show.output.on.console = T) 
        model_name <- "Second-order Model Family"
        file_name <- "input_secondorder.txt"
    }

    if (model==7) {
        if (show==0) system("uni_3pl", show.output.on.console = F) else system("uni_3pl", show.output.on.console = T) 
        model_name <- "Unidimensional 3PL Model Family"
        file_name <- "input_3pl.txt"
    }

    Call <- match.call()                 

    # error 
    if(!file.exists("parms.txt")) stop("Error in BNL. For details, use the control option, show=TRUE") 
 
    # names 
    
    name_pp_mg <- if(is.vector(names@name_pp_mg)) names@name_pp_mg else NULL  
    name_it_cov_beta <- if(is.vector(names@name_it_cov_beta)) names@name_it_cov_beta else NULL   
    name_it_cov_alpha <- if(is.vector(names@name_it_cov_alpha)) names@name_it_cov_alpha else NULL   
    name_pp_cov <- if(is.vector(names@name_pp_cov)) names@name_pp_cov else NULL  
     
    # directory
    direct <- direct  


    # read data from the input file            
    fname <- as.character(read.table(paste(file_name,sep=""), 
            skip=0, nrows=1 , sep = "" )[1,2] ) 
    resp <- read.table(paste(fname, sep=""))

    fname <- as.character(read.table(paste(file_name,sep=""), 
            skip=1, nrows=1 , sep = "" )[1,2] ) 
    person_cov <- if(fname=="0") 0 else  read.table(paste(fname, sep=""))

    on_theta <- as.numeric(read.table(paste(file_name,sep=""), 
            skip=2, nrows=1 , sep = "" )[1,2]  )  

    fname <- as.character(read.table(paste(file_name,sep=""), 
            skip=3, nrows=1 , sep = "" )[1,2] ) 
    item_cov <- if(fname=="0") 0 else read.table(paste(fname, sep=""))

    on_beta <- as.numeric(read.table(paste(file_name,sep=""), 
            skip=4, nrows=1 , sep = "" )[1,2]  )  

    fname <- as.character(read.table(paste(file_name,sep=""), 
            skip=5, nrows=1 , sep = "" )[1,2] ) 
    group <- if(fname=="0") 0 else read.table(paste(fname, sep=""))

    dif_beta <- as.numeric(read.table(paste(file_name,sep=""), 
            skip=6, nrows=1 , sep = "" )[1,-1]  )  

    dif_alpha <- as.numeric(read.table(paste(file_name,sep=""), 
            skip=7, nrows=1 , sep = "" )[1,-1]  )  

    mixture <- as.numeric(read.table(paste(file_name,sep=""), 
            skip=8, nrows=1 , sep = "" )[1,2]  )                                  

    within <- as.numeric(read.table(paste(file_name,sep=""), 
            skip=9, nrows=1 , sep = "" )[1,2]  ) 

    n_it_dim <- as.numeric(read.table(paste(file_name,sep=""), 
            skip=10, nrows=1 , sep = "" )[1,-1]  ) # vector 

    fname <- as.character(read.table(paste(file_name,sep=""), 
            skip=11, nrows=1 , sep = "" )[1,2] ) 
    dim_info <- if(fname=="0") 1 else read.table(paste(fname, sep=""))

    cov_info <- as.numeric(read.table(paste(file_name,sep=""), 
            skip=12, nrows=1 , sep = "" )[1,-1]  )   # vector 

    fname <- as.character(read.table(paste(file_name,sep=""), 
            skip=13, nrows=1 , sep = "" )[1,2] ) 
    weights <- if(fname=="0") 0 else read.table(paste(fname, sep=""))

    adapt <- as.numeric(read.table(paste(file_name,sep=""), 
            skip=14, nrows=1 , sep = "" )[1,2]  ) 

    bsize <- as.numeric(read.table(paste(file_name,sep=""), 
            skip=15, nrows=1 , sep = "" )[1,2]  ) 

    nqr <- as.numeric(read.table(paste(file_name,sep=""), 
            skip=16, nrows=1 , sep = "" )[1,-1]  ) 

    conv <- as.numeric(read.table(paste(file_name,sep=""), 
            skip=17, nrows=1 , sep = "" )[1,2]  ) 

    link <- as.numeric(read.table(paste(file_name,sep=""), 
            skip=18, nrows=1 , sep = "" )[1,2]  ) 

    fname <- as.character(read.table(paste(file_name,sep=""), 
            skip=19, nrows=1 , sep = "" )[1,2]  ) 
    start <- if(fname=="0") 0 else read.table(paste(fname, sep=""))
    
    minpercent <- as.numeric(read.table(paste(file_name,sep=""), 
            skip=20, nrows=1 , sep = "" )[1,2]  ) 

    fname <- as.character(read.table(paste(file_name,sep=""), 
            skip=21, nrows=1 , sep = "" )[1,2]  ) 
    constraint <- if(fname=="0") 0 else read.table(paste(fname, sep=""))
            
    verbose <- as.numeric(read.table(paste(file_name,sep=""), 
            skip=22, nrows=1 , sep = "" )[1,2] )
    se_num <- as.numeric(read.table(paste(file_name,sep=""), 
            skip=23, nrows=1 , sep = "" )[1,2] )  
    se_emp <- as.numeric(read.table(paste(file_name,sep=""), 
            skip=24, nrows=1 , sep = "" )[1,2] )  
    fname <- as.character(read.table(paste(file_name,sep=""), 
            skip=25, nrows=1 , sep = "" )[1,2] )  
    evaluate <- if(fname=="0") 0 else read.table(paste(fname, sep=""))
    
    post <- as.numeric(read.table(paste(file_name,sep=""), 
            skip=26, nrows=1 , sep = "" )[1,2] )

    data_new <- as.numeric(read.table(paste(file_name,sep=""), 
            skip=27, nrows=1 , sep = "" )[1,2] )
            
    max_it <- as.numeric(read.table(paste(file_name,sep=""), 
            skip=28, nrows=1 , sep = "" )[1,2] )

    fname <- as.character(read.table(paste(file_name,sep=""), 
            skip=29, nrows=1 , sep = "" )[1,2]  ) 
    embretson <- if(fname=="0") 0 else read.table(paste(fname, sep=""))
    
    alp_bounds_up <- as.numeric(read.table(paste(file_name,sep=""), 
            skip=30, nrows=1 , sep = "" )[1,2] )

    alp_bounds_low <- as.numeric(read.table(paste(file_name,sep=""), 
            skip=31, nrows=1 , sep = "" )[1,2] )
    
    if (alp_bounds_up==0) alp_bounds_up <- 15
    if (alp_bounds_low==0) alp_bounds_low <- -1
        
    est_inf <- list('adapt'=adapt, 'nqr'=nqr, 'conv'=conv, 'max_it'=max_it, 'link'=link, 'verbose'=verbose, 
                'alp_bounds_up'=alp_bounds_up, 'alp_bounds_low'=alp_bounds_low )

    # read new data (if available)
    if(data_new==1) {
        resp_long <- read.table(paste("data_new.txt", sep=""))[,1]
    }
        
##############################################
##    2. Read from data 
##############################################
        
    # number of observations 
    nobs <- as.numeric(nrow(resp))  
    names(nobs) <- "nobs"
    # number of items 
    nitem <- as.numeric(ncol(resp))
    names(nitem) <- "nitem"
    # number of categoris of items 
    if(data_new==1) {
        resp_wide <- matrix(resp_long, nobs, nitem, byrow=T)
        resp <- resp_wide
    } 
    S_item = apply(as.matrix(resp), 2, max, na.rm=T)+1
    # max categories of items 
    MS_item = max(S_item, na.rm=T)
    names(MS_item) = "maxcat"
  
    # number of categories 
    S_cat <- apply(resp, 2, count)  
    
    S_item <- S_cat
    
    # number of groups 
    if (!is.numeric(group)) {
        ngroup <- length(table(group)) 
        names(ngroup) <- "ngroup"
    } else { 
        ngroup <- 1 
        names(ngroup) <- "ngroup"
    } 
        
    # person covariates 
    if (!is.numeric(person_cov)) {
        length_person_cov <- ncol(person_cov)
    }    
    
    # item covariates for beta 
    if (!is.numeric(item_cov)) {
        length_item_cov_beta <- ncol(item_cov)
    }

    # item covariates for alpha 
    if (!is.numeric(embretson)) {
        length_item_cov_alpha <- ncol(embretson)
    }

    # dif 
    if (dif_beta[1] !=0) {
        length_dif_beta <- length(dif_beta)
    }    
    if (dif_alpha[1] !=0) {
        length_dif_alpha <- length(dif_alpha)
    }     
    
    # dimensions 
    
    if (n_it_dim[1]!=0) {
        ndim <- length(n_it_dim)
        names(ndim) <- "ndim"
        names(n_it_dim) <- paste("dim", 1:ndim, sep="")
    } else {
        ndim <- 1
        names(ndim) <- "ndim"
        names(n_it_dim) <- "n_it_dim"
        names(n_it_dim) <- paste("dim", 1:ndim, sep="")
    }
    within_name <- if (within==0) "between-item" else "within-item"
    names(within_name) <-""
    
    # parameterization (2pl)
    if (model %in% c(2,4)) {
        inside <- if (on_theta==1) "a(th+b)" else "a*th+b"
        names(inside) <-" "
    } else {
        inside <- NULL 
    }
    
    # dimension info (multidimensional models)
    if (model %in% c(3,4)) {
        sum_row_dim_info <- apply(dim_info, 1, sum)
        index_within <- which(sum_row_dim_info >1) # items that tap into multiple dimensons
        #howmany <- sum_row[index] # number of dimensions that they belong to
        #item_info <- apply(dim_info, 1, function(vec) { which(vec ==1)} )  # list: save dimensions that each item taps into
    }
    
    
    data_inf = list('nobs'=nobs, 'ngroup'=ngroup, 'nitem'=nitem, 'MS_item'=MS_item, 'S_item'=S_item)

    dim_inf = list('within'=within_name, 'ndim'=ndim, 'n_it_dim'=n_it_dim)

    
    

##############################################
##    3. Read from outputs
##############################################
    
    # est 
    
    if(file.exists("parms.txt")==FALSE) {
        error("Estimation did not terminate normally. Specify control$show=T and check MATLAB error messages")    
    }else {
        parms <- read.table(paste("parms.txt", sep=""))[,1]
    }

    if(file.exists("alp_embretson.txt")==TRUE) {
        parms_alp <- read.table(paste("alp_embretson.txt", sep=""))[,1]
    }

    if(file.exists("first_order.txt")==TRUE) {
        first_order <- read.table(paste("first_order.txt", sep=""))[,1]
    } else {
        first_order <- NULL 
    }
    if(file.exists("second_order.txt")==TRUE) {
        second_order <- read.table(paste("second_order.txt", sep=""))[,1]
    } else {
        second_order <- NULL 
    }
    if(file.exists("sd_square_first.txt")==TRUE) {
        variances_first <- read.table(paste("sd_square_first.txt", sep=""))[,1]
    } else {
        variances_first <- NULL 
    }
    if(file.exists("stand_fo.txt")==TRUE) {
        stand_first_order <- read.table(paste("stand_fo.txt", sep=""))[,1]
    } else {
        stand_first_order <- NULL 
    }
    if(file.exists("cor_first_second.txt")==TRUE) {
        cor_first_second <- read.table(paste("cor_first_second.txt", sep=""))[,1]
    } else {
        cor_first_second <- NULL 
    }



    # restriction
    rest <- read.table(paste("rest.txt", sep=""))[,1]
    
    n_const <- length(which(rest==1))  # n of constraints 

    const_nr=0
    if(file.exists("constraint.txt")) {
        const <- read.table( "constraint.txt")
        const_nr <- length(which(const[,1]==1))
    
    }


    infoemp_mat <- NULL
    infonum_mat <- NULL    
    # expand se 
    if (se_num == 1) {
        se <- read.table(paste("se_num.txt", sep=""))[,1]
        
        #information matrix 
        infonum_vec <- read.table(paste("infonum.txt", sep=""))[,1]
        mat_dim <- sqrt(length(infonum_vec))
        infonum_mat <- matrix(infonum_vec, mat_dim, mat_dim, byrow=F)
        
        # SE by restrictions 
        j=1 
        se0 <- rep(NA, length(rest)) 
        for (i in 1: length(rest)) { #
            if (rest[i] ==0)    {               
                se0[i] <- se[j]
                if (i ==j) { j <- i +1}     
                if (i >j) { j <- j+1 }
                #print(i)
            }
            
            if (rest[i] ==1) { 
                se0[i] <- NA
                if (i ==j) j <- i
                if (i > j) j <- j
                #print(j)  
            }
         
        }  
        
    }
    if (se_emp == 1) {
        se <- read.table(paste("se_emp.txt", sep=""))[,1]

        #information matrix 
        infoemp_vec <- read.table(paste("infoemp.txt", sep=""))[,1]
        mat_dim <- sqrt(length(infoemp_vec))
        infoemp_mat <- matrix(infoemp_vec, mat_dim, mat_dim, byrow=F)

        # SE by restrictions 
        j=1 
        se0 <- rep(NA, length(rest)) 
        for (i in 1: length(rest)) { #
            if (rest[i] ==0)    {               
                se0[i] <- se[j]
                if (i ==j) { j <- i +1}     
                if (i >j) { j <- j+1 }
                #print(i)
            }
            
            if (rest[i] ==1) { 
                se0[i] <- NA
                if (i ==j) j <- i
                if (i > j) j <- j
                #print(j)  
            }
         
        }  
        se0_e <- se0
    }
#
#    # information matrix 
#    if (se_num == 1 & se_emp ==0) {
#        infoemp_mat <- NULL
#    }else if (se_num == 0 & se_emp ==1) { 
#        infonum_mat <- NULL
#    }else if (se_num == 0 & se_emp ==0) { 
#        infoemp_mat <- NULL
#        infonum_mat <- NULL
#    }

    # se_emp and se_num
    if (se_num==0 & se_emp == 1) se0 = se0_e
    if (se_num==0 & se_emp == 0) se0 <- NULL  #se.use <- 0  else se.use <- 1
    if (se_num==1 & se_emp == 1) se_emp <- se0_e
    
    
    # eap stuff 
    if (post == 1) {
        eap_s <- read.table(paste("eap.txt", sep=""))[,1]
        var_s <- read.table(paste("var.txt", sep=""))[,1]
        cov_s <- read.table(paste("cov.txt", sep=""))[,1]
        exp_s <- read.table(paste("expected.txt", sep=""))[,1]
        rel_e <- read.table(paste("rel_e.txt", sep=""))[,1]        
        #rel_t0 <- read.table(paste("rel_t.txt", sep=""))[,1]
    
        # for multiple groups (ordered by group1, gropu2, ...etc 
        
        if(model==3 | model==4 ) { # for multidim models
            eap_s <- matrix(eap_s,  ncol=ndim)
            var_s <- matrix(var_s,  ncol=ndim)
            cov_s <- matrix(cov_s,  ncol=ndim)
            exp_s <- matrix(exp_s,  ncol=ndim)        
        } 
        if (model==5 | model==6) {  # bifactor model
            eap_s <- matrix(eap_s,  ncol=ndim+1)
            var_s <- matrix(var_s,  ncol=ndim+1)
            cov_s <- matrix(cov_s,  ncol=ndim+1)
            exp_s <- matrix(exp_s,  ncol=ndim) # ndim is the number of testlets        
        } 
        # EAP outputs
        #output_eap <- list("eap"=eap_s, "eap_var"=var_s, "eap_cov"=cov_s, "exp_s"=exp_s, "rel_e"=rel_e0)
    }  else {
        eap_s <- NULL 
        var_s <- NULL
        cov_s <- NULL
        exp_s <- NULL 
        rel_e <- NULL     
    }
    

    # parameter matrix with se 
    if (!is.null(se0)) {
        par <- cbind(parms, se0)
        colnames(par) <- c("Est","SE")
        #rownames(par) <- names(parms)
    } else {
        par <- as.matrix(parms)
        colnames(par) <- c("Est")  
        #rownames(par) <- names(parms)       
    }
    
    # se_e if any, is given as a vector 


############################################
#    4. Labeling 
############################################   

out_cov <- NULL  
     
########################################     
## uni and multi-dimensional rasch 
########################################     

    if (model%in%c(1,3)) {
 
        if (!is.numeric(group)) { 
            # mg    
            if (model==1) {    
                theta_sd <- parms[1:ngroup]
                names(parms)[1:ngroup] <- paste("th_sd", 1:ngroup, sep="")
                
                mg_end <- ngroup+ngroup-1
                over_m <- parms[(1+ngroup): mg_end]
                names(parms)[(1+ngroup):mg_end] <- paste("th_mean", 2:ngroup, sep="")           
             
            } 
            if (model==3) {   # multidimenisonal 
           
                # mg 
#                off_diag = (ndim*(ndim-1)/2)
#                th_chol <- matrix(NA, ngroup, (ndim+off_diag))
#                for (g in 1:ngroup){ 
#                    th_chol[g,] <- parms[ ((ndim+off_diag)*(g-1)+1):((ndim+off_diag)*g)] #(ndim*ngroup)
#                    names(parms)[((ndim+off_diag)*(g-1)+1):((ndim+off_diag)*g)] <- paste("mg_chol", 1:length(th_chol[g,]), sep="")
#                
#                }
#                vec_th_chol <- as.vector(th_chol) 
#               mg_end <- (ndim+off_diag)*ndim  + ndim 


                # cholesky elements 
                mg_sd_end <- ngroup*ndim #(for sd)
                mg_sd <- parms[1 : mg_sd_end ]
                temp <- paste("th_sd", 1, 1:ngroup, sep="")
                for (d in 2:ndim) {
                    temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                }
                names(parms)[1: mg_sd_end] <- temp         

                off_diag = (ndim*(ndim-1)/2)
                mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                temp <- paste("th_cov", 1, 1:ngroup, sep="")
                if (off_diag >1) {
                    for (d in 2:off_diag) {
                        temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                    }
                }
                names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp  
                
                # covariance matrix 
                out_cov <- cov_from_chol(parms=parms[1:mg_cov_end], ndim=ndim, ngroup=ngroup)
                par[1:mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                if (!is.null(se0)) {
                    par[1:mg_cov_end,2] <- rep(NA, length(1:mg_cov_end))
                } 

                # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                temp2 <- matrix(NA, ngroup, (ndim))
                for (ng in 1:ngroup) {
                    temp <- paste("th_sd", 1, ng, sep="")
                    for (d in 2:ndim ) {
                        temp <- c(temp, paste("th_sd", d,ng, sep=""))
                    }
                    temp2[ng,] <- temp
                }
                names(parms)[1: mg_sd_end] <- as.vector(t(temp2))

                # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                temp3 <- matrix(NA, ngroup, off_diag)
                for (ng in 1:ngroup) {
                    temp <- paste("th_cov", 1, ng, sep="")
                    if (off_diag >1) {
                        for (d in 2:off_diag ) {
                            temp <- c(temp, paste("th_cov", d,ng, sep=""))
                        }
                    }
                    temp3[ng,] <- temp
                }
                names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))

                # means 
                mg_end <- mg_cov_end  + ndim 
                over_m <- parms[(1+mg_cov_end) : mg_end]
                names(parms)[(1+mg_cov_end): mg_end] <- paste("th_mean", 1:ndim, sep="")             

            } 
            
                if (!is.numeric(item_cov)) {  # item_cov 
                    # mg + lltm 
                    lltm_end <- mg_end+length_item_cov_beta 
                    est_item_cov <- parms[(mg_end+1): lltm_end]
                    names(parms)[(mg_end+1): lltm_end] <- paste("bet_", name_it_cov_beta, 1:length((mg_end+1): lltm_end), sep="")
             
                        if (dif_beta[1]!=0) {  # dif 
                            # mg + lltm + dif 
                            num_dif_beta <- length(dif_beta) # sum(S_item[dif_beta]-1)  # total number of dif parameters (for polytomous items)
                            
                            dif_end <- lltm_end + num_dif_beta
                            est_dif_beta <- parms[(lltm_end+1): dif_end]
                        
                            temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each=1) # (S_item[dif_beta]-1)[1] )
                            if (length_dif_beta >1) {
                                for (dd in 2: length(dif_beta)) {
                                    temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= 1)) #(S_item[dif_beta]-1)[dd] )
                                }     
                            }
                            names(parms)[(lltm_end+1): dif_end] <- temp
                        
                                if (!is.numeric(person_cov)) {
                                    # mg + lltm + dif + person-cov 
                                    person_end <- dif_end + length_person_cov 
                                    est_person_cov <- parms[(dif_end+1): person_end]
                                    names(parms)[(dif_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length((dif_end+1): person_end), sep="")
                                } 
                        } else{ 
                            # mg + lltm + person_cov 
                            if (!is.numeric(person_cov)) {
                                dif_end = lltm_end
                                person_end <- dif_end + length_person_cov 
                                est_person_cov <- parms[(dif_end+1): person_end]
                                names(parms)[(dif_end+1): person_end] <- paste("gam_", name_pp_cov,  1:length((dif_end+1): person_end), sep="")
                            } 
                       }
    
               } else {
                    # no lltm 
                    rasch_end <- mg_end+sum(S_item-1)
                    est_beta <- parms[(mg_end+1): rasch_end]
                    # in case of polytomous items 
                    temp <- NA
                    for (jj in 1:nitem) {
                        temp <- c(temp, rep(jj, (S_item[jj]-1) ))
                    }
                    # this should be checked 
                    names(parms)[(mg_end+1): rasch_end] <- paste("bet",temp[-1], sep="") 
                    
                    #temp2 <-  as.character(paste("bet",temp[-1], sep="") ) 
                    #
                    #test <- parms[(mg_end+1): rasch_end]  #paste("bet",temp2, sep="")  
                    #names(test) <-   temp2
                    #names(parms)[(mg_end+1): rasch_end] <- test
                                           
                        if (dif_beta[1]!=0) {
                            # mg + dif 
                            lltm_end = rasch_end 
                            num_dif_beta <- sum(S_item[dif_beta]-1)  # total number of dif parameters (for polytomous items)
                            dif_end <- lltm_end + num_dif_beta
                            est_dif_beta <- parms[(lltm_end+1): dif_end]
                            #names(parms)[(lltm_end+1): dif_end] <- paste("DIF_bet", 1:length((lltm_end+1): dif_end), sep="")
                            temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= (S_item[dif_beta]-1)[1] )
                            if (length_dif_beta >1) {
                                for (dd in 2: length(dif_beta)) {
                                    temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= (S_item[dif_beta]-1)[dd] ))
                                }     
                            }
                            names(parms)[(lltm_end+1): dif_end] <- temp

                        
                                if (!is.numeric(person_cov)) {
                                    # mg + dif + person-cov 
                                    person_end <- dif_end + length_person_cov 
                                    est_person_cov <- parms[(dif_end+1): person_end]
                                    names(parms)[(dif_end+1): person_end] <- paste("gam_", name_pp_cov,  1:length((dif_end+1): person_end), sep="")
                                } 
                        } else{ 
                            # no dif 
                            # mg + person_cov 
                            if (!is.numeric(person_cov)) {
                                dif_end = rasch_end 
                                person_end <- dif_end + length_person_cov 
                                est_person_cov <- parms[(dif_end+1): person_end]
                                names(parms)[(dif_end+1): person_end] <- paste("gam_", name_pp_cov,  1:length((dif_end+1): person_end), sep="")
                            } 
                       }           
               }
           
          } else { 
                # no mg 
                if (model==1) {
                    mg_end = 1 
                    theta_sd <- parms[1]
                    names(parms)[1] <- "th_sd"
                }
                
                if (model==3) {
                    # no mg 
#                    off_diag = (ndim*(ndim-1)/2) 
#                    mg_end = off_diag + ndim 
#                    g=1 
#                    th_chol <- parms[ ((ndim+off_diag)*(g-1)+1):((ndim+off_diag)*g)] #(ndim*ngroup)
#                    names(parms)[((ndim+off_diag)*(g-1)+1):((ndim+off_diag)*g)] <- paste("mg_chol", 1:length(th_chol), sep="")

                    # cholesky elements 
                    ngroup=1 
                    mg_sd_end <- ngroup*ndim #(for sd)
                    mg_sd <- parms[1 : mg_sd_end ]
                    temp <- paste("th_sd", 1, 1:ngroup, sep="")
                    for (d in 2:ndim) {
                        temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                    }
                    names(parms)[1: mg_sd_end] <- temp         
    
                    off_diag = (ndim*(ndim-1)/2)
                    mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                    mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                    temp <- paste("th_cov", 1, 1:ngroup, sep="")
                    if (off_diag >1) {
                        for (d in 2:off_diag) {
                            temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                        }
                    }
                    names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp  
                    
        
                    # covariance matrix (group=1: they are already variance covariance estimates)
                    out_cov <- cov_from_chol(parms=parms[1:mg_cov_end], ndim=ndim, ngroup=ngroup)
                    par[1:mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                    if (!is.null(se0)) {
                        par[1:mg_cov_end,2] <- rep(NA, length(1:mg_cov_end))
                    } 

                    # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                    temp2 <- matrix(NA, ngroup, (ndim))
                    for (ng in 1:ngroup) {
                        temp <- paste("th_sd", 1, ng, sep="")
                        for (d in 2:ndim ) {
                            temp <- c(temp, paste("th_sd", d,ng, sep=""))
                        }
                        temp2[ng,] <- temp
                    }
                    names(parms)[1: mg_sd_end] <- as.vector(t(temp2))
                    # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                    temp3 <- matrix(NA, ngroup, off_diag)
                    for (ng in 1:ngroup) {
                        temp <- paste("th_cov", 1, ng, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag ) {
                                temp <- c(temp, paste("th_cov", d,ng, sep=""))
                            }
                        }
                        temp3[ng,] <- temp
                    }
                    names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))


                    mg_end = mg_cov_end 
                                
                }


                              
                if (!is.numeric(item_cov)) {
                    # lltm 
                    lltm_end <- mg_end+length_item_cov_beta 
                    est_item_cov <- parms[(mg_end+1): lltm_end]
                    names(parms)[(mg_end+1): lltm_end] <- paste("bet_", name_it_cov_beta, 1:length((mg_end+1): lltm_end), sep="")
             
                        if (dif_beta[1]!=0) {
                            # lltm + dif 
                            num_dif_beta <- sum(S_item[dif_beta]-1)  # total number of dif parameters (for polytomous items)
                            dif_end <- lltm_end + num_dif_beta 
                            est_dif_beta <- parms[(lltm_end+1): dif_end]
                            #names(parms)[(lltm_end+1): dif_end] <- paste("DIF_bet", 1:length((lltm_end+1): dif_end), sep="")
                            temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= (S_item[dif_beta]-1)[1] )
                            if (length_dif_beta >1) {
                                for (dd in 2: length(dif_beta)) {
                                    temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= (S_item[dif_beta]-1)[dd] ))
                                }     
                            }
                            names(parms)[(lltm_end+1): dif_end] <- temp
                            
                                if (!is.numeric(person_cov)) {
                                    # lltm + dif + person-cov 
                                    person_end <- dif_end + length_person_cov 
                                    est_person_cov <- parms[(dif_end+1): person_end]
                                    names(parms)[(dif_end+1): person_end] <- paste("gam_", name_pp_cov,  1:length((dif_end+1): person_end), sep="")
                                } 
                        } else{ 
                            # lltm + person_cov 
                            if (!is.numeric(person_cov)) {
                                dif_end = lltm_end
                                person_end <- dif_end + length_person_cov 
                                est_person_cov <- parms[(dif_end+1): person_end]
                                names(parms)[(dif_end+1): person_end] <- paste("gam_", name_pp_cov,  1:length((dif_end+1): person_end), sep="")
                            } 
                       }
    
               } else {
                    # no lltm 
                    rasch_end <- mg_end+sum(S_item-1)
                    est_beta <- parms[(mg_end+1): rasch_end]
                    # in case of polytomous items 
                    temp <- NA
                    for (jj in 1:nitem) {
                        temp <- c(temp, rep(jj, (S_item[jj]-1) ))
                    }
                    names(parms)[(mg_end+1): rasch_end]  <- paste("bet",temp[-1], sep="")  
                             
                        if (dif_beta[1]!=0) {
                            # dif 
                            lltm_end = rasch_end 
                            num_dif_beta <- sum(S_item[dif_beta]-1)  # total number of dif parameters (for polytomous items)
                            dif_end <- lltm_end + num_dif_beta
                            est_dif_beta <- parms[(lltm_end+1): dif_end]
                            #names(parms)[(lltm_end+1): dif_end] <- paste("DIF_bet", 1:length((lltm_end+1): dif_end), sep="")
                            temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= (S_item[dif_beta]-1)[1] )
                            if (length_dif_beta >1) {
                                for (dd in 2: length(dif_beta)) {
                                    temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= (S_item[dif_beta]-1)[dd] ))
                                }     
                            }
                            names(parms)[(lltm_end+1): dif_end] <- temp
                                                    
                                if (!is.numeric(person_cov)) {
                                    # mg + dif + person-cov 
                                    person_end <- dif_end + length_person_cov 
                                    est_person_cov <- parms[(dif_end+1): person_end]
                                    names(parms)[(dif_end+1): person_end] <- paste("gam_", name_pp_cov,  1:length((dif_end+1): person_end), sep="")
                                } 
                        } else{ 
                            # no dif 
                            # person_cov 
                            if (!is.numeric(person_cov)) {
                                dif_end = rasch_end 
                                person_end <- dif_end + length_person_cov 
                                est_person_cov <- parms[(dif_end+1): person_end]
                                names(parms)[(dif_end+1): person_end] <- paste("gam_", name_pp_cov,  1:length((dif_end+1): person_end), sep="")
                            } 
                       }           
               }
        }
    
        # mixture case 
        
#        est_person_cov 
#        est_dif_beta 
#        est_item_cov
#        est_beta
#        theta_sd
#        mg_sd
          
    
    }

########################################     
## 2pl Unidimensional models 
########################################     

    if (model%in%c(2)) {

        # alpha parameters 
        if (is.numeric(embretson)) {
            alpha_end <- nitem
            
            est_alpha <- parms[1: alpha_end]
            names(parms)[1: alpha_end] <- paste("alp", 1:nitem, sep="")
        } else {
            # embretson model 
            
            par <- rbind(matrix(parms_alp,ncol=1), matrix(parms[(nitem+1):length(parms)], ncol=1)) 
            colnames(par) <- "Est"
            parms <- c(parms_alp, parms[(nitem+1):length(parms)])

            alpha_end <- length_item_cov_alpha
            est_alpha <- parms[1: alpha_end]
            names(parms)[1: alpha_end] <- paste("alp_", name_it_cov_alpha, 1:length_item_cov_alpha , sep="")

        }
        
        if (!is.numeric(item_cov)) {  # item_cov 
        
            # lltm 
            lltm_end <- alpha_end+length_item_cov_beta 
            
            est_lltm <- parms[(alpha_end+1):lltm_end ]
            names(parms)[(alpha_end+1):lltm_end] <- paste("bet_", name_it_cov_beta, 1:length_item_cov_beta , sep="")
        
            if (dif_alpha[1]!=0) {  # dif alpha 
                # lltm + dif_alpha
                
                num_dif_alpha <- length(dif_alpha) #sum(S_item[dif_alpha]-1)  # total number of dif parameters (for polytomous items)
                dif_alp_end <- lltm_end + num_dif_alpha
                est_dif_alp <- parms[(lltm_end+1):dif_alp_end]
                #names(parms)[(lltm_end+1):dif_alp_end] <- paste("DIF_alp", 1:length_dif_alpha, sep="") 
                temp <- rep(paste("DIF_alp", dif_alpha[1], sep=""), each= 1 ) #(S_item[dif_alpha]-1)[1]
                if (length_dif_alpha >1) {
                    for (dd in 2: length(dif_alpha)) {
                        temp <- c(temp, rep(paste("DIF_alp", dif_alpha[dd], sep=""), each=1) ) #each= (S_item[dif_alpha]-1)[dd] )
                    }     
                }
                names(parms)[(lltm_end+1):dif_alp_end] <- temp

                if (dif_beta[1]!=0) {  # dif beta 
                    # lltm + dif_alpha + dif_beta
                    num_dif_beta <- length(dif_beta) #sum(S_item[dif_beta]-1) 
                    dif_bet_end <- dif_alp_end + num_dif_beta
                    est_dif_bet <- parms[(dif_bet_end+1):dif_bet_end]
                    #names(parms)[(dif_bet_end+1):dif_bet_end] <- paste("DIF_bet",1:length_dif_beta, sep="") 
                    temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= 1) #(S_item[dif_beta]-1)[1] 
                    if (length_dif_beta >1) {
                        for (dd in 2: length(dif_beta)) {
                            temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= 1) ) #(S_item[dif_beta]-1)[dd] )
                        }     
                    }
                    names(parms)[(dif_alp_end+1):dif_bet_end] <- temp
                       
                    # group 
                        # lltm + dif_alpha + dif_beta + group 
                        mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        mg_mean <- parms[(dif_bet_end + 1) : mg_mean_end ]
                        names(parms)[(dif_bet_end + 1) : mg_mean_end] <- paste("th_mean", 1:ngroup, sep="")
                        
                        mg_sd_end <- mg_mean_end  + ngroup #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- paste("th_sd", 1:ngroup, sep="")                    
                    
                        if (!is.numeric(person_cov)) {
                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_sd_end + length_person_cov 
                            est_person_cov <- parms[(mg_sd_end+1): person_end]
                            names(parms)[(mg_sd_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                        
                } else {   
                    #  lltm + dif_alpha + group + person-cov 
                        dif_bet_end = dif_alp_end 
                        mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        mg_mean <- parms[(dif_bet_end + 1) : mg_mean_end ]
                        names(parms)[(dif_bet_end + 1) : mg_mean_end] <- paste("th_mean", 1:ngroup, sep="")
                        
                        mg_sd_end <- mg_mean_end  + ngroup #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- paste("th_sd", 1:ngroup, sep="")                    
                           
                        if (!is.numeric(person_cov)) {
                            #mg_sd_end = dif_alp_end 
                            person_end <- mg_sd_end + length_person_cov 
                            est_person_cov <- parms[(mg_sd_end+1): person_end]
                            names(parms)[(mg_sd_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                        } 
                       
    
               } 
               
        } else {
 
                if (dif_beta[1]!=0) {  # dif beta 
                    # lltm + dif_beta
                    dif_alp_end = lltm_end 
                    num_dif_beta <- length(dif_beta) #sum(S_item[dif_beta]-1) 
                    dif_bet_end <- dif_alp_end + num_dif_beta
                    est_dif_bet <- parms[(dif_bet_end+1):dif_bet_end]
                    #names(parms)[(dif_bet_end+1):dif_bet_end] <- paste("DIF_bet",1:length_dif_beta, sep="") 
                    temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= 1) #(S_item[dif_beta]-1)[1] 
                    if (length_dif_beta >1) {
                        for (dd in 2: length(dif_beta)) {
                            temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= 1 )) #(S_item[dif_beta]-1)[dd]
                        }     
                    }
                    names(parms)[(dif_alp_end+1):dif_bet_end] <- temp

                       
                    # group 
                        # lltm + dif_beta + group
                        mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        mg_mean <- parms[(dif_bet_end + 1) : mg_mean_end ]
                        names(parms)[(dif_bet_end + 1) : mg_mean_end] <- paste("th_mean", 1:ngroup, sep="")
                        
                        mg_sd_end <- mg_mean_end  + ngroup #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- paste("th_sd", 1:ngroup, sep="")                    
                    
                        if (!is.numeric(person_cov)) {
                            #  lltm + dif_beta + mg + person-cov 
                            person_end <- mg_sd_end + length_person_cov 
                            est_person_cov <- parms[(mg_sd_end+1): person_end]
                            names(parms)[(mg_sd_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                        
                } else {   
                    #  lltm + group + person-cov 
                        dif_bet_end = lltm_end 
                        mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        mg_mean <- parms[(dif_bet_end + 1) : mg_mean_end ]
                        names(parms)[(dif_bet_end + 1) : mg_mean_end] <- paste("th_mean", 1:ngroup, sep="")
                        
                        mg_sd_end <- mg_mean_end  + ngroup #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- paste("th_sd", 1:ngroup, sep="")                    
                           
                        if (!is.numeric(person_cov)) {
                            #mg_sd_end = dif_alp_end 
                            person_end <- mg_sd_end + length_person_cov 
                            est_person_cov <- parms[(mg_sd_end+1): person_end]
                            names(parms)[(mg_sd_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                        } 
                       
    
               }  
                    
           
          }
          
        } else { # no lltm 
    
        # no lltm 
        beta_end <- alpha_end +  sum(S_item-1)
        
        est_beta <- parms[(alpha_end+1):(beta_end)]
        # in case of polytomous items 
        temp <- NA
        for (jj in 1:nitem) {
            temp <- c(temp, rep(jj, (S_item[jj]-1) ))
        }
        names(parms)[(alpha_end+1):(beta_end)]  <- paste("bet",temp[-1], sep="")  
                    
            if (dif_alpha[1]!=0) {  # dif alpha 
                # dif_alpha
                lltm_end = beta_end 
                num_dif_alpha <-  length_dif_alpha  #sum(S_item[dif_alpha]-1)  
                dif_alp_end <- lltm_end + num_dif_alpha
                est_dif_alp <- parms[(lltm_end+1):dif_alp_end]
                names(parms)[(lltm_end+1):dif_alp_end] <- paste("DIF_alp", 1:length_dif_alpha, sep="") 
                #temp <- rep(paste("DIF_alp", dif_alpha[1], sep=""), each= (S_item[dif_alpha]-1)[1] )
                #if (length_dif_alpha >1) {
                #    for (dd in 2: length(dif_alpha)) {
                #        temp <- c(temp, rep(paste("DIF_alp", dif_alpha[dd], sep="")) #, each= (S_item[dif_alpha]-1)[dd] )
                #    }     
                #}
                #names(parms)[(lltm_end+1):dif_alp_end] <- temp 
                                
                if (dif_beta[1]!=0) {  # dif beta 
                    # dif_alpha + dif_beta
                    num_dif_beta <- sum(S_item[dif_beta]-1) 
                    dif_bet_end <- dif_alp_end + num_dif_beta
                    est_dif_bet <- parms[(dif_bet_end+1):dif_bet_end]
                    #names(parms)[(dif_bet_end+1):dif_bet_end] <- paste("DIF_bet",1:length_dif_beta, sep="") 
                    temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= (S_item[dif_beta]-1)[1] )
                    if (length_dif_beta >1) {
                        for (dd in 2: length(dif_beta)) {
                            temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= (S_item[dif_beta]-1)[dd] ))
                        }     
                    }
                    names(parms)[(dif_alp_end+1):dif_bet_end] <- temp


                    # group 
                        # dif_alpha + dif_beta + group 
                        mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        mg_mean <- parms[(dif_bet_end + 1) : mg_mean_end ]
                        names(parms)[(dif_bet_end + 1) : mg_mean_end] <- paste("th_mean", 1:ngroup, sep="")
                        
                        mg_sd_end <- mg_mean_end  + ngroup #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- paste("th_sd", 1:ngroup, sep="")                    
                    
                        if (!is.numeric(person_cov)) {
                            # dif_alpha + dif_beta + mg + person-cov 
                            person_end <- mg_sd_end + length_person_cov 
                            est_person_cov <- parms[(mg_sd_end+1): person_end]
                            names(parms)[(mg_sd_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                        
                } else {   
                    #  dif_alpha + group + person-cov 
                        dif_bet_end = dif_alp_end # lltm_end 
                        mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        mg_mean <- parms[(dif_bet_end + 1) : mg_mean_end ]
                        names(parms)[(dif_bet_end + 1) : mg_mean_end] <- paste("th_mean", 1:ngroup, sep="")
                        
                        mg_sd_end <- mg_mean_end  + ngroup #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- paste("th_sd", 1:ngroup, sep="")                    
                           
                        if (!is.numeric(person_cov)) {
                            #mg_sd_end = dif_alp_end 
                            person_end <- mg_sd_end + length_person_cov 
                            est_person_cov <- parms[(mg_sd_end+1): person_end]
                            names(parms)[(mg_sd_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                        } 
                       
    
               } 
               
        } else {  # no dif alpha 
            lltm_end = beta_end 
                if (dif_beta[1]!=0) {  # dif beta 
                    lltm_end = beta_end 
                    # dif_beta
                    dif_alp_end = lltm_end 
                    num_dif_beta <- sum(S_item[dif_beta]-1) 
                    dif_bet_end <- dif_alp_end + num_dif_beta
                    est_dif_bet <- parms[(dif_bet_end+1):dif_bet_end]
                    #names(parms)[(dif_bet_end+1):dif_bet_end] <- paste("DIF_bet",1:length_dif_beta, sep="") 
                    temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= (S_item[dif_beta]-1)[1] )
                    if (length_dif_beta >1) {
                        for (dd in 2: length(dif_beta)) {
                            temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= (S_item[dif_beta]-1)[dd] ))
                        }     
                    }
                    names(parms)[(dif_alp_end+1):dif_bet_end] <- temp
                       
                    # group 
                        # dif_beta + group
                        mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        mg_mean <- parms[(dif_bet_end + 1) : mg_mean_end ]
                        names(parms)[(dif_bet_end + 1) : mg_mean_end] <- paste("th_mean", 1:ngroup, sep="")
                        
                        mg_sd_end <- mg_mean_end  + ngroup #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- paste("th_sd", 1:ngroup, sep="")                    
                    
                        if (!is.numeric(person_cov)) {
                            #  lltm + dif_beta + group + person-cov 
                            person_end <- mg_sd_end + length_person_cov 
                            est_person_cov <- parms[(mg_sd_end+1): person_end]
                            names(parms)[(mg_sd_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                        
                } else {   
                    # group + person-cov 
                        dif_bet_end = lltm_end 
                        mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        mg_mean <- parms[(dif_bet_end + 1) : mg_mean_end ]
                        names(parms)[(dif_bet_end + 1) : mg_mean_end] <- paste("th_mean", 1:ngroup, sep="")
                        
                        mg_sd_end <- mg_mean_end  + ngroup #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- paste("th_sd", 1:ngroup, sep="")                    
                           
                        if (!is.numeric(person_cov)) {
                            #mg_sd_end = dif_alp_end 
                            person_end <- mg_sd_end + length_person_cov 
                            est_person_cov <- parms[(mg_sd_end+1): person_end]
                            names(parms)[(mg_sd_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                        } 
                       
    
               }  
                    
           
          }
    
          
    
        }

    }

########################################     
## 2pl multidimensional models 
########################################     

    if (model%in%c(4)) {


        # alpha parameters 
        if (is.numeric(embretson)) {
            
            if(length(index_within)>0) { # within item 
            
                num_alpha <- sum(sum_row_dim_info) # number of alpha parameters 
                alpha_end <- num_alpha
                est_alpha <- parms[1: alpha_end]
                
                temp <- rep(1, sum_row_dim_info[1]) 
                for (jj in 2:nitem) {
                    temp <- c(temp, rep(jj, (sum_row_dim_info[jj]) ))
                }
                names(parms)[1: alpha_end]  <- paste("alp",temp, sep="")  
                    
            } else {
                alpha_end <- nitem
                
                est_alpha <- parms[1: alpha_end]
                names(parms)[1: alpha_end] <- paste("alp", 1:nitem, sep="")            
            
            }

        }else {
            # embretson model 
            
            if(length(index_within)>0) { # within item 
            
                num_alpha <- sum(sum_row_dim_info) # number of alpha parameters 
                
                par <- rbind(matrix(parms_alp,ncol=1), matrix(parms[(num_alpha+1):length(parms)], ncol=1)) 
                colnames(par) <- "Est"
                parms <- c(parms_alp, parms[(num_alpha+1):length(parms)])

                alpha_end <- length_item_cov_alpha
                est_alpha <- parms[1: alpha_end]
                names(parms)[1: alpha_end] <- paste("alp_", name_it_cov_alpha, 1:length_item_cov_alpha , sep="")
                    
            } else {
                par <- rbind(matrix(parms_alp,ncol=1), matrix(parms[(nitem+1):length(parms)], ncol=1)) 
                colnames(par) <- "Est"
                parms <- c(parms_alp, parms[(nitem+1):length(parms)])
                
                alpha_end <- length_item_cov_alpha
                est_alpha <- parms[1: alpha_end]
                names(parms)[1: alpha_end] <- paste("alp_", name_it_cov_alpha, 1:length_item_cov_alpha , sep="") 
            
            }

        }

        # item cov on beta 
        if (!is.numeric(item_cov)) {  # item_cov 
        
            # lltm 
            lltm_end <- alpha_end+length_item_cov_beta 
            
            est_lltm <- parms[(alpha_end+1):lltm_end ]
            names(parms)[(alpha_end+1):lltm_end] <- paste("bet_", name_it_cov_beta, 1:length_item_cov_beta , sep="")
        
            if (dif_alpha[1]!=0) {  # dif alpha 
                # lltm + dif_alpha
                
                num_dif_alpha <- length_dif_alpha  #sum(S_item[dif_alpha]-1)  # total number of dif parameters (for polytomous items)
                dif_alp_end <- lltm_end + num_dif_alpha
                est_dif_alp <- parms[(lltm_end+1):dif_alp_end]
                names(parms)[(lltm_end+1):dif_alp_end] <- paste("DIF_alp", 1:length_dif_alpha, sep="") 
#                temp <- rep(paste("DIF_alp", dif_alpha[1], sep=""), each= (S_item[dif_alpha]-1)[1] )
#                if (length_dif_alpha >1) {
#                    for (dd in 2: length(dif_alpha)) {
#                        temp <- c(temp, rep(paste("DIF_alp", dif_alpha[dd], sep=""), each= (S_item[dif_alpha]-1)[dd] ))
#                    }     
#                }
#                names(parms)[(lltm_end+1):dif_alp_end] <- temp

                if (dif_beta[1]!=0) {  # dif beta 
                    # lltm + dif_alpha + dif_beta
                    num_dif_beta <- sum(S_item[dif_beta]-1) 
                    dif_bet_end <- dif_alp_end + num_dif_beta
                    est_dif_bet <- parms[(dif_bet_end+1):dif_bet_end]
                    #names(parms)[(dif_bet_end+1):dif_bet_end] <- paste("DIF_bet",1:length_dif_beta, sep="") 
                    temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= (S_item[dif_beta]-1)[1] )
                    if (length_dif_beta >1) {
                        for (dd in 2: length(dif_beta)) {
                            temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= (S_item[dif_beta]-1)[dd] ))
                        }     
                    }
                    names(parms)[(dif_alp_end+1):dif_bet_end] <- temp

                    if (on_theta==0 && !is.numeric(person_cov)) {
                        # on_theta = 0
                        #  lltm + dif_alpha + dif_beta +  person-cov 
                        person_end <- dif_bet_end + length_person_cov 
                        est_person_cov <- parms[(dif_bet_end+1): person_end]
                        names(parms)[(dif_bet_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                     
                        # group 
                        # lltm + dif_alpha + dif_beta +  person-cov + group 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp                  

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))
                            
                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 

                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))

                        }
                        
                    }

                       

                    if (on_theta==1 && !is.numeric(person_cov)) {

                         # group 
                        # lltm + dif_alpha + dif_beta + group 
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp       

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 

                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))                        
                        
                        } 

                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")


                         
                        }
                    
                    # new 
                    if (is.numeric(person_cov)) {    
                         # group 
                        # lltm + dif_alpha + dif_beta + group 
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp       

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))
                            

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))                            
                        }   
                    }                     
                    
                        
                } else {   
                    #  lltm + dif_alpha + group + person-cov 
                    if (on_theta==0 && !is.numeric(person_cov)) {
                        # on_theta = 0
                        #  lltm + dif_alpha +   person-cov +group
                        person_end <- dif_alp_end + length_person_cov 
                        est_person_cov <- parms[(dif_alp_end+1): person_end]
                        names(parms)[(dif_alp_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                                       


                        # group 
                        # lltm + dif_alpha + dif_beta +  person-cov + group 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp                        

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))
                            

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))                            
                            
                        }
                    
                         
                    
                    
                    }


                    if (on_theta==1 && !is.numeric(person_cov)) {
                           
                        #person_end = dif_alp_end
                        person_end <- dif_alp_end 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp       

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))                            

                        } 
                         
                             #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
 
 
                        }
                      
                    # new   
                    if (is.numeric(person_cov)) {
                            
                        #person_end = dif_alp_end 
                        person_end <- dif_alp_end
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp       

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))
                            

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))                            

                        } 
                         
                        }    
               } 
               
        } else {
 
                if (dif_beta[1]!=0) {  # dif beta 
                    # lltm + dif_beta
                    dif_alp_end = lltm_end 
                    num_dif_beta <- sum(S_item[dif_beta]-1) 
                    dif_bet_end <- dif_alp_end + num_dif_beta
                    est_dif_bet <- parms[(dif_bet_end+1):dif_bet_end]
                    #names(parms)[(dif_bet_end+1):dif_bet_end] <- paste("DIF_bet",1:length_dif_beta, sep="") 
                    temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= (S_item[dif_beta]-1)[1] )
                    if (length_dif_beta >1) {
                        for (dd in 2: length(dif_beta)) {
                            temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= (S_item[dif_beta]-1)[dd] ))
                        }     
                    }
                    names(parms)[(dif_alp_end+1):dif_bet_end] <- temp

                    if (on_theta==0 && !is.numeric(person_cov)) {
                        # on_theta = 0
                        #  lltm + dif_beta +   person-cov +group
                        person_end <- dif_bet_end + length_person_cov 
                        est_person_cov <- parms[(dif_bet_end+1): person_end]
                        names(parms)[(dif_bet_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                     
                        # group 
                        # lltm + dif_beta +  person-cov + group 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp  
                                          
                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))
                            

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))                            
                        }
   
                    }
                       

                        if (on_theta==1 && !is.numeric(person_cov)) {

                    # group 
                        # lltm + dif_beta + group
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp 

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))
                            
                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))
                            

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))                            
                        }

                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")

                         
                        }

                        # new 
                        if (is.numeric(person_cov)) {

                    # group 
                        # lltm + dif_beta + group
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp 

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))
                            
                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))
                            

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))                            
                        }
                         
                        }

                        
                } else {   

                    if (on_theta==0 && !is.numeric(person_cov)) {
                        # on_theta = 0
                        #  lltm +  person-cov +group
                        dif_bet_end = lltm_end 
                        person_end <- dif_bet_end + length_person_cov 
                        est_person_cov <- parms[(dif_bet_end+1): person_end]
                        names(parms)[(dif_bet_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                     
                        # group 
                        # lltm +  person-cov + group 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp 
                        
                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))
                            
                            
                        }
                    }                
                
                

                        if (on_theta==1 && !is.numeric(person_cov)) {

                    #  lltm + group + person-cov 
                        dif_bet_end = lltm_end 
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp    

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))   


                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))                            
                                                      
                        }
                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }

                       if (is.numeric(person_cov)) {

                    #  lltm + group + person-cov 
                        dif_bet_end = lltm_end 
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp    

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))  
                            

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))                                                       
                        }

                        }
                       
    
               }  
                    
           
          }
          
        } else { 
        
        # no lltm 
        beta_end <- alpha_end +  sum(S_item-1)
        
        est_beta <- parms[(alpha_end+1):(beta_end)]
        # in case of polytomous items 
        temp <- NA
        for (jj in 1:nitem) {
            temp <- c(temp, rep(jj, (S_item[jj]-1) ))
        }
        names(parms)[(alpha_end+1):(beta_end)]  <- paste("bet",temp[-1], sep="")  
                    
            if (dif_alpha[1]!=0) {  # dif alpha 
                # dif_alpha
                lltm_end = beta_end 
                num_dif_alpha <- length_dif_alpha #sum(S_item[dif_alpha]-1)  
                dif_alp_end <- lltm_end + num_dif_alpha
                est_dif_alp <- parms[(lltm_end+1):dif_alp_end]
                names(parms)[(lltm_end+1):dif_alp_end] <- paste("DIF_alp", 1:length_dif_alpha, sep="") 
#                temp <- rep(paste("DIF_alp", dif_alpha[1], sep=""), each= (S_item[dif_alpha]-1)[1] )
#                if (length_dif_alpha >1) {
#                    for (dd in 2: length(dif_alpha)) {
#                        temp <- c(temp, rep(paste("DIF_alp", dif_alpha[dd], sep=""), each= (S_item[dif_alpha]-1)[dd] ))
#                    }     
#                }
#                names(parms)[(lltm_end+1):dif_alp_end] <- temp 
                                
                if (dif_beta[1]!=0) {  # dif beta 
                    # dif_alpha + dif_beta
                    num_dif_beta <- sum(S_item[dif_beta]-1) 
                    dif_bet_end <- dif_alp_end + num_dif_beta
                    est_dif_bet <- parms[(dif_bet_end+1):dif_bet_end]
                    #names(parms)[(dif_bet_end+1):dif_bet_end] <- paste("DIF_bet",1:length_dif_beta, sep="") 
                    temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= (S_item[dif_beta]-1)[1] )
                    if (length_dif_beta >1) {
                        for (dd in 2: length(dif_beta)) {
                            temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= (S_item[dif_beta]-1)[dd] ))
                        }     
                    }
                    names(parms)[(dif_alp_end+1):dif_bet_end] <- temp

                    if (on_theta==0 && !is.numeric(person_cov)) {
                        # on_theta = 0
                        # dif_alpha + dif_betaiperson-cov +group
                        
                        person_end <- dif_bet_end + length_person_cov 
                        est_person_cov <- parms[(dif_bet_end+1): person_end]
                        names(parms)[(dif_bet_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                     
                        # group 
                        # lltm +  person-cov + group 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp                     

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))
                            

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))                            
                            
                        }
                    }  

                        if (on_theta==1 && !is.numeric(person_cov)) {

                    # group 
                        # dif_alpha + dif_beta + group 
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp      

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))
                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))
                            

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))                            
                            
                        }
                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }


                        if (is.numeric(person_cov)) {

                    # group 
                        # dif_alpha + dif_beta + group 
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp      

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))
                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))
                            

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))                            
                            
                        }

                        }

                        
                } else {   
                    # no beta dif 
                    if (on_theta==0 && !is.numeric(person_cov)) {
                        # on_theta = 0
                        # dif_alpha + person-cov +group
                        
                        person_end <- dif_alp_end + length_person_cov 
                        est_person_cov <- parms[(dif_alp_end+1): person_end]
                        names(parms)[(dif_alp_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                     
                        # group 
                        # lltm +  person-cov + group 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp                    

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))     
                            

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))                                                   
                        }
                    }                  
                
                        if (on_theta==1 && !is.numeric(person_cov)) {
                    #  dif_alpha + group + person-cov 
                        dif_bet_end = dif_alp_end
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp    

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))
                            

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))                            
                        }

                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }

                        if (is.numeric(person_cov)) {
                    #  dif_alpha + group + person-cov 
                        dif_bet_end = dif_alp_end
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp    

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))
                            

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))                            
                            
                        }


                        }
                       
    
               } 
               
        } else {
            lltm_end = beta_end 
                if (dif_beta[1]!=0) {  # dif beta 
                    lltm_end = beta_end 
                    # dif_beta
                    dif_alp_end = lltm_end 
                    num_dif_beta <- sum(S_item[dif_beta]-1) 
                    dif_bet_end <- dif_alp_end + num_dif_beta
                    est_dif_bet <- parms[(dif_bet_end+1):dif_bet_end]
                    #names(parms)[(dif_bet_end+1):dif_bet_end] <- paste("DIF_bet",1:length_dif_beta, sep="") 
                    temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= (S_item[dif_beta]-1)[1] )
                    if (length_dif_beta >1) {
                        for (dd in 2: length(dif_beta)) {
                            temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= (S_item[dif_beta]-1)[dd] ))
                        }     
                    }
                    names(parms)[(dif_alp_end+1):dif_bet_end] <- temp
                    
                    if (on_theta==0 && !is.numeric(person_cov)) {
                        # on_theta = 0
                        # dif_beta + person-cov +group
                        
                        person_end <- dif_bet_end + length_person_cov 
                        est_person_cov <- parms[(dif_bet_end+1): person_end]
                        names(parms)[(dif_bet_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                     
                        # group 
                        # lltm +  person-cov + group 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp                    

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))
                            
                            

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))                            
                        }
                    } 

                       

                        if (on_theta==1 && !is.numeric(person_cov)) {
                        
                    # group 
                        # dif_beta + group
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp  

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))
                            

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))                            
                            
                        }                        
                        
                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }

                        if (is.numeric(person_cov)) {
                        
                    # group 
                        # dif_beta + group
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp  

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))
                            

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))                            
                            
                        }                        

                        }

                        
                } else {   
                    dif_bet_end = lltm_end 
                    if (on_theta==0 && !is.numeric(person_cov)) {
                        # on_theta = 0
                        # dif_beta + person-cov +group
                        dif_bet_end = lltm_end 
                        person_end <- dif_bet_end + length_person_cov 
                        est_person_cov <- parms[(dif_bet_end+1): person_end]
                        names(parms)[(dif_bet_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                     
                        # group 
                        # lltm +  person-cov + group 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp                     

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))
                            

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))                            
                            
                        }
                    } 


                        if (on_theta==1 && !is.numeric(person_cov)) {
                    # group + person-cov 
                        dif_bet_end = lltm_end 
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp    

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))
                            

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))                            
                            
                        }
                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        } 
                    # new 
                    if (is.numeric(person_cov)) {    
                        # group + person-cov 
                        dif_bet_end = lltm_end 
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*ndim #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_mean", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # cholesky elements 
                        mg_sd_end <- mg_mean_end  + ngroup*ndim #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd", 1, 1:ngroup, sep="")
                        for (d in 2:ndim) {
                            temp <- c(temp, paste("th_sd", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag = (ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp    

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd", 1, ng, sep="")
                                for (d in 2:ndim ) {
                                    temp <- c(temp, paste("th_sd", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end]  <- as.vector(t(temp2))
                            

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end]<- as.vector(t(temp3))                            
                            
                        }    
                    }   
    
               }  
                    
           
          }
    
          
    
        }

    }

########################################     
## Bifactor models 
########################################     

    if (model%in%c(5)) {

        # alpha parameters 
        if (is.numeric(embretson)) {
            # alpha parameters 
            alpha_g_end <- nitem
            
            est_alpha_g <- parms[1: alpha_g_end]
            names(parms)[1: alpha_g_end] <- paste("alp_g", 1:nitem, sep="")
    
            alpha_end <-alpha_g_end + nitem
            
            est_alpha_s <- parms[(alpha_g_end+1): alpha_end]
            names(parms)[(alpha_g_end+1): alpha_end] <- paste("alp_s", 1:nitem, sep="")

        }else {
            # embretson model 
            
            par <- rbind(matrix(parms_alp,ncol=1), matrix(parms[(2*nitem+1):length(parms)], ncol=1)) 
            colnames(par) <- "Est"
            parms <- c(parms_alp, parms[(2*nitem+1):length(parms)])
                        
            alpha_g_end <- length_item_cov_alpha
            est_alpha_g <- parms[1: alpha_g_end]
            names(parms)[1: alpha_g_end] <- paste("alp_g_", name_it_cov_alpha, 1:length_item_cov_alpha , sep="")

            alpha_end <- alpha_g_end +length_item_cov_alpha
            est_alpha_s <- parms[(alpha_g_end+1): alpha_end]
            names(parms)[(alpha_g_end+1): alpha_end] <- paste("alp_s_", name_it_cov_alpha, 1:length_item_cov_alpha , sep="")

        }


        
        if (!is.numeric(item_cov)) {  # item_cov 
        
            # lltm 
            lltm_end <- alpha_end+length_item_cov_beta 
            
            est_lltm <- parms[(alpha_end+1):lltm_end ]
            names(parms)[(alpha_end+1):lltm_end] <- paste("bet_", name_it_cov_beta, 1:length_item_cov_beta , sep="")
        
            if (dif_alpha[1]!=0) {  # dif alpha 
                # lltm + dif_alpha
                
                num_dif_alpha <- length_dif_alpha #sum(S_item[dif_alpha]-1)  # total number of dif parameters (for polytomous items)
                # general dimension 
                dif_alp_g_end <- lltm_end + num_dif_alpha
                est_dif_alp_g <- parms[(lltm_end+1):dif_alp_g_end]
                names(parms)[(lltm_end+1):dif_alp_g_end] <- paste("DIF_alp_g", 1:length_dif_alpha, sep="") 
                
                # specific dimension
                dif_alp_end <- dif_alp_g_end + num_dif_alpha
                est_dif_alp_s <- parms[(dif_alp_g_end+1):dif_alp_end]
                names(parms)[(dif_alp_g_end+1):dif_alp_end] <- paste("DIF_alp_s", 1:length_dif_alpha, sep="") 

                if (dif_beta[1]!=0) {  # dif beta 
                    # lltm + dif_alpha + dif_beta
                    num_dif_beta <- sum(S_item[dif_beta]-1) # total number of dif parameters (for polytomous items)
                    dif_bet_end <- dif_alp_end + num_dif_beta
                    est_dif_bet <- parms[(dif_bet_end+1):dif_bet_end]
                    #names(parms)[(dif_bet_end+1):dif_bet_end] <- paste("DIF_bet",1:length_dif_beta, sep="") 
                    temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= (S_item[dif_beta]-1)[1] )
                    if (length_dif_beta >1) {
                        for (dd in 2: length(dif_beta)) {
                            temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= (S_item[dif_beta]-1)[dd] ))
                        }     
                    }
                    names(parms)[(dif_alp_end+1):dif_bet_end] <- temp
                       
                    # group 
                        # lltm + dif_alpha + dif_beta + group 
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*(ndim+1) #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_mean_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # sd 
                        mg_sd_end <- mg_mean_end  + ngroup*(ndim+1) #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_sd_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag =  ndim #+ (ndim-1)  #(ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp       

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol_bifactor(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))
                            
                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            
                            # all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim+1))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd_g", 1, ng, sep="")
                                for (d in 1:ndim ) {
                                    temp <- c(temp, paste("th_sd_s", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end] <- as.vector(t(temp2))


                        } 
                        if (!is.numeric(person_cov)) {
                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                        
                } else {   
                        # no dif beta  
                        person_end = dif_alp_end 
                        mg_mean_end <- person_end+ ngroup*(ndim+1) #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_mean_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # sd 
                        mg_sd_end <- mg_mean_end  + ngroup*(ndim+1) #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_sd_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag =  ndim #+ (ndim-1)  #(ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp       

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol_bifactor(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 

                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim+1))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd_g", 1, ng, sep="")
                                for (d in 1:ndim ) {
                                    temp <- c(temp, paste("th_sd_s", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end] <- as.vector(t(temp2))

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end] <- as.vector(t(temp3))

                        } 
                        if (!is.numeric(person_cov)) {
                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                       
    
               } 
               
        } else {
                # no alaph dif 
                if (dif_beta[1]!=0) {  # dif beta 
                    # lltm + dif_beta
                    dif_alp_end = lltm_end 
                    num_dif_beta <- sum(S_item[dif_beta]-1) 
                    dif_bet_end <- dif_alp_end + num_dif_beta
                    est_dif_bet <- parms[(dif_bet_end+1):dif_bet_end]
                    #names(parms)[(dif_bet_end+1):dif_bet_end] <- paste("DIF_bet",1:length_dif_beta, sep="") 
                    temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= (S_item[dif_beta]-1)[1] )
                    if (length_dif_beta >1) {
                        for (dd in 2: length(dif_beta)) {
                            temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= (S_item[dif_beta]-1)[dd] ))
                        }     
                    }
                    names(parms)[(dif_alp_end+1):dif_bet_end] <- temp
                       
                    # group 
                        # lltm + dif_beta + group
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*(ndim+1) #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_mean_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # sd 
                        mg_sd_end <- mg_mean_end  + ngroup*(ndim+1) #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_sd_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag =  ndim #+ (ndim-1)  #(ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp       

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol_bifactor(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))
                            
                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 

                            # all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim+1))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd_g", 1, ng, sep="")
                                for (d in 1:ndim ) {
                                    temp <- c(temp, paste("th_sd_s", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end] <- as.vector(t(temp2))

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end] <- as.vector(t(temp3))

                        } 
                        if (!is.numeric(person_cov)) {
                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                        
                } else {   
                    # no beta dif 

                    #  lltm + group + person-cov 
                        dif_bet_end = lltm_end 
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*(ndim+1) #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_mean_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # sd 
                        mg_sd_end <- mg_mean_end  + ngroup*(ndim+1) #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_sd_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag =  ndim #+ (ndim-1)  #(ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp       

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol_bifactor(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 

                            # all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim+1))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd_g", 1, ng, sep="")
                                for (d in 1:ndim ) {
                                    temp <- c(temp, paste("th_sd_s", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end] <- as.vector(t(temp2))

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end] <- as.vector(t(temp3))
                        } 
                        if (!is.numeric(person_cov)) {
                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                       
    
               }  
                    
           
          }
          
        } else { 
    
        # no lltm 
        beta_end <- alpha_end +  sum(S_item-1)
        
        est_beta <- parms[(alpha_end+1):(beta_end)]
        # in case of polytomous items 
        temp <- NA
        for (jj in 1:nitem) {
            temp <- c(temp, rep(jj, (S_item[jj]-1) ))
        }
        names(parms)[(alpha_end+1):(beta_end)]  <- paste("bet",temp[-1], sep="")  
                    
            if (dif_alpha[1]!=0) {  # dif alpha 
                # dif_alpha
                lltm_end = beta_end 
                num_dif_alpha <- length_dif_alpha #sum(S_item[dif_alpha]-1)  # total number of dif parameters (for polytomous items)
                # general dimension 
                dif_alp_g_end <- lltm_end + num_dif_alpha
                est_dif_alp_g <- parms[(lltm_end+1):dif_alp_g_end]
                names(parms)[(lltm_end+1):dif_alp_g_end] <- paste("DIF_alp_g", 1:length_dif_alpha, sep="") 
                
                # specific dimension
                dif_alp_end <- dif_alp_g_end + num_dif_alpha
                est_dif_alp_s <- parms[(dif_alp_g_end+1):dif_alp_end]
                names(parms)[(dif_alp_g_end+1):dif_alp_end] <- paste("DIF_alp_s", 1:length_dif_alpha, sep="") 
                                
                if (dif_beta[1]!=0) {  # dif beta 
                    # dif_alpha + dif_beta
                    num_dif_beta <- sum(S_item[dif_beta]-1) 
                    dif_bet_end <- dif_alp_end + num_dif_beta
                    est_dif_bet <- parms[(dif_bet_end+1):dif_bet_end]
                    #names(parms)[(dif_bet_end+1):dif_bet_end] <- paste("DIF_bet",1:length_dif_beta, sep="") 
                    temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= (S_item[dif_beta]-1)[1] )
                    if (length_dif_beta >1) {
                        for (dd in 2: length(dif_beta)) {
                            temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= (S_item[dif_beta]-1)[dd] ))
                        }     
                    }
                    names(parms)[(dif_alp_end+1):dif_bet_end] <- temp

                    # group 
                        # dif_alpha + dif_beta + group 
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end  
                        mg_mean_end <- person_end+ ngroup*(ndim+1) #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_mean_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # sd 
                        mg_sd_end <- mg_mean_end  + ngroup*(ndim+1) #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_sd_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag =  ndim #+ (ndim-1)  #(ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp       

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol_bifactor(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 

                            # all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim+1))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd_g", 1, ng, sep="")
                                for (d in 1:ndim ) {
                                    temp <- c(temp, paste("th_sd_s", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end] <- as.vector(t(temp2))

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end] <- as.vector(t(temp3))
                        } 
                        if (!is.numeric(person_cov)) {
                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                        
                } else {   
                    # no beta dif 
                 
                    #  dif_alpha + group + person-cov 
                        dif_bet_end = dif_alp_end
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*(ndim+1) #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_mean_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # sd 
                        mg_sd_end <- mg_mean_end  + ngroup*(ndim+1) #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_sd_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag =  ndim #+ (ndim-1)  #(ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp       

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol_bifactor(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 

                            # all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim+1))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd_g", 1, ng, sep="")
                                for (d in 1:ndim ) {
                                    temp <- c(temp, paste("th_sd_s", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end] <- as.vector(t(temp2))

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end] <- as.vector(t(temp3))

                        } 
                        if (!is.numeric(person_cov)) {
                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                       
    
               } 
               
        } else {
            lltm_end = beta_end 
                if (dif_beta[1]!=0) {  # dif beta 
                    lltm_end = beta_end 
                    # dif_beta
                    dif_alp_end = lltm_end 
                    num_dif_beta <- sum(S_item[dif_beta]-1) 
                    dif_bet_end <- dif_alp_end + num_dif_beta
                    est_dif_bet <- parms[(dif_bet_end+1):dif_bet_end]
                    #names(parms)[(dif_bet_end+1):dif_bet_end] <- paste("DIF_bet",1:length_dif_beta, sep="") 
                    temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= (S_item[dif_beta]-1)[1] )
                    if (length_dif_beta >1) {
                        for (dd in 2: length(dif_beta)) {
                            temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= (S_item[dif_beta]-1)[dd] ))
                        }     
                    }
                    names(parms)[(dif_alp_end+1):dif_bet_end] <- temp
                                           
                    # group 
                        # dif_beta + group
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*(ndim+1) #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_mean_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # sd 
                        mg_sd_end <- mg_mean_end  + ngroup*(ndim+1) #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_sd_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag =  ndim #+ (ndim-1)  #(ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp       

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol_bifactor(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))
                            
                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 

                            # all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim+1))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd_g", 1, ng, sep="")
                                for (d in 1:ndim ) {
                                    temp <- c(temp, paste("th_sd_s", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end] <- as.vector(t(temp2))

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end] <- as.vector(t(temp3))
                            
                        } 
                        if (!is.numeric(person_cov)) {
                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                        
                } else {   
                    dif_bet_end = lltm_end 

                    # group + person-cov 
                        dif_bet_end = lltm_end 
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*(ndim+1) #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_mean_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # sd 
                        mg_sd_end <- mg_mean_end  + ngroup*(ndim+1) #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_sd_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag =  ndim #+ (ndim-1)  #(ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp       

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol_bifactor(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 

                            # all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim+1))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd_g", 1, ng, sep="")
                                for (d in 1:ndim ) {
                                    temp <- c(temp, paste("th_sd_s", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end] <- as.vector(t(temp2))
                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end] <- as.vector(t(temp3))

                        } 
                        if (!is.numeric(person_cov)) {
                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                       
    
               }  
                    
           
          }
    
          
    
        }

    }


########################################     
## second-order models 
## change: dif alpha 
## change SE (expand) 
########################################     

    if (model%in%c(6)) {
        #expand se0 (only if se0 exists)
        #ntestlet = ndim-1 
        #se00 <- se0[-ntestlet]
        #se0_expanded <- rep(NA, length(parms)) #se0[-ntestlet] 
        
        # alpha parameters 
        if (is.numeric(embretson)) {
            # alpha parameters 
            alpha_g_end <- nitem
            
            est_alpha_g <- parms[1: alpha_g_end]
            names(parms)[1: alpha_g_end] <- paste("alp_g", 1:nitem, sep="")
            
            alpha_end <-alpha_g_end + nitem
            
            est_alpha_s <- parms[(alpha_g_end+1): alpha_end]
            names(parms)[(alpha_g_end+1): alpha_end] <- paste("alp_s", 1:nitem, sep="")

            # se expanded 
            #se0_expanded[1: alpha_g_end] <- se00[1: alpha_g_end]                
            #se0_expanded[(alpha_g_end+1): alpha_end] <- rep(NA, nitem)            
            #se0_expanded[alpha_end+1 : length(se0_expanded)] <- se00[alpha_g_end+1: length(se00)]

        }else {
            # embretson model 
            
            par <- rbind(matrix(parms_alp,ncol=1), matrix(parms[(2*nitem+1):length(parms)], ncol=1)) 
            colnames(par) <- "Est"
            parms <- c(parms_alp, parms[(2*nitem+1):length(parms)])
                        
            alpha_g_end <- length_item_cov_alpha
            est_alpha_g <- parms[1: alpha_g_end]
            names(parms)[1: alpha_g_end] <- paste("alp_g_", name_it_cov_alpha, 1:length_item_cov_alpha , sep="")

            alpha_end <- alpha_g_end +length_item_cov_alpha
            est_alpha_s <- parms[(alpha_g_end+1): alpha_end]
            names(parms)[(alpha_g_end+1): alpha_end] <- paste("alp_s_", name_it_cov_alpha, 1:length_item_cov_alpha , sep="")

            # se expanded 
            #se0_expanded[1: alpha_g_end] <- se00[1: alpha_g_end]                
            #se0_expanded[(alpha_g_end+1): alpha_end] <- rep(NA, nitem)            
            #se0_expanded[alpha_end+1 : length(se0_expanded)] <- se00[alpha_g_end+1: length(se00)]

        }


        
        if (!is.numeric(item_cov)) {  # item_cov 
        
            # lltm 
            lltm_end <- alpha_end+length_item_cov_beta 
            
            est_lltm <- parms[(alpha_end+1):lltm_end ]
            names(parms)[(alpha_end+1):lltm_end] <- paste("bet_", name_it_cov_beta, 1:length_item_cov_beta , sep="")
        
            if (dif_alpha[1]!=0) {  # dif alpha 
                # lltm + dif_alpha
                
                num_dif_alpha <- length_dif_alpha #sum(S_item[dif_alpha]-1)  # total number of dif parameters (for polytomous items)
                # general dimension 
                dif_alp_g_end <- lltm_end + num_dif_alpha
                est_dif_alp_g <- parms[(lltm_end+1):dif_alp_g_end]
                names(parms)[(lltm_end+1):dif_alp_g_end] <- paste("DIF_alp_g", 1:length_dif_alpha, sep="") 
                
                # specific dimension (no separate loading for specific dimension)
                dif_alp_end <- dif_alp_g_end  #+ num_dif_alpha
                #est_dif_alp_s <- parms[(dif_alp_g_end+1):dif_alp_end]
                #names(parms)[(dif_alp_g_end+1):dif_alp_end] <- paste("DIF_alp_s", 1:length_dif_alpha, sep="") 

                if (dif_beta[1]!=0) {  # dif beta 
                    # lltm + dif_alpha + dif_beta
                    num_dif_beta <- sum(S_item[dif_beta]-1) # total number of dif parameters (for polytomous items)
                    dif_bet_end <- dif_alp_end + num_dif_beta
                    est_dif_bet <- parms[(dif_bet_end+1):dif_bet_end]
                    #names(parms)[(dif_bet_end+1):dif_bet_end] <- paste("DIF_bet",1:length_dif_beta, sep="") 
                    temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= (S_item[dif_beta]-1)[1] )
                    if (length_dif_beta >1) {
                        for (dd in 2: length(dif_beta)) {
                            temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= (S_item[dif_beta]-1)[dd] ))
                        }     
                    }
                    names(parms)[(dif_alp_end+1):dif_bet_end] <- temp
                       
                    # group 
                        # lltm + dif_alpha + dif_beta + group 
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*(ndim+1) #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_mean_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # sd 
                        mg_sd_end <- mg_mean_end  + ngroup*(ndim+1) #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_sd_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag =  ndim #+ (ndim-1)  #(ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp       

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol_bifactor(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))
                            
                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 
                            
                            # all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim+1))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd_g", 1, ng, sep="")
                                for (d in 1:ndim ) {
                                    temp <- c(temp, paste("th_sd_s", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end] <- as.vector(t(temp2))


                        } 
                        if (!is.numeric(person_cov)) {
                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                        
                } else {   
                        # no dif beta  
                        person_end = dif_alp_end 
                        mg_mean_end <- person_end+ ngroup*(ndim+1) #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_mean_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # sd 
                        mg_sd_end <- mg_mean_end  + ngroup*(ndim+1) #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_sd_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag =  ndim #+ (ndim-1)  #(ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp       

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol_bifactor(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 

                            # sd: all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim+1))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd_g", 1, ng, sep="")
                                for (d in 1:ndim ) {
                                    temp <- c(temp, paste("th_sd_s", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end] <- as.vector(t(temp2))

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end] <- as.vector(t(temp3))

                        } 
                        if (!is.numeric(person_cov)) {
                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                       
    
               } 
               
        } else {
                # no alaph dif 
                if (dif_beta[1]!=0) {  # dif beta 
                    # lltm + dif_beta
                    dif_alp_end = lltm_end 
                    num_dif_beta <- sum(S_item[dif_beta]-1) 
                    dif_bet_end <- dif_alp_end + num_dif_beta
                    est_dif_bet <- parms[(dif_bet_end+1):dif_bet_end]
                    #names(parms)[(dif_bet_end+1):dif_bet_end] <- paste("DIF_bet",1:length_dif_beta, sep="") 
                    temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= (S_item[dif_beta]-1)[1] )
                    if (length_dif_beta >1) {
                        for (dd in 2: length(dif_beta)) {
                            temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= (S_item[dif_beta]-1)[dd] ))
                        }     
                    }
                    names(parms)[(dif_alp_end+1):dif_bet_end] <- temp
                       
                    # group 
                        # lltm + dif_beta + group
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*(ndim+1) #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_mean_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # sd 
                        mg_sd_end <- mg_mean_end  + ngroup*(ndim+1) #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_sd_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag =  ndim #+ (ndim-1)  #(ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp       

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol_bifactor(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))
                            
                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 

                            # all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim+1))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd_g", 1, ng, sep="")
                                for (d in 1:ndim ) {
                                    temp <- c(temp, paste("th_sd_s", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end] <- as.vector(t(temp2))

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end] <- as.vector(t(temp3))

                        } 
                        if (!is.numeric(person_cov)) {
                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                        
                } else {   
                    # no beta dif 

                    #  lltm + group + person-cov 
                        dif_bet_end = lltm_end 
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*(ndim+1) #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_mean_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # sd 
                        mg_sd_end <- mg_mean_end  + ngroup*(ndim+1) #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_sd_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag =  ndim #+ (ndim-1)  #(ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp       

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol_bifactor(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 

                            # all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim+1))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd_g", 1, ng, sep="")
                                for (d in 1:ndim ) {
                                    temp <- c(temp, paste("th_sd_s", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end] <- as.vector(t(temp2))

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end] <- as.vector(t(temp3))
                        } 
                        if (!is.numeric(person_cov)) {
                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                       
    
               }  
                    
           
          }
          
        } else { 
    
        # no lltm 
        beta_end <- alpha_end +  sum(S_item-1)
        
        est_beta <- parms[(alpha_end+1):(beta_end)]
        # in case of polytomous items 
        temp <- NA
        for (jj in 1:nitem) {
            temp <- c(temp, rep(jj, (S_item[jj]-1) ))
        }
        names(parms)[(alpha_end+1):(beta_end)]  <- paste("bet",temp[-1], sep="")  
                    
            if (dif_alpha[1]!=0) {  # dif alpha 
                # dif_alpha
                lltm_end = beta_end 
                num_dif_alpha <- length_dif_alpha #sum(S_item[dif_alpha]-1)  # total number of dif parameters (for polytomous items)
                # general dimension 
                dif_alp_g_end <- lltm_end + num_dif_alpha
                est_dif_alp_g <- parms[(lltm_end+1):dif_alp_g_end]
                names(parms)[(lltm_end+1):dif_alp_g_end] <- paste("DIF_alp_g", 1:length_dif_alpha, sep="") 
                
                # specific dimension
                dif_alp_end <- dif_alp_g_end #+ num_dif_alpha
                #est_dif_alp_s <- parms[(dif_alp_g_end+1):dif_alp_end]
                #names(parms)[(dif_alp_g_end+1):dif_alp_end] <- paste("DIF_alp_s", 1:length_dif_alpha, sep="") 
                                
                if (dif_beta[1]!=0) {  # dif beta 
                    # dif_alpha + dif_beta
                    num_dif_beta <- sum(S_item[dif_beta]-1) 
                    dif_bet_end <- dif_alp_end + num_dif_beta
                    est_dif_bet <- parms[(dif_bet_end+1):dif_bet_end]
                    #names(parms)[(dif_bet_end+1):dif_bet_end] <- paste("DIF_bet",1:length_dif_beta, sep="") 
                    temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= (S_item[dif_beta]-1)[1] )
                    if (length_dif_beta >1) {
                        for (dd in 2: length(dif_beta)) {
                            temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= (S_item[dif_beta]-1)[dd] ))
                        }     
                    }
                    names(parms)[(dif_alp_end+1):dif_bet_end] <- temp

                    # group 
                        # dif_alpha + dif_beta + group 
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end  
                        mg_mean_end <- person_end+ ngroup*(ndim+1) #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_mean_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # sd 
                        mg_sd_end <- mg_mean_end  + ngroup*(ndim+1) #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_sd_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag =  ndim #+ (ndim-1)  #(ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp       

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol_bifactor(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 

                            # all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim+1))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd_g", 1, ng, sep="")
                                for (d in 1:ndim ) {
                                    temp <- c(temp, paste("th_sd_s", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end] <- as.vector(t(temp2))

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end] <- as.vector(t(temp3))
                        } 
                        if (!is.numeric(person_cov)) {
                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                        
                } else {   
                    # no beta dif 
                 
                    #  dif_alpha + group + person-cov 
                        dif_bet_end = dif_alp_end
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*(ndim+1) #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_mean_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # sd 
                        mg_sd_end <- mg_mean_end  + ngroup*(ndim+1) #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_sd_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag =  ndim #+ (ndim-1)  #(ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp       

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol_bifactor(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 

                            # all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim+1))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd_g", 1, ng, sep="")
                                for (d in 1:ndim ) {
                                    temp <- c(temp, paste("th_sd_s", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end] <- as.vector(t(temp2))

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end] <- as.vector(t(temp3))

                        } 
                        if (!is.numeric(person_cov)) {
                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                       
    
               } 
               
        } else {
            lltm_end = beta_end 
                if (dif_beta[1]!=0) {  # dif beta 
                    lltm_end = beta_end 
                    # dif_beta
                    dif_alp_end = lltm_end 
                    num_dif_beta <- sum(S_item[dif_beta]-1) 
                    dif_bet_end <- dif_alp_end + num_dif_beta
                    est_dif_bet <- parms[(dif_bet_end+1):dif_bet_end]
                    #names(parms)[(dif_bet_end+1):dif_bet_end] <- paste("DIF_bet",1:length_dif_beta, sep="") 
                    temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= (S_item[dif_beta]-1)[1] )
                    if (length_dif_beta >1) {
                        for (dd in 2: length(dif_beta)) {
                            temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= (S_item[dif_beta]-1)[dd] ))
                        }     
                    }
                    names(parms)[(dif_alp_end+1):dif_bet_end] <- temp
                                           
                    # group 
                        # dif_beta + group
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*(ndim+1) #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_mean_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # sd 
                        mg_sd_end <- mg_mean_end  + ngroup*(ndim+1) #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_sd_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag =  ndim #+ (ndim-1)  #(ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp       

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol_bifactor(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))
                            
                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 

                            # all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim+1))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd_g", 1, ng, sep="")
                                for (d in 1:ndim ) {
                                    temp <- c(temp, paste("th_sd_s", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end] <- as.vector(t(temp2))

                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end] <- as.vector(t(temp3))
                            
                        } 
                        if (!is.numeric(person_cov)) {
                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                        
                } else {   
                    dif_bet_end = lltm_end 

                    # group + person-cov 
                        dif_bet_end = lltm_end 
                        #mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        person_end = dif_bet_end 
                        mg_mean_end <- person_end+ ngroup*(ndim+1) #(for mean)
                        mg_mean <- parms[(person_end+ 1) : mg_mean_end ]
                        temp <- paste("th_mean_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_mean_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(person_end + 1) : mg_mean_end] <- temp
                        
                        # sd 
                        mg_sd_end <- mg_mean_end  + ngroup*(ndim+1) #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        temp <- paste("th_sd_g", 1, 1:ngroup, sep="")
                        for (d in 1:ndim ) {
                            temp <- c(temp, paste("th_sd_s", d, 1:ngroup, sep=""))
                        }
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- temp         
                        
                        off_diag =  ndim #+ (ndim-1)  #(ndim*(ndim-1)/2)
                        mg_cov_end <- mg_sd_end   + off_diag*ngroup # cov 
                        mg_cov <- parms[(mg_sd_end + 1) : mg_cov_end ]
                        temp <- paste("th_cov", 1, 1:ngroup, sep="")
                        if (off_diag >1) {
                            for (d in 2:off_diag) {
                                temp <- c(temp, paste("th_cov", d, 1:ngroup, sep=""))
                            }
                        }
                        names(parms)[(mg_sd_end + 1) : mg_cov_end] <- temp       

                        # covariance matrix 
                        if (ngroup >0) {
                            out_cov <- cov_from_chol_bifactor(parms=parms[(mg_mean_end + 1):mg_cov_end], ndim=ndim, ngroup=ngroup)
                            par[(mg_mean_end + 1):mg_cov_end,1] <- c(unlist(out_cov$sd_list), unlist(out_cov$cov_list))

                            if (!is.null(se0)) {
                                par[(mg_mean_end + 1):mg_cov_end,2] <- rep(NA, length((mg_mean_end + 1):mg_cov_end))
                            } 

                            # all groups (group 1 (all dimensions), group 2( all dimensions), ...
                            temp2 <- matrix(NA, ngroup, (ndim+1))
                            for (ng in 1:ngroup) {
                                temp <- paste("th_sd_g", 1, ng, sep="")
                                for (d in 1:ndim ) {
                                    temp <- c(temp, paste("th_sd_s", d,ng, sep=""))
                                }
                                temp2[ng,] <- temp
                            }
                            names(parms)[(mg_mean_end + 1) : mg_sd_end] <- as.vector(t(temp2))
                            # cov: all groups (group 1 (all dimensions), group 2( all dimensions), ...                          
                            temp3 <- matrix(NA, ngroup, off_diag)
                            for (ng in 1:ngroup) {
                                temp <- paste("th_cov", 1, ng, sep="")
                                if (off_diag >1) {
                                    for (d in 2:off_diag ) {
                                        temp <- c(temp, paste("th_cov", d,ng, sep=""))
                                    }
                                }
                                temp3[ng,] <- temp
                            }
                            names(parms)[(mg_sd_end + 1) : mg_cov_end] <- as.vector(t(temp3))

                        } 
                        if (!is.numeric(person_cov)) {
                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_cov_end + length_person_cov 
                            est_person_cov <- parms[(mg_cov_end+1): person_end]
                            names(parms)[(mg_cov_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                       
    
               }  
                    
           
          }
    
          
    
        }

    }


##################################
## 3pl Unidimensional models 
##################################

    if (model%in%c(7)) {

        # alpha parameters 
        if (is.numeric(embretson)) {
            alpha_end <- nitem
            
            est_alpha <- parms[1: alpha_end]
            names(parms)[1: alpha_end] <- paste("alp", 1:nitem, sep="")
        } else {
            # embretson model 
            
            par <- rbind(matrix(parms_alp,ncol=1), matrix(parms[(nitem+1):length(parms)], ncol=1)) 
            colnames(par) <- "Est"
            parms <- c(parms_alp, parms[(nitem+1):length(parms)])

            alpha_end <- length_item_cov_alpha
            est_alpha <- parms[1: alpha_end]
            names(parms)[1: alpha_end] <- paste("alp_", name_it_cov_alpha, 1:length_item_cov_alpha , sep="")

        }
        
        if (!is.numeric(item_cov)) {  # item_cov 
        
            # lltm 
            lltm_end <- alpha_end+length_item_cov_beta 
            
            est_lltm <- parms[(alpha_end+1):lltm_end ]
            names(parms)[(alpha_end+1):lltm_end] <- paste("bet_", name_it_cov_beta, 1:length_item_cov_beta , sep="")
        
            if (dif_alpha[1]!=0) {  # dif alpha 
                # lltm + dif_alpha
                
                num_dif_alpha <- length(dif_alpha) #sum(S_item[dif_alpha]-1)  # total number of dif parameters (for polytomous items)
                dif_alp_end <- lltm_end + num_dif_alpha
                est_dif_alp <- parms[(lltm_end+1):dif_alp_end]
                #names(parms)[(lltm_end+1):dif_alp_end] <- paste("DIF_alp", 1:length_dif_alpha, sep="") 
                temp <- rep(paste("DIF_alp", dif_alpha[1], sep=""), each= 1 ) #(S_item[dif_alpha]-1)[1]
                if (length_dif_alpha >1) {
                    for (dd in 2: length(dif_alpha)) {
                        temp <- c(temp, rep(paste("DIF_alp", dif_alpha[dd], sep=""), each=1) ) #each= (S_item[dif_alpha]-1)[dd] )
                    }     
                }
                names(parms)[(lltm_end+1):dif_alp_end] <- temp

                if (dif_beta[1]!=0) {  # dif beta 
                    # lltm + dif_alpha + dif_beta
                    num_dif_beta <- length(dif_beta) #sum(S_item[dif_beta]-1) 
                    dif_bet_end <- dif_alp_end + num_dif_beta
                    est_dif_bet <- parms[(dif_bet_end+1):dif_bet_end]
                    #names(parms)[(dif_bet_end+1):dif_bet_end] <- paste("DIF_bet",1:length_dif_beta, sep="") 
                    temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= 1) #(S_item[dif_beta]-1)[1] 
                    if (length_dif_beta >1) {
                        for (dd in 2: length(dif_beta)) {
                            temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= 1) ) #(S_item[dif_beta]-1)[dd] )
                        }     
                    }
                    names(parms)[(dif_alp_end+1):dif_bet_end] <- temp
                       
                    # group 
                        # lltm + dif_alpha + dif_beta + group 
                        mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        mg_mean <- parms[(dif_bet_end + 1) : mg_mean_end ]
                        names(parms)[(dif_bet_end + 1) : mg_mean_end] <- paste("th_mean", 1:ngroup, sep="")
                        
                        mg_sd_end <- mg_mean_end  + ngroup #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- paste("th_sd", 1:ngroup, sep="")                    
                    
                        if (!is.numeric(person_cov)) {
                            #  lltm + dif_alpha + dif_beta + group + person-cov 
                            person_end <- mg_sd_end + length_person_cov 
                            est_person_cov <- parms[(mg_sd_end+1): person_end]
                            names(parms)[(mg_sd_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                        
                } else {   
                    #  lltm + dif_alpha + group + person-cov 
                        dif_bet_end = dif_alp_end 
                        mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        mg_mean <- parms[(dif_bet_end + 1) : mg_mean_end ]
                        names(parms)[(dif_bet_end + 1) : mg_mean_end] <- paste("th_mean", 1:ngroup, sep="")
                        
                        mg_sd_end <- mg_mean_end  + ngroup #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- paste("th_sd", 1:ngroup, sep="")                    
                           
                        if (!is.numeric(person_cov)) {
                            #mg_sd_end = dif_alp_end 
                            person_end <- mg_sd_end + length_person_cov 
                            est_person_cov <- parms[(mg_sd_end+1): person_end]
                            names(parms)[(mg_sd_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                        } 
                       
    
               } 
               
        } else {
 
                if (dif_beta[1]!=0) {  # dif beta 
                    # lltm + dif_beta
                    dif_alp_end = lltm_end 
                    num_dif_beta <- length(dif_beta) #sum(S_item[dif_beta]-1) 
                    dif_bet_end <- dif_alp_end + num_dif_beta
                    est_dif_bet <- parms[(dif_bet_end+1):dif_bet_end]
                    #names(parms)[(dif_bet_end+1):dif_bet_end] <- paste("DIF_bet",1:length_dif_beta, sep="") 
                    temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= 1) #(S_item[dif_beta]-1)[1] 
                    if (length_dif_beta >1) {
                        for (dd in 2: length(dif_beta)) {
                            temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= 1 )) #(S_item[dif_beta]-1)[dd]
                        }     
                    }
                    names(parms)[(dif_alp_end+1):dif_bet_end] <- temp

                       
                    # group 
                        # lltm + dif_beta + group
                        mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        mg_mean <- parms[(dif_bet_end + 1) : mg_mean_end ]
                        names(parms)[(dif_bet_end + 1) : mg_mean_end] <- paste("th_mean", 1:ngroup, sep="")
                        
                        mg_sd_end <- mg_mean_end  + ngroup #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- paste("th_sd", 1:ngroup, sep="")                    
                    
                        if (!is.numeric(person_cov)) {
                            #  lltm + dif_beta + mg + person-cov 
                            person_end <- mg_sd_end + length_person_cov 
                            est_person_cov <- parms[(mg_sd_end+1): person_end]
                            names(parms)[(mg_sd_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                        
                } else {   
                    #  lltm + group + person-cov 
                        dif_bet_end = lltm_end 
                        mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        mg_mean <- parms[(dif_bet_end + 1) : mg_mean_end ]
                        names(parms)[(dif_bet_end + 1) : mg_mean_end] <- paste("th_mean", 1:ngroup, sep="")
                        
                        mg_sd_end <- mg_mean_end  + ngroup #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- paste("th_sd", 1:ngroup, sep="")                    
                           
                        if (!is.numeric(person_cov)) {
                            #mg_sd_end = dif_alp_end 
                            person_end <- mg_sd_end + length_person_cov 
                            est_person_cov <- parms[(mg_sd_end+1): person_end]
                            names(parms)[(mg_sd_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                        } 
                       
    
               }  
                    
           
          }
          
       ## lltm on guessing ? (later)   
          
          
        } else { # no lltm 
    
        # no lltm 
        beta_end <- alpha_end +  sum(S_item-1)
        
        est_beta <- parms[(alpha_end+1):(beta_end)]
        # in case of polytomous items 
        temp <- NA
        for (jj in 1:nitem) {
            temp <- c(temp, rep(jj, (S_item[jj]-1) ))
        }
        names(parms)[(alpha_end+1):(beta_end)]  <- paste("bet",temp[-1], sep="")  
                    
            if (dif_alpha[1]!=0) {  # dif alpha 
                # dif_alpha
                lltm_end = beta_end 
                num_dif_alpha <-  length_dif_alpha  #sum(S_item[dif_alpha]-1)  
                dif_alp_end <- lltm_end + num_dif_alpha
                est_dif_alp <- parms[(lltm_end+1):dif_alp_end]
                names(parms)[(lltm_end+1):dif_alp_end] <- paste("DIF_alp", 1:length_dif_alpha, sep="") 
                #temp <- rep(paste("DIF_alp", dif_alpha[1], sep=""), each= (S_item[dif_alpha]-1)[1] )
                #if (length_dif_alpha >1) {
                #    for (dd in 2: length(dif_alpha)) {
                #        temp <- c(temp, rep(paste("DIF_alp", dif_alpha[dd], sep="")) #, each= (S_item[dif_alpha]-1)[dd] )
                #    }     
                #}
                #names(parms)[(lltm_end+1):dif_alp_end] <- temp 
                                
                if (dif_beta[1]!=0) {  # dif beta 
                    # dif_alpha + dif_beta
                    num_dif_beta <- sum(S_item[dif_beta]-1) 
                    dif_bet_end <- dif_alp_end + num_dif_beta
                    est_dif_bet <- parms[(dif_bet_end+1):dif_bet_end]
                    #names(parms)[(dif_bet_end+1):dif_bet_end] <- paste("DIF_bet",1:length_dif_beta, sep="") 
                    temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= (S_item[dif_beta]-1)[1] )
                    if (length_dif_beta >1) {
                        for (dd in 2: length(dif_beta)) {
                            temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= (S_item[dif_beta]-1)[dd] ))
                        }     
                    }
                    names(parms)[(dif_alp_end+1):dif_bet_end] <- temp


                    # group 
                        # dif_alpha + dif_beta + group 
                        mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        mg_mean <- parms[(dif_bet_end + 1) : mg_mean_end ]
                        names(parms)[(dif_bet_end + 1) : mg_mean_end] <- paste("th_mean", 1:ngroup, sep="")
                        
                        mg_sd_end <- mg_mean_end  + ngroup #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- paste("th_sd", 1:ngroup, sep="")                    
                    
                        if (!is.numeric(person_cov)) {
                            # dif_alpha + dif_beta + mg + person-cov 
                            person_end <- mg_sd_end + length_person_cov 
                            est_person_cov <- parms[(mg_sd_end+1): person_end]
                            names(parms)[(mg_sd_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                        
                } else {   
                    #  dif_alpha + group + person-cov 
                        dif_bet_end = dif_alp_end # lltm_end 
                        mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        mg_mean <- parms[(dif_bet_end + 1) : mg_mean_end ]
                        names(parms)[(dif_bet_end + 1) : mg_mean_end] <- paste("th_mean", 1:ngroup, sep="")
                        
                        mg_sd_end <- mg_mean_end  + ngroup #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- paste("th_sd", 1:ngroup, sep="")                    
                           
                        if (!is.numeric(person_cov)) {
                            #mg_sd_end = dif_alp_end 
                            person_end <- mg_sd_end + length_person_cov 
                            est_person_cov <- parms[(mg_sd_end+1): person_end]
                            names(parms)[(mg_sd_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                        } 
                       
    
               } 
               
        } else {  # no dif alpha 
            lltm_end = beta_end 
                if (dif_beta[1]!=0) {  # dif beta 
                    lltm_end = beta_end 
                    # dif_beta
                    dif_alp_end = lltm_end 
                    num_dif_beta <- sum(S_item[dif_beta]-1) 
                    dif_bet_end <- dif_alp_end + num_dif_beta
                    est_dif_bet <- parms[(dif_bet_end+1):dif_bet_end]
                    #names(parms)[(dif_bet_end+1):dif_bet_end] <- paste("DIF_bet",1:length_dif_beta, sep="") 
                    temp <- rep(paste("DIF_bet", dif_beta[1], sep=""), each= (S_item[dif_beta]-1)[1] )
                    if (length_dif_beta >1) {
                        for (dd in 2: length(dif_beta)) {
                            temp <- c(temp, rep(paste("DIF_bet", dif_beta[dd], sep=""), each= (S_item[dif_beta]-1)[dd] ))
                        }     
                    }
                    names(parms)[(dif_alp_end+1):dif_bet_end] <- temp
                       
                    # group 
                        # dif_beta + group
                        mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        mg_mean <- parms[(dif_bet_end + 1) : mg_mean_end ]
                        names(parms)[(dif_bet_end + 1) : mg_mean_end] <- paste("th_mean", 1:ngroup, sep="")
                        
                        mg_sd_end <- mg_mean_end  + ngroup #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- paste("th_sd", 1:ngroup, sep="")                    
                    
                        if (!is.numeric(person_cov)) {
                            #  lltm + dif_beta + group + person-cov 
                            person_end <- mg_sd_end + length_person_cov 
                            est_person_cov <- parms[(mg_sd_end+1): person_end]
                            names(parms)[(mg_sd_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                         
                        }
                        
                } else {   
                    # group + person-cov 
                        dif_bet_end = lltm_end 
                        mg_mean_end <- dif_bet_end + ngroup #(for mean)
                        mg_mean <- parms[(dif_bet_end + 1) : mg_mean_end ]
                        names(parms)[(dif_bet_end + 1) : mg_mean_end] <- paste("th_mean", 1:ngroup, sep="")
                        
                        mg_sd_end <- mg_mean_end  + ngroup #(for sd)
                        mg_sd <- parms[(mg_mean_end + 1) : mg_sd_end ]
                        names(parms)[(mg_mean_end + 1) : mg_sd_end] <- paste("th_sd", 1:ngroup, sep="")                    
                           
                        if (!is.numeric(person_cov)) {
                            #mg_sd_end = dif_alp_end 
                            person_end <- mg_sd_end + length_person_cov 
                            est_person_cov <- parms[(mg_sd_end+1): person_end]
                            names(parms)[(mg_sd_end+1): person_end] <- paste("gam_", name_pp_cov, 1:length_person_cov , sep="")
                        } 
                       
    
               }  
                    
           
          }
    
         # guessing parameters (for all cases)
         
         if (!is.numeric(person_cov)) {
            mg_sd_end  <- person_end
         }
            
         guessing_end <- mg_sd_end + nitem 
         guessing <- exp(parms[(mg_sd_end+ 1) : guessing_end ])
         parms[(mg_sd_end + 1) : guessing_end] <- guessing 
         names(parms)[(mg_sd_end + 1) : guessing_end] <- paste("guess", 1:nitem, sep="")    
         
         parms <- parms[1:guessing_end]
         par <- par[1:guessing_end,] 
         par[1:guessing_end,1] <- parms 
                  
        }



    }



#####################  
# naming 
#####################  

    if (!is.null(se0)) {
        rownames(par) <- names(parms)
    } else {
        rownames(par) <- names(parms)    
    }

  # for now, no stadard errors 
    if (model==6) {
        par <- matrix(par[,1], ncol=1) # rep(NA, nrow(par)) 
        rownames(par) <- names(parms)  
    } else {
        par <- par
        rownames(par) <- names(parms)  
    }


    # boundary values 
    
    index_b <- NULL
    index_b <- unique(c(which(parms==15) , which(parms== -15)  ))
    
    if (ncol(par)==1) {
    
        if (length(index_b)>0) {
            par[index_b,1] <- rep(NA,1) 
            warning("Some parameter estimates are boundary values (NA is returned).")
        }    
    
    } else {
    
        if (length(index_b)>0) {
            par[index_b,1:2] <- rep(NA,2) 
            warning("Some parameter estimates are boundary values (NA is returned).")
        }     
    
    }
        
#############################################
#    5. Model fit 
#############################################    

    # rasch, mul rasch 
    if (model%in% c(1,3)) {
        # number of parameters             
        npar <- length(parms) - n_const
        
        if (const_nr >0) {
        npar <- length(parms) - const_nr
        }
        
        names(npar) <- "npar"
        
        # constraints ?
        # n_const
        
    }
    
    # 2pl model 
    if (model%in% c(2) ) {
        # number of parameters             
        npar <- length(parms)   - n_const #- 2

        if (const_nr >0) {
        npar <- length(parms)- const_nr
        }

        names(npar) <- "npar"
    }

    # mul 2pl model 
    if (model%in% c(4) ) {
        # number of parameters             
        npar <- length(parms)  - n_const # - 2*ndim  

        if (const_nr >0) {
        npar <- length(parms) - const_nr
        }

        names(npar) <- "npar"
    }


    # bifactor model + second-order model 
    if (model%in% c(5,6) ) {
        # number of parameters             
        npar <- length(parms)  - n_const # - 2*(ndim+1) - ndim 
        if (const_nr >0) {
        npar <- length(parms) - const_nr
        }

        names(npar) <- "npar"
    }        
    
#    if (bifac==1) {
#
#        # number of parameters             
#        if (model ==31) npar <- length(parms) # correlation 
#        if (model ==32) npar <- length(parms)- length(cor) # independence    
#        if (model ==33) npar <- length(parms)- length(cor) - length(std) # independence meanonly
#        if (model ==34) npar <- length(parms)- length(cor) - 2 * ntestlet # main 
#        if (model ==35) npar <- length(parms)- length(cor) - 2 * ntestlet - 1 # main meanonly        
#        if (model ==36) npar <- length(parms)- length(cor) - length(mean) - length(std) # noimpact 
#        names(npar) <- "npar"
#    
#    } 


    # 3pl model 
    if (model%in% c(7) ) {
        # number of parameters             
        npar <- length(parms)   - n_const - nitem 

        if (const_nr >0) {
        npar <- length(parms)- const_nr - nitem 
        }

        names(npar) <- "npar"
    }


    # log-likelihood    
    loglik <- as.numeric(read.table(paste("loglikelihood.txt",sep=""))[,1])
    names(loglik) <- "loglik"
    # deviance , AIC, BIC 
    
    dev <- -2* loglik 
    names(dev) <- "dev"
    AIC <- -2*loglik+2*npar;
    names(AIC) <- "AIC"
    BIC <- -2*loglik+npar*log(nobs);
    names(BIC) <- "BIC"
    
    #output_fit <- list("loglik"=loglik,"dev"=dev,"AIC"=AIC,"BIC"=BIC,"npar"=npar)
    
    
    # final number of iterations 
    final_it <- round(as.numeric(read.table(paste("final_it.txt",sep=""))[,1]),0)
    names(final_it) <- "final_it"    
    
#############################################
#    6. Return values 
#############################################   
    
     out <- new('flirtClass', direct = direct, 
            pars= par, se = se0, se_emp=se_emp, parms=parms, out_cov=out_cov, 
            first_order=first_order, second_order=second_order, variances_first=variances_first, stand_first_order=stand_first_order, 
            cor_first_second=cor_first_second, 
            info_num = infonum_mat, info_emp = infoemp_mat, 
            loglik=loglik, AIC=AIC, BIC=BIC, npar=npar,  
            post = list('eap'=eap_s, 'eap_var'=var_s, 'eap_cov' = cov_s, 'exp'=exp_s, 'rel'=rel_e),
            data_inf = list('nobs'=nobs, 'ngroup'=ngroup, 'nitem'=nitem, 'MS_item'=MS_item, 'S_item'=S_item), 
            dim_inf = list('within'=within_name, 'ndim'=ndim, 'n_it_dim'=n_it_dim), 
            inside = inside,  model = model, 
            dif_beta=dif_beta, dif_alpha=dif_alpha, 
            est_inf =list('adapt'=adapt, 'nqr'=nqr, 'conv'=conv, 'max_it'=max_it, 'final_it'=final_it, 'link'=link, 
                'alp_bounds_up'=alp_bounds_up,  'alp_bounds_low'=alp_bounds_low, 'verbose'=verbose ),
                'model_name' = model_name, 'Call'=Call)
            
     return(out)
       
}

#############################################
#    Methods 
#############################################


setMethod(
    f = "show",
    signature = signature(object = 'flirtClass'),
    definition = function(object){  

        cat("Estimation of", object@model_name, "\n")
        
        if (object@dim_inf$ndim >1) 
        cat("with", object@dim_inf$ndim, "dimensions", "\n")
        
        if(object@est_inf$adapt==1) 
        cat("\nusing Adaptive quadrature", "\n")
        cat("using", object@est_inf$nqr, "quadrature points,", "convergence =", 
                object@est_inf$conv, "and total iterations=", object@est_inf$final_it,"\n")   
        link_f <- if (object@est_inf$link==1) "logit" else if (object@est_inf$link==3) "adjacent logit" else if (object@est_inf$link==2) "cumulative logit"
        cat("with", link_f, "link function",  "\n")      
        
        cat("\nLog-likelihood =", object@loglik, "with npar=", object@npar, "\n")
        cat("AIC =", object@AIC, "\n")       
        cat("BIC =", object@BIC, "\n")        
    }
)



setMethod(
    f = "summary",
    signature = 'flirtClass',
    definition = function(object, digits = 3, ...){
        cat("Estimation of", object@model_name, "\n")
        
        cat("\nData:\n")
        print(c(object@data_inf$nobs, object@data_inf$nitem, object@data_inf$MS_item, object@data_inf$ngroup))
        
        cat("\nModel fit:\n")

        print(c(round(object@npar, 0), round(object@AIC,0), round(object@BIC,0), 
                round(object@loglik,0) ))
        
        if (object@model%in% c(2,4)){ # 2pl models 
        cat("\nParameterization:\n") 
        print(c(object@inside ))  
        }

        if (object@model%in% c(3,4)) { # for multidimensional model 
        cat("\nType:\n") 
        print(c(object@dim_inf$within) )      
        }
        
        if (object@model%in% c(3,4,5)) { # for multidimensional model 
        cat("\nDimension:\n") 
        print(c(object@dim_inf$ndim, object@dim_inf$n_it_dim))      
        }
                
        cat("\nParameter estimates:\n") 

        printCoefmat(object@pars, P.value=F, has.Pvalue=F)
        cat("\n")

    }
)



setMethod(
    f = "coef",
    signature = 'flirtClass',
    definition = function(object, digits = NULL, ...){  
        num <- if (is.null(digits) ) 3 else digits 
        parameters <- as.matrix(object@pars)
        
        result <- round(parameters, num)   
        result 
        #invisible(result)
    }
)


setMethod(
    f = "logLik",
    signature = 'flirtClass',
    definition = function(object, digits = NULL, ...){  
    
    num <- if (is.null(digits)) 3 else digits 
    out <- round(object@loglik, num)
    attr(out, "npar") <- object@npar
    attr(out, "nobs") <- object@data_inf$nobs
    class(out) <- "logLik"
    return(out)    
    }
)


setMethod(
    f = "anova",
    signature = signature(object = 'flirtClass'),
    definition = function(object, object2, ...){
        dots <- list(...)       
        df <-  object2@npar  - object@npar 
        if(df < 0 ){
            stop("'object' is not nested in 'object2'. \n")
        }
        X2 <- 2*object2@loglik - 2*object@loglik      
        #if (X2 <0 )
        #warning("the two models are not nested or fell on a local maxima. \n")
          
        AICdiff <- object@AIC - object2@AIC    
        BICdiff <- object@BIC - object2@BIC
        cat("\nChi-squared difference: \nX2 = ", round(X2,3), ", df = ",
            df, ", p = ", round(1 - pchisq(X2,abs(df)),4), "\n", sep="")
        cat("AIC difference = ", round(AICdiff,3), "\n")  
        cat("BIC difference = ", round(BICdiff,3), "\n")
    }
)


#############################################
#    Functions 
#############################################

## multidimensional models 
cov_from_chol <- function(parms, ndim, ngroup) {

    nelement = (ndim*(ndim-1)/2) + ndim 
    element <- matrix(NA, nelement, ngroup) 
    for (d in 1: nelement) {
        element[d,] <- parms[((d-1)*ngroup+1): (d*ngroup) ]
    }
    colnames(element) <- paste("group", 1:ngroup, sep="")

    # construct covariance 
    covM <- vector("list", ngroup)
    sdG <- vector("list", ngroup)
    covG <- vector("list", ngroup)
    for (g in 1: ngroup ) {
        # group g 
        aa <- diag(element[1:ndim,g])  # lower triangle 
        if (ndim>1) {
            second = ndim  
            for (d in 2:ndim) {
                first = second +1
                second= first +(d-2) 
                aa[d,1:(d-1)] <- element[ (first : second),g  ]
    
            }
        }
    
        bb <- aa %*% t(aa)  # covariance matrix 
        est_sd <- sqrt(diag(bb))
        est_cov <- bb[col(bb)<row(bb)] 
        covM[[g]] <- bb 
        sdG[[g]] <- est_sd 
        covG[[g]] <- est_cov 
    }


    return(list("cov_matrix"=covM, "sd_list"=sdG, "cov_list"=covG)) 

}


## bifactor model 

cov_from_chol_bifactor <- function(parms, ndim, ngroup) {

    nelement = (ndim+1) + ndim 
    element <- matrix(NA, nelement, ngroup) 
    for (d in 1: nelement) {
        element[d,] <- parms[((d-1)*ngroup+1): (d*ngroup) ]
    }
    colnames(element) <- paste("group", 1:ngroup, sep="")

    # construct covariance 
    covM <- vector("list", ngroup)
    sdG <- vector("list", ngroup)
    covG <- vector("list", ngroup)
    for (g in 1: ngroup ) {
        # group g 
        aa <- diag(element[1:(ndim+1),g])  # lower triangle 
        aa[2:(ndim+1),1] <- element[(ndim+1+1): (ndim+1+ndim),g]
    
        bb <- aa %*% t(aa)  # covariance matrix 
        est_sd <- sqrt(diag(bb))
        est_cov <- bb[2:(ndim+1), 1]  #bb[col(bb)<row(bb)] 
        covM[[g]] <- bb 
        sdG[[g]] <- est_sd 
        covG[[g]] <- est_cov 
    }


    return(list("cov_matrix"=covM, "sd_list"=sdG, "cov_list"=covG)) 

}
