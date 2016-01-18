

input <- function(model, data, select, subset, loading=list(on, inside), mul=list(on, dim_info, cov_info), bifac=list(on, dim_info, cov_info), 
                    second=list(on, dim_info, cov_info), guess=list(on), 
                    person_cov=list(on, person_matrix, main), item_cov=list(on, item_matrix_beta, item_matrix_alpha), dif=list(on, dif_beta, dif_alpha), 
                    mg=list(on, group_matrix), mixture =list(on, num), 
                    weight=list(on, weight_matrix), post, 
                    start = list(on, npar, start_info, start_value),
                    constraint = list(on, npar, cons_info, cons_value), 
                    evaluate = list(on, eval_value), 
                    control=list(minpercent, max_it, nq, conv, link, adapt, se_num, se_emp, alp_bounds_up, alp_bounds_low, verbose , show)){
 
    ## read input 
    resp <- data
    # item names 
    item_name <- colnames(resp)

    # Number of cases 
    N <- nrow(resp)

    # Number of Items
    I <- ncol(resp)


    # Max item responses
    S_item <- apply(resp, 2, max, na.rm=T)
    M_item <- apply(resp,2,min, na.rm=T)
    #if (min(S_item) !=0 ) warning("minimum response category should be 0")

    # number of categories 
    S_cat <- apply(resp, 2, count)
     
    loading_on <- loading$on
    mul_on <- mul$on 
    mul_dim_info <- mul$dim_info
    mul_cov_info <- mul$cov_info
    bifac_on <- bifac$on
    bifac_dim_info <- bifac$dim_info 
    bifac_cov_info <- bifac$cov_info
    second_on <- second$on
    second_dim_info <- second$dim_info 
    second_cov_info <- second$cov_info

    guess_on <- guess$on
    
    person_cov_on <- person_cov$on 
    person_main <- person_cov$main
    item_cov_on <- item_cov$on 
    item_main <- item_cov$main 
    #embretson <- item_cov$embretson
    embretson_T <- if (is.null(item_cov$item_matrix_alpha)) 0 else 1  
    if (embretson_T ==1 & loading_on==0 & bifac_on==0  & second_on==0) 
    stop("Item covariates can be regressed on alpha only when alpha is estimated")

    # second order 
    #if (embretson_T ==1 & second_on==1) 
    #stop("Item covariates can be regressed only on beta for second-order models")

     
    dif_on <- dif$on 
    mg_on <- mg$on
    mixture_on <- mixture$on 
    mixture_num <- if (mixture_on==1) 1 else mixture$num 
    weight_on <- weight$on 
    cov_info_file <- 0

    # model parametrization 
    on_theta <- if(person_cov$on==1 && person_main==1 ) 0   else if(person_cov$on==1 && person_main==1) 0 else 0 
    on_beta <- if(item_cov$on==1 && loading$inside==1) 1 else if(loading$inside==1)  1 else 0        
    
    # person_cov
    if(person_cov_on==0) { person_cov_file <- 0 
    }else {
        if (is.null(person_cov$person_matrix)) {
            stop("person_matrix should be provided. \n")
        } else {
            pp_cov <- if(is.vector(person_cov$person_matrix)) as.matrix(data[,person_cov$person_matrix]) else data.frame(person_cov$person_matrix) 
            if(nrow(pp_cov) !=N) stop("number of cases in person_matrix should be same as number of cases in data")
            person_cov_file <- "person_cov.txt"
            name_pp_cov <- colnames(pp_cov)
            write.table(pp_cov, person_cov_file, row.names=FALSE, col.names =FALSE, na="-1")     
        
        }
    }
    
    
    # mg 
    if(mg_on==0) { person_group_file  <-  0 
    } else {
        if (is.null(mg$group_matrix)) {
            stop("group_matrix should be provided. \n")
        } else { 
            pp_mg <- if(is.vector(mg$group_matrix)) as.matrix(data[,mg$group_matrix]) else data.frame(mg$group_matrix) 
            if(ncol(pp_mg) !=1) stop("number of columns in group_matrix should be 1")
            if(nrow(pp_mg) !=N) stop("number of cases in group_matrix should be same as number of cases in data")
            mg_cat <- min(pp_mg)
            if (mg_cat !=0 ) stop("group membership should start from 0")
            #if (!is.integer(pp_mg) ) warning("group membership should be integer")
            person_group_file <- "person_group.txt"
            name_pp_mg <- colnames(pp_mg)
            write.table(pp_mg, person_group_file, row.names=FALSE, col.names =FALSE, na="-1")     
        }
    }
     
    # dim_info: number of dimensions, items in each dimension 
    # dim_info = list(dim1=1:5, dim2=3:10)
    if (mul_on ==1 ) {
        #within <- 0 
        if (is.null(mul$dim_info)) {
            stop("dimension information should be provided. \n")
        } else {     
            ndim <- length(mul$dim_info)
            aa <- length(mul$dim_info[[1]])
            for (d in 2:ndim) {
                aa <- c(aa, length(mul$dim_info[[d]]))
            }
            n_it_dim <- aa 
            
            # loading matrix 
            nI <- length(unique(unlist(mul$dim_info)))  # n of items 
            within <- if (nI < length(unlist(mul$dim_info))) 1 else 0 
            if (I > length(unlist(mul$dim_info))) warning("all items should be used to define dimensions")
            
            load <- matrix(0, nI, ndim)
            rownames(load) <- item_name
            for (d in 1: ndim) {
                load[mul$dim_info[[d]],d] <- 1
            }
            dim_info <- load 
            dim_info_file <- "dim_info.txt"
            write.table(dim_info, dim_info_file , row.names=FALSE, col.names =FALSE, na="-1")
        }
    } else {
        within <- 0
        n_it_dim  <- 0
        dim_info_file <- 0
        
    }
    # for bifactor models 
    # dim_info: number of dimensions, items in each dimension 
    if (bifac_on ==1  ) {
        #within <- 0 
        if (is.null(bifac$dim_info)) {
            stop("dimension information should be provided. \n")
        } else {     
            ndim <- length(bifac$dim_info) #not include general dimension  
            aa <- length(bifac$dim_info[[1]])
            for (d in 2:ndim) {
                aa <- c(aa, length(bifac$dim_info[[d]]))
            }
            n_it_dim <- aa 
            
            # loading matrix 
            nI <- length(unique(unlist(bifac$dim_info)))  # n of items 
            within <- if (nI < length(unlist(bifac$dim_info))) 1 else 0 
            
            load <- matrix(0, nI, ndim)
            for (d in 1: ndim) {
                load[bifac$dim_info[[d]],d] <- 1
            }
            dim_info <- load 
            dim_info_file <- "dim_info.txt"
            write.table(dim_info, dim_info_file , row.names=FALSE, col.names =FALSE, na="-1")
        }
    } 

# for second-order models 
    # dim_info: number of dimensions, items in each dimension 
    if (second_on ==1  ) {
        #within <- 0 
        if (is.null(second$dim_info)) {
            stop("dimension information should be provided. \n")
        } else {     
            ndim <- length(second$dim_info) #not include general dimension  
            aa <- length(second$dim_info[[1]])
            for (d in 2:ndim) {
                aa <- c(aa, length(second$dim_info[[d]]))
            }
            n_it_dim <- aa 
            
            # loading matrix 
            nI <- length(unique(unlist(second$dim_info)))  # n of items 
            within <- if (nI < length(unlist(second$dim_info))) 1 else 0 
            
            load <- matrix(0, nI, ndim)
            for (d in 1: ndim) {
                load[second$dim_info[[d]],d] <- 1
            }
            dim_info <- load 
            dim_info_file <- "dim_info.txt"
            write.table(dim_info, dim_info_file , row.names=FALSE, col.names =FALSE, na="-1")
        }
    } 

    # item cov 
    item_cov_file_beta <- 0 ; item_cov_file_alpha <- 0 
    if(item_cov_on==1 && is.null(item_cov$item_matrix_beta) && is.null(item_cov$item_matrix_alpha)) { 
        stop("item_matrix should be provided either for beta or alpha. \n")
     
    }
        
     if(!is.null(item_cov$item_matrix_beta)) {
                it_cov <- data.frame(item_cov$item_matrix_beta) 
                if (max(S_item) ==1) { 
                    if (within==0) {
                        if(nrow(it_cov) !=I) stop("number of rows in item design matrix should be same as number of items")
                    }
                } else { # polytomous items 
                    
                    if (within==0) {
                        #n_it_cov <- sum(S_item)
                        if(nrow(it_cov) != sum(S_item)) stop("number of rows in item design matrix should match the data  ")            
                    }
                }
                item_cov_file_beta <- "item_cov_beta.txt"
                name_it_cov_beta <- colnames(it_cov)
                write.table(it_cov, item_cov_file_beta , row.names=FALSE, col.names =FALSE, na="-1")     
     } 
     
     if (!is.null(item_cov$item_matrix_alpha)){
                # alpha 
                it_cov2 <- data.frame(item_cov$item_matrix_alpha) 
                if (max(S_item) ==1) { 
                    if (within==0) {
                        if(nrow(it_cov2) !=I) stop("number of rows in item design matrix should be same as number of items")
                    }
                } else { # polytomous items 
                    
                    if (within==0) {
                        #n_it_cov <- sum(S_item)
                        if(nrow(it_cov2) != I) stop("number of rows in item design matrix should match the data ")            
                    }
                }
                item_cov_file_alpha <- "item_cov_alpha.txt"
                name_it_cov_alpha <- colnames(it_cov2)
                write.table(it_cov2, item_cov_file_alpha , row.names=FALSE, col.names =FALSE, na="-1")               
            
        }
            
            
    # cov info: covariance information (which covariates are used for each dimension 
    # for multidimensional models 
#    if (mul_on ==1 && person_cov_on==1) {
#        #within <- 0 
#        if (is.null(mul$cov_info)) {
#            stop("covariates information should be provided in each dimension. \n")
#        }    
#    }    
    #cov_info = list(dim1=c(1,2), dim2=1, dim2=2) # columns of the person matrix 
    
    
    #cov_info_file <- 0
    if (mul_on ==1 && person_cov_on==1) {
    
        if (is.null(mul_cov_info))
        stop("covariates information should be provided in all dimensions.")

        if(is.list(mul_cov_info) ) {
            if (is.null(person_cov$person_matrix)) {
                stop("person_matrix should be provided. \n")
            } else   { 
                #ndim <- length(mul$dim_info)
                tdim <- length(mul_cov_info) # should be the same as ndim
                if (tdim != ndim)
                stop("covariates information should be provided in all dimensions.")
                
                # number of covariates in each dimension
#                aa <-unlist(mul_cov_info)
#                aa <- length(mul_cov_info[[1]])
#                for (d in 2:tdim) {
#                    aa <- c(aa, length(mul_cov_info[[d]]))
#                }
#                each <- aa # number of covariates in each dimension 
                aa <- NA
                for (d in 1:tdim) {
                    if (mul_cov_info[[d]][1]!=0) {
                        aa <- c(aa, length(mul_cov_info[[d]]))
                    } else {
                        aa <- c(aa, 0)
                    }
                }
                each <- aa[-1] # number of covariates in each dimension 
                n_cov_info <- sum(each)
                cov_info_file <- each    
                
                # latent regression 
                if (on_theta ==1 ) {                             
                    # person matrix (reconstruct)
                    bb <- pp_cov[, mul_cov_info[[1]]]
                    for (d in 2:tdim) {
                        bb <- cbind(bb, pp_cov[, mul_cov_info[[d]]])
                    }         
                    pp_cov <- bb 
                    person_cov_file <- "person_cov.txt"
                    name_pp_cov <- colnames(pp_cov)
                    write.table(pp_cov, person_cov_file, row.names=FALSE, col.names =FALSE, na="-1")     
                } else {
                    # main effects 
                    if (ncol(pp_cov) < n_cov_info) 
                    stop("person covariates are entered as main effects")     # ??           
                }
            }
        }
    }
    
    # for bifactor models 

    #bifac_cov_info = list(dim0=0, dim1=c(1,3), dim2=2) # dim1 is the general dimension
    #cov_info_file <- 0
    if (bifac_on ==1 && person_cov_on==1) {

        if (is.null(bifac_cov_info))
        stop("covariates information should be provided in general and specific dimension.")

        if(is.list(bifac_cov_info) ) {
            if (is.null(person_cov$person_matrix)) {
                stop("person_matrix should be provided. \n")
            } else   { 
    
                tdim <- length(bifac_cov_info) # should be the same as ndim
                if (tdim != (ndim+1))
                stop("covariates information should be provided in general and specific dimension.")
               

                # number of covariates in each dimension
                #aa <-unlist(bifac_cov_info)
                #aa <- length(bifac_cov_info[[1]])
                aa <- NA
                for (d in 1:tdim) {
                    if (bifac_cov_info[[d]][1]!=0) {
                        aa <- c(aa, length(bifac_cov_info[[d]]))
                    } else {
                        aa <- c(aa, 0)
                    }
                }
                each <- aa[-1] # number of covariates in each dimension
                n_cov_info <- sum(each)
                #cov_list = 1:n_cov_info
                cov_info_file <- each 
                # latent regression 
                if (person_main ==0 ) {
                    
                    # person matrix (reconstruct)
                    bb <- pp_cov[, bifac_cov_info[[1]]]
                    for (d in 2:tdim) {
                        bb <- cbind(bb, pp_cov[, bifac_cov_info[[d]]])
                    }         
                    pp_cov <- bb 
                    person_cov_file <- "person_cov.txt"
                    name_pp_cov <- colnames(pp_cov)
                    write.table(pp_cov, person_cov_file, row.names=FALSE, col.names =FALSE, na="-1")     
                } else {
                    #if (ncol(pp_cov) < n_cov_info) 
                    #stop("person covariates are entered as main effects")
                
                }
            }
        }
    }
    
    if (second_on ==1 && person_cov_on==1) {

        if (is.null(second_cov_info))
        stop("covariates information should be provided in general and specific dimension.")

        if(is.list(second_cov_info) ) {
            if (is.null(person_cov$person_matrix)) {
                stop("person_matrix should be provided. \n")
            } else   { 
    
                tdim <- length(second_cov_info) # should be the same as ndim
                if (tdim != (ndim+1))
                stop("covariates information should be provided in general and specific dimension.")
               

                # number of covariates in each dimension
                #aa <-unlist(bifac_cov_info)
                #aa <- length(bifac_cov_info[[1]])
                aa <- NA
                for (d in 1:tdim) {
                    if (second_cov_info[[d]][1]!=0) {
                        aa <- c(aa, length(second_cov_info[[d]]))
                    } else {
                        aa <- c(aa, 0)
                    }
                }
                each <- aa[-1] # number of covariates in each dimension
                n_cov_info <- sum(each)
                #cov_list = 1:n_cov_info
                cov_info_file <- each 
                # latent regression 
                if (person_main ==0) {
                    
                    # person matrix (reconstruct)
                    bb <- pp_cov[, second_cov_info[[1]]]
                    for (d in 2:tdim) {
                        bb <- cbind(bb, pp_cov[, second_cov_info[[d]]])
                    }         
                    pp_cov <- bb 
                    person_cov_file <- "person_cov.txt"
                    name_pp_cov <- colnames(pp_cov)
                    write.table(pp_cov, person_cov_file, row.names=FALSE, col.names =FALSE, na="-1")     
                } else {
                    #main effects 
                    #if (ncol(pp_cov) < n_cov_info) 
                    #stop("person covariates are entered as main effects")
                
                }
            }
        }
    }    
    
    
    # dif (item column or variable name)
    dif_beta_col <- 0
    dif_alpha_col <- 0 
    if (dif_on ==1) {
        if (model%in%c(1,3)) { # rasch models 
            if (is.null(dif$dif_beta) ) {
                stop("dif items should be provided. \n")
            } else {       
            dif_beta_col <- if (is.numeric(dif$dif_beta)==FALSE) which(item_name%in%dif$dif_beta) else dif$dif_beta
            #dif_alpha_col <- if (is.numeric(dif$dif_alpha)==FALSE) which(item_name%in%dif$dif_alpha) else dif$dif_alpha
            }
        } 
        if (model%in%c(2,4,5,6)) { # 2pl models + bifactor models + secondorder models 
            if (is.null(dif$dif_alpha) && is.null(dif$dif_beta)  ) {
                stop("dif items should be provided. \n")
            } 
            
            if (!is.null(dif$dif_beta)) {      
                dif_beta_col <- if (is.numeric(dif$dif_beta)==FALSE) which(item_name%in%dif$dif_beta) else dif$dif_beta
            } else {
                dif_beta_col <- 0 
            }
            
            if (!is.null(dif$dif_alpha)) {      
                dif_alpha_col <- if (is.numeric(dif$dif_alpha)==FALSE) which(item_name%in%dif$dif_alpha) else dif$dif_alpha
            } else {
                dif_alpha_col <- 0 
            }

        }
    } else {
        dif_beta_col <- 0
        dif_alpha_col <- 0
    }

    #weight 
    if(weight_on==0) { weight_matrix_file  <-  0 
    } else {
        if (is.null(weight$weight_matrix)) {
            stop("weight_matrix should be provided. \n")
        } else { 
            pp_weight <- if(is.vector(weight$weight_matrix)) as.matrix(data[,weight$weight_matrix]) else data.frame(weight$weight_matrix) 
            if(nrow(pp_weight) !=N) stop("number of cases in weight_matrix should be same as number of cases in data")
            weight_matrix_file <- "weight.matrix.txt"
            write.table(pp_weight, weight_matrix_file , row.names=FALSE, col.names =FALSE, na="-1")     
        }
    }

    
    # save response data 
    resp_file <- "rundata.txt"
    write.table(resp, resp_file , row.names=FALSE, col.names =FALSE, na="-1")
            
    # nq (for a multidimensinal case)
    nqr <- if (mul_on==0) control$nq
    if(mul_on==1 ) { # multidimensional
        if (length(control$nq)==1)  {
            nqr <- rep(control$nq, ndim )
        }    
        else if (length(control$nq) != ndim ) {
                stop("quadrature points should be provided for each dimension")
            }        
        else  {nqr <- control$nq
        }
    }
    
    if (bifac_on==1 | second_on==1) {  # bifactor + second order 
        if (length(control$nq)==1) {
            nqr <- rep(control$nq, (ndim+1))
        }    
        else if (length(control$nq) != (ndim+1) ) {
                stop("quadrature points should be provided for each dimension")
            }        
        else  {nqr <- control$nq
        }
    }            
 
   
    # starting values 
    if(start$on==1 ) {
        if (is.null(start$start_info)) {
            stop("parameter for starting values should be provided. \n")
        } else {
            if(length(start$start_info)!= length(start$start_value)) {
                stop("all specified parameters need starting values. \n")
            }         
            
            rest <- rep(0, start$npar)
            parm <- rep(0, start$npar)
            rest[start$start_info] <- 1  # restricted parameters 
            parm[start$start_info] <- start$start_value
            
            # constraint matrix 
            start <- cbind(rest, parm)
            start_file <- "start.txt"
            write.table(start, start_file , row.names=FALSE, col.names =FALSE, na="-1")
                    
            #start_alp_g <- "start1.txt"
            #write.table(start$start, start_alp_g, row.names=FALSE, col.names =FALSE )        
            #start_alp_s <- 0
            #start_beta <- 0  
        }
    } else {
            start_file  <- 0
   
    }

    # constraints     
    if (constraint$on==1 ) {
        if (is.null(constraint$cons_info)) {
            stop("constraint information should be provided. \n")
        } else {     
            
            if(length(constraint$cons_info)!= length(constraint$cons_value)) {
                stop("all specified parameters need values to be fixed. \n")
            } 
            
            nnpar <- constraint$npar
            nninfo <- constraint$cons_info
            
            rest <- rep(0,nnpar )
            parm <- rep(0,nnpar )
            rest[nninfo] <- 1  # restricted parameters 
            parm[nninfo] <- constraint$cons_value
            
            # constraint matrix 
            const <- cbind(rest, parm)
            constraint_file <- "constraint.txt"
            write.table(const, constraint_file, row.names=FALSE, col.names =FALSE, na="-1")
            
            # SE should be F 
            if (nnpar == length(nninfo) ) {
                control$se_num <- 0
                control$se_emp <- 0                
            }    
        }
   } else {
            constraint_file <- 0 
   
   }                  


    # other default options     
    minpercent <- control$minpercent
    max_it <- control$max_it
    conv <- control$conv
    link <- control$link
    verbose <- control$verbose
    show <- control$show
    alp_bounds_up <- control$alp_bounds_up 
    alp_bounds_low <- control$alp_bounds_low 
    se_num <- control$se_num
    se_emp <- control$se_emp
    adapt <- control$adapt 
    bsize <- 100

    if (embretson_T==1 ) {
        # for the embretson model, no standard errors are estimated 
        se_num=0 ; se_emp=0 
    } 
#
#    if (second_on==1 ) {
#        # for secon-order models, no standard errors are estimated 
#        se_num=0 ; se_emp=0
#        
#        # linear constraints on alpha  
#        embretson_T==1 ; 
#    } 

    # 
    data_new <- if (minpercent ==0) 0 else 1  

    # evalaute log-likelihood 
    if (evaluate$on==1 ) {
        if (is.null(evaluate$eval_value)) {
            stop("parameter values should be provided. \n")
        } else {     
            se_num <- 0 ; se_emp <- 0 ; post <- 0
            parm <- evaluate$eval_value

            evaluate_file <- "evaluate.txt"
            write.table(parm, evaluate_file , row.names=FALSE, col.names =FALSE, na="-1")
            
        }
   } else {
            evaluate_file <- 0 
   
   } 



    # output options 
    if (model%in%c(1:5,7)) {
        if (se_num==0 & se_emp==0 & post==0 & data_new==0 & embretson_T==0) output <- "output parms rest loglikelihood final_it"
        if (se_num==1 & se_emp==0 & post==0 & data_new==0 & embretson_T==0) output <- "output parms rest loglikelihood se_num final_it infonum"
        if (se_num==0 & se_emp==1 & post==0 & data_new==0 & embretson_T==0) output <- "output parms rest loglikelihood se_emp final_it infoemp"   
        if (se_num==1 & se_emp==1 & post==0 & data_new==0 & embretson_T==0) output <- "output parms rest loglikelihood se_num se_emp final_it infonum infoemp"       
        if (se_num==1 & se_emp==0 & post==1 & data_new==0 & embretson_T==0) output <- "output parms rest loglikelihood se_num eap var cov expected rel_e final_it infonum"   
        if (se_num==0 & se_emp==1 & post==1 & data_new==0 & embretson_T==0) output <- "output parms rest loglikelihood se_emp eap var cov expected rel_e final_it infoemp"    
        if (se_num==1 & se_emp==1 & post==1 & data_new==0 & embretson_T==0) output <- "output parms rest loglikelihood se_num se_emp eap var cov expected rel_e final_it infonum infoemp"   
        if (se_num==0 & se_emp==0 & post==1 & data_new==0 & embretson_T==0) output <- "output parms rest loglikelihood eap var cov expected rel_e final_it" 
    
        if (se_num==0 & se_emp==0 & post==0 & data_new==1 & embretson_T==0) output <- "output parms rest loglikelihood data_new final_it"
        if (se_num==1 & se_emp==0 & post==0 & data_new==1 & embretson_T==0) output <- "output parms rest loglikelihood se_num data_new final_it infonum"
        if (se_num==0 & se_emp==1 & post==0 & data_new==1 & embretson_T==0) output <- "output parms rest loglikelihood se_emp data_new final_it infoemp"   
        if (se_num==1 & se_emp==1 & post==0 & data_new==1 & embretson_T==0) output <- "output parms rest loglikelihood se_num se_emp data_new final_it infonum infoemp"       
        if (se_num==1 & se_emp==0 & post==1 & data_new==1 & embretson_T==0) output <- "output parms rest loglikelihood se_num eap var cov expected rel_e data_new final_it infonum"   
        if (se_num==0 & se_emp==1 & post==1 & data_new==1 & embretson_T==0) output <- "output parms rest loglikelihood se_emp eap var cov expected rel_e data_new final_it infoemp"    
        if (se_num==1 & se_emp==1 & post==1 & data_new==1 & embretson_T==0) output <- "output parms rest loglikelihood se_num se_emp eap var cov expected rel_e data_new final_it infonum infoemp"   
        if (se_num==0 & se_emp==0 & post==1 & data_new==1 & embretson_T==0) output <- "output parms rest loglikelihood eap var cov expected rel_e data_new final_it" 
    
        if (se_num==0 & se_emp==0 & post==0 & data_new==0 & embretson_T==1) output <- "output parms rest loglikelihood final_it alp_embretson"
        if (se_num==0 & se_emp==0 & post==1 & data_new==0 & embretson_T==1) output <- "output parms rest loglikelihood eap var cov expected rel_e final_it alp_embretson" 
    
        if (se_num==0 & se_emp==0 & post==0 & data_new==1 & embretson_T==1) output <- "output parms rest loglikelihood data_new final_it alp_embretson"
        if (se_num==0 & se_emp==0 & post==1 & data_new==1 & embretson_T==1) output <- "output parms rest loglikelihood eap var cov expected rel_e data_new final_it alp_embretson" 
        
    } else {  # second-order 
        if (se_num==0 & se_emp==0 & post==0 & data_new==0 & embretson_T==0) output <- "output parms rest loglikelihood final_it first_order second_order sd_square_first stand_fo cor_first_second"
        if (se_num==1 & se_emp==0 & post==0 & data_new==0 & embretson_T==0) output <- "output parms rest loglikelihood se_num final_it infonum first_order second_order sd_square_first stand_fo cor_first_second"
        if (se_num==0 & se_emp==1 & post==0 & data_new==0 & embretson_T==0) output <- "output parms rest loglikelihood se_emp final_it infoemp first_order second_order sd_square_first stand_fo cor_first_second"   
        if (se_num==1 & se_emp==1 & post==0 & data_new==0 & embretson_T==0) output <- "output parms rest loglikelihood se_num se_emp final_it infonum infoemp first_order second_order sd_square_first stand_fo cor_first_second"       
        if (se_num==1 & se_emp==0 & post==1 & data_new==0 & embretson_T==0) output <- "output parms rest loglikelihood se_num eap var cov expected rel_e final_it infonum first_order second_order sd_square_first stand_fo cor_first_second"   
        if (se_num==0 & se_emp==1 & post==1 & data_new==0 & embretson_T==0) output <- "output parms rest loglikelihood se_emp eap var cov expected rel_e final_it infoemp first_order second_order sd_square_first stand_fo cor_first_second"    
        if (se_num==1 & se_emp==1 & post==1 & data_new==0 & embretson_T==0) output <- "output parms rest loglikelihood se_num se_emp eap var cov expected rel_e final_it infonum infoemp first_order second_order sd_square_first stand_fo cor_first_second"   
        if (se_num==0 & se_emp==0 & post==1 & data_new==0 & embretson_T==0) output <- "output parms rest loglikelihood eap var cov expected rel_e final_it first_order second_order sd_square_first stand_fo cor_first_second" 
    
        if (se_num==0 & se_emp==0 & post==0 & data_new==1 & embretson_T==0) output <- "output parms rest loglikelihood data_new final_it first_order second_order sd_square_first stand_fo cor_first_second"
        if (se_num==1 & se_emp==0 & post==0 & data_new==1 & embretson_T==0) output <- "output parms rest loglikelihood se_num data_new final_it infonum first_order second_order sd_square_first stand_fo cor_first_second"
        if (se_num==0 & se_emp==1 & post==0 & data_new==1 & embretson_T==0) output <- "output parms rest loglikelihood se_emp data_new final_it infoemp first_order second_order sd_square_first stand_fo cor_first_second"   
        if (se_num==1 & se_emp==1 & post==0 & data_new==1 & embretson_T==0) output <- "output parms rest loglikelihood se_num se_emp data_new final_it infonum infoemp first_order second_order sd_square_first stand_fo cor_first_second"       
        if (se_num==1 & se_emp==0 & post==1 & data_new==1 & embretson_T==0) output <- "output parms rest loglikelihood se_num eap var cov expected rel_e data_new final_it infonum first_order second_order sd_square_first stand_fo cor_first_second"   
        if (se_num==0 & se_emp==1 & post==1 & data_new==1 & embretson_T==0) output <- "output parms rest loglikelihood se_emp eap var cov expected rel_e data_new final_it infoemp first_order second_order sd_square_first stand_fo cor_first_second"    
        if (se_num==1 & se_emp==1 & post==1 & data_new==1 & embretson_T==0) output <- "output parms rest loglikelihood se_num se_emp eap var cov expected rel_e data_new final_it infonum infoemp first_order second_order sd_square_first stand_fo cor_first_second"   
        if (se_num==0 & se_emp==0 & post==1 & data_new==1 & embretson_T==0) output <- "output parms rest loglikelihood eap var cov expected rel_e data_new final_it first_order second_order sd_square_first stand_fo cor_first_second" 
    
        if (se_num==0 & se_emp==0 & post==0 & data_new==0 & embretson_T==1) output <- "output parms rest loglikelihood final_it alp_embretson first_order second_order sd_square_first stand_fo cor_first_second"
        if (se_num==0 & se_emp==0 & post==1 & data_new==0 & embretson_T==1) output <- "output parms rest loglikelihood eap var cov expected rel_e final_it alp_embretson first_order second_order sd_square_first stand_fo cor_first_second" 
    
        if (se_num==0 & se_emp==0 & post==0 & data_new==1 & embretson_T==1) output <- "output parms rest loglikelihood data_new final_it alp_embretson first_order second_order sd_square_first stand_fo cor_first_second"
        if (se_num==0 & se_emp==0 & post==1 & data_new==1 & embretson_T==1) output <- "output parms rest loglikelihood eap var cov expected rel_e data_new final_it alp_embretson first_order second_order sd_square_first stand_fo cor_first_second" 
        
    
    }
            
    ## list of entries in the input file 
        
    if (model==1) sink("input_uni.txt") 
    if (model==2) sink("input_2pl.txt")
    if (model==3) sink("input_mul.txt")
    if (model==4) sink("input_2pl_mul.txt")
    if (model==5) sink("input_bifactor.txt")
    if (model==6) sink("input_secondorder.txt")
    if (model==7) sink("input_3pl.txt")
              
    cat("datafile",resp_file, "\n") 
    cat("person_cov",  person_cov_file,  "\n") 
    cat("latreg_on_theta",  on_theta,  "\n")  
    cat("item_cov",  item_cov_file_beta,  "\n")  
    cat("lltm_on_beta", on_beta,   "\n") 
    cat("group", person_group_file,  "\n") 
    cat("difitems",  dif_beta_col,  "\n")  
    cat("non_uniform_dif", dif_alpha_col,   "\n")  
    cat("mixture", mixture_num,   "\n")  
    cat("within",  within,  "\n")  
    cat("n_it_dim", n_it_dim,   "\n") 
    cat("dim_info", dim_info_file,   "\n")  
    cat("cov_info", cov_info_file,   "\n") 
    cat("weights", weight_matrix_file,   "\n")  
    cat("adapt", adapt,   "\n")  
    cat("bsize",  bsize,  "\n")  
    cat("nrquadpoints", nqr,   "\n")  
    cat("convergence_diff", conv,   "\n")  
    cat("link_function", link,   "\n")  
    cat("start",  start_file ,  "\n")  
    cat("minpercent",  minpercent,   "\n") 
    cat("constraint", constraint_file ,  "\n")  
    cat("verbose",  verbose,  "\n")  
    cat("se_num",  se_num,  "\n")  
    cat("se_emp",  se_emp,  "\n")  
    cat("evaluate", evaluate_file,  "\n")  
    cat("eap",  post,  "\n")       
    cat("data_new",  data_new,  "\n")  
    cat("max_it",  max_it,  "\n")  
    cat("embretson",  item_cov_file_alpha,  "\n")  
    cat("alp_bounds",  alp_bounds_up,  "\n")  
    cat("alp_low",  alp_bounds_low,  "\n")  
    cat(output, "\n")     
    
    sink()



}
