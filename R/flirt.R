

# flirt main function 
flirt <- function(data, select=NULL, subset=NULL, loading=list(on=FALSE, inside=NULL), mul=list(on=FALSE, dim_info=NULL, cov_info=NULL), 
                    bifac=list(on=FALSE, dim_info=NULL, cov_info=NULL), guess=list(on=FALSE), 
                    second=list(on=FALSE, dim_info=NULL, cov_info=NULL), 
                    person_cov=list(on=FALSE, person_matrix=NULL, main=NULL), 
                    item_cov=list(on=FALSE, item_matrix_beta=NULL,item_matrix_alpha=NULL ), 
                    dif=list(on=FALSE, dif_beta=NULL, dif_alpha=NULL), 
                    mg=list(on=FALSE, group_matrix=NULL), mixture =list(on=FALSE, num=NULL), 
                    weight=list(on=FALSE, weight_matrix=NULL), post=FALSE, 
                    start = list(on=FALSE, npar=NULL, start_info=NULL, start_value=NULL), 
                    constraint = list(on=FALSE, npar=NULL, cons_info=NULL, cons_value=NULL), 
                    evaluate = list(on=FALSE, eval_value=NULL), 
                    control=list(minpercent= NULL, max_it=NULL, nq=NULL, conv=NULL, link=NULL, adapt=NULL, 
                    se_num=NULL, se_emp=NULL, alp_bounds_up=NULL, alp_bounds_low=NULL,  verbose=NULL, show=NULL) ,
                    delete_output=TRUE )  {
  
    # data: data frame or matrix. wide form (person x item)
    # select: vector. columns or names of the items (if the data include other than responses or want to analyze only subset of items)
    # subset: vector. rows or names of the cases 
    # loading: list. if on is TRUE, 2pl model. if False, 1pl model (dafault is 2pl); for the 2pl model, need to choose the model formulation
    #           if inside is TRUE, alpha(theta +beta), or alpha*theta + beta
    # mul: list. if on is TRUE, multidimensional model, and needs dim_info. dim_info is a list of item numbers or names for each dimension 
    #       if there are person covariates, cov_info should be provided, which tells which covariates are used in each dimension.
    # bifac: list, if on is TRUE, bifactor model, and needs dim_info. dim_info is a list of item numbers or columns for each dimension 
    #       if there are covariates, cov_info should be provided, which tells which covariates are used in each dimension.       
    # second: list, if on is TRUE, second model, and needs dim_info. dim_info is a list of item numbers or columns for each dimension 
    #       if there are covariates, cov_info should be provided, which tells which covariates are used in each dimension.       
    # guess: list, if on is TRUE, 3PL model. 
    # person_cov: list, if on is TRUE, need to indicate person covariates colums (or name) or input the data matrix. 
    #             if main is TRUE, person covariates are entered as main effects (default is main==FALSE) .   
    # item_cov: list, if one is TRUE, need to provide item design matrix for beta or/and alpha 
    #          (items by covariates; the items should be in the same order as items in the data (after selection)
    # dif: list, if on is TRUE, dif analysis. need to provide for uniform dif, provide item colums or names for beta. 
    #      for non-uniform dif, provide item colums or names for alpha. 
    # mg: list, if on is TRUE, multiple group analysis. need to indicate a group column or inpute data matrix in group_matrix  
    # mixture: list, if on is TRUE, mixture analysis. input the number of latent groups in num (For unidimensional Rasch models only)
    # weight: list, if on is TRUE, provide data matrix for sampling/frequency weights 
    # post: logical. if TRUE, EAP, expected scores, IRT reliability are provided 
     
    # start: list. if on is FALSE, random starting values (default). If on is true, provide total number of parameters (npar) 
    #       and a vector of parameters for providing starting values (start_info) 
    #       and starting values for the specified parameters (start_value). 
    # constraint: list. if on is true,  provide total number of parameters (npar) 
    #       and a vector of parameters to be constrained (cons_info) 
    #       and fixed values for the specified parameters (cons_value). 
    # evaluate: list. if on is true, provide a vector of parameters values for the log-likelihood to be evaluated at. 
    # control: estimation options as the following: 
    # - minpercent: numeric. for polytomous items, mininum percentage for each category (default is 0%). 
    #           if lower than minpercent, the category is collapsed with the lower adjacent category.
    # - max_it: integer. number of maximum iterations (default is 10000) 
    # - nq: integer. number of quadrature points (default is 20. For multidimensional models, a vector length of ndim; for bifactor models, 1+dim)
    # - conv: numeric. convergence criterion (default is 0.0001)
    # - link: 1,2,3 or multinomial, cumulative, and adjacent link functions (default is multinomial (1) for binary responses and adjacent (3) for polytomous responses) 
    # - adapt: logical. if TRUE, adaptive quadrature, otherwise Gaussian quadrature (default)
    # - se_num: logical. if TRUE, SE computed numerically
    # - se_emp: logical, if TRUE, SE computed empirically (default)
    # - alp_bounds_up: integer. upper boundary value for alpha. 
    # - alp_bounds_low: integer. lower boundary value for alpha. 
    # - verbose: logical. if TRUE, print estimation process (number of iterations, difference in the parameter estiamtes between adjacent iterations, etc.)  
    # - show: logical. if TRUE, print matlab output (error messages) on console 

    # (hidden) delete_output: if TRUE, all outputs are deleted in the folder. (TRUE is default)
    
    
    # get current directory    
    current_direct <- getwd()

    ## change directory
    direct <- searchpaths()
    direct <- direct[grep("flirt",direct)]
    dirdta <- paste(direct, "/doc", sep="")
    setwd(dirdta) 
    
    # default options    
     
    select <- if(is.vector(select)) select else NULL 
    if (is.matrix(select)) stop("select should be a vector") 
    #if (is.numeric(select)==FALSE && is.integer(select)==FALSE && is.null(select)==FALSE) stop("select should be an integer") 
    subset <- if(is.vector(subset)) subset else NULL 
    if (is.matrix(subset)) stop("subset should be a vector") 
    #if (is.numeric(subset)==FALSE && is.integer(subset)==FALSE && is.null(subset)==FALSE) stop("subset should be an integer") 

    resp <- if (is.vector(select)) data[,select] else data
    if(length(select) > ncol(resp)) stop("selected items cannot be more than total number of items") 
    resp <- if (is.vector(subset)) resp[subset,] else resp
    if(length(subset) > nrow(resp)) stop("selected cases cannot be more than total number of cases") 

    max_resp <- max(resp, na.rm=T)

    loading_on <- if (loading$on==TRUE) 1 else 0 
    loading_inside <- if (is.null(loading$inside)) 0 else if (loading$inside==FALSE) 0 else 1 
    mul_on   <- if(mul$on==TRUE) 1 else 0 
    mul_dim_info <- if(is.null(mul$dim_info)) NULL else mul$dim_info
    mul_cov_info <- if(is.null(mul$cov_info)) NULL else mul$cov_info
    bifac_on <- if(bifac$on==FALSE) 0 else 1         
    bifac_dim_info <- if(is.null(bifac$dim_info)) NULL else bifac$dim_info
    bifac_cov_info <- if(is.null(bifac$cov_info)) NULL else bifac$cov_info
    second_on <- if(second$on==FALSE) 0 else 1         
    second_dim_info <- if(is.null(second$dim_info)) NULL else second$dim_info
    second_cov_info <- if(is.null(second$cov_info)) NULL else second$cov_info
    guess_on <- if(guess$on==FALSE) 0 else 1      
        
    person_cov_on <- if(person_cov$on==FALSE) 0 else 1
    person_cov_matrix <- if(is.null(person_cov$person_matrix)) NULL 
                else if (is.vector(person_cov$person_matrix) & length(person_cov$person_matrix) <  nrow(resp) ) as.matrix(data[,person_cov$person_matrix])
                else if (is.vector(person_cov$person_matrix) & length(person_cov$person_matrix) == nrow(resp)) matrix(person_cov$person_matrix, ncol= 1)  
                else person_cov$person_matrix
                
    #if (as.matrix(person_cov_matrix)!=            
    name_pp_cov <- colnames(person_cov_matrix) #else names(person_cov_matrix)
    person_main <- if(is.null(person_cov$main)) 0 else if (person_cov$main ==TRUE) 1 else 0
        
    item_cov_on <- if(item_cov$on==FALSE) 0 else 1
    item_cov_beta <- if(is.null(item_cov$item_matrix_beta)) NULL else item_cov$item_matrix_beta 
    name_it_cov_beta <- colnames(item_cov_beta)
    item_cov_alpha <- if(is.null(item_cov$item_matrix_alpha)) NULL else item_cov$item_matrix_alpha
    name_it_cov_alpha <- colnames(item_cov_alpha)
    
    dif_on <- if(dif$on==FALSE) 0 else 1
    dif_on_beta <- if (is.null(dif$dif_beta)) NULL else dif$dif_beta 
    dif_on_alpha <- if (is.null(dif$dif_alpha)) NULL else dif$dif_alpha 
    mg_on <- if(mg$on==FALSE) 0 else 1 
    mg_group_matrix <- if(is.null(mg$group_matrix)) NULL 
            else if (is.vector(mg$group_matrix) & length(mg$group_matrix) <  nrow(resp) ) matrix(data[,mg$group_matrix], ncol=length(mg$group_matrix)) 
            else if(is.vector(mg$group_matrix)& length(mg$group_matrix) ==  nrow(resp) ) matrix(mg$group_matrix, ncol= 1)   else mg$group_matrix
    name_pp_mg <-  colnames(mg_group_matrix) #else names(mg_group_matrix)
    mixture_on <- if(mixture$on==FALSE) 0 else 1 
    if(mixture_on==1) stop("mixture analysis is not available in current version of flirt")
    mixture_num <- if(is.null(mixture$num)) 0 else mixture$num   
    weight_on <- if(weight$on==FALSE) 0 else 1
    weight_on_matrix <- if(is.null(weight$weight_matrix)) NULL 
            else if (is.vector(weight$weight_matrix) & length(weight$weight_matrix) <  nrow(resp) ) matrix(data[,weight$weight_matrix], ncol=length(weight$weight_matrix))
            else if (is.vector(weight$weight_matrix)& length(weight$weight_matrix) ==  nrow(resp)) matrix(weight$weight_matrix, ncol= 1)  else weight$weight_matrix
    post <- if(post==FALSE) 0 else 1 
    
    # starting values 
    start_on <- if(start$on==TRUE) 1 else 0 
    st_npar   <- if(is.null(start$npar)) NULL else start$npar 
    st_info <- if(is.null(start$start_info)) NULL else start$start_info
    st_value <- if(is.null(start$start_value)) NULL else start$start_value

    # constraints 
    const_on   <- if(constraint$on==TRUE) 1 else 0 
    const_npar   <- if(is.null(constraint$npar)) NULL else constraint$npar 
    const_info <- if(is.null(constraint$cons_info)) NULL else constraint$cons_info
    const_value <- if(is.null(constraint$cons_value)) NULL else constraint$cons_value

    # evaluate log likelihood 
    evalt_on   <- if(evaluate$on==TRUE) 1 else 0 
    evalt_value <- if(is.null(evaluate$eval_value)) NULL else evaluate$eval_value

    
    # control options 
    control_minpercent <- if (is.null(control$minpercent)) 0 else control$minpercent
    control_max_it <- if (is.null(control$max_it)) 10000 else control$max_it
    control_nq <- if (is.null(control$nq)) 20 else control$nq
    control_conv <- if (is.null(control$conv)) 0.0001 else control$conv
    
    # link names 
    linknames <- c("multinomial","cumulative","adjacent")    
    link_c <- if (is.null(control$link) & max_resp==1 ) 1 else if (is.null(control$link) & max_resp >1 ) 2 else control$link
    if (link_c %in% linknames ==FALSE && link_c %in% c(1,2,3) ==FALSE) stop("link function should be one of 1,2,3 or multinomial,cumulative,adjacent")
    control_link <- if (link_c==1 |link_c=="multinomial" ) 1 
    else if (link_c==2 |link_c=="cumulative") 2 
    else if (link_c==3 |link_c=="adjacent" ) 3
       
    control_adapt <- if (is.null(control$adapt)) 0 else if (control$adapt==FALSE) 0  else if (control$adapt==TRUE) 1     
    control_se_num <- if (is.null(control$se_num)) 1 else if (control$se_num==FALSE) 0 else if (control$se_num==TRUE) 1
    control_se_emp <- if (is.null(control$se_emp)) 0 else if (control$se_emp==TRUE) 1 else if (control$se_emp==FALSE) 0 
    control_alp_bounds_up <- if (is.null(control$alp_bounds_up)) 0 else control$alp_bounds_up
    control_alp_bounds_low <- if (is.null(control$alp_bounds_low)) 0 else control$alp_bounds_low
    control_verbose <- if (is.null(control$verbose)) 0 else if (control$verbose==FALSE) 0 else if (control$verbose==TRUE) 1 
    control_show <- if (is.null(control$show)) 0 else if (control$show==FALSE) 0  else if (control$show==TRUE) 1 
    if (control_verbose==1) control_show <- 1
    
    # Determine model/file type 
    
    # model 1: unidimensional Rasch 
    # model 2: unidimensional 2pl 
    # model 3: multidimensional Rasch
    # model 4: multidimensional 2pl
    # model 5: bifactor  
    # model 6: second order   
    # model 7: unidimensional 3pl    
            
    model <- if (loading_on==0 && mul_on==0 && bifac_on==0 && second_on==0 && guess_on==0) 1    
    else if (loading_on==1 && mul_on==0 && bifac_on==0 && second_on==0 && guess_on==0) 2
    else if (loading_on==0 && mul_on==1 && bifac_on==0 && second_on==0 && guess_on==0) 3    
    else if (loading_on==1 && mul_on==1 && bifac_on==0 && second_on==0 && guess_on==0)  4
    else if (bifac_on==1 && second_on==0 && guess_on==0) 5
    else if (second_on==1  && guess_on==0) 6
    else if (guess_on==1) 7
        
    if (mul_on==1 && bifac_on==1) 
    stop("specify either a multidimensional or bifactor model")

    if (mul_on==1 && second_on==1) 
    stop("specify either a multidimensional or second-order model")

    if (bifac_on==1 && second_on==1) 
    stop("specify either a bifactor model or second-order model")

    if (mul_on==1 && loading_inside==1)
    stop("for multidimensional 2PL models, inside option is not possible")    
 
    # minimum response category should be 0
    
    if(min(resp, na.rm=T) != 0) {
       stop("minimum response category should be 0")
        #min_resp <- min(data, na.rm=T)
        #data <- data - min_resp
    }
    
    
    # minimuum person group should be 0  
 
    if (!is.null(mg_group_matrix) ) {
        if (min(mg_group_matrix, na.rm=T) != 0) {
            stop("minimum group membership should be 0")
            
        #    min_group <- min(mg_group_matrix, na.rm=T)
        #    mg_group_matrix <- mg_group_matrix - min_group
        
        }
    
    }
 

    # if there is missing in person covariates, person 

    if (!is.null(person_cov_matrix)   & is.null(mg_group_matrix) & is.null(weight_on_matrix)  ) {
     if( any(is.na(person_cov_matrix)) ) {    
        # list wise deletion 
        index <- which(is.na(person_cov_matrix))    
        resp <- resp[-index,]
        person_cov_matrix <- matrix(person_cov_matrix[-index,] , ncol=1)         
     }
    } 
 
    if (!is.null(mg_group_matrix)  & is.null(person_cov_matrix) & is.null(weight_on_matrix) ) {
     if (any(is.na(mg_group_matrix)) ) {    
        # list wise deletion 
        index <- which(is.na(mg_group_matrix))    
        resp <- resp[-index,]
        mg_group_matrix <- matrix(mg_group_matrix[-index,]   , ncol=1)      
     }
    } 

    if (!is.null(weight_on_matrix)   & is.null(mg_group_matrix) & is.null(person_cov_matrix)) {
     if( any(is.na(weight_on_matrix)) ) {    
        # list wise deletion 
        index <- which(is.na(weight_on_matrix))    
        resp <- resp[-index,]
        weight_on_matrix <- matrix(weight_on_matrix[-index,] , ncol=1)        
     }
    } 

    if (!is.null(person_cov_matrix) & !is.null(mg_group_matrix) & is.null(weight_on_matrix)   ) {

     if ( any(is.na(person_cov_matrix))  & !any(is.na(mg_group_matrix)) ) {    
        # list wise deletion 
        index1 <- which(is.na(person_cov_matrix))     
        #index2 <- which(is.na(mg_group_matrix))   
        index <- unique(c(index1)) 
        
        resp <- resp[-index,]
        person_cov_matrix <- matrix(person_cov_matrix[-index,] , ncol=1)              
        mg_group_matrix <- matrix(mg_group_matrix[-index,] , ncol=1)       
      }

     if ( !any(is.na(person_cov_matrix))  & any(is.na(mg_group_matrix)) ) {    
        # list wise deletion 
        #index1 <- which(is.na(person_cov_matrix))     
        index2 <- which(is.na(mg_group_matrix))   
        index <- unique(c(index2)) 
        
        resp <- resp[-index,]
        person_cov_matrix <- matrix(person_cov_matrix[-index,] , ncol=1)              
        mg_group_matrix <- matrix(mg_group_matrix[-index,]    , ncol=1)    
      }


     if ( any(is.na(person_cov_matrix))  & any(is.na(mg_group_matrix)) ) {    
        # list wise deletion 
        index1 <- which(is.na(person_cov_matrix))    
        index2 <- which(is.na(mg_group_matrix))    
        index <- unique(c(index1, index2)) 
        
        resp <- resp[-index,]
        person_cov_matrix <- matrix(person_cov_matrix[-index,]  , ncol=1)           
        mg_group_matrix <- matrix(mg_group_matrix[-index,]   , ncol=1)   
      }
    }

    if (!is.null(mg_group_matrix)  & !is.null(weight_on_matrix) & is.null(person_cov_matrix)  ) {

     if( any(is.na(mg_group_matrix))  & !any(is.na(weight_on_matrix)) ) {    
        # list wise deletion 
        index2 <- which(is.na(mg_group_matrix)) 
        #index3 <- which(is.na(weight_on_matrix))   
        index <- unique(c(index2))
         
        resp <- resp[-index,]
        mg_group_matrix <- matrix(mg_group_matrix[-index,] , ncol=1)             
        weight_on_matrix <- matrix(weight_on_matrix[-index,]  , ncol=1)     
     }

     if( !any(is.na(mg_group_matrix))  & any(is.na(weight_on_matrix)) ) {    
        # list wise deletion 
        #index2 <- which(is.na(mg_group_matrix)) 
        index3 <- which(is.na(weight_on_matrix))   
        index <- unique(c(index3))
         
        resp <- resp[-index,]
        mg_group_matrix <- matrix(mg_group_matrix[-index,] , ncol=1)             
        weight_on_matrix <- matrix(weight_on_matrix[-index,]  , ncol=1)     
     }
     
     if( any(is.na(mg_group_matrix))  & any(is.na(weight_on_matrix)) ) {    
        # list wise deletion 
        index2 <- which(is.na(mg_group_matrix)) 
        index3 <- which(is.na(weight_on_matrix))   
        index <- unique(c(index2, index3))
         
        resp <- resp[-index,]
        mg_group_matrix <- matrix(mg_group_matrix[-index,]  , ncol=1)            
        weight_on_matrix <- matrix(weight_on_matrix[-index,]  , ncol=1)     
     }
    }

    if (!is.null(person_cov_matrix)  & !is.null(weight_on_matrix) & is.null(mg_group_matrix) ) {
     if( any(is.na(person_cov_matrix)) & !any(is.na(weight_on_matrix)) ) {    
        # list wise deletion 
        index1 <- which(is.na(person_cov_matrix))    
        #index3 <- which(is.na(weight_on_matrix))   
        index <- unique(c(index1))
         
        resp <- resp[-index,]
        person_cov_matrix <- matrix(person_cov_matrix[-index,] , ncol=1)            
        weight_on_matrix <- matrix(weight_on_matrix[-index,]  , ncol=1)     
     }

     if( !any(is.na(person_cov_matrix)) & any(is.na(weight_on_matrix)) ) {    
        # list wise deletion 
        #index1 <- which(is.na(person_cov_matrix))    
        index3 <- which(is.na(weight_on_matrix))   
        index <- unique(c(index3))
         
        resp <- resp[-index,]
        person_cov_matrix <- matrix(person_cov_matrix[-index,] , ncol=1)            
        weight_on_matrix <- matrix(weight_on_matrix[-index,]  , ncol=1)     
     }

     if( any(is.na(person_cov_matrix)) & any(is.na(weight_on_matrix)) ) {    
        # list wise deletion 
        index1 <- which(is.na(person_cov_matrix))    
        index3 <- which(is.na(weight_on_matrix))   
        index <- unique(c(index1, index3))
         
        resp <- resp[-index,]
        person_cov_matrix <- matrix(person_cov_matrix[-index,] , ncol=1)            
        weight_on_matrix <- matrix(weight_on_matrix[-index,]  , ncol=1)     
     }
    }
 
    if (!is.null(person_cov_matrix)  & !is.null(mg_group_matrix)  &  !is.null(weight_on_matrix) ) {  

      if (any(is.na(person_cov_matrix)) & !any(is.na(mg_group_matrix)) & !any(is.na(weight_on_matrix)) ) {
        # list wise deletion 
        index1 <- which(is.na(person_cov_matrix))    
        #index2 <- which(is.na(mg_group_matrix))    
        #index3 <- which(is.na(weight_on_matrix))   
        index <- unique(c(index1)) 
        resp <- resp[-index,]
        person_cov_matrix <- matrix(person_cov_matrix[-index,] , ncol=1)            
        mg_group_matrix <- matrix(mg_group_matrix[-index,] , ncol=1)     
        weight_on_matrix <- matrix(weight_on_matrix[-index,]  , ncol=1)  

     }

      if (!any(is.na(person_cov_matrix)) & any(is.na(mg_group_matrix)) & !any(is.na(weight_on_matrix)) ) {
        # list wise deletion 
        #index1 <- which(is.na(person_cov_matrix))    
        index2 <- which(is.na(mg_group_matrix))    
        #index3 <- which(is.na(weight_on_matrix))   
        index <- unique(c(index2)) 
        resp <- resp[-index,]
        person_cov_matrix <- matrix(person_cov_matrix[-index,]  , ncol=1)           
        mg_group_matrix <- matrix(mg_group_matrix[-index,]  , ncol=1)    
        weight_on_matrix <- matrix(weight_on_matrix[-index,] , ncol=1)   

     }

      if (!any(is.na(person_cov_matrix)) & !any(is.na(mg_group_matrix)) & any(is.na(weight_on_matrix)) ) {
        # list wise deletion 
        #index1 <- which(is.na(person_cov_matrix))    
        #index2 <- which(is.na(mg_group_matrix))    
        index3 <- which(is.na(weight_on_matrix))   
        index <- unique(c(index3)) 
        resp <- resp[-index,]
        person_cov_matrix <- matrix(person_cov_matrix[-index,]  , ncol=1)           
        mg_group_matrix <- matrix(mg_group_matrix[-index,]  , ncol=1)    
        weight_on_matrix <- matrix(weight_on_matrix[-index,] , ncol=1)   

     } 

      if (any(is.na(person_cov_matrix)) & any(is.na(mg_group_matrix)) & !any(is.na(weight_on_matrix)) ) {
        # list wise deletion 
        index1 <- which(is.na(person_cov_matrix))    
        index2 <- which(is.na(mg_group_matrix))    
        #index3 <- which(is.na(weight_on_matrix))   
        index <- unique(c(index1, index2)) 
        resp <- resp[-index,]
        person_cov_matrix <- matrix(person_cov_matrix[-index,] , ncol=1)            
        mg_group_matrix <- matrix(mg_group_matrix[-index,] , ncol=1)     
        weight_on_matrix <- matrix(weight_on_matrix[-index,] , ncol=1)   

     }

      if (any(is.na(person_cov_matrix)) & !any(is.na(mg_group_matrix)) & any(is.na(weight_on_matrix)) ) {
        # list wise deletion 
        index1 <- which(is.na(person_cov_matrix))    
        #index2 <- which(is.na(mg_group_matrix))    
        index3 <- which(is.na(weight_on_matrix))   
        index <- unique(c(index1, index3)) 
        resp <- resp[-index,]
        person_cov_matrix <- matrix(person_cov_matrix[-index,], ncol=1)             
        mg_group_matrix <- matrix(mg_group_matrix[-index,] , ncol=1)     
        weight_on_matrix <- matrix(weight_on_matrix[-index,] , ncol=1)   

     }

      if (any(!is.na(person_cov_matrix)) & any(is.na(mg_group_matrix)) & any(is.na(weight_on_matrix)) ) {
        # list wise deletion 
        #index1 <- which(is.na(person_cov_matrix))    
        index2 <- which(is.na(mg_group_matrix))    
        index3 <- which(is.na(weight_on_matrix))   
        index <- unique(c(index2, index3)) 
        resp <- resp[-index,]
        person_cov_matrix <- matrix(person_cov_matrix[-index,]  , ncol=1)           
        mg_group_matrix <- matrix(mg_group_matrix[-index,] , ncol=1)     
        weight_on_matrix <- matrix(weight_on_matrix[-index,] , ncol=1)   

     }
      
      if (any(is.na(person_cov_matrix)) & any(is.na(mg_group_matrix)) & any(is.na(weight_on_matrix)) ) {
        # list wise deletion 
        index1 <- which(is.na(person_cov_matrix))    
        index2 <- which(is.na(mg_group_matrix))    
        index3 <- which(is.na(weight_on_matrix))   
        index <- unique(c(index1, index2, index3)) 
        resp <- resp[-index,]
        person_cov_matrix <- matrix(person_cov_matrix[-index,] , ncol=1)            
        mg_group_matrix <- matrix(mg_group_matrix[-index,] , ncol=1)     
        weight_on_matrix <- matrix(weight_on_matrix[-index,] , ncol=1)   

     }
    }
 
    
    # names 

    name <- new('inputClass',
            name_pp_cov=name_pp_cov, 
            name_it_cov_beta=name_it_cov_beta, 
            name_it_cov_alpha=name_it_cov_alpha, 
            name_pp_mg=name_pp_mg)    
    

    # Input 
    input(model=model, data=resp, select=select, subset=subset, loading=list(on=loading_on, inside=loading_inside), 
                mul=list(on=mul_on, dim_info=mul_dim_info, cov_info=mul_cov_info), 
                bifac=list(on=bifac_on, dim_info=bifac_dim_info, cov_info=bifac_cov_info), 
                second=list(on=second_on, dim_info=second_dim_info, cov_info=second_cov_info), 
                guess=list(on=guess_on), 
                person_cov=list(on=person_cov_on, person_matrix=person_cov_matrix, main= person_main), 
                item_cov=list(on=item_cov_on, item_matrix_beta=item_cov_beta, item_matrix_alpha=item_cov_alpha), 
                dif=list(on=dif_on, dif_beta=dif_on_beta, dif_alpha=dif_on_alpha), 
                mg=list(on=mg_on, group_matrix=mg_group_matrix), mixture =list(on=mixture_on, num=mixture_num), 
                weight=list(on=weight_on, weight_matrix=weight_on_matrix), post=post, 
                start = list(on=start_on, npar=st_npar, start_info=st_info, start_value=st_value),
                constraint = list(on=const_on, npar=const_npar, cons_info=const_info, cons_value=const_value), 
                evaluate = list(on=evalt_on, eval_value = evalt_value), 
                control=list(minpercent= control_minpercent, max_it=control_max_it, nq=control_nq, 
                conv=control_conv, link=control_link, adapt=control_adapt, 
                se_num=control_se_num, se_emp=control_se_emp, alp_bounds_up =control_alp_bounds_up, alp_bounds_low =control_alp_bounds_low, 
                verbose=control_verbose, show=control_show))
    
    # Output 
    est <- output(model=model, names=name, direct = current_direct, show=control_show )  
    
    ## delete output files (from matlab)
    
    if (delete_output==TRUE) {
    
        file_names <- c("parms.txt", "rest.txt","loglikelihood.txt", "se_emp.txt", "se_num.txt", "infonum.txt", "infoemp.txt", 
                        "eap.txt", "var.txt", "cov.txt",
                        "expected.txt", "rel_e.txt", "data_new.txt", "rundata.txt", "item_cov_beta.txt", "item_cov_alpha.txt","person_cov.txt", 
                        "person_group.txt", "dim_info.txt","weight.matrix.txt","start.txt",
                        "constraint.txt","evaluate.txt","input_2pl.txt","input_uni.txt",
                        "input_mul.txt","input_2pl_mul.txt","input_bifactor.txt", "input_secondorder.txt",  "input_3pl.txt",
                        "final_it.txt", "tempfd2_i.mat")
    
        for (f in 1: length(file_names)) {
            if(file.exists(file_names[f])) file.remove(file_names[f])
        }
    
   }
   
    ## change directory

    setwd(est@direct) 

    # return
    est


}
