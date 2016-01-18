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

# input class 

setClassUnion("vec.null",c("vector","NULL"))
setClass(
        Class ='inputClass',
        representation = representation(
            name_pp_cov='vec.null', 
            name_it_cov_beta='vec.null', 
            name_it_cov_alpha='vec.null', 
            name_pp_mg='vec.null' ),
        validity = function(object) return(TRUE)
)
