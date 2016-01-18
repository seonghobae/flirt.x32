\name{flirt}

\alias{flirt}

\title{Uni- and Multi- dimensional Item Response Theory Analysis}

\description{
   Maximum likelihood estimation of item response theory (IRT) models for binary and polytomous data
}

\usage{
flirt(data, select=NULL, subset=NULL, loading=list(on=FALSE, inside=FALSE), 
    mul=list(on=FALSE, dim_info=NULL, cov_info=NULL), 
    bifac=list(on=FALSE, dim_info=NULL, cov_info=NULL), 
    second=list(on=FALSE, dim_info=NULL, cov_info=NULL), 
    guess=list(on=FALSE), 
    person_cov=list(on=FALSE, person_matrix=NULL, main=NULL), 
    item_cov=list(on=FALSE, item_matrix_beta=NULL, item_matrix_alpha=NULL), 
    dif=list(on=FALSE, dif_beta=NULL, dif_alpha=NULL), 
    mg=list(on=FALSE, group_matrix=NULL), mixture =list(on=FALSE, num=NULL), 
    weight=list(on=FALSE, weight_matrix=NULL), post=FALSE, 
    start = list(on=FALSE, npar=NULL, start_info=NULL, start_value=NULL), 
    constraint = list(on=FALSE, npar=NULL, cons_info=NULL, cons_value=NULL), 
    evaluate = list(on=FALSE, eval_value=NULL), 
    control=list(minpercent= NULL, max_it=NULL, nq=NULL, conv=NULL, 
            link=NULL, adapt=NULL, se_num=NULL, se_emp=NULL, alp_bounds_up=NULL,
            verbose=NULL, show=NULL) )
}

\arguments{
  \item{data}{
        matrix or data.frame in wide form - persons in rows and items in columns. 
        Group membership, person covariates and sampling weights may be included as additional columns 
  }
  \item{select}{
        vector of item numbers  or item names. If data include other than item responses or the user wants to analyze only subset of items, 
        \code{select} should be specified.  
  }  
  \item{subset}{
        vector of names or numbers of cases (rows) of choice. 
  }
  \item{loading}{
        list. 
        \itemize{
            \item \bold{on}: logical. if \code{TRUE}, a 2PL model family, if \code{FALSE}, a 1PL model family (default) is used. 
            \item \bold{inside}: logical. When on==\code{TRUE}, if inside==\code{TRUE},  \eqn{\alpha_i(\theta_p+ \beta_i)} parameterization, 
            if \code{FALSE},  \eqn{\alpha_i \theta_p + \beta_i} parameterization (default) is used. 
        }
  }
  \item{mul}{
        list. 
        \itemize{
            \item \bold{on}: if \code{TRUE}, a multidimensional model, if \code{FALSE}, a unidimensional model (default) is used. 
            \item \bold{dim_info}: list. If on==\code{TRUE}, a list of item numbers or columns for each dimension should be provided. 
            \item \bold{cov_info}: matrix or data.frame. If there are person covariates, cov_info should be provided, 
                    which indicates which covariates are used in each dimension.
        }
  }  
  \item{bifac}{
        list. 
        \itemize{
            \item \bold{on}: logical. If \code{TRUE}, bifactor model, if \code{FALSE}, unidimensional model (default). 
            \item \bold{dim_info}: list. If on==\code{TRUE}, list of item numbers or columns for each dimension should be provided. 
            \item \bold{cov_info}: matrix or data.frame. If there are person covariates, cov_info should be provided, 
                        which indicates which covariates are used in each dimension.
        }

  }  
  \item{second}{
        list. 
        \itemize{
            \item \bold{on}: logical. If \code{TRUE}, second-order (testlet) model, if \code{FALSE}, unidimensional model (default). 
            \item \bold{dim_info}: list. If on==\code{TRUE}, list of item numbers or columns for each dimension should be provided. 
            \item \bold{cov_info}: matrix or data.frame. If there are person covariates, cov_info should be provided, 
                        which indicates which covariates are used in each dimension.
        }

  } 
  \item{guess}{
        list. 
        \itemize{
            \item \bold{on}: logical. If \code{TRUE}, a 3PL model is estimated (\eqn{\alpha_i}, \eqn{\beta_i}, and \eqn{c_i} parameters), 
            if \code{FALSE}, only \eqn{\beta_i} parameters are estimated (default is 1PL models). 

        }

  }
  \item{person_cov}{
        list. 
        \itemize{
            \item \bold{on}: logical. If \code{TRUE},  person covariates columns (or names) or the data matrix should be provided in person_matrix.
            \item \bold{person_matrix}: column numbers or variable names if part of data, otherwise, matrix or data.frame for the person covariates. 
                                    Number and order of rows should be the same as those in data. 
                                    If \code{subset} is used, the number of rows should be adjusted accordingly. 
            \item \bold{main}: logical. If main==\code{TRUE}, person covariates are treated as main effects  
                    in the linear predictor instead of as predictors of a latent regression model. Default is main==\code{FALSE}.
        }
  }  
  \item{item_cov}{
        list. 
        \itemize{
            \item \bold{on}: logical. If \code{TRUE}, data matrix for item covariates should be provided either for \eqn{\beta_i} or \eqn{\alpha_i}. 
            \item \bold{item_matrix_beta}: matrix or data.frame for item covariates that are regressed on \eqn{\beta_i}. 
            Number of rows should be the same as the number of items. 
            If \code{select} is used, the number of rows should be adjusted accordingly. 
            \item \bold{item_matrix_alpha}: matrix or data.frame for item covariates that are regressed on \eqn{\alpha_i}. 
            Number of rows should be the same as the number of items. If this option is used, standard errors are not estimated. 
            If \code{select} is used, the number of rows should be adjusted accordingly. 
        }
  }
  \item{dif}{
        list. 
        \itemize{
            \item \bold{on}: logical. If \code{TRUE}, DIF analysis is performed. \code{FALSE} is default. 
            \item \bold{dif_beta}: vector. Item numbers or variable names for DIF for \eqn{\beta_i}.
            \item \bold{dif_alpha}: vector. Item numbers or variable names for DIF for \eqn{\alpha_i}.
        }
  }
  \item{mg}{
        list.
        \itemize{
            \item \bold{on}: logical. If \code{TRUE}, multiple-group analysis is performed.  \code{FALSE} is default. 
            \item \bold{group_matrix}: column number or variable name for person membership if part of data, 
                                    otherwise data.matrix should be provided in group_matrix. 
                                    Number and order of rows should be the same as those in data. 
                                    If \code{subset} is used, the number of rows should be adjusted accordingly. 
                                    Persons should be ordered by groups. 
                                    
        }
  } 
  \item{mixture}{
        list. 
        \itemize{
            \item \bold{on}: logical. If \code{TRUE}, mixture model is specified (currently, not available).
            \item \bold{num}: logical. If on==\code{TRUE}, number of latent classes should be provided. 
        }
  }
  \item{weight}{
        list. 
        \itemize{
            \item \bold{on}: logical. If \code{TRUE}, sampling or frequency weights are included. \code{FALSE} is default. 
            \item \bold{weight_matrix}: if on==\code{TRUE}, column number or variable name if part of data, 
                                        otherwise data.matrix for weights should be provided. 
                                        Number and order of rows should be the same as those in data. 
                                        If \code{subset} is used, the number of rows should be adjusted accordingly. 
        }
  } 
  \item{post}{
        logical. if \code{TRUE}, expected a posteriori (EAP) (and its variance and covariance), expected sum-scores, and IRT reliability are provided. 
        \code{FALSE} is default. 
  }
  \item{start}{
        \itemize{
            \item \bold{on}: logical. If \code{TRUE}, use-specified starting values are used; otherwise, random starting values are used (default).
            \item \bold{npar}: integer of the total number of parameters. 
            \item \bold{start_info}: vector of  the parameter numbers for starting values to be used.  
            \item \bold{start_value}: vector of the starting values for the specified parameters in \code{start_info}. 
        }
  }          
  \item{constraint}{
        \itemize{
            \item \bold{on}: logical. If \code{TRUE}, users can specify parameter constraints.  \code{FALSE} is default. 
            \item \bold{npar}: integer of  the total number of parameters. 
            \item \bold{cons_info}: vector of the  parameter numbers to be constrained. 
            \item \bold{cons_value}: vector of the specific values for the specified parameters to be fixed at. 
        }
  }
  \item{evaluate}{
        \itemize{
            \item \bold{on}: logical. If \code{TRUE}, log-likelihood is evaluated at given parameter values.  \code{FALSE} is default. 
            \item \bold{eval_value}: vector of parameter values where the log-likelihood is evaluated at.  
        }
  }
  \item{control}{
        list of control options with components:
        \itemize{
            \item \bold{minpercent}: positive real value. Minimum percentage for a response category for polytomous items. 
                    If the relative frequency for a category is lower than minpercent, that category is collapsed into 
                    the lower adjacent category. Default is 0.  
                    To see how the category is collapsed, put the control option show=\code{TRUE}. 
            \item \bold{max_it}: positive integer. Maximum number of iterations. Default is 10,000.       
            \item \bold{nq}: positive integer. Numbers of quadrature points. Default is 20. 
                        For multidimensional and bifactor models, a vector of quadrature points for each dimension in order 
                        (dimension 1, dimension 2, and so on for multidimensional models;  
                        general dimension, specific dimension 1, specific dimension 2, and so on for bifactor models).
                        If a scalar is specified, the specified number is used for all dimensions.   
            \item \bold{conv}: positive real value. Convergence criterion. 
            The iteration between EM estimations stops when the maximum absolute difference 
            in the parameter estimates becomes equal or smaller than the criterion 
            between two subsequent iterations. Default is 0.0001. 
            \item \bold{link}: positive integer 1, 2, or 3. or  "multinomial", "cumulative", "adjacent". 
                                Default is multinomial or 1 for binary data (that leads to logit link) and "adjacent" for polytomous data. 
            \item \bold{adapt}: logical. If \code{TRUE}, adaptive quadrature is used. if \code{FALSE}, Gauss-Hermite quadrature is used (default). 
            \item \bold{se_num}: logical. If \code{TRUE}, standard error estimates are computed using the Hessian matrix obtained by numerical differentiation.
            \item \bold{se_emp}: logical. If \code{TRUE}, standard error estimates are computed using an empirical information matrix.(default) 
                            If \bold{se_num} is \code{TRUE}, standard errors are computed using this method are contained as extra attributes.
                            If \bold{se_emp} is \code{FALSE}, these standard errors are used as default standard errors. 
            \item \bold{alp_bounds_up}: positive integer. Upper boundary value (positive) for \eqn{\alpha_i}. Default is 15. 
            Cannot be used when item covariates are regressed on \eqn{\alpha_i}. 
            \item \bold{alp_bounds_low}: positive integer. Lower boundary value for \eqn{\alpha}. Default is -1. 
            Cannot be used when item covariates are regressed on \eqn{\alpha_i}. 
            \item \bold{verbose}: logical. If \code{TRUE}, the iteration number, 
            the maximum absolute difference in the parameter estimates (par_diff) and 
            difference in the log-likelihood (lik_diff) between adjacent iterations are printed. 
            \item \bold{show}: logical. If \code{TRUE}, print Matlab output (error messages) on R console. 
        }
  }


}

\details{

        \itemize{

            \item \bold{Parameterization}: For a linear predictor \code{flirt} uses \eqn{\theta_p + \beta_i} for 1PL models, 
                    \eqn{\alpha_i(\theta_p + \beta_i)} or \eqn{\alpha_i \theta_p + \beta_i} for 2PL models, 
                    and  \eqn{\alpha_{ig} \theta_{pg} +\alpha_{is} \theta_{ps} + \beta_i} for bifactor and second-order models, 
                    where \eqn{\theta_p} is the ability of a person \eqn{p}, 
                    \eqn{\beta_i} is the item easiness (or intercept), 
                    \eqn{\alpha_i} is the loading (or slope), and \eqn{\theta_{pg}} and \eqn{\theta_{ps}} are abilities for the general 
                    \eqn{g} and specific dimension \eqn{s} with
                    \eqn{\alpha_{ig}}  and \eqn{\alpha_{is}}, respectively. 
                    For 3PL models, the guessing parameter \eqn{c_i} is incorporated in the probability,  
                    \eqn{P_i = c_i + \frac{(1-c_i)}{1+ exp(-(\alpha_i \theta_p + \beta_i))}}.   
                    For second-order models, \eqn{\alpha_{ig}/ \alpha_{is}} is  
                    the second-order loading for the \eqn{s}th first-order factor on the second-order factor \eqn{\theta_{pg}}, 
                    which is constant for the items within \eqn{s}th first-order factor. 
             
            \item \bold{Polytomous item responses}: Minimum category should be 0.
            
            \item \bold{Multiple group analysis}: Group membership should start from 0. 
            
            \item \bold{Standard errors}: 
                    For variance-covariance parameters, standard errors are not provided and NA is returned. 
                    For parameters that are constrained, standard errors are not calculated and NA is returned.  

            \item \bold{Starting values, evaluation}: 
                    Parameter values (to be fixed at) for variance-covariance parameters 
                    should be Cholesky elements of lower triangular matrix \eqn{L} (\eqn{Cov = LL'} )  

            \item \bold{Boundary values}: The maximum boundary values for parameters are set to 
                    \eqn{\pm 15}. The upper and lower boundary values for \eqn{\alpha_i} can be modified using 
                    the control options \code{alp_bounds_up} and \code{alp_bounds_low}.   
                    If a parameter estimate crosses the boundary value during iterations, 
                    the parameter estimate is automatically restricted to the boundary value and no parameter and standard error 
                    estimates are provided with a warning message. 
            
            \item \bold{Missing values}: Missing values should be specified as NA. Missing values in item responses and item design
                    matrices are treated as ignorable, but for missing values in person design, person group, and weight matrices, 
                    listwise deletion is used.    

        }
 
  
}

\value{

  An object of class \code{flirt}, with the following slots that can be extracted using object@
  
  \item{pars}{matrix of parameters estimates and standard errors}
  \item{parms}{original parameter estimates from BNLflirt (including cholesky estimates for multidimensional models)}
  \item{info_num}{information matrix (numerical) evaluated at the maximum likelihood estimates}
  \item{se_emp}{standard errors using empirical information matrix (if used) }
  \item{info_emp}{information matrix (empirical) evaluated at the maximum likelihood estimates (if used)}
  \item{loglik}{log-likelihood value at convergence}
  \item{AIC}{Akaike information criterion: \eqn{-2*loglik+2*npar}, where \eqn{npar} is number of parameters }
  \item{BIC}{Bayesian information criterion: \eqn{-2*loglik+npar*log(nobs)}, where \eqn{nobs} is number of cases (persons) }
  \item{npar}{number of parameters} 

  \item{post}{post analysis with a list of 
      \itemize{ 
          \item{eap:}{ expected a posteriori (EAP) ability estimate(s)}  
          \item{eap_var:}{ variances of expected a posteriori (EAP) ability estimate(s) }  
          \item{eap_cov:}{ covariance matrix of expected a posteriori (EAP) ability estimate(s) }  
          \item{exp_s:}{ expected scores}  
          \item{rel:}{ IRT reliability using empirical priors }    
      }    
  }  


  \item{data_inf}{data information with a list of 
      \itemize{ 
          \item{nobs:}{ number of people}
          \item{ngroup:}{ number of groups}
          \item{nitem:}{ number of items}
          \item{MS_item:}{maximum category of items (scalar)}  
          \item{S_item:}{categories of all items (vector)}  
      }
    }

  \item{dim_inf}{dimension information with a list of 
        \itemize{ 
          \item{within:}{ within-item model:1, otherwise:0  } 
          \item{ndim:}{ number of dimensions  } 
          \item{n_it_dim:}{ number of items in each dimension  }     
        }
  }      
  \item{inside}{inside:1, otherwise:0 (2PL parameterization) }   
  \item{model}{1:unidimensional 1PL model family, 2:unidimensional 2PL model family, 3: multidimensional 1PL model family,
                4: multidimensional 2PL model family, 5: bifactor model family }   
  
  \item{dif_beta}{items that are under investigation for DIF for \eqn{\beta_i}  }     
  \item{dif_alpha}{items that are under investigation for DIF for \eqn{\alpha_i}    }   

  \item{est_inf}{estimation information with a list of   
        \itemize{ 
            \item{adapt:}{ adaptive quadrature  }
            \item{nqr:}{ number of quadrature points  }            
            \item{conv:}{ convergence criterion  } 
            \item{max_it:}{ total number of iterations  } 
            \item{link:}{ link function  } 
            \item{verbose:}{ verbose  } 
            \item{alp_bounds_up:}{ upper boundary value for \eqn{\alpha_i} } 
            \item{alp_bounds_low:}{ lower boundary value for \eqn{\alpha_i}  } 
        
         }
  }
  
}



\author{
  Minjeong Jeon <jeon.117@osu.edu>
}


\seealso{
 \code{\link{summary}}, \code{\link{coef}}, \code{\link{logLik}}, \code{\link{anova}} 
}

\examples{

# set directory where the .tar is located. 
install.packages("flirt", type="source", repos=NULL) 
library(flirt)

# show built-in datasets 
data(package="flirt")

# with adjacent link function (partial credit model)
result1 <- flirt(data=charity, subset=1:100, 
        control=list(minpercent=0.05, nq=5, link="adjacent", show=TRUE))

## verbal aggression data
data(verb2)

# 2-dimensional 2PL model for binary data 
# with a(th+b) parameterization
result2 <- flirt(data=verb2, select=2:25,  
            mul=list(on=TRUE, dim_info=list(dim1=1:12, dim2=13:24))  )

# output 
result2
summary(result2)
coef(result2)
logLik(result2)


# 1PL graded response model for polytomous responses 
# with cumulative link function (graded response model)
data(charity)
result3 <- flirt(data=charity, subset=1:100, 
        control=list(minpercent=0.05, nq=5, link="cumulative", show=TRUE))



}


\keyword{flirt} 
