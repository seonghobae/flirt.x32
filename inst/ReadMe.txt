==========================================================================================
*** FLIRT: Flexible Item Response Theory Modeling *** 01.18.2015
==========================================================================================

Overview:
---------

  The FLIRT R package provides a flexible framework for uni- and multi- dimensional explanatory IRT modeling
for binary and polytomous item responses


Modeling Features:
---------

    - One parameter logistic (1PL) and two parameter logistic (2PL) models

    - Multi-dimensional models (between-item and within-item models)

    - Bifactor models
    
    - Person covariates (latent regression)

    - Item covariates (linear logistic test models)

    - Multiple group analysis

    - Differential item functioning analysis

    - Polytomous responses (partial credit models, rating scale models, graded response models)

    - Combinations of above

    ** New features (1.15) **

    - second-order models
    
    - Three parameter logistic (3PL) models 
    

Estimation Features:
---------

    - Efficient ML estimation with a modified EM algorithm using graphical model theory

    - Adaptive quadrature

    - For computation, FLIRT relies on Matlab executables (BNLflirt)



Installation:
-------------


  - Install R (R-2.13.0 or later)

  - Install MATLAB Compiler Runtime (MCR) (for Matlab 2014a, windows 32bit)

  - MCR is freely downloadable (http://www.mathworks.com/products/compiler/mcr/).

  - It is critical to install the correct (specified above) version of MCR.
  (Matlab policy: The version of the MCR on the target computer should be compatible with
  the version of MATLAB Compiler (Matlab 2014a, windows 32bit) that built applications(FLIRT))

  * Installation

    - Download the source file flirt_1.15.tar.gz for 64bit for flirt.x32_1.15.tar.gz for 32 bit 
       from http://faculty.psy.ohio-state.edu/jeon/lab/flirt.php

    - Type in the R console, 
        for 64bit 
        install.packages("flirt_1.15.tar.gz", type="source", repos=NULL)
        
        for 32bit 
        install.packages("flirt.x32_1.15.tar.gz", type="source", repos=NULL)

     (Note: flirt is not currently available on CRAN)

    - Ready to use FLIRT by tying in the R console

    Examples:

    ###### procedure ######
    ## Load the package (64bit version) ##
    install.packages("flirt_1.15.tar.gz", type="source", repos=NULL)
    library(flirt)

    ## Load the package (32bit version) ##
    install.packages("flirt.x32_1.15.tar.gz", type="source", repos=NULL)
    library(flirt.x32)


Notes:
-------------

    - Due to the MCR version requirement, this version of
      FLIRT is available only for Windows 64/32 bit operating systems.
      
    - The versions for Mac will be released in future.

    - FLIRT cannot be run in multiple R consoles simultaneously (MCR can be used once at a time per computer.)

    - For package updates, visit http://faculty.psy.ohio-state.edu/jeon/lab/flirt.php

    - Matlab code (BNLflirt) is available from http://faculty.psy.ohio-state.edu/jeon/lab/flirt.php


--------------------------------
Minjeong Jeon
jeon.117@osu.edu
