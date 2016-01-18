## These are the functions for data expansion based on mapping matrix

dendrify2 <- function(mat, cmx, missing.omit=FALSE, wide=FALSE) {

    # mat: original item response data 
    # cmx: mapping matrix 
    # missing.omit: for a long-form data, the lines with 
    # a missing item response are removed
    # wide: for a wide-form expanded data 

    ff <- factor((m1 <- tolong(mat) )[["value"]])
    stopifnot(is.matrix(cmx) | is.data.frame(mat),
              (nr <- nrow(cmx)) == length(levels(ff)),
              (nc <- ncol(cmx)) < nr)
    if (missing.omit ==TRUE) {
        long <-  subset(within(data.frame(value = 
                 as.vector(cmx[as.integer(ff), ]),
                 item = rep(m1$item, nc),
                 person = rep(m1$person, nc),
                 node = gl(nc, nrow(m1),
                 labels = sprintf(paste("node%0", nchar(nc), "d", sep=''),
                 seq_len(nc)))),sub <- item:node), !is.na(value))        

        return(long)
   } else { 

        if (wide==FALSE) {   
            long <- within(data.frame(value = as.vector(cmx[as.integer(ff), ]),
                 item = rep(m1$item, nc),
                 person = rep(m1$person, nc),
                 node = gl(nc, nrow(m1),
                 labels = sprintf(paste("node%0", nchar(nc), "d", sep=''),
                 seq_len(nc)))), sub <- item:node )
                             
            return(long)
                             
        } else {     

            long <- within(data.frame(value = as.vector(cmx[as.integer(ff), ]),
                 item = rep(m1$item, nc),
                 person = rep(m1$person, nc),
                 node = gl(nc, nrow(m1),
                 labels = sprintf(paste("node%0", nchar(nc), "d", sep=''),
                 seq_len(nc)))), sub <- item:node )


            options(warn=-1)       
            wide <-  reshape(long, v.names = "value", idvar = "person",
                timevar = "sub",  direction = "wide", drop=c("item","node")) 

            return(wide)
        }                               
                             

    }
        
} 

# sub-function for dendrify2 
tolong <- function(mat) {
    stopifnot(is.matrix(mat)| is.data.frame(mat))
    mat <- as.matrix(mat)
    nr <- nrow(mat)
    nc <- ncol(mat)
    data.frame(value = as.vector(mat),
       item = gl(nc, nr,
       labels = sprintf(paste("i%0", nchar(nc), "d", sep=''), 
       seq_len(nc))),
       person = gl(nr, 1, length=length(mat),
       labels = sprintf(paste("p%0", nchar(nr), "d", sep=''), 
       seq_len(nr))))
}
