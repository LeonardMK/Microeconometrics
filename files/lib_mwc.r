####################################################################################
# mwc_2way(fit, v1, v2)
#
# Description
# -----------
# Take a fitted model and two panel dimensions, and return a revised estimate of
# the parameter covariance matrix clustered on each of the dimensions, as in 
# Cameron, Gelbach & Miller 2006
#
# Arguments
# ---------
#       fit             Fitted model
#       v1,2            Panel dimensions on which to cluster
#
####################################################################################

mwc_2way <- function(fit, v1, v2){

    # give proper names, attributes etc. to return matrix
    retvar <- robcov(fit, v1)$var
    retvar[,] <- 0

    # list factors and interactions to cluster on and the corresponding signs
    clustervars <- list(v1, 
                        v2, 
                        interaction(v1,v2))             
    signs <- c(1,1,-1)
    
    for(i in 1:NROW(signs)){
    
        curclusvar <- clustervars[[i]]
        cursign <- signs[i]
        
        # calculate a finite-clusters inflation factor
        D <- NROW(unique(curclusvar))
        inflator <- D/(D-1)
        
        retvar <- retvar + cursign*inflator*robcov(fit, curclusvar)$var
    
    }

    return(retvar)
}

####################################################################################
# mwc_3way(fit, v1, v2, v3)
#
# Description
# -----------
# Take a fitted model and three panel dimensions, and return a revised estimate of
# the parameter covariance matrix clustered on each of the dimensions, as in 
# Cameron, Gelbach & Miller 2006
#
# Arguments
# ---------
#       fit             Fitted model
#       v1,2,3          Panel dimensions on which to cluster
#
####################################################################################

mwc_3way <- function(fit, v1, v2, v3){

    # give proper names, attributes etc. to return matrix
    retvar <- robcov(fit, v1)$var
    retvar[,] <- 0

    # list factors and interactions to cluster on and the corresponding signs
    clustervars <- list(v1, 
                        v2, 
                        v3, 
                        interaction(v1,v2),
                        interaction(v2,v3),
                        interaction(v1,v3),
                        interaction(v1,v2,v3))              
    signs <- c(1,1,1,-1,-1,-1,1)
    
    for(i in 1:NROW(signs)){
    
        curclusvar <- clustervars[[i]]
        cursign <- signs[i]
        
        # calculate a finite-clusters inflation factor
        D <- NROW(unique(curclusvar))
        inflator <- D/(D-1)
        
        retvar <- retvar + cursign*inflator*robcov(fit, curclusvar)$var
    
    }

    return(retvar)
}
