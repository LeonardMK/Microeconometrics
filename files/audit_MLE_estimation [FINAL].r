###############################################################################
#
#   audit_MLE_estimation.r
#   CLEANED UP VERSION OF [V6][8-17-2012]
#   
###############################################################################

### setup #####################################################################

    # source
    library(epicalc)
    library(rms)
    
    # clear
    zap()
    rm(list=ls(all=TRUE))
    
    # set locations
    #rootdir <- SET ROOT DIRECTORY HERE
    
    workingdir <- paste(rootdir, "workingdir", sep="/") # STORE DATA FILES AND HELPER FILES HERE
    tabdir <- paste(rootdir, "tables", sep="/") # OUTPUT HERE

    setwd(workingdir)

### likelihood functions ######################################################

########### 1st order in t

    objfun.1st <- function(theta, tstar, Ts, months, years, ds){
    
        logps <- rep(0, NROW(Ts))
    
        for (i in 1:NROW(Ts)){
        
            T <- Ts[i]
            d <- ds[i]
        
            if(!is.na(months[i])){
        
                t <- months[i]
                
                logps[i] <- log(prob.1st.monthly(theta, tstar, t, T, d))
                for (tau in (t+1):T){
                    logps[i] <- logps[i] + log(1 - prob.1st.monthly(theta, tstar, tau, T, d))
                }
                
            } else if (!is.na(years[i])) {
            
                t <- years[i]
                 
                logps[i] <- log(prob.1st.yearly(theta, tstar, t, 1, d))
                for (tau in (t*12 + 1):T){
                    logps[i] <- logps[i] + log(1 - prob.1st.monthly(theta, tstar, tau, T, d))
                }
                
            } else {
            
                # HARDCODED CUTOFF DATE HERE
                for (tau in (-22):T){
                    logps[i] <- logps[i] + log(1 - prob.1st.monthly(theta, tstar, tau, T, d))
                }
            
            }
        }
        
        return(-sum(logps))
    
    }
    
    prob.1st.monthly <- function(theta, tstar, t, T, d){
    
        temp <- theta[1]*as.numeric(t >= tstar) + theta[2]*as.numeric(d==11) + theta[3]*as.numeric(d==24) + theta[4]*as.numeric(d==29) + theta[5]*t
        return(exp(temp) / (1 + exp(temp)))
    
    }
    
    prob.1st.yearly <- function(theta, tstar, t, T, d){
        
        temp <- 1
        for (m in 1:12){
            temp <- temp * (1 - prob.1st.monthly(theta, tstar, (t-1)*12 + m, T, d)) 
        }
        return(1 - temp)
    
    }
    
    # test suite
    #theta <- rep(0,5)
    #prob.1st.monthly(theta, tstar=5, t=1, T=14, d=1)
    #prob.1st.yearly(theta, tstar=5, t=1, T=14, d=1)
    #objfun.1st(theta, tstar=5, Ts=c(14), months=c(NA), years=c(1), ds=c(1))
    #-log((1 - 0.5^12)*(0.5^2))
    #objfun.1st(theta, tstar=5, Ts=c(14,14), months=c(NA,13), years=c(1,NA), ds=c(1,1))
    #-log((1 - 0.5^12)*(0.5^2)) - log(0.5*0.5)
    
########### 2nd order in t

    objfun.2nd <- function(theta, tstar, Ts, months, years, ds){
    
        logps <- rep(0, NROW(Ts))
    
        for (i in 1:NROW(Ts)){
        
            T <- Ts[i]
            d <- ds[i]
        
            if(!is.na(months[i])){
        
                t <- months[i]
                
                logps[i] <- log(prob.2nd.monthly(theta, tstar, t, T, d))
                for (tau in (t+1):T){
                    logps[i] <- logps[i] + log(1 - prob.2nd.monthly(theta, tstar, tau, T, d))
                }
                
            } else if (!is.na(years[i])) {
            
                t <- years[i]
                 
                logps[i] <- log(prob.2nd.yearly(theta, tstar, t, 1, d))
                for (tau in (t*12 + 1):T){
                    logps[i] <- logps[i] + log(1 - prob.2nd.monthly(theta, tstar, tau, T, d))
                }
                
            } else {
            
                # HARDCODED CUTOFF DATE HERE
                for (tau in (-22):T){
                    logps[i] <- logps[i] + log(1 - prob.2nd.monthly(theta, tstar, tau, T, d))
                }
            
            }
        }
        
        return(-sum(logps))
    
    }
    
    prob.2nd.monthly <- function(theta, tstar, t, T, d){
    
        temp <- theta[1]*as.numeric(t >= tstar) + theta[2]*as.numeric(d==11) + theta[3]*as.numeric(d==24) + theta[4]*as.numeric(d==29) + theta[5]*t + theta[6]*t*t
        return(exp(temp) / (1 + exp(temp)))
    
    }
    
    prob.2nd.yearly <- function(theta, tstar, t, T, d){
        
        temp <- 1
        for (m in 1:12){
            temp <- temp * (1 - prob.2nd.monthly(theta, tstar, (t-1)*12 + m, T, d)) 
        }
        return(1 - temp)
    
    }

### load data ######################################################

    data <- read.csv(paste0(workingdir, "/data/village_data_final.csv"))
    
    # restrict to villages with visits post-NREGA
    # (preferred cutoff == -22 == February 2005, the national start date)
    cutoff <- -22
    yearcutoff <- -1
    
    # Orissa BDO series with NAs
    or.bdo.data <- data[data$sid==24 & !is.na(data$did), ]
    or.bdo.data$q10_BDO_month[or.bdo.data$q10_BDO_month < cutoff] <- NA
    or.bdo.data$q10_BDO_year[or.bdo.data$q10_BDO_year < yearcutoff] <- NA
    
    # Orissa collector series with NAs
    or.collector.data <- data[data$sid==24 & !is.na(data$did), ]
    or.collector.data$q11_collector_month[or.bdo.data$q11_collector_month < cutoff] <- NA
    or.collector.data$q11_collector_year[or.bdo.data$q11_collector_year < yearcutoff] <- NA

    # set date to test: May 2007 = 5, April 2006 = -8, February 2006 = -10
    thetstar <- -10

### 1st-order polynomial in t ######################################################
    
########### BDO visits
    
    or.bdo.1.mle <- nlm(objfun.1st, rep(0,5), 
                            tstar=thetstar, 
                            Ts=rep(max(or.bdo.data$q10_BDO_month, na.rm=TRUE), NROW(or.bdo.data$q10_BDO_month)), 
                            months=or.bdo.data$q10_BDO_month,
                            years=or.bdo.data$q10_BDO_year,
                            ds=or.bdo.data$did,
                            hessian=TRUE)
    or.bdo.1.se <- sqrt(diag(solve(or.bdo.1.mle$hessian)))
    or.bdo.1.e <- or.bdo.1.mle$estimate
    
    or.bdo.1.e
    or.bdo.1.se

########### MLEs for Collector visits
    
    or.collector.1.mle <- nlm(objfun.1st, rep(0,5), 
                            tstar=thetstar, 
                            Ts=rep(max(or.collector.data$q11_collector_month, na.rm=TRUE), NROW(or.collector.data$q11_collector_month)), 
                            months=or.collector.data$q11_collector_month,
                            years=or.collector.data$q11_collector_year, 
                            ds=or.collector.data$did,
                            hessian=TRUE)
    or.collector.1.se <- sqrt(diag(solve(or.collector.1.mle$hessian)))
    or.collector.1.e <- or.collector.1.mle$estimate
    
    or.collector.1.e
    or.collector.1.se
    
### 2nd-order polynomial in t ######################################################
    
########### BDO visits
    
    or.bdo.2.mle <- nlm(objfun.2nd, rep(0,6), 
                            tstar=thetstar, 
                            Ts=rep(max(or.bdo.data$q10_BDO_month, na.rm=TRUE), NROW(or.bdo.data$q10_BDO_month)), 
                            months=or.bdo.data$q10_BDO_month,
                            years=or.bdo.data$q10_BDO_year,
                            ds=or.bdo.data$did,
                            hessian=TRUE)
    or.bdo.2.se <- sqrt(diag(solve(or.bdo.2.mle$hessian)))
    or.bdo.2.e <- or.bdo.2.mle$estimate
    
    or.bdo.2.e
    or.bdo.2.se

########### MLEs for Collector visits
    
    or.collector.2.mle <- nlm(objfun.2nd, rep(0,6), 
                            tstar=thetstar, 
                            Ts=rep(max(or.collector.data$q11_collector_month, na.rm=TRUE), NROW(or.collector.data$q11_collector_month)), 
                            months=or.collector.data$q11_collector_month,
                            years=or.collector.data$q11_collector_year, 
                            ds=or.collector.data$did,
                            hessian=TRUE)
    or.collector.2.se <- sqrt(diag(solve(or.collector.2.mle$hessian)))
    or.collector.2.e <- or.collector.2.mle$estimate
    
    or.collector.2.e
    or.collector.2.se

### output ###############################################################
    
    # load sigstar fn
    source(paste0(workingdir, "/files/lib_multiregtable.r"))
    source(paste0(workingdir, "/files/lib_hacktex.r"))
    
    # helper to fill in se cells
    secell <- function(est, se){
        return(paste("{\\scriptsize (",round(se,3),")$^{", multiregtable_sigsymbol(est/se) ,"}$}", sep=""))
    }

    out <- as.data.frame(matrix(0, nrow=NROW(or.bdo.2.e)*2, ncol=5))
    varname <- c("Shock","Koraput","Gajapati","Rayagada","Day","Day$^2$")
    for (i in 1:NROW(or.bdo.2.se)){
    
        out[1 + 2*(i-1), 1] <- varname[i]
        out[2 + 2*(i-1), 1] <- c("")
        if (i < 6){
            out[1 + 2*(i-1), 2] <- round(or.bdo.1.e[i], 3)
            out[2 + 2*(i-1), 2] <- secell(or.bdo.1.e[i], or.bdo.1.se[i])
            out[1 + 2*(i-1), 4] <- round(or.collector.1.e[i], 3)
            out[2 + 2*(i-1), 4] <- secell(or.collector.1.e[i], or.collector.1.se[i])
        } else{
            out[1 + 2*(i-1), 2] <- ""
            out[2 + 2*(i-1), 2] <- ""
            out[1 + 2*(i-1), 4] <- ""
            out[2 + 2*(i-1), 4] <- ""
        }
        out[1 + 2*(i-1), 3] <- round(or.bdo.2.e[i], 3)
        out[2 + 2*(i-1), 3] <- secell(or.bdo.2.e[i], or.bdo.2.se[i])
        out[1 + 2*(i-1), 5] <- round(or.collector.2.e[i], 3)
        out[2 + 2*(i-1), 5] <- secell(or.collector.2.e[i], or.collector.2.se[i])
        
        
    }

    result <- hacktex(out,file=paste(tabdir, "audit_MLE_february2006.tex", sep="/"),
                        label="tab:audit_MLE",
                        table.env=FALSE,
                        caption.loc="top",
                        rowname=NULL,
                        center="none",
                        colheads=c("Regressor","BDO","BDO","Collector","Collector"),
                        collabel.just=c("l","c","c","c","c"))
    
