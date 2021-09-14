###############################################################################
#
#   distributor_functions[v2].r
#   Paul Niehaus/Sandip Sukhtankar
#   April/May 2008
#
#   Helper functions that take a workspell observation and return a distribution
#   of its days of work over months or days. These are stored here as a library
#   so that we can call on them to distribute spells in both the official and the
#   survey data.
#
###############################################################################

### helper functions ##########################################################

    # 2007 month lengths
    daysinmonth <- function(month){
        return(switch(month, 31,28,31,30,31,30,31,31,30,31,30,31))
    }

    # month/day to raw days conversion
    rawdays <- function(month, day){
        m <- 1
        while (m < month){
            day <- day + daysinmonth(m)
            m <- m + 1
        }
        return(day)
    }
    
    # raw days to month/day conversion
    monthday <- function(rawdays){
        m <- 1
        while ((newdays <- rawdays - daysinmonth(m)) > 0){
            rawdays <- newdays
            m <- m + 1
        }
        return(c(m, rawdays))
    }
    


### spells over months ########################################################

    bumpmonths <- function(startday, startmonth, endday, endmonth, daysworked){
    
        ret <- rep(0, 12)
        
        # if we don't know the start/end days, assume uniform distributions over all possibilities
        if (is.na(startday)){ startday <- 0 }
        if (is.na(endday)){ endday <- daysinmonth(endmonth) }
        
        # assign days to month   
        diff <- endmonth - startmonth
        if (diff == 0){
            ret[startmonth] <- daysworked
        } else {
            firstmonthshare <- (daysinmonth(startmonth) - startday) / (daysinmonth(startmonth) - startday + endday)
        
            ret[startmonth] <- (daysworked / (diff)) * firstmonthshare
            if (diff > 1){
                for (m in (startmonth+1):(endmonth-1)){
                    ret[m] <- daysworked / diff
                }
            }
            ret[endmonth] <- (daysworked / (diff)) * (1 - firstmonthshare)
        }
        
        # return
        return(ret)
    }
    
    # test suite
    #bumpmonths(NA,5,NA,5,10)
    #bumpmonths(NA,4,NA,5,10)    
    #bumpmonths(15,4,NA,5,10)    
    #bumpmonths(15,4,25,4,10)    
    #bumpmonths(15,4,25,5,10)
    
### spells over days ##########################################################

    bumpdays <- function(startday, startmonth, endday, endmonth, daysworked){
    
        ret <- rep(0, 365)
        
        # if we don't know the start/end days, assume uniform distributions over all possibilities
        if (is.na(startday)){ startday <- 1 }
        if (is.na(endday)){ endday <- daysinmonth(endmonth) }
        
        # assign days
        diff <- endmonth - startmonth

        if (diff == 0){
            if (daysworked <= (endday - startday + 1)) {
                ret[rawdays(startmonth, startday):rawdays(endmonth,endday)] <- daysworked / (endday - startday + 1)
                } else {
                spread <- (daysworked - (endday - startday + 1))/2 
                if (is.integer(spread)) {
                    ret[rawdays(startmonth, startday - spread):rawdays(endmonth, endday + spread)] <- 1
                    } else {
                        spreadbefore <- ((daysworked - (endday - startday + 1))/2) + 0.5
                        spreadafter <- ((daysworked - (endday - startday + 1))/2) - 0.5
                        ret[rawdays(startmonth, startday - spreadbefore):rawdays(endmonth, endday + spreadafter)] <- 1
                } 
            }     
        }
        if (diff == 1){
            if (daysworked <= (rawdays(endmonth, endday) - rawdays(startmonth, startday) + 1)) {              
                firstmonthshare <- (daysinmonth(startmonth) - startday) / (daysinmonth(startmonth) - startday + endday)
                ret[rawdays(startmonth, startday):rawdays(startmonth, daysinmonth(startmonth))] <- (daysworked / (diff * (daysinmonth(startmonth)-startday+1))) * firstmonthshare
                ret[rawdays(endmonth,1):rawdays(endmonth,endday)] <- (daysworked / (diff * endday )) * (1 - firstmonthshare)
                } else {
                    spread <- (daysworked - (rawdays(endmonth, endday) - rawdays(startmonth, startday) + 1))/2
                    if (is.integer(spread)) {
                        ret[rawdays(startmonth, startday - spread):rawdays(endmonth, endday + spread)] <- 1
                    } else {
                        spreadbefore <- ((daysworked - (rawdays(endmonth, endday) - rawdays(startmonth, startday) + 1))/2) + 0.5
                        spreadafter <- ((daysworked - (rawdays(endmonth, endday) - rawdays(startmonth, startday) + 1))/2) - 0.5
                        ret[rawdays(startmonth, startday - spreadbefore):rawdays(endmonth, endday + spreadafter)] <- 1                
                }
            } 
        }
        if (diff > 1){
            if (daysworked <= (rawdays(endmonth, endday) - rawdays(startmonth, startday) + 1)) {
                firstmonthshare <- (daysinmonth(startmonth) - startday) / (daysinmonth(startmonth) - startday + endday)
                ret[rawdays(startmonth, startday):rawdays(startmonth, daysinmonth(startmonth))] <- (daysworked / (diff * (daysinmonth(startmonth)-startday+1))) * firstmonthshare

                for (m in (startmonth+1):(endmonth-1)){
                    ret[rawdays(m,0):rawdays(m,daysinmonth(m))] <- daysworked / (diff * daysinmonth(m))
                    }
                ret[rawdays(endmonth,1):rawdays(endmonth,endday)] <- (daysworked / (diff * endday )) * (1 - firstmonthshare)
            } else {
                    spread <- (daysworked - (rawdays(endmonth, endday) - rawdays(startmonth, startday) + 1))/2
                    if (is.integer(spread)) {
                        ret[rawdays(startmonth, startday - spread):rawdays(endmonth, endday + spread)] <- 1
                    } else {
                        spreadbefore <- ((daysworked - (rawdays(endmonth, endday) - rawdays(startmonth, startday) + 1))/2) + 0.5
                        spreadafter <- ((daysworked - (rawdays(endmonth, endday) - rawdays(startmonth, startday) + 1))/2) - 0.5
                        ret[rawdays(startmonth, startday - spreadbefore):rawdays(endmonth, endday + spreadafter)] <- 1                
                }
            } 
        }        
        # return
        return(ret[1:365])
    }
    
    # test suite
#    bumpdays(NA,5,NA,5,10)
#    bumpdays(NA,4,NA,5,10)    
#    bumpdays(15,4,NA,5,10)    
#    bumpdays(15,4,24,4,10)    
#    bumpdays(15,4,25,5,10)
#    bumpdays(15,4,25,4,20)

    # NB still some bugs to work out: see - don't understand this Paul - looks fine to me...
    sum(bumpdays(15,4,NA,5,10))

# This spreads the wage over days worked before and after wage change

    bumpwage <- function(dailywork,totalpaid,sid){
        
        retwage <- rep(0,365)
        retunderpay <- rep(0,365)
        
        retw1 <- rep(0,rawdays(4,30))
        retw2 <- rep(0,365-rawdays(4,30))
        retu1 <- rep(0,rawdays(4,30))
        retu2 <- rep(0,365-rawdays(4,30))
        
        totaldays <- sum(dailywork)
        dailyamount <- totalpaid/totaldays
        
        daysbefore <- sum(dailywork[1:rawdays(4,30)])
        daysafter <-  totaldays - daysbefore
                
        propbefore <- daysbefore/totaldays
        
        if(is.integer(propbefore)){
            totalamountbefore <- totalpaid * propbefore
        } else {
            totalamountbefore <- totalpaid * daysbefore * 55 / (daysbefore * 55 + daysafter * 70)
        }
        
        totalamountafter <- totalpaid - totalamountbefore
        
        if(propbefore==0){
            dailyamountbefore <- 0
        } else {
            dailyamountbefore <- totalamountbefore/daysbefore
        }
        
        if(propbefore==1){
            dailyamountafter <- 0
        } else {
            dailyamountafter <- totalamountafter/daysafter
        }
               
        if(sid==24){
            
            retw1 <- dailywork[1:rawdays(4,30)] * dailyamountbefore
            retw2 <- dailywork[rawdays(5,1):365] * dailyamountafter
            retwage <- c(retw1,retw2)
            
            retu1 <- (55 * dailywork[1:rawdays(4,30)]) - retw1
            retu2 <- (70 * dailywork[rawdays(5,1):365]) - retw2
            retunderpay <- c(retu1,retu2)
        } else {
            retwage <- dailywork * dailyamount
            retunderpay <- (80 * dailywork) - retwage
        }            

        return(retwage)
    }

# This spreads the piece rate over days worked before and after wage change

    bumppr <- function(dailywork,totalpaid,sid){
        
        retwage <- rep(0,365)
        
        retw1 <- rep(0,rawdays(4,24))
        retw2 <- rep(0,365-rawdays(4,24))
        
        totaldays <- sum(dailywork)
        dailyamount <- totalpaid/totaldays
        
        daysbefore <- sum(dailywork[1:rawdays(4,24)])
        daysafter <-  totaldays - daysbefore
                
        propbefore <- daysbefore/totaldays
        
        if(is.integer(propbefore)){
            totalamountbefore <- totalpaid * propbefore
        } else {
            totalamountbefore <- totalpaid * daysbefore * 100 / (daysbefore * 100 + daysafter * 120)
        }
        
        totalamountafter <- totalpaid - totalamountbefore
        
        if(propbefore==0){
            dailyamountbefore <- 0
        } else {
            dailyamountbefore <- totalamountbefore/daysbefore
        }
        
        if(propbefore==1){
            dailyamountafter <- 0
        } else {
            dailyamountafter <- totalamountafter/daysafter
        }
               
        if(sid==99){
            
            retw1 <- dailywork[1:rawdays(4,24)] * dailyamountbefore
            retw2 <- dailywork[rawdays(4,25):365] * dailyamountafter
            retwage <- c(retw1,retw2)
            
        } else {
            retwage <- dailywork * dailyamount
        }            

        return(retwage)
    }
