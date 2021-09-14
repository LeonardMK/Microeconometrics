 ###############################################################################
#
#   regressions_did.r
#   CLEANED UP VERSION OF [V4][3-29-2012]
#
###############################################################################

### setup #####################################################################

    # source
    library(epicalc)
    library(Design)
    
    # clear
    zap()
    rm(list=ls(all=TRUE))

    # directories
    #rootdir <- SET ROOT DIRECTORY HERE
    
    workingdir <- paste(rootdir, "workingdir", sep="/") # STORE DATA FILES AND HELPER FILES HERE
    tabledir <- paste(rootdir, "tables", sep="/") # OUTPUT HERE
    setwd(workingdir)
    
    # helper functions
    source("lib_mwc.r")
    source("lib_multiregtable.r")
    source("lib_hacktex.r")
    source("[5-5-2008][v2] distributor_functions.R")

    usedata <- read.csv("didanalysisdata.csv")
    
    # variable descriptions

# sid: State ID, 24 = Orissa, 99 = Andhra Pradesh
# did: District ID
# day: Day of year
# daysproff: Officially reported days worked on Piece Rate Projects
# daysdwoff: Officially reported days worked on Daily Wage Projects
# rateproff: Officially reported amounts paid on Piece Rate Projects
# ratedwoff: Officially reported amounts paid on Daily Wage Projects
# dayspr: Survey reports of days worked on Piece Rate Projects
# daysdw: Survey reports of days worked on Daily Wage Projects
# ratepr: Survey reports of amounts paid on Piece Rate Projects
# ratedw: Survey reports of amounts paid on Daily Wage Projects
# season1: Main agricultural season
# season2: Secondary agricultural season
# dwactive: Daily wage project is active according to official reports
# practive: Piece rate project is active according to official reports
# upid: Unique panchayat ID
# ubid: Unique block ID
# orshock: Post May 1
# or_orshock: Orissa x orshock
# apshock1: Post March 5
# ap_apshock1: AP x apshock1
# apshock2: Post Apr 25
# ap_apshock2: AP x apshock2
# day2: Day ^ 2
# day3: Day ^ 3
# shock_day: Shock x day
# shock_day2: Shock x day2
# shock_day3: Shock x day3
# dayofmonth: Day of month
# dayofmonth2: Day of month ^ 2
# dayofmonth3: Day of month ^ 3
# holiday: Official government holiday
# anypr: Panchayat had piece rate project active between March 1 - June 30
# anydw: Panchayat had daily wage project active between March 1 - June 30
    
    # label variables for output               
    varlabels <- pairlist("orshock"             = "OR Shock",
                          "or_orshock"          = "OR Shock * OR",
                          "apshock1"            = "AP Shock 1",
                          "ap_apshock1"         = "AP Shock 1 * AP",
                          "apshock2"            = "AP Shock 2",
                          "ap_apshock2"         = "AP Shock 2 * AP",
                          "holiday"             = "Holiday",
                          "season1"             = "Season 1",
                          "season2"             = "Season 2",
                          "ratepr"              = "Actual PR Payments")

### sample selection #############################################################################

    # conservative
    fulldwsample <- usedata$anydw==1
    fullprsample <- usedata$anypr==1

### main tables ##################################################################################

    # linear
    fm.pr.1 <- ols(rateproff ~ orshock + or_orshock + apshock1 + ap_apshock1 + apshock2 + ap_apshock2
                                + day + ratepr 
                                + holiday + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3
                                + as.factor(sid), 
                                data=usedata[fullprsample,], x=TRUE, y=TRUE)
    fm.pr.1$var <- mwc_2way(fm.pr.1, usedata$upid[fullprsample], usedata$day[fullprsample])
    
    # linear, controls
    fm.pr.2 <- ols(rateproff ~ orshock + or_orshock + apshock1 + ap_apshock1 + apshock2 + ap_apshock2
                                + day + ratepr
                                + holiday + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3
                                + as.factor(did), 
                                data=usedata[fullprsample,], x=TRUE, y=TRUE)
    fm.pr.2$var <- mwc_2way(fm.pr.2, usedata$upid[fullprsample], usedata$day[fullprsample])
    
    # RD, controls
    fm.pr.3 <- ols(rateproff ~ orshock + or_orshock + apshock1 + ap_apshock1 + apshock2 + ap_apshock2
                                + day + shock_day + ratepr
                                + holiday + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3
                                + as.factor(did), 
                                data=usedata[fullprsample,], x=TRUE, y=TRUE)
    fm.pr.3$var <- mwc_2way(fm.pr.3, usedata$upid[fullprsample], usedata$day[fullprsample])

    fm.pr.3$coefficients["orshock"] <- fm.pr.3$coefficients["orshock"] + fm.pr.3$coefficients["shock_day"]*121
    fm.pr.3$var["orshock","orshock"] <- fm.pr.3$var["orshock","orshock"] + 121*121*fm.pr.3$var["shock_day","shock_day"] + 2*121*fm.pr.3$var["orshock","shock_day"]

### output ##################################################################################

    vars.did <- c("or_orshock","ap_apshock1","ap_apshock2","orshock","apshock1","apshock2","ratepr")
    addrows.did <- rbind(c("Time Controls","Day","Day","Shock*Day"), c("FEs","State","District","District"))
    table.did <- multiregtable(vars.did, varlabels, list(fm.pr.1, fm.pr.2, fm.pr.3), 2, addrows.did)
    result <- hacktex(table.did, 
                    file=paste(tabledir, "did.tex", sep="/"),
                    label="tab:did",
                    table.env=FALSE,
                    caption.loc="top",
                    rowname =NULL,
                    center="none",
                    colheads=c("Regressor","I","II","III"),
                    collabel.just=c("l","c","c","c"))

### ROBUSTNESS ##################################################################################
### SQUARED ##################################################################################

    # linear
    fm.pr.1 <- ols(rateproff ~ orshock + or_orshock + apshock1 + ap_apshock1 + apshock2 + ap_apshock2
                                + day + day2 + ratepr 
                                + holiday + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3
                                + as.factor(sid), 
                                data=usedata[fullprsample,], x=TRUE, y=TRUE)
    fm.pr.1$var <- mwc_2way(fm.pr.1, usedata$upid[fullprsample], usedata$day[fullprsample])
    
    # linear, controls
    fm.pr.2 <- ols(rateproff ~ orshock + or_orshock + apshock1 + ap_apshock1 + apshock2 + ap_apshock2
                                + day + day2 + ratepr
                                + holiday + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3
                                + as.factor(did), 
                                data=usedata[fullprsample,], x=TRUE, y=TRUE)
    fm.pr.2$var <- mwc_2way(fm.pr.2, usedata$upid[fullprsample], usedata$day[fullprsample])
    
    # RD, controls
    fm.pr.3 <- ols(rateproff ~ orshock + or_orshock + apshock1 + ap_apshock1 + apshock2 + ap_apshock2
                                + day + day2 + shock_day + shock_day2 + ratepr
                                + holiday + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3
                                + as.factor(did), 
                                data=usedata[fullprsample,], x=TRUE, y=TRUE)
    fm.pr.3$var <- mwc_2way(fm.pr.3, usedata$upid[fullprsample], usedata$day[fullprsample])

    fm.pr.3$coefficients["orshock"] <- fm.pr.3$coefficients["orshock"] + fm.pr.3$coefficients["shock_day"]*121 + fm.pr.3$coefficients["shock_day2"]*14641
    fm.pr.3$var["orshock","orshock"] <- fm.pr.3$var["orshock","orshock"] + (121^2)*fm.pr.3$var["shock_day","shock_day"] + 2*121*fm.pr.3$var["orshock","shock_day"] + (14641^2)*fm.pr.3$var["shock_day2","shock_day2"] + 2*14641*fm.pr.3$var["orshock","shock_day2"] + 2*121*14641*fm.pr.3$var["shock_day","shock_day2"]


### output ##################################################################################

    vars.did <- c("or_orshock","ap_apshock1","ap_apshock2","orshock","apshock1","apshock2","ratepr")
    addrows.did <- rbind(c("Time Controls","Day2","Day2","Shock*Day2"), c("FEs","State","District","District"))
    table.did <- multiregtable(vars.did, varlabels, list(fm.pr.1, fm.pr.2, fm.pr.3), 2, addrows.did)
    result <- hacktex(table.did, 
                    file=paste(tabledir, "didsquared.tex", sep="/"),
                    label="tab:did",
                    table.env=FALSE,
                    caption.loc="top",
                    rowname =NULL,
                    center="none",
                    colheads=c("Regressor","I","II","III"),
                    collabel.just=c("l","c","c","c"))
