###############################################################################
#
#   regressions.r
#   CLEANED UP VERSION OF [V22][8-9-2012]
#   AND regressions_rdplots [V20][12-16-2011]
###############################################################################

### setup #####################################################################

    # source
    library(epicalc)
    library(Design)
# Use rms instead of Design for recent versions of R
#    library(rms)
    library(quantreg)
    library(car)
    library(AER)
    
    # clear
    rm(list=ls(all=TRUE))

    # directories

    #rootdir <- SET ROOT DIRECTORY HERE
    
    workingdir <- paste(rootdir, "workingdir", sep="/") # STORE DATA FILES AND HELPER FILES HERE
    tabledir <- paste(rootdir, "tables", sep="/") # OUTPUT TABLES HERE
    figdir <- paste(rootdir, "figures", sep="/") # OUTPUT FIGURES HERE
    
    setwd(workingdir)
    
    # helper functions
    # this does multi-way clustering a la Cameron-Gehlbach-Miller
    source("lib_mwc.r")
    
    # these two help with producing .tex tables
    source("lib_multiregtable.r")
    source("lib_hacktex.r")
    
    # this distributes amounts earned and days worked evenly between a given period when the exact days worked are not known
    source("[5-5-2008][v2] distributor_functions.R")

    # read in data
    usedata <- read.csv(paste(workingdir, "/data/finalanalysisdata.csv", sep="/"))

    # Variable explanations
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
# dwactive: Daily wage project is active according to official reports
# practive: Piece rate project is active according to official reports
# season1: Main agricultural season
# season2: Secondary agricultural season
# res_tribe: Panchayat chief (sarpanch) seat reserved for Scheduled Tribe
# res_gender: Panchayat chief (sarpanch) seat reserved for women
# res_sbc: Panchayat chief (sarpanch) seat reserved for Scheduled Caste or Other Backward Class (OBC)
# holiday: Official government holiday
# upid: Unique panchayat ID
# ubid: Unique block ID
# fdwfrac: Proportion of daily wage project-days in the panchayat in the next two months 
# bdwfrac: Proportion of daily wage project-days in the panchayat in the previous two months 
# fdwfrac1: Proportion of daily wage project-days in the panchayat in the next one month 
# bdwfrac1: Proportion of daily wage project-days in the panchayat in the previous one month 
# fdwfrac3: Proportion of daily wage project-days in the panchayat in the next three months 
# bdwfrac3: Proportion of daily wage project-days in the panchayat in the previous three months 
# shock: Post May 1
# shock_fdwfrac: Shock x fdwfrac
# shock_bdwfrac: Shock x bdwfrac
# shock_res_gender: Shock x res_gender
# shock_res_sbc: Shock x res_sbc
# shock_res_tribe: Shock x res_tribe
# month2: April
# month3: May
# month4: June
# day2: Day ^ 2
# day3: Day ^ 3
# dayofmonth: Day of month
# dayofmonth2: Day of month ^ 2
# dayofmonth3: Day of month ^ 3
# anydw: Panchayat had daily wage project active between March 1 - June 30
# anypr: Panchayat had piece rate project active between March 1 - June 30
# fdw_all: Proportion of daily wage project-days in the panchayat in the next two months = 1
# fdw_none: Proportion of daily wage project-days in the panchayat in the next two months = 0
# fdw_some: Proportion of daily wage project-days in the panchayat in the next two months between (0,1)
# shock_fdw_all: Shock x fdw_all
# shock_fdw_none: Shock x fdw_none
# shock_fdw_some: Shock x fdw_some
# bdw_all: Proportion of daily wage project-days in the panchayat in the previous two months = 1
# bdw_none: Proportion of daily wage project-days in the panchayat in the previous two months = 0
# bdw_some: Proportion of daily wage project-days in the panchayat in the previous two months between (0,1)
# shock_bdw_all: Shock x bdw_all
# shock_bdw_none: Shock x bdw_none
# shock_bdw_some: Shock x bdw_some
# shock_day: Shock x day
# shock_day2: Shock x day2
# shock_day3: Shock x day3
# month2fdw_all: Month2 x fdw_all
# month3fdw_all: Month3 x fdw_all
# month4fdw_all: Month4 x fdw_all
# month2fdw_some: Month2 x fdw_some
# month3fdw_some: Month3 x fdw_some
# month4fdw_some: Month4 x fdw_some
# shock_alwaysdw: Shock x alwasydw
# alwaysdw: Panchayat has daily wage project active throughout March 1 - June 30
# shock_alwayspr: Shock x alwasypr
# alwayspr: Panchayat has piece rate project active throughout March 1 - June 30
# ndaysdwoff: Official reported days worked for people who did not work at all
# nrateproff: Official reported amounts paid for people who did not work at all

    # label variables for output              
    varlabels <- pairlist("shock"               = "Shock",
                          "shock_fdwfrac"       = "Shock * FwdWageFrac",
                          "shock_bdwfrac"       = "Shock * BkWageFrac",
                          "fdwfrac"             = "FwdWageFrac",
                          "bdwfrac"             = "BkWageFrac",
                          "month2"              = "April",
                          "month3"              = "May",
                          "month4"              = "June",
                          "day"                 = "Day",
                          "day2"                = "Day$^2$",
                          "day3"                = "Day$^3$",
                          "holiday"             = "Holiday",
                          "season1"             = "Season 1",
                          "season2"             = "Season 2",
                          "res_gender"          = "Reserved (W)",
                          "res_sbc"             = "Reserved (SBC)",
                          "res_tribe"           = "Reserved (ST)",
                          "shock_res_gender"    = "Shock * Reserved (W)",
                          "shock_res_sbc"       = "Shock * Reserved (SBC)",
                          "shock_res_tribe"     = "Shock * Reserved (ST)",
                          "daysdw"              = "Actual DW Days",
                          "ratepr"              = "Actual PR Payments",
                          "shock_alwaysdw"      = "Shock * AlwaysDW",
                          "shock_fdw_all"       = "Shock * FdwAll",
                          "shock_fdw_some"      = "Shock * FdwSome",
                          "shock_bdw_all"       = "Shock * BdwAll",
                          "shock_bdw_some"      = "Shock * BdwSome",
                          "shock_alwayspr"      = "Shock * AlwaysPR",
                          "alwaysdw"            = "AlwaysDW",
                          "alwayspr"            = "AlwaysPR",
                          "month2fdw_all"       = "April * FdwAll",
                          "month3fdw_all"       = "May * FdwAll",
                          "month4fdw_all"       = "June * FdwAll",
                          "shock_day"           = "Shock * Day")

                          
#########################################################################################
# START
#########################################################################################



### Table 1: Summary stats of Main Regression Variables ############################################################

    out.means<-c(mean(usedata$daysdwoff[usedata$anydw==1]), mean(usedata$daysdw[usedata$anydw==1]), mean(usedata$rateproff[usedata$anypr==1]), mean(usedata$ratepr[usedata$anypr==1]), mean(usedata$fdwfrac, na.rm=T))
    out.sds<-c(sd(usedata$daysdwoff[usedata$anydw==1]), sd(usedata$daysdw[usedata$anydw==1]), sd(usedata$rateproff[usedata$anypr==1]), sd(usedata$ratepr[usedata$anypr==1]), sd(usedata$fdwfrac, na.rm=T))
    out.obs <-c(length(usedata$daysdwoff[usedata$anydw==1]), length(usedata$daysdw[usedata$anydw==1]), length(usedata$rateproff[usedata$anypr==1]), length(usedata$ratepr[usedata$anypr==1]), length(usedata$fdwfrac))
    
    out.stats <- cbind(out.obs, roundsig(out.means, 2), roundsig(out.sds,2))
    colnames(out.stats) <- c("$N$", "Mean", "SD")
    rownames(out.stats) <- c("Official DW Days",
                             "Actual DW Days", 
                             "Official PR Payments", 
                             "Actual PR Payments", 
                             "FwdWageFrac")
    
    result <- latex(out.stats,
                        file=paste(tabledir, "outcome_stats.tex", sep="/"),
                        table.env=FALSE,
                        col.just=c("r","r","r"),
                        collabel.just=c("c","c","c"),
                        rowlabel="")
    
    average.pr <-mean(usedata$rateproff[usedata$anypr==1 & usedata$shock==0])- mean(usedata$ratepr[usedata$anypr==1 & usedata$shock==0])


############# Table 2: Wage shock effects on Project Composition

    # dw fraction forward
    fm.fdwfrac.1 <- ols(fdwfrac ~ shock + day
                                + holiday + res_gender + res_sbc + res_tribe + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3, 
                                data=usedata,x=TRUE, y=TRUE)
    fm.fdwfrac.1$var <- mwc_2way(fm.fdwfrac.1, usedata$upid, usedata$day)  
    
    fm.fdwfrac.2 <- ols(fdwfrac ~ shock + day + as.factor(did)
                                + holiday + res_gender + res_sbc + res_tribe + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3, 
                                data=usedata,x=TRUE, y=TRUE)
    fm.fdwfrac.2$var <- mwc_2way(fm.fdwfrac.2, usedata$upid, usedata$day)  
    
    fm.fdwfrac.3 <- ols(fdwfrac ~ shock + day + day2 + as.factor(did)
                                + holiday + res_gender + res_sbc + res_tribe + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3, 
                                data=usedata,x=TRUE, y=TRUE)
    fm.fdwfrac.3$var <- mwc_2way(fm.fdwfrac.3, usedata$upid, usedata$day)  
    
    # normalized impacts
    coefficients(fm.fdwfrac.1)["shock"] / sd(usedata$fdwfrac, na.rm=T)

    # output
    vars.end <- c("shock","day","day2")
    addrows.end <- rbind(c("District FEs","N","Y","Y"))
    table.end <- multiregtable(vars.end, varlabels, list(fm.fdwfrac.1, fm.fdwfrac.2, fm.fdwfrac.3), 3, addrows.end)
    result <- hacktex(table.end, 
                    file=paste(tabledir, "fdwfrac.tex", sep="/"),
                    label="tab:fdwfrac",
                    table.env=FALSE,
                    caption.loc="top",
                    rowname =NULL,
                    center="none",
                    colheads=c("Regressor","I","II","III"),
                    collabel.just=c("l","c","c","c"))


### Table 3a: Wage Shock Effects on Daily Wage Overreporting ######################################################################################
    
    # Column 1: shock
    fm.dw.1 <- ols(daysdwoff ~ daysdw + shock
                                + day
                                + holiday + res_gender + res_sbc + res_tribe + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anydw==1,], x=TRUE, y=TRUE)
    fm.dw.1$var <- mwc_2way(fm.dw.1, usedata$upid[usedata$anydw==1], usedata$day[usedata$anydw==1])

    # Column 2: shock + did
    fm.dw.2 <- ols(daysdwoff ~ daysdw + shock
                                + day + as.factor(did)
                                + holiday + res_gender + res_sbc + res_tribe + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anydw==1,], x=TRUE, y=TRUE)
    fm.dw.2$var <- mwc_2way(fm.dw.2, usedata$upid[usedata$anydw==1], usedata$day[usedata$anydw==1])

    # Column 3: shock, RD controls
    fm.dw.3 <- ols(daysdwoff ~ daysdw + shock 
                                + day + shock_day + as.factor(did)
                                + holiday + res_gender + res_sbc + res_tribe + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anydw==1,], x=TRUE, y=TRUE)
    fm.dw.3$var <- mwc_2way(fm.dw.3, usedata$upid[usedata$anydw==1], usedata$day[usedata$anydw==1])

    # Calculating intercept difference at the time the shock occurs for all RD-type regs
    fm.dw.3$coefficients["shock"] <- fm.dw.3$coefficients["shock"] + fm.dw.3$coefficients["shock_day"]*121
    fm.dw.3$var["shock","shock"] <- fm.dw.3$var["shock","shock"] + 121*121*fm.dw.3$var["shock_day","shock_day"] + 2*121*fm.dw.3$var["shock","shock_day"]

    # Column 4: DiD with alwaysdw
    fm.dw.4 <- ols(daysdwoff  ~ daysdw + shock + shock_alwaysdw + alwaysdw
                                + day
                                + holiday + res_gender + res_sbc + res_tribe + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anydw==1,], x=TRUE, y=TRUE)
    fm.dw.4$var <- mwc_2way(fm.dw.4, usedata$upid[usedata$anydw==1], usedata$day[usedata$anydw==1])

    # Column 5: DiD with alwaysdw + did
    fm.dw.5 <- ols(daysdwoff  ~ daysdw + shock + shock_alwaysdw + alwaysdw
                                + day + as.factor(did)
                                + holiday + res_gender + res_sbc + res_tribe + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anydw==1,], x=TRUE, y=TRUE)
    fm.dw.5$var <- mwc_2way(fm.dw.5, usedata$upid[usedata$anydw==1], usedata$day[usedata$anydw==1])


    # Column 6: DiD with alwaysdw, RD 
    fm.dw.6 <- ols(daysdwoff  ~ daysdw + shock + shock_alwaysdw + alwaysdw
                                + day + shock_day + as.factor(did)
                                + holiday + res_gender + res_sbc + res_tribe + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anydw==1,],
                                x=TRUE, y=TRUE)
    fm.dw.6$var <- mwc_2way(fm.dw.6, usedata$upid[usedata$anydw==1], usedata$day[usedata$anydw==1])

    fm.dw.6$coefficients["shock"] <- fm.dw.6$coefficients["shock"] + fm.dw.6$coefficients["shock_day"]*121
    fm.dw.6$var["shock","shock"] <- fm.dw.6$var["shock","shock"] + 121*121*fm.dw.6$var["shock_day","shock_day"] + 2*121*fm.dw.6$var["shock","shock_day"]

    # output
    vars <- c("shock","shock_alwaysdw","alwaysdw")
    addrows <- rbind(c("Time Controls","Day","Day","Shock*Day","Day","Day","Shock*Day"),
                     c("District FEs","N","Y","Y","N","Y","Y"))
    table.dw <- multiregtable(vars, varlabels, list(fm.dw.1,fm.dw.2,fm.dw.3,fm.dw.4,fm.dw.5,fm.dw.6), 2, addrows)
    result <- hacktex(table.dw, 
                    file=paste(tabledir, "table3a.tex", sep="/"),
                    label="tab:wageregs",
                    table.env=FALSE,
                    caption.loc="top",
                    rowname =NULL,
                    center="none",
                    colheads=c("Regressor","I","II","III","IV","V","VI"),
                    collabel.just=c("l","c","c","c","c","c","c"))

### Table 3b: Dynamic Wage Shock Effects on Daily Wage Overreporting ######################################################################################

    # Column 1: interaction with fdw_all 
    fm.dw.1 <- ols(daysdwoff  ~ daysdw + shock + shock_fdw_all + shock_fdw_some + fdw_all + fdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day
                                + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anydw==1,],
                                x=TRUE, y=TRUE)
    fm.dw.1$var <- mwc_2way(fm.dw.1, usedata$upid[usedata$anydw==1], usedata$day[usedata$anydw==1])


    # Column 2: interaction with fdw_all 
    fm.dw.2 <- ols(daysdwoff  ~ daysdw + shock + shock_fdw_all + shock_fdw_some + fdw_all + fdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + as.factor(did)
                                + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anydw==1,],
                                x=TRUE, y=TRUE)
    fm.dw.2$var <- mwc_2way(fm.dw.2, usedata$upid[usedata$anydw==1], usedata$day[usedata$anydw==1])

    # Column 3: interaction with fdw_all, RD 
    fm.dw.3 <- ols(daysdwoff ~ daysdw + shock + shock_fdw_all + shock_fdw_some + fdw_all + fdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + shock_day + as.factor(did)
                                + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anydw==1,],
                                x=TRUE, y=TRUE)
    fm.dw.3$var <- mwc_2way(fm.dw.3, usedata$upid[usedata$anydw==1], usedata$day[usedata$anydw==1])

    fm.dw.3$coefficients["shock"] <- fm.dw.3$coefficients["shock"] + fm.dw.3$coefficients["shock_day"]*121
    fm.dw.3$var["shock","shock"] <- fm.dw.3$var["shock","shock"] + 121*121*fm.dw.3$var["shock_day","shock_day"] + 2*121*fm.dw.3$var["shock","shock_day"]

    # Column 4: interaction with fdw_frac bins + bdw_frac bins
    fm.dw.4 <- ols(daysdwoff  ~ daysdw + shock + shock_fdw_all + shock_fdw_some + shock_bdw_all + shock_bdw_some + fdw_all + fdw_some + bdw_all + bdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day
                                + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anydw==1,],
                                x=TRUE, y=TRUE)
    fm.dw.4$var <- mwc_2way(fm.dw.4, usedata$upid[usedata$anydw==1], usedata$day[usedata$anydw==1])

    # Column 5: interaction with fdw_frac bins + bdw_frac bins + did
    fm.dw.5 <- ols(daysdwoff  ~ daysdw + shock + shock_fdw_all + shock_fdw_some + shock_bdw_all + shock_bdw_some + fdw_all + fdw_some + bdw_all + bdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + as.factor(did)
                                + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anydw==1,],
                                x=TRUE, y=TRUE)
    fm.dw.5$var <- mwc_2way(fm.dw.5, usedata$upid[usedata$anydw==1], usedata$day[usedata$anydw==1])


    # Column 6: interaction with fdw_frac bins + bdw_frac bins + RD specs, full
    fm.dw.6 <- ols(daysdwoff  ~ daysdw + shock + shock_fdw_all + shock_fdw_some + shock_bdw_all + shock_bdw_some + fdw_all + fdw_some + bdw_all + bdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + shock_day + as.factor(did)
                                + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anydw==1,],
                                x=TRUE, y=TRUE)
    fm.dw.6$var <- mwc_2way(fm.dw.6, usedata$upid[usedata$anydw==1], usedata$day[usedata$anydw==1])

    fm.dw.6$coefficients["shock"] <- fm.dw.6$coefficients["shock"] + fm.dw.6$coefficients["shock_day"]*121
    fm.dw.6$var["shock","shock"] <- fm.dw.6$var["shock","shock"] + 121*121*fm.dw.6$var["shock_day","shock_day"] + 2*121*fm.dw.6$var["shock","shock_day"]

    # output    
    vars <- c("shock","shock_fdw_all","shock_fdw_some","shock_bdw_all","shock_bdw_some")
    addrows <- rbind(c("Time Controls","Day","Day","Shock*Day","Day","Day","Shock*Day"),
                     c("District FEs","N","Y","Y","N","Y","Y"))
    table.dw <- multiregtable(vars, varlabels, list(fm.dw.1,fm.dw.2,fm.dw.3,fm.dw.4,fm.dw.5,fm.dw.6), 2, addrows)
    result <- hacktex(table.dw, 
                    file=paste(tabledir, "table3b.tex", sep="/"),
                    label="tab:wageregs",
                    table.env=FALSE,
                    caption.loc="top",
                    rowname =NULL,
                    center="none",
                    colheads=c("Regressor","I","II","III","IV","V","VI"),
                    collabel.just=c("l","c","c","c","c","c","c"))


    # THIS IS FOR ESTIMATING GG BOTTOM LINE EFFECTS & standard errors
    dw.coef <- fm.dw.2$coefficients["shock"]
    dw.coef.fdwall <- fm.dw.2$coefficients["shock_fdw_all"]
    dw.coef.fdwsome <-  fm.dw.2$coefficients["shock_fdw_some"]

    dw.var <- fm.dw.2$var["shock","shock"]
    dw.fdwall.var <- fm.dw.2$var["shock_fdw_all","shock_fdw_all"]
    dw.fdwsome.var <-  fm.dw.2$var["shock_fdw_some","shock_fdw_some"]
    dw.shock.fdwall <- fm.dw.2$var["shock","shock_fdw_all"]
    dw.shock.fdwsome <- fm.dw.2$var["shock","shock_fdw_some"]
    dw.fdwall.fdwsome <- fm.dw.2$var["shock_fdw_some","shock_fdw_all"]


### Table 4a: Wage Shock Effects on Piece Rate Theft ######################################################################################
    
    # Column 1: shock
    fm.pr.1 <- ols(rateproff ~ ratepr + shock
                                + day
                                + holiday + res_gender + res_sbc + res_tribe + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anypr==1,], x=TRUE, y=TRUE)
    fm.pr.1$var <- mwc_2way(fm.pr.1, usedata$upid[usedata$anypr==1], usedata$day[usedata$anypr==1])

    # Column 2: shock + did
    fm.pr.2 <- ols(rateproff ~ ratepr + shock
                                + day + as.factor(did)
                                + holiday + res_gender + res_sbc + res_tribe + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anypr==1,], x=TRUE, y=TRUE)
    fm.pr.2$var <- mwc_2way(fm.pr.2, usedata$upid[usedata$anypr==1], usedata$day[usedata$anypr==1])

    # Column 3: shock, RD controls
    fm.pr.3 <- ols(rateproff ~ ratepr + shock 
                                + day + shock_day + as.factor(did)
                                + holiday + res_gender + res_sbc + res_tribe + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anypr==1,], x=TRUE, y=TRUE)
    fm.pr.3$var <- mwc_2way(fm.pr.3, usedata$upid[usedata$anypr==1], usedata$day[usedata$anypr==1])

    fm.pr.3$coefficients["shock"] <- fm.pr.3$coefficients["shock"] + fm.pr.3$coefficients["shock_day"]*121
    fm.pr.3$var["shock","shock"] <- fm.pr.3$var["shock","shock"] + 121*121*fm.pr.3$var["shock_day","shock_day"] + 2*121*fm.pr.3$var["shock","shock_day"]

    # Column 4: DiD with alwayspr
    fm.pr.4 <- ols(rateproff  ~ ratepr + shock + shock_alwayspr + alwayspr
                                + day
                                + holiday + res_gender + res_sbc + res_tribe + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anypr==1,], x=TRUE, y=TRUE)
    fm.pr.4$var <- mwc_2way(fm.pr.4, usedata$upid[usedata$anypr==1], usedata$day[usedata$anypr==1])

    linear.hypothesis(fm.pr.4, "shock + shock_alwayspr = 0", vcov=fm.pr.4$var)

    # Column 5: DiD with alwayspr + did
    fm.pr.5 <- ols(rateproff  ~ ratepr + shock + shock_alwayspr + alwayspr
                                + day + as.factor(did)
                                + holiday + res_gender + res_sbc + res_tribe + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anypr==1,], x=TRUE, y=TRUE)
    fm.pr.5$var <- mwc_2way(fm.pr.5, usedata$upid[usedata$anypr==1], usedata$day[usedata$anypr==1])

    linear.hypothesis(fm.pr.5, "shock + shock_alwayspr = 0", vcov=fm.pr.5$var)

    # Column 6: DiD with alwayspr, RD 
    fm.pr.6 <- ols(rateproff  ~ ratepr + shock + shock_alwayspr + alwayspr
                                + day + shock_day + as.factor(did)
                                + holiday + res_gender + res_sbc + res_tribe + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anypr==1,],
                                x=TRUE, y=TRUE)
    fm.pr.6$var <- mwc_2way(fm.pr.6, usedata$upid[usedata$anypr==1], usedata$day[usedata$anypr==1])

    fm.pr.6$coefficients["shock"] <- fm.pr.6$coefficients["shock"] + fm.pr.6$coefficients["shock_day"]*121
    fm.pr.6$var["shock","shock"] <- fm.pr.6$var["shock","shock"] + 121*121*fm.pr.6$var["shock_day","shock_day"] + 2*121*fm.pr.6$var["shock","shock_day"]

    # Output
    vars <- c("shock","shock_alwayspr","alwayspr")
    addrows <- rbind(c("Time Controls","Day","Day","Shock*Day","Day","Day","Shock*Day"),
                     c("District FEs","N","Y","Y","N","Y","Y"))
    table.pr <- multiregtable(vars, varlabels, list(fm.pr.1,fm.pr.2,fm.pr.3,fm.pr.4,fm.pr.5,fm.pr.6), 2, addrows)
    result <- hacktex(table.pr, 
                    file=paste(tabledir, "table4a.tex", sep="/"),
                    label="tab:wageregs",
                    table.env=FALSE,
                    caption.loc="top",
                    rowname =NULL,
                    center="none",
                    colheads=c("Regressor","I","II","III","IV","V","VI"),
                    collabel.just=c("l","c","c","c","c","c","c"))
    
    # THIS IS FOR ESTIMATING GG BOTTOM-LINE EFFECTS
    pr.coef <- fm.pr.2$coefficients["shock"]
    pr.var <- fm.pr.2$var["shock","shock"]

### Table 4b: Dynamic Wage Shock Effects on Piece Rate Theft ######################################################################################

    # Column 1: interaction with fdw_all 
    fm.pr.1 <- ols(rateproff  ~ ratepr + shock + shock_fdw_all + shock_fdw_some + fdw_all + fdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day
                                + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anypr==1,],
                                x=TRUE, y=TRUE)
    fm.pr.1$var <- mwc_2way(fm.pr.1, usedata$upid[usedata$anypr==1], usedata$day[usedata$anypr==1])

    # Column 2: interaction with fdw_all 
    fm.pr.2 <- ols(rateproff  ~ ratepr + shock + shock_fdw_all + shock_fdw_some + fdw_all + fdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + as.factor(did)
                                + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anypr==1,],
                                x=TRUE, y=TRUE)
    fm.pr.2$var <- mwc_2way(fm.pr.2, usedata$upid[usedata$anypr==1], usedata$day[usedata$anypr==1])

    # Column 3: interaction with fdw_all, RD 
    fm.pr.3 <- ols(rateproff ~ ratepr + shock + shock_fdw_all + shock_fdw_some + fdw_all + fdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + shock_day + as.factor(did)
                                + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anypr==1,],
                                x=TRUE, y=TRUE)
    fm.pr.3$var <- mwc_2way(fm.pr.3, usedata$upid[usedata$anypr==1], usedata$day[usedata$anypr==1])

    fm.pr.3$coefficients["shock"] <- fm.pr.3$coefficients["shock"] + fm.pr.3$coefficients["shock_day"]*121
    fm.pr.3$var["shock","shock"] <- fm.pr.3$var["shock","shock"] + 121*121*fm.pr.3$var["shock_day","shock_day"] + 2*121*fm.pr.3$var["shock","shock_day"]

    # Column 4: interaction with fdw_frac bins + bdw_frac bins
    fm.pr.4 <- ols(rateproff  ~ ratepr + shock + shock_fdw_all + shock_fdw_some + shock_bdw_all + shock_bdw_some + fdw_all + fdw_some + bdw_all + bdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day
                                + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anypr==1,],
                                x=TRUE, y=TRUE)
    fm.pr.4$var <- mwc_2way(fm.pr.4, usedata$upid[usedata$anypr==1], usedata$day[usedata$anypr==1])

    # Column 5: interaction with fdw_frac bins + bdw_frac bins + did
    fm.pr.5 <- ols(rateproff  ~ ratepr + shock + shock_fdw_all + shock_fdw_some + shock_bdw_all + shock_bdw_some + fdw_all + fdw_some + bdw_all + bdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + as.factor(did)
                                + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anypr==1,],
                                x=TRUE, y=TRUE)
    fm.pr.5$var <- mwc_2way(fm.pr.5, usedata$upid[usedata$anypr==1], usedata$day[usedata$anypr==1])


    # Column 6: interaction with fdw_frac bins + bdw_frac bins + RD specs, full
    fm.pr.6 <- ols(rateproff  ~ ratepr + shock + shock_fdw_all + shock_fdw_some + shock_bdw_all + shock_bdw_some + fdw_all + fdw_some + bdw_all + bdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + shock_day + as.factor(did)
                                + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anypr==1,],
                                x=TRUE, y=TRUE)
    fm.pr.6$var <- mwc_2way(fm.pr.6, usedata$upid[usedata$anypr==1], usedata$day[usedata$anypr==1])

    fm.pr.6$coefficients["shock"] <- fm.pr.6$coefficients["shock"] + fm.pr.6$coefficients["shock_day"]*121
    fm.pr.6$var["shock","shock"] <- fm.pr.6$var["shock","shock"] + 121*121*fm.pr.6$var["shock_day","shock_day"] + 2*121*fm.pr.6$var["shock","shock_day"]

    # output    
    vars <- c("shock","shock_fdw_all","shock_fdw_some","shock_bdw_all","shock_bdw_some")
    addrows <- rbind(c("Time Controls","Day","Day","Shock*Day","Day","Day","Shock*Day"),
                     c("District FEs","N","Y","Y","N","Y","Y"))
    table.pr <- multiregtable(vars, varlabels, list(fm.pr.1,fm.pr.2,fm.pr.3,fm.pr.4,fm.pr.5,fm.pr.6), 2, addrows)
    result <- hacktex(table.pr, 
                    file=paste(tabledir, "table4b.tex", sep="/"),
                    label="tab:wageregs",
                    table.env=FALSE,
                    caption.loc="top",
                    rowname =NULL,
                    center="none",
                    colheads=c("Regressor","I","II","III","IV","V","VI"),
                    collabel.just=c("l","c","c","c","c","c","c"))

### Table 5: See regressions_did ##################################################################################################


### Table 6: Robustness ##################################################################################################

    # actual quantities on the LHS
    fm.dw.6.LHS <- ols(daysdwoff - daysdw ~ shock + shock_fdw_all + shock_fdw_some + fdw_all + fdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anydw==1,],x=TRUE, y=TRUE)
    fm.dw.6.LHS$var <- mwc_2way(fm.dw.6.LHS, usedata$upid[usedata$anydw==1], usedata$day[usedata$anydw==1])    
    
    fm.pr.6.LHS <- ols(rateproff - ratepr ~ shock + shock_fdw_all + shock_fdw_some + fdw_all + fdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anypr==1,],x=TRUE, y=TRUE)
    fm.pr.6.LHS$var <- mwc_2way(fm.pr.6.LHS, usedata$upid[usedata$anypr==1], usedata$day[usedata$anypr==1]) 

    # adding time interaction 
    fm.dw.6.time <- ols(daysdwoff ~ daysdw + month2 + month3 + month4 
                                + month2fdw_all + month3fdw_all + month4fdw_all + fdw_all
                                + month2fdw_some + month3fdw_some + month4fdw_some + fdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + season1 + season2, 
                                data=usedata[usedata$anydw==1,],x=TRUE, y=TRUE)
    fm.dw.6.time$var <- mwc_2way(fm.dw.6.time, usedata$upid[usedata$anydw==1], usedata$day[usedata$anydw==1])    
    
    fm.pr.6.time <- ols(rateproff ~ ratepr + month2 + month3 + month4 
                                + month2fdw_all + month3fdw_all + month4fdw_all + fdw_all
                                + month2fdw_some + month3fdw_some + month4fdw_some + fdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + season1 + season2, 
                                data=usedata[usedata$anypr==1,],x=TRUE, y=TRUE)
    fm.pr.6.time$var <- mwc_2way(fm.pr.6.time, usedata$upid[usedata$anypr==1], usedata$day[usedata$anypr==1])    

    # 1 month windows
    usedata$fdw_all <- as.numeric(usedata$fdwfrac1==1)
    usedata$shock_fdw_all <- usedata$fdw_all * usedata$shock
    usedata$fdw_some <- as.numeric(usedata$fdwfrac1 > 0 & usedata$fdwfrac1 < 1)
    usedata$shock_fdw_some <- usedata$fdw_some * usedata$shock
    
    fm.dw.6.1mo <- ols(daysdwoff ~ daysdw + shock + shock_fdw_all + shock_fdw_some + fdw_all + fdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anydw==1,],x=TRUE, y=TRUE)
    fm.dw.6.1mo$var <- mwc_2way(fm.dw.6.1mo, usedata$upid[usedata$anydw==1], usedata$day[usedata$anydw==1])
        
    fm.pr.6.1mo <- ols(rateproff ~ ratepr + shock + shock_fdw_all + shock_fdw_some + fdw_all + fdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anypr==1,],x=TRUE, y=TRUE)
    fm.pr.6.1mo$var <- mwc_2way(fm.pr.6.1mo, usedata$upid[usedata$anypr==1], usedata$day[usedata$anypr==1])    
    
    # 3 month windows
    usedata$fdw_all <- as.numeric(usedata$fdwfrac3==1)
    usedata$shock_fdw_all <- usedata$fdw_all * usedata$shock
    usedata$fdw_some <- as.numeric(usedata$fdwfrac3 > 0 & usedata$fdwfrac3 < 1)
    usedata$shock_fdw_some <- usedata$fdw_some * usedata$shock

    fm.dw.6.3mo <- ols(daysdwoff ~ daysdw + shock + shock_fdw_all + shock_fdw_some + fdw_all + fdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anydw==1,],x=TRUE, y=TRUE)
    fm.dw.6.3mo$var <- mwc_2way(fm.dw.6.3mo, usedata$upid[usedata$anydw==1], usedata$day[usedata$anydw==1])  
      
    fm.pr.6.3mo <- ols(rateproff ~ ratepr + shock + shock_fdw_all + shock_fdw_some + fdw_all + fdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anypr==1,],x=TRUE, y=TRUE)
    fm.pr.6.3mo$var <- mwc_2way(fm.pr.6.3mo, usedata$upid[usedata$anypr==1], usedata$day[usedata$anypr==1])       

    # output for robustness

    vars.rob <- c("shock","shock_fdw_all","shock_fdw_some","month2","month3","month4","month2fdw_all","month3fdw_all","month4fdw_all")
    addrows.rob <- rbind(c("Time Window (months)","1","3","2","2","1","3","2","2"))
    table.rob <- multiregtable(vars.rob, varlabels, list(fm.dw.6.1mo,fm.dw.6.3mo,fm.dw.6.time,fm.dw.6.LHS,fm.pr.6.1mo,fm.pr.6.3mo,fm.pr.6.time,fm.pr.6.LHS), 2, addrows.rob)
    result <- hacktex(table.rob, 
                    file=paste(tabledir, "robust.tex", sep="/"),
                    tabwidth="7in",
                    label="tab:rob",
                    table.env=FALSE,
                    caption.loc="top",
                    rowname =NULL,
                    center="none",
                    cgroup=c("","Daily Wage","Piece Rate"),
                    n.cgroup=c(1,4,4),
                    colheads=c("Regressor","I","II","III","IV","V","VI","VII","VIII"),
                    collabel.just=c("l","c","c","c","c","c","c","c","c"))

# fixing fdw_all back to 2 month windows

    usedata$fdw_all <- as.numeric(usedata$fdwfrac==1)
    usedata$shock_fdw_all <- usedata$fdw_all * usedata$shock
    usedata$fdw_some <- as.numeric(usedata$fdwfrac > 0 & usedata$fdwfrac < 1)
    usedata$shock_fdw_some <- usedata$fdw_some * usedata$shock


### Table 7: Additional outcome variables ##################################################################################################

# TOTAL EXTRACTION (PR + DW)
usedata$totdw <- usedata$ratedwoff - usedata$ratedw
usedata$totpr <- usedata$rateproff - usedata$ratepr
usedata$tot <- usedata$totdw + usedata$totpr

usedata$anyprdw <- as.numeric(usedata$anypr==1 | usedata$anydw==1)

    fm.tot.1 <- ols(tot  ~ shock + shock_fdw_all + shock_fdw_some + fdw_all + fdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + as.factor(did)
                                + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anyprdw==1,],
                                x=TRUE, y=TRUE)
    fm.tot.1$var <- mwc_2way(fm.tot.1, usedata$upid[usedata$anyprdw==1], usedata$day[usedata$anyprdw==1])

    fm.dw.2.tot <- ols(totdw  ~ shock + shock_fdw_all + shock_fdw_some + fdw_all + fdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + as.factor(did)
                                + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anydw==1,],
                                x=TRUE, y=TRUE)
    fm.dw.2.tot$var <- mwc_2way(fm.dw.2.tot, usedata$did[usedata$anydw==1], usedata$day[usedata$anydw==1])

    fm.dw.3.tot <- ols(totdw  ~ shock + shock_fdw_all + shock_fdw_some + shock_bdw_all + shock_bdw_some + fdw_all + fdw_some + bdw_all + bdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + as.factor(did)
                                + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anydw==1,],
                                x=TRUE, y=TRUE)
    fm.dw.3.tot$var <- mwc_2way(fm.dw.3.tot, usedata$did[usedata$anydw==1], usedata$day[usedata$anydw==1])

### Using only obs for people who didn't work ##################################################################

    fm.dw.1.nowork <- ols(ndaysdwoff  ~ shock + shock_fdw_all + shock_fdw_some + shock_bdw_all + shock_bdw_some + fdw_all + fdw_some + bdw_all + bdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + as.factor(did)
                                + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anydw==1,],
                                x=TRUE, y=TRUE)
    fm.dw.1.nowork$var <- mwc_2way(fm.dw.1.nowork, usedata$did[usedata$anydw==1], usedata$day[usedata$anydw==1])

    fm.dw.2.nowork <- ols(ndaysdwoff  ~ shock + shock_fdw_all + shock_fdw_some + fdw_all + fdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + as.factor(did)
                                + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anydw==1,],
                                x=TRUE, y=TRUE)
    fm.dw.2.nowork$var <- mwc_2way(fm.dw.2.nowork, usedata$did[usedata$anydw==1], usedata$day[usedata$anydw==1])

    fm.pr.1.nowork <- ols(nrateproff  ~ shock + shock_fdw_all + shock_fdw_some + shock_bdw_all + shock_bdw_some + fdw_all + fdw_some + bdw_all + bdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + as.factor(did)
                                + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anypr==1,],
                                x=TRUE, y=TRUE)
    fm.pr.1.nowork$var <- mwc_2way(fm.pr.1.nowork, usedata$did[usedata$anypr==1], usedata$day[usedata$anypr==1])
 
    fm.pr.2.nowork <- ols(nrateproff  ~ shock + shock_fdw_all + shock_fdw_some + fdw_all + fdw_some
                                + holiday + res_gender + res_sbc + res_tribe + day + as.factor(did)
                                + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3,
                                data=usedata[usedata$anypr==1,],
                                x=TRUE, y=TRUE)
    fm.pr.2.nowork$var <- mwc_2way(fm.pr.2.nowork, usedata$did[usedata$anypr==1], usedata$day[usedata$anypr==1])

    # output
    vars.appref <- c("shock","shock_fdw_all","shock_fdw_some","shock_alwaysdw","shock_alwayspr")
    addrows.appref <- rbind(c("Time Controls","Day","Day","Shock*Day","Day","Day","Day","Day"),
                     c("Fixed Effects","Dist","Dist","Dist","Dist","Dist","Dist","Dist"))
    table.appref <- multiregtable(vars.appref, varlabels, list(fm.tot.1,fm.dw.2.tot,fm.dw.3.tot,fm.dw.1.nowork,fm.dw.2.nowork,fm.pr.1.nowork,fm.pr.2.nowork), 2, addrows.appref)
    result <- hacktex(table.appref, 
                    file=paste(tabledir, "appref.tex", sep="/"),
                    tabwidth="7in",
                    label="tab:appref",
                    table.env=FALSE,
                    caption.loc="top",
                    rowname =NULL,
                    center="none",
                    cgroup=c("","DW+PR","Daily Wage","Piece Rate"),
                    n.cgroup=c(1,1,4,2),
                    colheads=c("Regressor","I","II","III","IV","V","VI","VII"),
                    collabel.just=c("l","c","c","c","c","c","c","c"))


### Table 8: See audit_MLE_estimation ##################################################################################################



### estimating bottom-line g.g. effects & standard errors #########################################

    # coefficients from dw regression times average of fwd-looking var
    # ignoring backward interactions
    
    dw.n <- nrow(usedata[usedata$anydw==1 & usedata$shock==1 & !is.na(usedata$fdwfrac),])
    fdwfrac.sum.all <- sum(usedata$fdwfrac[usedata$dwactive==1 & usedata$shock==1 & usedata$fdwfrac==1], na.rm=TRUE)
    fdwfrac.sum.some <- sum(usedata$fdwfrac[usedata$dwactive==1 & usedata$shock==1 & usedata$fdwfrac>0 & usedata$fdwfrac<1], na.rm=TRUE)
    dw.meanwage <- 66.8
    
    # coeffs from pr regression
    pr.n <- nrow(usedata[usedata$anypr==1 & usedata$shock==1,])
    
    # calculations (note: conservatively assume that backwards effects do not become new forwards effects in the counterfactual)
    act.dw <- (dw.coef * dw.n 
                        + dw.coef.fdwall * fdwfrac.sum.all
                        + dw.coef.fdwsome * fdwfrac.sum.some) * dw.meanwage
    drop.dw <- (dw.coef.fdwall * fdwfrac.sum.all + dw.coef.fdwsome * fdwfrac.sum.some) * dw.meanwage
    drop.pr <- pr.coef * pr.n

    round((-drop.dw - drop.pr)/(act.dw - drop.dw -drop.pr), 2)

    # variance calculations (note: conservatively assume that backwards effects do not become new forwards effects in the counterfactual)
    # also assuming dw and pr independent, distributions have no mass on 0, and using taylor approximation for variance of x/y
    act.dw.var <- ((dw.n^2)*dw.var + (fdwfrac.sum.all^2)*dw.fdwall.var + (fdwfrac.sum.some^2)*dw.fdwsome.var + 2*dw.n*fdwfrac.sum.all*dw.shock.fdwall + 2*dw.n*fdwfrac.sum.some*dw.shock.fdwsome + 2*fdwfrac.sum.some*fdwfrac.sum.all*dw.fdwall.fdwsome)*(dw.meanwage)^2
    drop.dw.var <- ((fdwfrac.sum.all^2)*dw.fdwall.var + (fdwfrac.sum.some^2)*dw.fdwsome.var + 2*fdwfrac.sum.some*fdwfrac.sum.all*dw.fdwall.fdwsome)*(dw.meanwage)^2
    drop.pr.var <- pr.var*(pr.n^2)
    num.var <- drop.dw.var + drop.pr.var
    denom.var <- act.dw.var + drop.dw.var + drop.pr.var
    
    full.var <- ((drop.dw + drop.pr)^2)/((act.dw - drop.dw - drop.pr)^2)*(num.var/((drop.dw + drop.pr)^2) + denom.var/((act.dw - drop.dw - drop.pr)^2))
    
    round((full.var^0.5)*1.6, 2)



# Figure 1 - FROM MICRO DATA: AVAILABLE ON REQUEST


# Figure 2: Histogram of Forward Daily Wage Fraction
    pdf(file=paste(figdir, "fdwfrac.pdf", sep="/"), width = 7, height = 4.5)
    par(lwd=2)
    hist(usedata$fdwfrac, main="", xlab="Fraction of Future Daily Wage Projects")
    dev.off()

### Figure 3: Wage Time-Series Plot #########################################################

    # define study period as X-axis
    sp <- 60:181

    # piece rate, actual, OR
    total.days.pr.daily.or <- tapply(usedata$dayspr[usedata$sid==24], usedata$day[usedata$sid==24], sum)

    # daily wage, actual, OR
    total.rate.dw.daily.or <- tapply(usedata$ratedw[usedata$sid==24], usedata$day[usedata$sid==24], sum)
    total.days.dw.daily.or <- tapply(usedata$daysdw[usedata$sid==24], usedata$day[usedata$sid==24], sum)
    averateall.dw.daily.or <- total.rate.dw.daily.or/total.days.dw.daily.or

    # daily wage, official sample, OR
    total.rate.dw.daily.of.or <- tapply(usedata$ratedwoff[usedata$sid==24], usedata$day[usedata$sid==24], sum)
    total.days.dw.daily.of.or <- tapply(usedata$daysdwoff[usedata$sid==24], usedata$day[usedata$sid==24], sum)
    averateall.dw.daily.of.or <- total.rate.dw.daily.of.or/total.days.dw.daily.of.or
    
    # daily wage, official frame, OR
    total.rate.dw.daily.of.or.f <- tapply(usedata$ratedwoff[usedata$sid==24], usedata$day[usedata$sid==24], sum)
    total.days.dw.daily.of.or.f <- tapply(usedata$daysdwoff[usedata$sid==24], usedata$day[usedata$sid==24], sum)
    averateall.dw.daily.of.or.f <- total.rate.dw.daily.of.or.f/total.days.dw.daily.of.or.f

    # basic graph
    pdf(file=paste(figdir, "wagetrends.pdf", sep="/"), width = 7, height = 4.5)
    par(lwd=2)
    plot(sp, averateall.dw.daily.or, type="l", lty="solid", ylim=c(50,70),ylab="Rs.", xlab="Day of Year",
        xaxt="n")
    lines(sp, averateall.dw.daily.of.or, type="l", lty="dashed")
    lines(c(121,121),c(45,75), lty="solid")
    lines(sp, averateall.dw.daily.of.or.f, lty="dotted")
    legend(x="topleft", legend=c("Actual Sample","Official Sample","Official Frame"), lty=c("solid","dashed","dotted"))
    axis(side=1, at=c(60,80,100,140,160,180))
    axis(side=1, at=121, labels="(Shock)")
    dev.off()

# Figure 4 and 5 #########################################################

    # create dataset for aggregate analysis
    dwdays <- tapply(usedata$daysdwoff[usedata$anydw==1], usedata$day[usedata$anydw==1], sum) - tapply(usedata$daysdw[usedata$anydw==1], usedata$day[usedata$anydw==1], sum)
    ratepr <- tapply(usedata$rateproff[usedata$anypr==1], usedata$day[usedata$anypr==1], sum) - tapply(usedata$ratepr[usedata$anypr==1], usedata$day[usedata$anypr==1], sum)
    tdata <- data.frame(dwdays = dwdays, ratepr = ratepr, day=sp)
    tdata$shock <- as.numeric(tdata$day > 120)
    tdata$shock_day <- tdata$shock * (tdata$day - 121)
    tdata$day2 <- tdata$day * tdata$day
    tdata$shock_day2 <- tdata$shock * (tdata$day - 121)^2

    # estimate models and standard errors
    fm.dw.plain <- ols(dwdays ~ day + shock + shock_day, data=tdata, x=TRUE, y=TRUE)
    dwdays.fit <- fm.dw.plain$fitted.values
    fm.dw.plain.pred <- predict(fm.dw.plain, se.fit=TRUE)
    dwdays.se <- fm.dw.plain.pred$se.fit

    fm.dw.plain2 <- ols(dwdays ~ day + day2 + shock + shock_day + shock_day2, data=tdata, x=TRUE, y=TRUE)
    dwdays.fit2 <- fm.dw.plain2$fitted.values
    fm.dw.plain.pred2 <- predict(fm.dw.plain2, se.fit=TRUE) 
    dwdays.se2 <- fm.dw.plain.pred2$se.fit

    fm.pr.plain <- ols(ratepr ~ day + shock + shock_day, data=tdata, x=TRUE, y=TRUE)
    ratepr.fit <- fm.pr.plain$fitted.values
    fm.pr.plain.pred <- predict(fm.pr.plain, se.fit=TRUE) 
    ratepr.se <- fm.pr.plain.pred$se.fit

    fm.pr.plain2 <- ols(ratepr ~ day + day2 + shock + shock_day + shock_day2, data=tdata, x=TRUE, y=TRUE)
    ratepr.fit2 <- fm.pr.plain2$fitted.values
    fm.pr.plain.pred2 <- predict(fm.pr.plain2, se.fit=TRUE) 
    ratepr.se2 <- fm.pr.plain.pred2$se.fit

    # FIGURE 4
    pdf(file=paste(figdir, "dw_fits.pdf", sep="/"), width = 5.5, height = 7)
    par(mfrow=c(2,1))
    
        par(mar=c(4,4,4,1))
        plot(sp, tdata$dwdays, type="l", col="grey", lwd=2, ylim=c(0,600), xlab="Day of Year", ylab="Days of Work", main="(a) Linear Fit")
        lines(sp[1:61], dwdays.fit[1:61], lwd=2)
        lines(sp[1:61], dwdays.fit[1:61] + 2*dwdays.se[1:61], lwd=2, lty="dotted")
        lines(sp[1:61], dwdays.fit[1:61] - 2*dwdays.se[1:61], lwd=2, lty="dotted")
        lines(sp[62:122], dwdays.fit[62:122], lwd=2)
        lines(sp[62:122], dwdays.fit[62:122] + 2*dwdays.se[62:122], lwd=2, lty="dotted")
        lines(sp[62:122], dwdays.fit[62:122] - 2*dwdays.se[62:122], lwd=2, lty="dotted")
        lines(sp[c(61,61)], c(0,600), lwd=2, col="black")
        legend(x="topright", legend=c("Data","Model"), lty=c("solid","solid"), col=c("grey","black"))

        plot(sp, tdata$dwdays, type="l", col="grey", lwd=2, ylim=c(0,600), xlab="Day of Year", ylab="Days of Work", main="(b) Quadratic Fit")
        lines(sp[1:61], dwdays.fit2[1:61], lwd=2)
        lines(sp[1:61], dwdays.fit2[1:61] + 2*dwdays.se2[1:61], lwd=2, lty="dotted")
        lines(sp[1:61], dwdays.fit2[1:61] - 2*dwdays.se2[1:61], lwd=2, lty="dotted")
        lines(sp[62:122], dwdays.fit2[62:122], lwd=2)
        lines(sp[62:122], dwdays.fit2[62:122] + 2*dwdays.se2[62:122], lwd=2, lty="dotted")
        lines(sp[62:122], dwdays.fit2[62:122] - 2*dwdays.se2[62:122], lwd=2, lty="dotted")
        lines(sp[c(61,61)], c(0,600), lwd=2, col="black")
        legend(x="topright", legend=c("Data","Model"), lty=c("solid","solid"), col=c("grey","black"))

    dev.off()

    # FIGURE 5
    pdf(file=paste(figdir, "pr_fits.pdf", sep="/"), width = 5.5, height = 7)
    par(mfrow=c(2,1))
        
        plot(sp, tdata$ratepr, type="l", col="grey", lwd=2, xlab="Day of Year", ylab="Amount", ylim=c(0,12000), main="(a) Linear Fit")
        lines(sp[1:61], ratepr.fit[1:61], lwd=2)
        lines(sp[1:61], ratepr.fit[1:61] + 2*ratepr.se[1:61], lwd=2, lty="dotted")
        lines(sp[1:61], ratepr.fit[1:61] - 2*ratepr.se[1:61], lwd=2, lty="dotted")
        lines(sp[62:122], ratepr.fit[62:122], lwd=2)
        lines(sp[62:122], ratepr.fit[62:122] + 2*ratepr.se[62:122], lwd=2, lty="dotted")
        lines(sp[62:122], ratepr.fit[62:122] - 2*ratepr.se[62:122], lwd=2, lty="dotted")
        lines(sp[c(61,61)], c(0,12000), lwd=2, col="black")
        legend(x="topright", legend=c("Data","Model"), lty=c("solid","solid"), col=c("grey","black"))

        plot(sp, tdata$ratepr, type="l", col="grey", lwd=2, xlab="Day of Year", ylab="Amount", ylim=c(0,12000), main="(b) Quadratic Fit")
        lines(sp[1:61], ratepr.fit2[1:61], lwd=2)
        lines(sp[1:61], ratepr.fit2[1:61] + 2*ratepr.se2[1:61], lwd=2, lty="dotted")
        lines(sp[1:61], ratepr.fit2[1:61] - 2*ratepr.se2[1:61], lwd=2, lty="dotted")
        lines(sp[62:122], ratepr.fit2[62:122], lwd=2)
        lines(sp[62:122], ratepr.fit2[62:122] + 2*ratepr.se2[62:122], lwd=2, lty="dotted")
        lines(sp[62:122], ratepr.fit2[62:122] - 2*ratepr.se2[62:122], lwd=2, lty="dotted")
        lines(sp[c(61,61)], c(0,12000), lwd=2, col="black")
        legend(x="topright", legend=c("Data","Model"), lty=c("solid","solid"), col=c("grey","black"))

    dev.off()

### Finis ##################################################################################################
