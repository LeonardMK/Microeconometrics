library(tidyverse)
library(AER)
library(ggplot2)

# Load dataset
usedata <- read.csv("C:/Users/Wilms/OneDrive - uni-bonn.de/Uni Bonn/2. Semester/Microeconometrics/Replication study/Github Replication/student-project-LeonardMK/data/finalanalysisdata.csv")
diddata <- read.csv("C:/Users/Wilms/OneDrive - uni-bonn.de/Uni Bonn/2. Semester/Microeconometrics/Replication study/Github Replication/student-project-LeonardMK/data/didanalysisdata.csv")

# Create a  months since shock variable, and corruption variables
usedata$months_shock <- as.numeric(cut(usedata$day, c(0, 90, 120, 150, 180, 300), c(0, 1, 2, 3, 4)))
usedata$dwdiff <- usedata$daysdwoff - usedata$daysdw
usedata$rateprdiff <- usedata$rateproff - usedata$ratepr

# Check for outliers
summary(usedata[, c("dwdiff", "rateprdiff")])

# Explanations for negative theft values
# Measurement errors.
me_dw <- matrix(table(usedata$daysdw > 0, usedata$daysdwoff > 0), 2, 2)
row.names(me_dw) <-  c("Act. Days DW <= 0", "Act. Days DW > 0")
colnames(me_dw) <-  c("Off. Days DW <= 0", "Off. Days DW > 0")

me_pr <- matrix(table(usedata$ratepr > 0, usedata$rateproff > 0), 2, 2)
row.names(me_pr) <-  c("Act. Rate PR <= 0", "Act. Rate PR > 0")
colnames(me_pr) <-  c("Act. Rate PR <= 0", "Off. Rate PR > 0")

write.csv(me_dw, "tables/Recall_dw.csv")
write.csv(me_pr, "tables/Recall_pr.csv")

# Mistake made by the official

# Calculate Correlation table to find potential problems in data
varvec <- c("daysdwoff", "daysdw", "rateproff", "ratepr", "dwdiff", "rateprdiff", "fdwfrac", "bdwfrac", "shock", "day", "dayofmonth")

cor_table = round(cor(usedata[, varvec], use = "pairwise.complete.obs"), 3)

# Replace lower values with empty cells
cor_table[lower.tri(cor_table)] = ""

cor_table <- data.frame(cor_table)

# Publication ready tables
varnames <- c("Off. DW days", "Act. DW days", "Off. rate PR", "Act. rate PR", "DW Diff.", "Rate PR diff", 
              "Forw. fraction DW", "Backw. fraction DW", "Shock", "Day", "Day of Month")

row.names(cor_table) <- varnames
colnames(cor_table) <- varnames

# Save table
write.csv(cor_table, file = "Robustness/tables/CorTable.csv")

# Add summary of dwdiff and prdiff
diff <- matrix(nrow = 2, ncol = 2)
diff[1, 1:2] <- table(usedata$dwdiff < 0)
diff[2, 1:2] <- table(usedata$prdiff < 0)

# Paper claims no effect of shock and day. However show this
agg_fdwfrac = data.frame("mean_fwddwfrac" = tapply(usedata[, "fdwfrac"], usedata[, "day"], mean, na.rm = TRUE))
agg_dwdiff = data.frame("mean_dwdiff" = tapply(usedata[, "dwdiff"], usedata[, "day"], mean, na.rm = TRUE))


# Change names to actual dates
agg_fdwfrac$Date <- as.Date(as.numeric(rownames(agg_fdwfrac)) - 1, origin = "2007-01-01")
agg_dwdiff$Date <- as.Date(as.numeric(rownames(agg_dwdiff)) - 1, origin = "2007-01-01")

# Plot data
ggplot(agg_fdwfrac, aes(x = as.numeric(rownames(agg_fdwfrac)), y = mean_fwddwfrac)) + geom_point() + theme_bw() + geom_vline(xintercept = 121) + 
scale_x_continuous(seq(60, 182, 7), name = "Date", labels = agg_fdwfrac$Date[seq(1, 122, 7)]) + xlab("Date") + ylab("Mean forward daily wage fraction") + theme(axis.text.x = element_text(angle = 90)) + 
  geom_text(aes(x = 119, y = 0.69, label = "Shock"), angle = 90, size = 4, col = "black")

# Plot data
ggplot(agg_dwdiff, aes(x = as.numeric(rownames(agg_dwdiff)), y = mean_dwdiff)) + geom_point() + theme_bw() + geom_vline(xintercept = 121) + 
scale_x_continuous(seq(60, 182, 7), name = "Date", labels = agg_dwdiff$Date[seq(1, 122, 7)]) + xlab("Date") + ylab("Mean forward daily wage fraction") + theme(axis.text.x = element_text(angle = 90)) + 
  geom_text(aes(x = 119, y = 0.69, label = "Shock"), angle = 90, size = 4, col = "black")

# Save plot
ggsave("Robustness/figures/Fdwfrac.png", scale = 2)

# Discuss that I am unable to decide whether this change is due to a change in project decomposition. For example maybe more daily-wage projects are performed during the summar months.
# WOuld need to compare data from different timeframes or states.

# Check for problems of regression (3) in Table 2 expand on model by removing multicollinearity and performig tobit
m1 = lm(fdwfrac ~ shock + day + day2 + as.factor(did) + holiday + res_gender + res_sbc + res_tribe + season1 + season2 + dayofmonth + dayofmonth2 + dayofmonth3, 
        data=usedata)

# Check diagnositcs
plot(m1)  # Definitly heteroskedastic. Lots of patterns in the residuals. Not-normal. No influential observations. Still a complete mess

# Test for normality
ks.test(m1$residuals, "pnorm")  # Not normal inference is wrong

# Test for multicollinearity
vif(m1) # day and day2, there is also high multicollinearity for the shock variable

# Still need a function to report results in the same manner as in Python

# Motivation for using tobit
# Fdwfrac
ggplot(usedata, aes(x = fdwfrac)) + geom_histogram(bins = 20) + theme_bw() + xlab("Forward daily wage fraction") + ylab("Frequency")
ggsave("Robustness/figures/Hist_fdwfrac.png", scale = 2)

# Days DW
ggplot(usedata, aes(x = daysdwoff)) + geom_histogram(bins = 20) + theme_bw() + xlab("Forward daily wage fraction") + ylab("Frequency")
ggsave("Robustness/figures/Hist_daysdw.png", scale = 2)

# Piece rate
ggplot(usedata, aes(x = rateproff)) + geom_histogram(bins = 20) + theme_bw() + xlab("Off. piece-rate") + ylab("Frequency")
ggsave("Robustness/figures/Hist_rateproff.png", scale = 2)

# Excluding all those obseervations that have y = 0 isn't ideal. Panel structure plus losing observations. Try Tobit instead to model the relationship for those values that are > 0.
# Loading table function
source("Robustness/functions/results.R")

# Table 2 tobit
tobit_t21 = tobit(fdwfrac ~ shock + day + as.factor(did) + holiday + res_gender + res_sbc + res_tribe + season1 + season2 + cluster(upid) + cluster(day), 0, 1, data = usedata)
tobit_t22 = tobit(fdwfrac ~ shock + day+ as.factor(ubid) + holiday + res_gender + res_sbc + res_tribe + season1 + season2 + cluster(ubid) + cluster(day), 0, 1, data = usedata) # Add block FE

# Save output
Table2 <- results(list(tobit_t21, tobit_t22), c("shock", "day"), 4, 2, T, c("Day", "Day"), c("District", "Block"), T, c("Shock", "Day"))
write.csv(Table2, "Robustness/tables/Table2.csv")

# Table 3 a)
tobit_32a = tobit(daysdwoff ~ daysdw + shock + day + as.factor(did) + holiday + res_gender + res_sbc + res_tribe + season1 + season2 + dayofmonth
                              + cluster(upid) + cluster(day), 0, data = usedata)

tobit_32ab = tobit(daysdwoff ~ daysdw + shock + day + ubid + holiday + res_gender + res_sbc + res_tribe + season1 + season2 + dayofmonth
                              + cluster(ubid) + cluster(day), 0, data = usedata)

tobit_36a = tobit(daysdwoff  ~ daysdw + shock + shock_alwaysdw + alwaysdw
                              + day + shock_day + as.factor(did)
                              + holiday + res_gender + res_sbc + res_tribe + season1 + season2
                              + cluster(upid) + cluster(day), 0, data = usedata)

# Adjust coefficient
# Need to find numeric index of shock and shock_day
shock_ind <- which(names(tobit_36a$coefficients) == "shock")
shock_day_ind <- which(names(tobit_36a$coefficients) == "shock_day")
tobit_36a$coefficients["shock"] <- tobit_36a$coefficients["shock"] + tobit_36a$coefficients["shock_day"]*121
tobit_36a$var[shock_ind, shock_ind] <- tobit_36a$var[shock_ind, shock_ind] + 121*121*tobit_36a$var[shock_day_ind, shock_day_ind] + 2*121*tobit_36a$var[shock_ind, shock_day_ind]

# Add block FE
tobit_36ab = tobit(daysdwoff  ~ daysdw + shock + shock_alwaysdw + alwaysdw
                              + day + shock_day + ubid
                              + holiday + res_gender + res_sbc + res_tribe + season1 + season2
                              + cluster(ubid) + cluster(day), 0, data = usedata)

# Adjust coefficient
tobit_36ab$coefficients["shock"] <- tobit_36ab$coefficients["shock"] + tobit_36ab$coefficients["shock_day"]*121

# Need to find numeric index of shock and shock_day
shock_ind <- which(names(tobit_36ab$coefficients) == "shock")
shock_day_ind <- which(names(tobit_36ab$coefficients) == "shock_day")
tobit_36ab$var[shock_ind,shock_ind] <- tobit_36ab$var[shock_ind,shock_ind] + 121*121*tobit_36ab$var[shock_day_ind,shock_day_ind] + 2*121*tobit_36ab$var[shock_ind,shock_day_ind]

# Save Regression Outputs
Table3a <- results(list(tobit_32a, tobit_32ab, tobit_36a, tobit_36ab), c("shock", "shock_alwaysdw", "alwaysdw"), 2, 2, T, 
                   TE = c("Day", "Day", "Shock x Day", "Shock x Day"), labels = c("Shock", "Shock x (Always DW)", "Always DW"))
write.csv(Table3a, "Robustness/tables/Table3a.csv")

# Table 3 b)
tobit_32b = tobit(daysdwoff ~ daysdw + shock + shock_fdw_all + shock_fdw_some + fdw_all + fdw_some
                              + holiday + res_gender + res_sbc + res_tribe + day + as.factor(did)
                              + season1 + season2 + dayofmonth + cluster(upid) + cluster(day), 0, data = usedata)

# Block FE
tobit_32bb = tobit(daysdwoff ~ daysdw + shock + shock_fdw_all + shock_fdw_some + fdw_all + fdw_some
                              + holiday + res_gender + res_sbc + res_tribe + day + ubid
                              + season1 + season2 + dayofmonth + cluster(ubid) + cluster(day), 0, data = usedata)

tobit_36b = tobit(daysdwoff  ~  daysdw + shock + shock_fdw_all + shock_fdw_some + shock_bdw_all + shock_bdw_some + fdw_all + fdw_some + bdw_all + bdw_some
                              + holiday + res_gender + res_sbc + res_tribe + day + shock_day + as.factor(did)
                              + season1 + season2 + dayofmonth + cluster(upid) + cluster(day), 0, data = usedata)

# Need to find numeric index of shock and shock_day
shock_ind <- which(names(tobit_36b$coefficients) == "shock")
shock_day_ind <- which(names(tobit_36b$coefficients) == "shock_day")

# Adjusting shock coefficient
tobit_36b$coefficients[shock_ind] <- tobit_36b$coefficients[shock_ind] + tobit_36b$coefficients[shock_day_ind]*121
tobit_36b$var[shock_ind,shock_ind] <- tobit_36b$var[shock_ind,shock_ind] + 121*121*tobit_36b$var[shock_day_ind,shock_day_ind] + 2*121*tobit_36b$var[shock_ind,shock_day_ind]

# Block FE
tobit_36bb = tobit(daysdwoff  ~  daysdw + shock + shock_fdw_all + shock_fdw_some + shock_bdw_all + shock_bdw_some + fdw_all + fdw_some + bdw_all + bdw_some
                              + holiday + res_gender + res_sbc + res_tribe + day + shock_day + ubid
                              + season1 + season2 + dayofmonth + cluster(ubid) + cluster(day), 0, data = usedata)

# Need to find numeric index of shock and shock_day
shock_ind <- which(names(tobit_36bb$coefficients) == "shock")
shock_day_ind <- which(names(tobit_36bb$coefficients) == "shock_day")

# ADjusting shock coefficient
tobit_36bb$coefficients[shock_ind] <- tobit_36bb$coefficients[shock_ind] + tobit_36bb$coefficients[shock_day_ind]*121

tobit_36bb$var[shock_ind,shock_ind] <- tobit_36bb$var[shock_ind,shock_ind] + 121*121*tobit_36bb$var[shock_day_ind,shock_day_ind] + 2*121*tobit_36bb$var[shock_ind,shock_day_ind]

# Save Regression Outputs
Table3b <- results(list(tobit_32b, tobit_32bb, tobit_36b, tobit_36bb), c("shock", "shock_fdw_all", "shock_fdw_some", "shock_bdw_all", "shock_bdw_some"),
                   2, 2, T, TE = c("Day", "Day", "Shock x Day", "Shock x Day"), 
                   labels = c("Shock", "Shock x (Forward DW All)", "Shock x (Forward DW Some)", "Shock x (Backward DW All)", "Shock x (Backward DW Some)"))
write.csv(Table3b, "Robustness/tables/Table3b.csv")

# Table 4
# Column 2
tobit_42a = tobit(rateproff ~ ratepr + shock + day + as.factor(did)  + holiday + res_gender + res_sbc + res_tribe
                               + season1 + season2 + dayofmonth + cluster(upid) + cluster(day), 0, data = usedata)

tobit_42ab = tobit(rateproff ~ ratepr + shock + day + ubid  + holiday + res_gender + res_sbc + res_tribe
                               + season1 + season2 + dayofmonth + cluster(ubid) + cluster(day), 0, data = usedata)

# Column 6
tobit_46a = tobit(rateproff  ~ ratepr + shock + shock_alwayspr + alwayspr + day + shock_day + as.factor(did)  + holiday
                               + res_gender + res_sbc + res_tribe + season1 + season2 + dayofmonth + cluster(upid) + cluster(day),
                               data = usedata)

# Need to find numeric index of shock and shock_day
shock_ind <- which(names(tobit_46a$coefficients) == "shock")
shock_day_ind <- which(names(tobit_46a$coefficients) == "shock_day")

# Adjusting Coefficient
tobit_46a$coefficients[shock_ind] <- tobit_46a$coefficients[shock_ind] + tobit_46a$coefficients[shock_day_ind]*121
tobit_46a$var[shock_ind,shock_ind] <- tobit_46a$var[shock_ind,shock_ind] + 121*121*tobit_46a$var[shock_day_ind,shock_day_ind] + 2*121*tobit_46a$var[shock_ind,shock_day_ind]

# Block level FE
tobit_46ab = tobit(rateproff  ~ ratepr + shock + shock_alwayspr + alwayspr + day + shock_day + ubid  + holiday
                               + res_gender + res_sbc + res_tribe + season1 + season2 + dayofmonth + cluster(ubid) + cluster(day),
                               data = usedata)

# Need to find numeric index of shock and shock_day
shock_ind <- which(names(tobit_46ab$coefficients) == "shock")
shock_day_ind <- which(names(tobit_46ab$coefficients) == "shock_day")

# Adjusting coefficient 
tobit_46ab$coefficients[shock_ind] <- tobit_46ab$coefficients[shock_ind] + tobit_46ab$coefficients[shock_day_ind]*121
tobit_46ab$var[shock_ind,shock_ind] <- tobit_46ab$var[shock_ind,shock_ind] + 121*121*tobit_46ab$var[shock_day_ind,shock_day_ind] + 2*121*tobit_46ab$var[shock_ind,shock_day_ind]

# Save Table 4a. Controlling for Blocks made the system unsolvable
Table4a <- results(list(tobit_42a, tobit_46a), c("shock", "shock_alwayspr", "alwayspr"), 
                   TE = c("Day", "Day", "Shock x Day", "Shock x Day"),
                   FE = c("District", "Block", "District", "Block"),
                   labels = c("Shock", "Shock x (Always PR)", "Always PR"))
write.csv(Table4a, "Robustness/tables/Table4a.csv")

# Panel B: Dynamic effects
tobit_42b = tobit(rateproff  ~ ratepr + shock + shock_fdw_all + shock_fdw_some + fdw_all + fdw_some + holiday + res_gender + res_sbc + res_tribe
                              + day + as.factor(did) + season1 + season2 + dayofmonth + cluster(upid) + cluster(day), data = usedata)

# Block FE
tobit_42bb = tobit(rateproff  ~ ratepr + shock + shock_fdw_all + shock_fdw_some + fdw_all + fdw_some + holiday + res_gender + res_sbc + res_tribe
                              + day + ubid + season1 + season2 + dayofmonth + cluster(ubid) + cluster(day), data = usedata)

# Column 6
tobit_46b <- tobit(rateproff  ~ ratepr + shock + shock_fdw_all + shock_fdw_some + shock_bdw_all + shock_bdw_some + fdw_all + fdw_some
                            + bdw_all + bdw_some  + holiday + res_gender + res_sbc + res_tribe + day + shock_day + as.factor(did)
                            + season1 + season2 + dayofmonth + cluster(upid) + cluster(day), 0, 
                            data = usedata)

# Need to find numeric index of shock and shock_day
shock_ind <- which(names(tobit_46b$coefficients) == "shock")
shock_day_ind <- which(names(tobit_46b$coefficients) == "shock_day")

# Adjusting Shock effect
tobit_46b$coefficients[shock_ind] <- tobit_46b$coefficients[shock_ind] + tobit_46b$coefficients[shock_day_ind]*121
tobit_46b$var[shock_ind,shock_ind] <- tobit_46b$var[shock_ind,shock_ind] + 121*121*tobit_46b$var[shock_day_ind,shock_day_ind] + 2*121*tobit_46b$var[shock_ind,shock_day_ind]

# Adding block FE
tobit_46bb <- tobit(rateproff  ~ ratepr + shock + shock_fdw_all + shock_fdw_some + shock_bdw_all + shock_bdw_some + fdw_all + fdw_some
                                + bdw_all + bdw_some  + holiday + res_gender + res_sbc + res_tribe + day + shock_day + ubid
                                + season1 + season2 + dayofmonth + cluster(ubid) + cluster(day), 0, 
                                data = usedata)

# Need to find numeric index of shock and shock_day
shock_ind <- which(names(tobit_46bb$coefficients) == "shock")
shock_day_ind <- which(names(tobit_46bb$coefficients) == "shock_day")

# Adjusting Shock effect
tobit_46bb$coefficients[shock_ind] <- tobit_46bb$coefficients[shock_ind] + tobit_46bb$coefficients[shock_day_ind]*121
tobit_46bb$var[shock_ind,shock_ind] <- tobit_46bb$var[shock_ind,shock_ind] + 121*121*tobit_46bb$var[shock_day_ind,shock_day_ind] + 2*121*tobit_46bb$var[shock_ind,shock_day_ind]

# Write Table 4b. Block FE not solvable
Table4b <- results(list(tobit_42b, tobit_46b), c("shock", "shock_fdw_all", "shock_fdw_some", "shock_bdw_all", "shock_bdw_some"),
                   2, 2, T, TE = c("Day", "Day", "Shock x Day", "Shock x Day"), 
                   labels = c("Shock", "Shock x (Forward DW All)", "Shock x (Forward DW Some)", "Shock x (Backward DW All)", "Shock x (Backward DW Some)"))
write.csv(Table4b, "Robustness/tables/Table4b.csv")

# Assessing did-Assumptions
did_assump = t(tapply((diddata$rateproff), list(diddata$sid, diddata$day), sum, na.rm = TRUE, simplify = TRUE))
colnames(did_assump) <- c("Orissa", "Andra Pradesh")

# Bring into single column form
did_assump <- gather(data.frame(did_assump), "State", "SumPRoff")

# Generate Day column
did_assump$day <- rep(1:122, 2)

# Plot
ggplot(did_assump, aes(x = day, y = SumPRoff, col = State)) + geom_line() + theme_bw() + labs(x = "Day", y = "Sum of rate PR") + geom_vline(xintercept = c(5, 54, 61)) +
  scale_x_continuous(breaks = seq(1, 122, 7), labels = as.Date(did_assump$day - 1, origin = "2007-03-01")[seq(1, 122, 7)]) + theme(axis.text.x = element_text(angle = 90)) +
  geom_text(aes(x = 60, y = 25000, label = "OR Shock"), angle = 90, col = "black", size = 4) + 
  geom_text(aes(x = 53, y = 25000, label = "AP Shock 2"), angle = 90, col = "black", size = 4) + 
  geom_text(aes(x = 4, y = 25000, label = "AP Shock 1"), angle = 90, col = "black", size = 4)

# Save image 
ggsave("Robustness/figures/Did_assump.png", scale = 2)

# Histogram of rateproff by states
ggplot(diddata, aes(x = rateproff)) + geom_histogram(aes(col = sid), bins = 20)

# DiD assumption doesn't hold. Replicate using Tobit
tobit_51a = tobit(rateproff ~ orshock + or_orshock + apshock1 + ap_apshock1 + apshock2 + ap_apshock2
                              + day + ratepr + holiday + season1 + season2 + dayofmonth + as.factor(sid)
                              + cluster(upid) + cluster(day), data = diddata)

# District FE
tobit_52a = tobit(rateproff ~ orshock + or_orshock + apshock1 + ap_apshock1 + apshock2 + ap_apshock2
                              + day + ratepr  + holiday + season1 + season2 + dayofmonth + as.factor(did)
                              + cluster(upid) + cluster(day), data = diddata)

# DID and district FE
tobit_53a = tobit(rateproff ~ orshock + or_orshock + apshock1 + ap_apshock1 + apshock2 + ap_apshock2
                              + day + shock_day + ratepr  + holiday + season1 + season2 + dayofmonth + as.factor(did)
                              + cluster(upid) + cluster(day), data = diddata)

# Need to find numeric index of shock and shock_day
orshock_ind <- which(names(tobit_53a$coefficients) == "orshock")
shock_day_ind <- which(names(tobit_53a$coefficients) == "shock_day")

# Adjusting effect
tobit_53a$coefficients[orshock_ind] <- tobit_53a$coefficients[orshock_ind] + tobit_53a$coefficients[shock_day_ind]*121
tobit_53a$var[orshock_ind,orshock_ind] <- tobit_53a$var[orshock_ind,orshock_ind] + 121*121*tobit_53a$var[shock_day_ind,shock_day_ind] + 2*121*tobit_53a$var[orshock_ind,shock_day_ind]

# UBID instead of did
tobit_53ab = tobit(rateproff ~ orshock + or_orshock + apshock1 + ap_apshock1 + apshock2 + ap_apshock2
                              + day + shock_day + ratepr  + holiday + season1 + season2 + dayofmonth + as.factor(did)
                              + cluster(ubid) + cluster(day), data = diddata)

# Need to find numeric index of shock and shock_day
orshock_ind <- which(names(tobit_53ab$coefficients) == "orshock")
shock_day_ind <- which(names(tobit_53ab$coefficients) == "shock_day")

# Adjusting effect
tobit_53ab$coefficients[orshock_ind] <- tobit_53ab$coefficients[orshock_ind] + tobit_53ab$coefficients[shock_day_ind]*121
tobit_53ab$var[orshock_ind,orshock_ind] <- tobit_53ab$var[orshock_ind,orshock_ind] + 121*121*tobit_53ab$var[shock_day_ind,shock_day_ind] + 2*121*tobit_53ab$var[orshock_ind,shock_day_ind]

# Output Table
Table5 <- results(list(tobit_51a, tobit_52a, tobit_53a, tobit_53ab), 
                  c("or_orshock", "ap_apshock1", "ap_apshock2", "orshock", "apshock1", "apshock2", "ratepr"),
                  TE = c("Day", "Day", "Shock x Day", "Shock x Day"), FE = c("State", "District", "District", "Block"),
                  labels = c("OR x (OR Shock)", "AP x (AP Shock 1)", "AP x (AP Shock 2)", "OR Shock", "AP Shock 1", "AP Shock 2", "Actual PR payments"))
write.csv(Table5, "Robustness/tables/Table5.csv")

# Calculating magnitude
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


# End