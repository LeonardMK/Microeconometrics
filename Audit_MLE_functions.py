
# coding: utf-8

# In[1]:


import pandas as pd
import scipy as sp
import numpy as np


# In[198]:


# Implementation of objective function 1
def objfun1(theta, tstar, Ts, months, years, ds):
    
    logps = np.repeat(0, len(Ts))
    logps = logps.astype("double")
    
    for i in range(len(Ts)):
        
        T = Ts[i].astype("int")
        d = ds[i]
        
        if not np.isnan(months[i]):
            
            t = months[i].astype("int")
            
            logps[i] = np.log(prob_1st_monthly(theta, tstar, t, T, d))
            
            for tau in range(t + 1, T + 1):
                
                logps[i] = logps[i] + np.log(1 - prob_1st_monthly(theta, tstar, tau, T, d))
        
        elif not np.isnan(years[i]):
            t = years[i].astype("int")
            
            logps[i] = np.log(prob_1st_yearly(theta, tstar, t, 1, d))
            
            for tau in range(t * 12 + 1, T + 1):
                logps[i] = logps[i] + np.log(1 - prob_1st_monthly(theta, tstar, tau, T, d))
        
        else:
            
            for tau in range(-22, T + 1):
                logps[i] = logps[i] + np.log(1 - prob_1st_monthly(theta, tstar, tau, T, d))
                
    return -np.sum(logps)


# In[3]:


def prob_1st_monthly(theta, tstar, t, T, d):
    
    temp = theta[0] * (t >= tstar) + theta[1] * (d==11) + theta[2] * (d==24) + theta[3] * (d==29) + theta[4] * t
    return (np.exp(temp) / (1 + np.exp(temp)))
        
def prob_1st_yearly(theta, tstar, t, T, d):

    temp = 1
    for m in range(12):
        
        temp = temp * (1 - prob_1st_monthly(theta, tstar, (t - 1) * 12 + m + 1, T, d)) 
    
    return (1 - temp)


# In[202]:


# Implementation of objective function 2
def objfun2(theta, tstar, Ts, months, years, ds):
    
    logps = np.repeat(0, len(Ts))
    logps = logps.astype("double")
    
    for i in range(len(Ts)):
        
        T = Ts[i].astype("int")
        d = ds[i]

        if not np.isnan(months[i]):

            t = months[i].astype("int")

            logps[i] = np.log(prob_2nd_monthly(theta, tstar, t, T, d))
            
            for tau in range(t + 1, T + 1):
                
                logps[i] = logps[i] + np.log(1 - prob_2nd_monthly(theta, tstar, tau, T, d))
            
        elif not np.isnan(years[i]):
            
            t = years[i].astype("int")

            logps[i] = np.log(prob_2nd_yearly(theta, tstar, t, 1, d))
            
            for tau in range((t * 12 + 1), T + 1):
            
                logps[i] = logps[i] + np.log(1 - prob_2nd_monthly(theta, tstar, tau, T, d))
            
        else:

            # HARDCODED CUTOFF DATE HERE
            for tau in range(-22, T + 1):
                
                logps[i] = logps[i] + np.log(1 - prob_2nd_monthly(theta, tstar, tau, T, d))
            
    return (-np.sum(logps))


# In[205]:


# Define prob2 functions
def prob_2nd_monthly(theta, tstar, t, T, d):
    
    temp = theta[0] * (t >= tstar) + theta[1] * (d==11) + theta[2] * (d==24) + theta[3] * (d==29) + theta[4] * t + theta[5] * t * t
    return (np.exp(temp) / (1 + np.exp(temp)))

def prob_2nd_yearly(theta, tstar, t, T, d):
        
    temp = 1
    
    for m in range(12):
            
        temp = temp * (1 - prob_2nd_monthly(theta, tstar, (t-1)*12 + m + 1, T, d)) 
        
    return (1 - temp)

