# Financial Econometrics with R - Midterm assignment
#-----------------------------------------------
# Authors   : Pellerin Gautier
# version  : v6, 18/11/2022
#-----------------------------------------------


# Package downloading
install.packages("TTR")     # req. for quantomod
install.packages("xts")     # req. for quantomod
install.packages("zoo")     # req. for 
install.packages("readr")   # fast read .csv (and any) file, set each variable to right format (char, name, num)
install.packages('latex2exp')
#install.packages('gridExtra')
install.packages('summarytools')
install.packages('qwraps2')
install.packages('nortest') 
install.packages('moments')
install.packages("xtable") # produce nice tables 
install.packages("sm")     # nice kernel estimation
install.packages('astsa')  # acf1 and acf2 (both acf and pacf together)

# setup WD
setwd("C:\\Users\\gautp\\OneDrive\\Documents\\EDHEC\\S1 -2022\\Financial econometrics with R\\stylized_facts")
getwd()
# WARNING:installation of package "portes" does not work as it should, as the file has been taken out form CRAN due to compativbility issues, still we need it!
# install.packages('forecast')
install.packages("portes_3.0.tar.gz", repos=NULL, type="source")


#install.packages('xlsx')
#install.packages('timeSeries')
install.packages('forecast')
# install.packages('quantmod')
install.packages('quantmod')

#######################################################################
# LOAD ALL THE PACKAGES/LIBRARIES (already installed)
#######################################################################
# library(ggplot2)   # produce good looking graphs (qplot)

library(quantmod)  # allows to easily import data directly from downloading financial data from the internet, directly
# from some open sources, including Yahoo Finance, Google Finance, and the Federal
# Reserve Economic Data (FRED) of Federal Reserve Bank of St. Louis.
library(xts)
library(readr)
library(latex2exp) # to wtie latex formulas in graphs!
#library(gridExtra) # multiple plots in one graph
library(summarytools)
library(qwraps2)
# library(normtest) -> replaced by nortest ?
library(nortest)
library(moments)
library(xtable)
library(sm)
library(astsa)
library(portes)
#library(xlsx)
library(timeSeries)
#library(forecast)
library(portes)
library(forecast)
library(tseries)

rm(list=ls()) # clear all variables from environment/workspace

cac40_with_nan    <- getSymbols("^FCHI",from="1998-03-10", to="2021-12-31", auto.assign=FALSE)
days_total  <- length(cac40_with_nan)
days_total
nb_Nan = sum(is.na(cac40_with_nan$FCHI.Adjusted))
nb_Nan
cac40 <- na.omit(cac40_with_nan)


# we consider only the adjusted prices
# daily prices
Pt_d  <- cac40$FCHI.Adjusted ; names(Pt_d)  <- "Pt_d" # Prices
# compute the log prices
pt_d  <- log(Pt_d)       ; names(Pt_d)  <- "pt_d" # log -prices

# find end of month/year dates
last_day_of_month <- endpoints(pt_d, on = "months") # works on xts objects
last_day_of_year  <- endpoints(pt_d, on = "years")

# Compute  monthly(m) and annual(y) log prices
pt_m <- pt_d[last_day_of_month]; names(pt_m)   <- "pt_m"
pt_y <- pt_d[last_day_of_year] ; names(pt_y)   <- "pt_y"

# compute log returns for the time period
rt_d  <- diff(pt_d)     ; names(rt_d)  <- "rt_d"
rt_m  <- diff(pt_m)     ; names(rt_m)  <- "rt_m"
rt_y  <- diff(pt_y)     ; names(rt_y)  <- "rt_y"


# convert prices into dataframes 
Pt_d_df <- cbind(index(Pt_d), data.frame(Pt_d)); names(Pt_d_df)[1] <- "date";
pt_d_df <- cbind(index(pt_d), data.frame(pt_d)); names(pt_d_df)[1] <- "date";
rt_d_df <- cbind(index(rt_d), data.frame(rt_d)); names(rt_d_df)[1] <- "date";
rt_m_df <- cbind(index(rt_m), data.frame(rt_m)); names(rt_m_df)[1] <- "date";
rt_y_df <- cbind(index(rt_y), data.frame(rt_y)); names(rt_y_df)[1] <- "date";

#create an object containing daily, monthly and annual log return
X <-list("daily" = rt_d_df[-1,2],
         "montly" = rt_m_df[-1,2],
         "annual" = rt_y_df[-1,2]);


#  Create function computes the summary statistic provided in class
###############################################
multi.fun <- function(x) {
  c(Mean = mean(x)*100, 
    St.Deviation = sd(x)*100,
    Diameter.C.I.Mean =  qnorm(0.975)*sqrt(var(x)/length(x))*100,
    Skewness=moments::skewness(x),
    Kurtosis=moments::kurtosis(x),
    Excess.Kurtosis=moments::kurtosis(x)-3,
    Min    = min(x)*100, 
    Quant  = quantile(x, probs = 0.05)*100,
    Quant  = quantile(x, probs = 0.25)*100,
    Median = quantile(x, probs = 0.50)*100,
    Quant  = quantile(x, probs = 0.75)*100,
    Quant  = quantile(x, probs = 0.95)*100,
    Max    = max(x)*100,
    Jarque.Bera.stat = jarque.bera.test(x)$statistic,
    Jarque.Bera.pvalue.X100 = jarque.bera.test(x)$p.value*100,
    Lillie.test.stat = lillie.test(x)$statistic,
    Lillie.test.pvalue.X100 = lillie.test(x)$p.value*100,
    N.obs = length(x) 
  )}

###############################################
# GENERATE TABLE 1 in slide 91

summary_stats <- sapply(X, multi.fun) # apply function to all elements of list X, 
round(summary_stats, digits = 5) # round to 5 digits

# # SHOW TABLE IN LATEX
n_series <- length(X);
n_stats  <- nrow(summary_stats)
digits_m <- rbind(4*(matrix(1,n_stats-1,n_series+1)),           
                  0*(matrix(1,1,n_series+1)))
xtable(summary_stats, digits = digits_m)




###############################################
#Stylized fact one : prices are not stationary
###############################################



# We compute ACF for daily & monthly log-prices
par(mfrow=c(1,2))
Autocorrelations_daily = acf(pt_d, main="Autocorrelations of the daily Prices")
Autocorrelations_monthly = acf(pt_m, main="Autocorrelations of the monthly Prices")
dev.off()

# We compute and draw the Variation of the mean & variance over time
par(mfrow=c(1,2))
mean_overTime <- vector(mode="numeric", length = nrow(cac40))
var_overTime <- vector(mode="numeric", length = nrow(cac40))
for (i in 0:nrow(cac40)) {
  mean_overTime[i] = mean(pt_d[0:i,])
  var_overTime[i] = var(cac40$FCHI.Adjusted[0:i,])
}
plot(mean_overTime, type="s", main="Mean value of daily log prices from day 0 to day x(i)", xlab="time (nb of days)", ylab="pt", col="red")
plot(var_overTime, type="s", main="Variance value of daily log prices from day 0 to day x(i)", xlab="time (nb of days)", ylab="var(pt)", col="blue")
dev.off()




###############################################
#Stylized fact two & six : prices are not stationary
###############################################



# we compute Ljung-box test , chi square val and create the tab we will display
# first for daily return
max_lag          <- 16
all_lag      <- seq(1,max_lag,1)
LjungBox     <- LjungBox(rt_d_df[-1,2], lags=all_lag)
crit.value.5.LB <- qchisq(0.95,all_lag) 

lb_d <- cbind(LjungBox[,1],
              LjungBox[,2],
              LjungBox[,4],
              crit.value.5.LB)

lb_d_df <-as.data.frame(lb_d)
names(lb_d_df)  <- c("lag","LB stat","LB pval","chi 2")
rownames(lb_d_df) <-c()
options(scipen = 999)
LB_d <- data.matrix(lb_d_df)
# to display table in r console : 
# round(LB_d, digits = 5) # round to 5 digits

# # SHOW TABLE IN LATEX 
n_series <- length(lb_d_df);
n_stats  <- nrow(LB_d)
digits_m <- rbind(4*(matrix(1,n_stats-1,n_series+1)),           
                  0*(matrix(1,1,n_series+1)))
xtable(LB_d, digits = digits_m)

# # For monthly return
LjungBox     <- LjungBox(rt_m_df[-1,2], lags=all_lag)

lb_m <- cbind(LjungBox[,2],
              LjungBox[,4])

lb_m_df <-as.data.frame(lb_m)
names(lb_m_df)  <- c("LB stat","LB pval")
rownames(lb_m_df) <-c()
options(scipen = 999)
LB_m <- data.matrix(lb_m_df)
# to display table in r console : 
# round(LB_m, digits = 5) # round to 5 digits


# # SHOW TABLE IN LATEX 
n_series <- length(lb_m_df);
n_stats  <- nrow(LB_m)
digits_m <- rbind(4*(matrix(1,n_stats-1,n_series+1)),           
                  0*(matrix(1,1,n_series+1)))
xtable(LB_m, digits = digits_m)


# We use Acf function and not acf to remove the first autocorrelation
par(mfrow=c(1,3))
Autocorrelations_return_daily = Acf(rt_d[2:nrow(cac40),], main="daily ")
Autocorrelations_return_monthly = Acf(rt_m[2:nrow(rt_m),], main="monthly ")
Autocorrelations_return_yearly = Acf(rt_y[2:nrow(rt_y),], main="yearly ")
dev.off()

# Empirical evidence of mean and variance finite values over time
mean_rt_overTime <- vector(mode="numeric", length = nrow(cac40))
var_rt_overTime <- vector(mode="numeric", length = nrow(cac40))

par(mfrow=c(1,2))
for (i in 2:nrow(cac40)) {
  mean_rt_overTime[i] = mean(rt_d[2:i,])
  var_rt_overTime[i] = var(rt_d[2:i,])
}
plot(mean_rt_overTime, type="s", main="Mean value variation over time", xlab="time (nb of days)", ylab="pt", col="red")
plot(var_rt_overTime, type="s", main="Variance value variation over time", xlab="time (nb of days)", ylab="var(pt)", col="blue")
dev.off()




###############################################
#Stylized Fact 3 & Stylized Fact 4
###############################################




# We draw histograms & Normal distribution over it
par(mfrow=c(2,1))
# daily returns 
hist_rt_d <- hist(rt_d_df[,2], freq = FALSE, breaks = 50, col="lightgreen",  xlab="", main=TeX('daily log-return'), )
norm_y <-  dnorm(hist_rt_d$mids, mean=mean(rt_d_df[,2], na.rm=TRUE), sd=sd(rt_d_df[,2], na.rm=TRUE));
lines(x=hist_rt_d$mids, y=norm_y,col="red", lwd=1)

# monthly returns 
hist_rt_m <- hist(rt_m_df[,2], freq = FALSE, breaks = 50, col="lightgreen",  xlab="", main=TeX('monthly log-return'), )
norm_y <-  dnorm(hist_rt_m$mids, mean=mean(rt_m_df[,2], na.rm=TRUE), sd=sd(rt_m_df[,2], na.rm=TRUE));
lines(x=hist_rt_m$mids, y=norm_y,col="red", lwd=1)
dev.off()
# draw of Q-Q plot & normal distribution over it 
par(mfrow=c(1,2))
qqnorm(rt_d_df[-1,2], pch = 1, frame = FALSE, xlab="Normal Quantiles", main=TeX('daily log-return'))
qqline(rt_d_df[-1,2], col = "red", lwd = 2)

qqnorm(rt_m_df[-1,2], pch = 1, frame = FALSE, xlab="Normal Quantiles", main=TeX('monthly log-return'))
qqline(rt_m_df[-1,2], col = "red", lwd = 2)




###############################################
#Stylized Fact 5:  Aggregational Gaussianity
###############################################




#  Create a function to compute JB & Lillie test & p-value
gaussianity_func <- function(x) {
  c(Jarque.Bera.stat = jarque.bera.test(x)$statistic,
    Jarque.Bera.pvalue.X100 = jarque.bera.test(x)$p.value*100,
    Lillie.test.stat = lillie.test(x)$statistic,
    Lillie.test.pvalue.X100 = lillie.test(x)$p.value*100,
    N.obs = length(x) 
  )}

gauss_stat <- sapply(X, gaussianity_func) # apply function to all elements of list X, 

# display table in r console : 
round(gauss_stat, digits = 5) # round to 5 digits
# # SHOW TABLE IN LATEX
n_series <- length(X);
n_stats  <- nrow(gauss_stat)

digits_m <- rbind(4*(matrix(1,n_stats-1,n_series+1)),           
                  0*(matrix(1,1,n_series+1)))
xtable(gauss_stat, digits = digits_m)
dev.off()



###############################################
#Stylized Fact 7 : Volatility clustering and long range dependence of squared returns 
###############################################



# ACF of squared returns
par(mfrow=c(1,2))
lag.max.acf = 80;  lim.y.axes = c(-0.10,0.30)
daily_squared_return = (rt_d_df[,2])^2 ; # daily squared returns 
Acf(daily_squared_return, main=TeX('Daily squared returns : $r_t^2$'), lag.max = lag.max.acf, xlab = "lag in days", ylim=lim.y.axes)
monthly_squared_return = (rt_m_df[,2])^2; # monthly squared returns 
Acf(monthly_squared_return, main=TeX('Monthly squared returns : $r_t^{m 2}$'), lag.max = lag.max.acf, xlab = "lag in months", ylim=lim.y.axes)
dev.off()
##########################################################
# ACF of absolute returns
par(mfrow=c(1,2))
lag.max.acf = 80;  lim.y.axes = c(-0.10,0.30)
daily_abs_return = abs(rt_d_df[,2]) ; # daily abs returns 
Acf(daily_abs_return, main=TeX('Daily absolute returns : $|r_t|$'), lag.max = lag.max.acf, xlab = "lag in days", ylim=lim.y.axes)
monthly_abs_return = abs(rt_m_df[,2]); # monthly abs returns 
Acf(monthly_abs_return, main=TeX('Monthly absolute returns : $|r_t^{m}$|'), lag.max = lag.max.acf, xlab = "lag in months", ylim=lim.y.axes)
dev.off()



##########################################################
# Create tab for Ljung-Box test for daily squared and absolute return
# daily squared returns

rt_d_squared <- (rt_d_df[2:nrow(rt_d_df),2])^2 ; 

# Custom table with LjungBox statistics
LjungBox     <- LjungBox(rt_d_squared, lags=all_lag)

lb_d_squared <- cbind(LjungBox[,1],
                      LjungBox[,2],
                      LjungBox[,4])

lb_d_squared_df <-as.data.frame(lb_d_squared)
names(lb_d_squared_df)  <- c("lag","LB stat","LB pval")
rownames(lb_d_squared_df) <-c()
options(scipen = 999)
LB_d_squared <- data.matrix(lb_d_squared_df)
# to display table in r console : 
# round(LB_d_squared, digits = 5) # round to 5 digits

# # SHOW TABLE IN LATEX
n_series <- length(lb_d_squared_df);
n_stats  <- nrow(LB_d_squared)

digits_m <- rbind(4*(matrix(1,n_stats-1,n_series+1)),           
                  0*(matrix(1,1,n_series+1)))
xtable(LB_d_squared, digits = digits_m)


##########################################################
# daily absolute returns 
rt_d_abs <- abs((rt_d_df[2:nrow(rt_d_df),2])) ; 

# same custom table
my.LjungBox     <- LjungBox(rt_d_abs, lags=all_lag)
lb_d_abs <- cbind(my.LjungBox[,1],
                  my.LjungBox[,2],
                  my.LjungBox[,4])

lb_d_abs_df <-as.data.frame(lb_d_abs)
names(lb_d_abs_df)  <- c("lag","LB stat","LB pval")
rownames(lb_d_abs_df) <-c()
options(scipen = 999)
LB_d_abs <- data.matrix(lb_d_abs_df)
# to display table in r console : 
# round(LB_d_abs, digits = 5) # round to 5 digits

# # SHOW TABLE IN LATEX
n_series <- length(lb_d_abs_df);
n_stats  <- nrow(LB_d_abs)

digits_m <- rbind(4*(matrix(1,n_stats-1,n_series+1)),           
                  0*(matrix(1,1,n_series+1)))
xtable(LB_d_abs, digits = digits_m)




##########################################################
#Stylized Fact 8 , Leverage effect
##########################################################




# CROSS-COVARIANCE OF DAILY RETURNS with squared returns
# ccf function

d_return = (rt_d_df[-1,2])   ; # daily returns 
d_squared_return  = (rt_d_df[-1,2])^2 ; # daily SQUARED returns 

par(mfrow=c(1,1))
ccf(d_return, d_squared_return, lag.max = 10, type = "correlation",  plot = TRUE, 
    main=TeX('Cross-correlation between daily $r_{t+j}$ and $r_t^2$ = corr($r_{t+j}$, $r_{t}^2$)'), 
    xlab = TeX('lag $j$ in days'), ylab=TeX('Cross-correlation'))
dev.off()

