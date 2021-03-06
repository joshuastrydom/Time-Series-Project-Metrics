# Time-Series-Project-Metrics
Replication of "Oil prices, inflation and interest rates in a structural cointegrated VAR model for the G-7 countries"

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tibble)
pacman::p_load(tidyverse)
library(readr)
pacman::p_load(data.table)
library(data.table)
pacman::p_load(xts)
library(xts)
pacman::p_load(zoo)
library(zoo)
pacman::p_load(tsbox)
library(tsbox)
library(ggplot2)
pacman::p_load(ggpubr)
library(ggpubr)
pacman::p_load(tseries)
library(tseries)
pacman::p_load(urca)
library(urca)
pacman::p_load(vars)
library(vars)
pacman::p_load(mfilter)
library(mFilter)
pacman::p_load(forecast)
library(forecast)

IFS_03_27_2017_19_08_48_11_timeSeries_IFS_03_27_2017_19_08_48_11_timeSeries <- read_csv("Data/IFS_03-27-2017 19-08-48-11_timeSeries_IFS_03-27-2017 19-08-48-11_timeSeries.csv")


df_fulldata = as.data.frame(IFS_03_27_2017_19_08_48_11_timeSeries_IFS_03_27_2017_19_08_48_11_timeSeries)
df_fulldata <- df_fulldata |> filter(df_fulldata$`Indicator Code`=="PCPI_IX"|df_fulldata$`Indicator Code`=="ENSA_XDC_XDR_RATE"|df_fulldata$`Indicator Code`=="NGDP_R_SA_IX"|df_fulldata$`Indicator Code`=="FITB_PA"|df_fulldata$`Indicator Code`=="FM1_A2_SA_XDC"|df_fulldata$`Indicator Code`=="FM1_USD"|df_fulldata$`Indicator Code`=="FM1_SA_USD"|df_fulldata$`Indicator Code`=="PZPIOIL_USD_BBL_RATE") 
df_fulldata <- df_fulldata |> filter(df_fulldata$`Country Name`=="Canada"|df_fulldata$`Country Name`=="United States"|df_fulldata$`Country Name`=="World")


fulldata_trans <- as.data.frame(t(df_fulldata))
df_fulldata_trans <- fulldata_trans[-c(1:5),c(2,5,8,12,15,18,23,26,29,32,35,41)] |> drop_na()
setnames(df_fulldata_trans, old = c("V2", "V5", "V8", "V12", "V15", "V18","V23", "V26","V29", "V32", "V35","V41"), new = c("M1-SA: Canada", "National Currency per SDR: Canada", "CPI: Canada", "Interest rate: Canada", "Real GDP: Canada", "CPI: USA","M1: USA","M1-SA: USA","Interest rate: USA","Real GDP: USA", "National Currency per SDR: USA","Oil Price per barrel(dollars): World"))
df_fulldata_trans <- df_fulldata_trans[-c(1:20, 117:168),]
df_fulldata_trans <- tibble::rownames_to_column(df_fulldata_trans, "Date")
strDates <- c("1980Q1", "1980Q2", "1980Q3", "1980Q4","1981Q1", "1981Q2", "1981Q3", "1981Q4","1982Q1", "1982Q2", "1982Q3", "1982Q4","1983Q1", "1983Q2", "1983Q3", "1983Q4","1984Q1", "1984Q2", "1984Q3", "1984Q4","1985Q1", "1985Q2", "1985Q3", "1985Q4","1986Q1", "1986Q2", "1986Q3", "1986Q4","1987Q1", "1987Q2", "1987Q3", "1987Q4","1988Q1", "1988Q2", "1988Q3", "1988Q4","1989Q1", "1989Q2", "1989Q3", "1989Q4","1990Q1", "1990Q2", "1990Q3", "1990Q4","1991Q1","1991Q2","1991Q3","1991Q4","1992Q1", "1992Q2", "1992Q3", "1992Q4","1993Q1", "1993Q2", "1993Q3", "1993Q4","1994Q1", "1994Q2", "1994Q3", "1994Q4","1995Q1", "1995Q2", "1995Q3", "1995Q4","1996Q1", "1996Q2", "1996Q3", "1996Q4","1997Q1", "1997Q2", "1997Q3", "1997Q4","1998Q1", "1998Q2", "1998Q3", "1998Q4","1999Q1", "1999Q2", "1999Q3", "1999Q4","2000Q1", "2000Q2","2000Q3","2000Q4","2001Q1", "2001Q2","2001Q3","2001Q4","2002Q1", "2002Q2","2002Q3","2002Q4","2003Q1", "2003Q2","2003Q3","2003Q4")
strDates <- as.Date(as.yearqtr(strDates), frac = 1)
df_fulldata_trans$Date <- strDates
tibble::tibble(df_fulldata_trans)

char_columns <- sapply(df_fulldata_trans, is.character)
data_as_num <- df_fulldata_trans
data_as_num[,char_columns] <- as.data.frame(apply(data_as_num[,char_columns], 2, as.numeric))
sapply(data_as_num, class)

data_as_num[,c(2,3,4,6,7,8,9,11,12,13)] <- log(data_as_num[,c(2,3,4,6,7,8,9,11,12,13)])
data_as_num

data_ts <- xts(data_as_num[,-1], order.by = as.Date(data_as_num[,1],"%Y/%q"))
is.xts(data_ts)

canada_ts <- data_ts[,c(1:5,12)]
USA_ts <- data_ts[,c(6:12)]

canada_ts_plot_M1 <- canada_ts[, 1]
canada_M1 <- ggplot(canada_ts_plot_M1, aes(x = strDates)) +
    geom_line(aes(y=`M1-SA: Canada`), color = "darkred", alpha = 0.8, size = 1) +
    labs(title = "M1-SA: Canada", x = "Date", y = "M1-SA", subtitle = "Log of M1_SA: Canada") +
    theme_bw()

canada_ts_plot_SDR <- canada_ts[, 2]
canada_SDR <- ggplot(canada_ts_plot_SDR, aes(x = strDates)) +
    geom_line(aes(y=`National Currency per SDR: Canada`), color = "blue", alpha = 0.8, size = 1) +
    labs(title = "National Currency per SDR: Canada", x = "Date", y = "CAD per SDR", subtitle = "Log of National Currency per SDR: Canada") +
    theme_bw()

canada_ts_plot_CPI <- canada_ts[, 3]
canada_CPI <- ggplot(canada_ts_plot_CPI, aes(x = strDates)) +
    geom_line(aes(y=`CPI: Canada`), color = "green", alpha = 0.8, size = 1) +
    labs(title = "CPI: Canada", x = "Date", y = "CPI", subtitle = "Log of CPI: Canada") +
    theme_bw()

canada_ts_plot_interest <- canada_ts[, 4]
canada_interest <- ggplot(canada_ts_plot_interest, aes(x = strDates)) +
    geom_line(aes(y=`Interest rate: Canada`), color = "yellow", alpha = 0.8, size = 1) +
    labs(title = "Interest rate: Canada", x = "Date", y = "Interest Rate", subtitle = "Interest rate: Canada") +
    theme_bw()

canada_ts_plot_rGDP <- canada_ts[, 5]
canada_rGDP <- ggplot(canada_ts_plot_rGDP, aes(x = strDates)) +
    geom_line(aes(y=`Real GDP: Canada`), color = "orange", alpha = 0.8, size = 1) +
    labs(title = "Real GDP: Canada", x = "Date", y = "Real GDP", subtitle = "Log of Real GDP: Canada") +
    theme_bw()

USA_ts_plot_M1 <- USA_ts[, 3]
USA_M1 <- ggplot(USA_ts_plot_M1, aes(x = strDates)) +
    geom_line(aes(y=`M1-SA: USA`), color = "darkred", alpha = 0.8, size = 1) +
    labs(title = "M1-SA: USA", x = "Date", y = "M1-SA", subtitle = "Log of M1_SA: USA") +
    theme_bw()

USA_ts_plot_SDR <- USA_ts[, 6]
USA_SDR <- ggplot(USA_ts_plot_SDR, aes(x = strDates)) +
    geom_line(aes(y=`National Currency per SDR: USA`), color = "blue", alpha = 0.8, size = 1) +
    labs(title = "National Currency per SDR: USA", x = "Date", y = "USD per SDR", subtitle = "Log of National Currency per SDR: USA") +
    theme_bw()

USA_ts_plot_CPI <- USA_ts[, 1]
USA_CPI <- ggplot(USA_ts_plot_CPI, aes(x = strDates)) +
    geom_line(aes(y=`CPI: USA`), color = "green", alpha = 0.8, size = 1) +
    labs(title = "CPI: USA", x = "Date", y = "CPI", subtitle = "Log of CPI: USA") +
    theme_bw()

USA_ts_plot_interest <- USA_ts[, 4]
USA_interest <- ggplot(USA_ts_plot_interest, aes(x = strDates)) +
    geom_line(aes(y=`Interest rate: USA`), color = "yellow", alpha = 0.8, size = 1) +
    labs(title = "Interest rate: USA", x = "Date", y = "Interest Rate", subtitle = "Interest rate: USA") +
    theme_bw()

USA_ts_plot_rGDP <- USA_ts[, 5]
USA_rGDP <- ggplot(USA_ts_plot_rGDP, aes(x = strDates)) +
    geom_line(aes(y=`Real GDP: USA`), color = "orange", alpha = 0.8, size = 1) +
    labs(title = "Real GDP: USA", x = "Date", y = "Real GDP", subtitle = "Log of Real GDP: USA") +
    theme_bw()

joined_canada <- ggarrange(world_oilprice, canada_CPI, canada_M1, canada_interest, canada_rGDP, canada_SDR, ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")
joined_canada

joined_USA <- ggarrange(world_oilprice, USA_CPI, USA_M1, USA_interest, USA_rGDP, USA_SDR, ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")
joined_USA


level_canada <- canada_ts[,c("M1-SA: Canada", "National Currency per SDR: Canada", "CPI: Canada", "Interest rate: Canada", "Real GDP: Canada")]
nr_level_canada <- nrow(level_canada)

diff_canada <- as.data.frame(diff(as.matrix(level_canada), lag = 1))
colnames(diff_canada) <- c("M1-SA: Canada", "National Currency per SDR: Canada", "CPI: Canada", "Interest rate: Canada", "Real GDP: Canada")

adf.level_canada.none <- list(
    M1.SA = ur.df(level_canada$`M1-SA: Canada`, type = 'none', selectlags = c("BIC")),
    CAD.per.SDR = ur.df(level_canada$`National Currency per SDR: Canada`, type = 'none', selectlags = c("BIC")),
    CPI = ur.df(level_canada$`CPI: Canada`, type = 'none', selectlags = c("BIC")),
    Interest.rate = ur.df(level_canada$`Interest rate: Canada`, type = 'none', selectlags = c("BIC")),
    real.GDP = ur.df(level_canada$`Real GDP: Canada`, type = 'none', selectlags = c("BIC")))
adf.level_canada.drift <- list(
    M1.SA = ur.df(level_canada$`M1-SA: Canada`, type = 'drift', selectlags = c("BIC")),
    CAD.per.SDR = ur.df(level_canada$`National Currency per SDR: Canada`, type = 'drift', selectlags = c("BIC")),
    CPI = ur.df(level_canada$`CPI: Canada`, type = 'drift', selectlags = c("BIC")),
    Interest.rate = ur.df(level_canada$`Interest rate: Canada`, type = 'drift', selectlags = c("BIC")),
    real.GDP = ur.df(level_canada$`Real GDP: Canada`, type = 'drift', selectlags = c("BIC")))
adf.level_canada.trend <- list(
    M1.SA = ur.df(level_canada$`M1-SA: Canada`, type = 'trend', selectlags = c("BIC")),
    CAD.per.SDR = ur.df(level_canada$`National Currency per SDR: Canada`, type = 'trend', selectlags = c("BIC")),
    CPI = ur.df(level_canada$`CPI: Canada`, type = 'trend', selectlags = c("BIC")),
    Interest.rate = ur.df(level_canada$`Interest rate: Canada`, type = 'trend', selectlags = c("BIC")),
    real.GDP = ur.df(level_canada$`Real GDP: Canada`, type = 'trend', selectlags = c("BIC")))

adf.diff_canada.none <- list(
    M1.SA = ur.df(diff_canada$`M1-SA: Canada`, type = 'none', selectlags = c("BIC")),
    CAD.per.SDR = ur.df(diff_canada$`National Currency per SDR: Canada`, type = 'none', selectlags = c("BIC")),
    CPI = ur.df(diff_canada$`CPI: Canada`, type = 'none', selectlags = c("BIC")),
    Interest.rate = ur.df(diff_canada$`Interest rate: Canada`, type = 'none', selectlags = c("BIC")),
    real.GDP = ur.df(diff_canada$`Real GDP: Canada`, type = 'none', selectlags = c("BIC")))
adf.diff_canada.drift <- list(
    M1.SA = ur.df(diff_canada$`M1-SA: Canada`, type = 'drift', selectlags = c("BIC")),
    CAD.per.SDR = ur.df(diff_canada$`National Currency per SDR: Canada`, type = 'drift', selectlags = c("BIC")),
    CPI = ur.df(diff_canada$`CPI: Canada`, type = 'drift', selectlags = c("BIC")),
    Interest.rate = ur.df(diff_canada$`Interest rate: Canada`, type = 'drift', selectlags = c("BIC")),
    real.GDP = ur.df(diff_canada$`Real GDP: Canada`, type = 'drift', selectlags = c("BIC")))
adf.diff_canada.trend <- list(
    M1.SA = ur.df(diff_canada$`M1-SA: Canada`, type = 'trend', selectlags = c("BIC")),
    CAD.per.SDR = ur.df(diff_canada$`National Currency per SDR: Canada`, type = 'trend', selectlags = c("BIC")),
    CPI = ur.df(diff_canada$`CPI: Canada`, type = 'trend', selectlags = c("BIC")),
    Interest.rate = ur.df(diff_canada$`Interest rate: Canada`, type = 'trend', selectlags = c("BIC")),
    real.GDP = ur.df(diff_canada$`Real GDP: Canada`, type = 'trend', selectlags = c("BIC")))

interp_urdf <- function(urdf, level="5pct"){
  if(class(urdf) != "ur.df") 
    stop('parameter is not of class ur.df from urca package')
  if(!(level %in% c("1pct", "5pct", "10pct") ) ) 
    stop('parameter level is not one of 1pct, 5pct, or 10pct')
  #cat(???========================================================================\n???)
  cat( paste("At the", level, "level:\n") )
  if(urdf@model == "none") {
    cat("The model is of type none : "); print(urdf@testreg$call$formula)
    tau1_crit = urdf@cval["tau1",level]
    tau1_teststat = urdf@teststat["statistic","tau1"]
    tau1_teststat_wi_crit = tau1_teststat > tau1_crit
    if(tau1_teststat_wi_crit) {
      cat("tau1: The null hypothesis is not rejected, unit root is present\n")
    } else {
      cat("tau1: The null hypothesis is rejected, unit root is not present\n")
    }
  } else if(urdf@model == "drift") {
    #cat(???The model is of type drift\n???)
    cat("The model is of type drift : "); print(urdf@testreg$call$formula)
    tau2_crit = urdf@cval["tau2",level]
    tau2_teststat = urdf@teststat["statistic","tau2"]
    tau2_teststat_wi_crit = tau2_teststat > tau2_crit
    phi1_crit = urdf@cval["phi1",level]
    phi1_teststat = urdf@teststat["statistic","phi1"]
    phi1_teststat_wi_crit = phi1_teststat < phi1_crit
    if(tau2_teststat_wi_crit) {
      # Unit root present branch
      cat("tau2: The first null hypothesis is not rejected, unit root is present\n")
      if(phi1_teststat_wi_crit) {
        cat("phi1: The second null hypothesis is not rejected, unit root is present\n")
        cat("      and there is no drift.\n")
      } else {
        cat("phi1: The second null hypothesis is rejected, unit root is present\n")
        cat("      and there is drift.\n")
      }
    } else {
      # Unit root not present branch
      cat("tau2: The first null hypothesis is rejected, unit root is not present\n")
      if(phi1_teststat_wi_crit) {
        cat("phi1: The second null hypothesis is not rejected, unit root is present\n")
        cat("      and there is no drift.\n")
        warning("This is inconsistent with the first null hypothesis.")
      } else {
        cat("phi1: The second null hypothesis is rejected, unit root is not present\n")
        cat("      and there is drift.\n")
      }
    }
  } else if(urdf@model == "trend") {
    #cat(???The model is of type trend\n???)
    cat("The model is of type trend : "); print(urdf@testreg$call$formula)
    tau3_crit = urdf@cval["tau3",level]
    tau3_teststat = urdf@teststat["statistic","tau3"]
    tau3_teststat_wi_crit = tau3_teststat > tau3_crit
    phi2_crit = urdf@cval["phi2",level]
    phi2_teststat = urdf@teststat["statistic","phi2"]
    phi2_teststat_wi_crit = phi2_teststat < phi2_crit
    phi3_crit = urdf@cval["phi3",level]
    phi3_teststat = urdf@teststat["statistic","phi3"]
    phi3_teststat_wi_crit = phi3_teststat < phi3_crit
    if(tau3_teststat_wi_crit) {
      # First null hypothesis is not rejected, Unit root present branch
      cat("tau3: The first null hypothesis is not rejected, unit root is present\n")
      if(phi3_teststat_wi_crit) {
        # Second null hypothesis is not rejected
        cat("phi3: The second null hypothesis is not rejected, unit root is present\n")
        cat("      and there is no trend\n")
        if(phi2_teststat_wi_crit) {
          # Third null hypothesis is not rejected
          # a0-drift = gamma = a2-trend = 0
          cat("phi2: The third null hypothesis is not rejected, unit root is present\n")
          cat("      there is no trend, and there is no drift\n")
        } else {
          # Third null hypothesis is rejected
          cat("phi2: The third null hypothesis is rejected, unit root is present\n")
          cat("      there is no trend, and there is drift\n")
        }
      }
      else {
        # Second null hypothesis is rejected
        cat("phi3: The second null hypothesis is rejected, unit root is present\n")
        cat("      and there is trend\n")
        if(phi2_teststat_wi_crit) {
          # Third null hypothesis is not rejected
          # a0-drift = gamma = a2-trend = 0
          cat("phi2: The third null hypothesis is not rejected, unit root is present\n")
          cat("      there is no trend, and there is no drift\n")
          warning("This is inconsistent with the second null hypothesis.")
        } else {
          # Third null hypothesis is rejected
          cat("phi2: The third null hypothesis is rejected, unit root is present\n")
          cat("      there is trend, and there may or may not be drift\n")
          warning("Presence of drift is inconclusive.")
        }
      }
    } else {
      # First null hypothesis is rejected, Unit root not present branch
      cat("tau3: The first null hypothesis is rejected, unit root is not present\n")
      if(phi3_teststat_wi_crit) {
        cat("phi3: The second null hypothesis is not rejected, unit root is present\n")
        cat("      and there is no trend\n")
        warning("This is inconsistent with the first null hypothesis.")
        if(phi2_teststat_wi_crit) {
          # Third null hypothesis is not rejected
          # a0-drift = gamma = a2-trend = 0
          cat("phi2: The third null hypothesis is not rejected, unit root is present\n")
          cat("      there is no trend, and there is no drift\n")
          warning("This is inconsistent with the first null hypothesis.")
        } else {
          # Third null hypothesis is rejected
          cat("phi2: The third null hypothesis is rejected, unit root is not present\n")
          cat("      there is no trend, and there is drift\n")
        }
      } else {
        cat("phi3: The second null hypothesis is rejected, unit root is not present\n")
        cat("      and there may or may not be trend\n")
        warning("Presence of trend is inconclusive.")
        if(phi2_teststat_wi_crit) {
          # Third null hypothesis is not rejected
          # a0-drift = gamma = a2-trend = 0
          cat("phi2: The third null hypothesis is not rejected, unit root is present\n")
          cat("      there is no trend, and there is no drift\n")
          warning("This is inconsistent with the first and second null hypothesis.")
        } else {
          # Third null hypothesis is rejected
          cat("phi2: The third null hypothesis is rejected, unit root is not present\n")
          cat("      there may or may not be trend, and there may or may not be drift\n")
          warning("Presence of trend and drift is inconclusive.")
        }
      }
    }
  } else warning('urdf model type is not one of none, drift, or trend')
  cat("========================================================================\n")
}

#For M1-SA variable:
#Level
adf.level.n.canada.M1.SA = ur.df(level_canada$`M1-SA: Canada`, type = 'none', selectlags = c("BIC"))
adf.level.d.canada.M1.SA = ur.df(level_canada$`M1-SA: Canada`, type = 'drift', selectlags = c("BIC"))
adf.level.t.canada.M1.SA = ur.df(level_canada$`M1-SA: Canada`, type = 'trend', selectlags = c("BIC"))
interp_urdf(adf.level.n.canada.M1.SA, "5pct")
interp_urdf(adf.level.d.canada.M1.SA, "5pct")
interp_urdf(adf.level.t.canada.M1.SA, "5pct")
#Differenced
adf.diff.n.canada.M1.SA = ur.df(diff_canada$`M1-SA: Canada`, type = 'none', selectlags = c("BIC"))
adf.diff.d.canada.M1.SA = ur.df(diff_canada$`M1-SA: Canada`, type = 'drift', selectlags = c("BIC"))
adf.diff.t.canada.M1.SA = ur.df(diff_canada$`M1-SA: Canada`, type = 'trend', selectlags = c("BIC"))
interp_urdf(adf.diff.n.canada.M1.SA, "5pct")
interp_urdf(adf.diff.d.canada.M1.SA, "5pct")
interp_urdf(adf.diff.t.canada.M1.SA, "5pct")

#For CAD.per.SDR
#Level
adf.level.n.canada.CAD.per.SDR = ur.df(level_canada$`National Currency per SDR: Canada`, type = 'none', selectlags = c("BIC"))
adf.level.d.canada.CAD.per.SDR = ur.df(level_canada$`National Currency per SDR: Canada`, type = 'drift', selectlags = c("BIC"))
adf.level.t.canada.CAD.per.SDR = ur.df(level_canada$`National Currency per SDR: Canada`, type = 'trend', selectlags = c("BIC"))
interp_urdf(adf.level.n.canada.CAD.per.SDR, "5pct")
interp_urdf(adf.level.d.canada.CAD.per.SDR, "5pct")
interp_urdf(adf.level.t.canada.CAD.per.SDR, "5pct")
#Differenced
adf.diff.n.canada.CAD.per.SDR = ur.df(diff_canada$`National Currency per SDR: Canada`, type = 'none', selectlags = c("BIC"))
adf.diff.d.canada.CAD.per.SDR = ur.df(diff_canada$`National Currency per SDR: Canada`, type = 'drift', selectlags = c("BIC"))
adf.diff.t.canada.CAD.per.SDR = ur.df(diff_canada$`National Currency per SDR: Canada`, type = 'trend', selectlags = c("BIC"))
interp_urdf(adf.diff.n.canada.CAD.per.SDR, "5pct")
interp_urdf(adf.diff.d.canada.CAD.per.SDR, "5pct")
interp_urdf(adf.diff.t.canada.CAD.per.SDR, "5pct")

#For CPI
#Level
adf.level.n.canada.CPI = ur.df(level_canada$`CPI: Canada`, type = 'none', selectlags = c("BIC"))
adf.level.d.canada.CPI = ur.df(level_canada$`CPI: Canada`, type = 'drift', selectlags = c("BIC"))
adf.level.t.canada.CPI = ur.df(level_canada$`CPI: Canada`, type = 'trend', selectlags = c("BIC"))
interp_urdf(adf.level.n.canada.CPI, "5pct")
interp_urdf(adf.level.d.canada.CPI, "5pct")
interp_urdf(adf.level.t.canada.CPI, "5pct")
#Differenced
adf.diff.n.canada.CPI = ur.df(diff_canada$`CPI: Canada`, type = 'none', selectlags = c("BIC"))
adf.diff.d.canada.CPI = ur.df(diff_canada$`CPI: Canada`, type = 'drift', selectlags = c("BIC"))
adf.diff.t.canada.CPI = ur.df(diff_canada$`CPI: Canada`, type = 'trend', selectlags = c("BIC"))
interp_urdf(adf.diff.n.canada.CPI, "5pct")
interp_urdf(adf.diff.d.canada.CPI, "5pct")
interp_urdf(adf.diff.t.canada.CPI, "5pct")

#For Interest.rate
#Level
adf.level.n.canada.Interest.rate = ur.df(level_canada$`Interest rate: Canada`, type = 'none', selectlags = c("BIC"))
adf.level.d.canada.Interest.rate = ur.df(level_canada$`Interest rate: Canada`, type = 'drift', selectlags = c("BIC"))
adf.level.t.canada.Interest.rate = ur.df(level_canada$`Interest rate: Canada`, type = 'trend', selectlags = c("BIC"))
interp_urdf(adf.level.n.canada.Interest.rate, "5pct")
interp_urdf(adf.level.d.canada.Interest.rate, "5pct")
interp_urdf(adf.level.t.canada.Interest.rate, "5pct")
#Differenced
adf.diff.n.canada.Interest.rate = ur.df(diff_canada$`Interest rate: Canada`, type = 'none', selectlags = c("BIC"))
adf.diff.d.canada.Interest.rate = ur.df(diff_canada$`Interest rate: Canada`, type = 'drift', selectlags = c("BIC"))
adf.diff.t.canada.Interest.rate = ur.df(diff_canada$`Interest rate: Canada`, type = 'trend', selectlags = c("BIC"))
interp_urdf(adf.diff.n.canada.Interest.rate, "5pct")
interp_urdf(adf.diff.d.canada.Interest.rate, "5pct")
interp_urdf(adf.diff.t.canada.Interest.rate, "5pct")

#For real.GDP
#Level
adf.level.n.canada.rGDP = ur.df(level_canada$`Real GDP: Canada`, type = 'none', selectlags = c("BIC"))
adf.level.d.canada.rGDP = ur.df(level_canada$`Real GDP: Canada`, type = 'drift', selectlags = c("BIC"))
adf.level.t.canada.rGDP = ur.df(level_canada$`Real GDP: Canada`, type = 'trend', selectlags = c("BIC"))
interp_urdf(adf.level.n.canada.rGDP, "5pct")
interp_urdf(adf.level.d.canada.rGDP, "5pct")
interp_urdf(adf.level.t.canada.rGDP, "5pct")
#Differenced
adf.diff.n.canada.rGDP = ur.df(diff_canada$`Real GDP: Canada`, type = 'none', selectlags = c("BIC"))
adf.diff.d.canada.rGDP = ur.df(diff_canada$`Real GDP: Canada`, type = 'drift', selectlags = c("BIC"))
adf.diff.t.canada.rGDP = ur.df(diff_canada$`Real GDP: Canada`, type = 'trend', selectlags = c("BIC"))
interp_urdf(adf.diff.n.canada.rGDP, "5pct")
interp_urdf(adf.diff.d.canada.rGDP, "5pct")
interp_urdf(adf.diff.t.canada.rGDP, "5pct")


level_USA <- USA_ts[,c("M1-SA: USA", "National Currency per SDR: USA", "CPI: USA", "Interest rate: USA", "Real GDP: USA")]
nr_level_USA <- nrow(level_USA)

diff_USA <- as.data.frame(diff(as.matrix(level_USA), lag = 1))
colnames(diff_USA) <- c("M1-SA: USA", "National Currency per SDR: USA", "CPI: USA", "Interest rate: USA", "Real GDP: USA")

adf.level_USA.none <- list(
    M1.SA = ur.df(level_USA$`M1-SA: USA`, type = 'none', selectlags = c("BIC")),
    USD.per.SDR = ur.df(level_USA$`National Currency per SDR: USA`, type = 'none', selectlags = c("BIC")),
    CPI = ur.df(level_USA$`CPI: USA`, type = 'none', selectlags = c("BIC")),
    Interest.rate = ur.df(level_USA$`Interest rate: USA`, type = 'none', selectlags = c("BIC")),
    real.GDP = ur.df(level_USA$`Real GDP: USA`, type = 'none', selectlags = c("BIC")))
adf.level_USA.drift <- list(
    M1.SA = ur.df(level_USA$`M1-SA: USA`, type = 'drift', selectlags = c("BIC")),
    USD.per.SDR = ur.df(level_USA$`National Currency per SDR: USA`, type = 'drift', selectlags = c("BIC")),
    CPI = ur.df(level_USA$`CPI: USA`, type = 'drift', selectlags = c("BIC")),
    Interest.rate = ur.df(level_USA$`Interest rate: USA`, type = 'drift', selectlags = c("BIC")),
    real.GDP = ur.df(level_USA$`Real GDP: USA`, type = 'drift', selectlags = c("BIC")))
adf.level_USA.trend <- list(
    M1.SA = ur.df(level_USA$`M1-SA: USA`, type = 'trend', selectlags = c("BIC")),
    USD.per.SDR = ur.df(level_USA$`National Currency per SDR: USA`, type = 'trend', selectlags = c("BIC")),
    CPI = ur.df(level_USA$`CPI: USA`, type = 'trend', selectlags = c("BIC")),
    Interest.rate = ur.df(level_USA$`Interest rate: USA`, type = 'trend', selectlags = c("BIC")),
    real.GDP = ur.df(level_USA$`Real GDP: USA`, type = 'trend', selectlags = c("BIC")))

adf.diff_USA.none <- list(
    M1.SA = ur.df(diff_USA$`M1-SA: USA`, type = 'none', selectlags = c("BIC")),
    USD.per.SDR = ur.df(diff_USA$`National Currency per SDR: USA`, type = 'none', selectlags = c("BIC")),
    CPI = ur.df(diff_USA$`CPI: USA`, type = 'none', selectlags = c("BIC")),
    Interest.rate = ur.df(diff_USA$`Interest rate: USA`, type = 'none', selectlags = c("BIC")),
    real.GDP = ur.df(diff_USA$`Real GDP: USA`, type = 'none', selectlags = c("BIC")))
adf.diff_USA.drift <- list(
    M1.SA = ur.df(diff_USA$`M1-SA: USA`, type = 'drift', selectlags = c("BIC")),
    USD.per.SDR = ur.df(diff_USA$`National Currency per SDR: USA`, type = 'drift', selectlags = c("BIC")),
    CPI = ur.df(diff_USA$`CPI: USA`, type = 'drift', selectlags = c("BIC")),
    Interest.rate = ur.df(diff_USA$`Interest rate: USA`, type = 'drift', selectlags = c("BIC")),
    real.GDP = ur.df(diff_USA$`Real GDP: USA`, type = 'drift', selectlags = c("BIC")))
adf.diff_USA.trend <- list(
    M1.SA = ur.df(diff_USA$`M1-SA: USA`, type = 'trend', selectlags = c("BIC")),
    USD.per.SDR = ur.df(diff_USA$`National Currency per SDR: USA`, type = 'trend', selectlags = c("BIC")),
    CPI = ur.df(diff_USA$`CPI: USA`, type = 'trend', selectlags = c("BIC")),
    Interest.rate = ur.df(diff_USA$`Interest rate: USA`, type = 'trend', selectlags = c("BIC")),
    real.GDP = ur.df(diff_USA$`Real GDP: USA`, type = 'trend', selectlags = c("BIC")))    

#For M1-SA variable:
#Level
adf.level.n.USA.M1.SA = ur.df(level_USA$`M1-SA: USA`, type = 'none', selectlags = c("BIC"))
adf.level.d.USA.M1.SA = ur.df(level_USA$`M1-SA: USA`, type = 'drift', selectlags = c("BIC"))
adf.level.t.USA.M1.SA = ur.df(level_USA$`M1-SA: USA`, type = 'trend', selectlags = c("BIC"))
interp_urdf(adf.level.n.USA.M1.SA, "5pct")
interp_urdf(adf.level.d.USA.M1.SA, "5pct")
interp_urdf(adf.level.t.USA.M1.SA, "5pct")
#Differenced
adf.diff.n.USA.M1.SA = ur.df(diff_USA$`M1-SA: USA`, type = 'none', selectlags = c("BIC"))
adf.diff.d.USA.M1.SA = ur.df(diff_USA$`M1-SA: USA`, type = 'drift', selectlags = c("BIC"))
adf.diff.t.USA.M1.SA = ur.df(diff_USA$`M1-SA: USA`, type = 'trend', selectlags = c("BIC"))
interp_urdf(adf.diff.n.USA.M1.SA, "5pct")
interp_urdf(adf.diff.d.USA.M1.SA, "5pct")
interp_urdf(adf.diff.t.USA.M1.SA, "5pct")

#For CAD.per.SDR
#Level
adf.level.n.USA.USD.per.SDR = ur.df(level_USA$`National Currency per SDR: USA`, type = 'none', selectlags = c("BIC"))
adf.level.d.USA.USD.per.SDR = ur.df(level_USA$`National Currency per SDR: USA`, type = 'drift', selectlags = c("BIC"))
adf.level.t.USA.USD.per.SDR = ur.df(level_USA$`National Currency per SDR: USA`, type = 'trend', selectlags = c("BIC"))
interp_urdf(adf.level.n.USA.USD.per.SDR, "5pct")
interp_urdf(adf.level.d.USA.USD.per.SDR, "5pct")
interp_urdf(adf.level.t.USA.USD.per.SDR, "5pct")
#Differenced
adf.diff.n.USA.USD.per.SDR = ur.df(diff_USA$`National Currency per SDR: USA`, type = 'none', selectlags = c("BIC"))
adf.diff.d.USA.USD.per.SDR = ur.df(diff_USA$`National Currency per SDR: USA`, type = 'drift', selectlags = c("BIC"))
adf.diff.t.USA.USD.per.SDR = ur.df(diff_USA$`National Currency per SDR: USA`, type = 'trend', selectlags = c("BIC"))
interp_urdf(adf.diff.n.USA.USD.per.SDR, "5pct")
interp_urdf(adf.diff.d.USA.USD.per.SDR, "5pct")
interp_urdf(adf.diff.t.USA.USD.per.SDR, "5pct")

#For CPI
#Level
adf.level.n.USA.CPI = ur.df(level_USA$`CPI: USA`, type = 'none', selectlags = c("BIC"))
adf.level.d.USA.CPI = ur.df(level_USA$`CPI: USA`, type = 'drift', selectlags = c("BIC"))
adf.level.t.USA.CPI = ur.df(level_USA$`CPI: USA`, type = 'trend', selectlags = c("BIC"))
interp_urdf(adf.level.n.USA.CPI, "5pct")
interp_urdf(adf.level.d.USA.CPI, "5pct")
interp_urdf(adf.level.t.USA.CPI, "5pct")
#Differenced
adf.diff.n.USA.CPI = ur.df(diff_USA$`CPI: USA`, type = 'none', selectlags = c("BIC"))
adf.diff.d.USA.CPI = ur.df(diff_USA$`CPI: USA`, type = 'drift', selectlags = c("BIC"))
adf.diff.t.USA.CPI = ur.df(diff_USA$`CPI: USA`, type = 'trend', selectlags = c("BIC"))
interp_urdf(adf.diff.n.USA.CPI, "5pct")
interp_urdf(adf.diff.d.USA.CPI, "5pct")
interp_urdf(adf.diff.t.USA.CPI, "5pct")

#For Interest.rate
#Level
adf.level.n.USA.Interest.rate = ur.df(level_USA$`Interest rate: USA`, type = 'none', selectlags = c("BIC"))
adf.level.d.USA.Interest.rate = ur.df(level_USA$`Interest rate: USA`, type = 'drift', selectlags = c("BIC"))
adf.level.t.USA.Interest.rate = ur.df(level_USA$`Interest rate: USA`, type = 'trend', selectlags = c("BIC"))
interp_urdf(adf.level.n.USA.Interest.rate, "5pct")
interp_urdf(adf.level.d.USA.Interest.rate, "5pct")
interp_urdf(adf.level.t.USA.Interest.rate, "5pct")
#Differenced
adf.diff.n.USA.Interest.rate = ur.df(diff_USA$`Interest rate: USA`, type = 'none', selectlags = c("BIC"))
adf.diff.d.USA.Interest.rate = ur.df(diff_USA$`Interest rate: USA`, type = 'drift', selectlags = c("BIC"))
adf.diff.t.USA.Interest.rate = ur.df(diff_USA$`Interest rate: USA`, type = 'trend', selectlags = c("BIC"))
interp_urdf(adf.diff.n.USA.Interest.rate, "5pct")
interp_urdf(adf.diff.d.USA.Interest.rate, "5pct")
interp_urdf(adf.diff.t.USA.Interest.rate, "5pct")

#For real.GDP
#Level
adf.level.n.USA.rGDP = ur.df(level_USA$`Real GDP: USA`, type = 'none', selectlags = c("BIC"))
adf.level.d.USA.rGDP = ur.df(level_USA$`Real GDP: USA`, type = 'drift', selectlags = c("BIC"))
adf.level.t.USA.rGDP = ur.df(level_USA$`Real GDP: USA`, type = 'trend', selectlags = c("BIC"))
interp_urdf(adf.level.n.USA.rGDP, "5pct")
interp_urdf(adf.level.d.USA.rGDP, "5pct")
interp_urdf(adf.level.t.USA.rGDP, "5pct")
#Differenced
adf.diff.n.USA.rGDP = ur.df(diff_USA$`Real GDP: USA`, type = 'none', selectlags = c("BIC"))
adf.diff.d.USA.rGDP = ur.df(diff_USA$`Real GDP: USA`, type = 'drift', selectlags = c("BIC"))
adf.diff.t.USA.rGDP = ur.df(diff_USA$`Real GDP: USA`, type = 'trend', selectlags = c("BIC"))
interp_urdf(adf.diff.n.USA.rGDP, "5pct")
interp_urdf(adf.diff.d.USA.rGDP, "5pct")
interp_urdf(adf.diff.t.USA.rGDP, "5pct")    


level_world <- USA_ts[,7]
nr_level_world <- nrow(level_world)

diff_world <- as.data.frame(diff(as.matrix(level_world), lag = 1))
colnames(diff_world) <- c("Oil Price per barrell(dollars): World")


adf.level_world.none <- list(
    Oil.price = ur.df(level_world, type = 'none', selectlags = c("BIC")))
adf.level_world.drift <- list(
    Oil.price = ur.df(level_world, type = 'drift', selectlags = c("BIC")))
adf.level_world.trend <- list(
    Oil.price = ur.df(level_world, type = 'trend', selectlags = c("BIC")))

adf.diff_world.none <- list(
    Oil.price = ur.df(diff_world, type = 'none', selectlags = c("BIC")))
adf.diff_world.drift <- list(
    Oil.price = ur.df(diff_world, type = 'drift', selectlags = c("BIC")))
adf.diff_world.trend <- list(
    Oil.price = ur.df(diff_world, type = 'trend', selectlags = c("BIC")))
    
#For Oil.price variable:
#Level
adf.level.n.world.Oil.price = ur.df(level_world, type = 'none', selectlags = c("BIC"))
adf.level.d.world.Oil.price = ur.df(level_world, type = 'drift', selectlags = c("BIC"))
adf.level.t.world.Oil.price = ur.df(level_world, type = 'trend', selectlags = c("BIC"))
interp_urdf(adf.level.n.world.Oil.price, "5pct")
interp_urdf(adf.level.d.world.Oil.price, "5pct")
interp_urdf(adf.level.t.world.Oil.price, "5pct")
##This indicates that a unit root is present, there is no trend, and there is no drift
#Differenced
adf.diff.n.world.Oil.price = ur.df(diff_world, type = 'none', selectlags = c("BIC"))
adf.diff.d.world.Oil.price = ur.df(diff_world, type = 'drift', selectlags = c("BIC"))
adf.diff.t.world.Oil.price = ur.df(diff_world, type = 'trend', selectlags = c("BIC"))
interp_urdf(adf.diff.n.world.Oil.price, "5pct")
interp_urdf(adf.diff.d.world.Oil.price, "5pct")
interp_urdf(adf.diff.t.world.Oil.price, "5pct")
##This indicates that there is no unit root, there is drift and there may or may not be a trend. 
##The logarithm of M1-SA contains a unit root and can be stationary time series by differencing the first order.    