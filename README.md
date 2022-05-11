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
canada_M1

canada_ts_plot_SDR <- canada_ts[, 2]
canada_SDR <- ggplot(canada_ts_plot_SDR, aes(x = strDates)) +
    geom_line(aes(y=`National Currency per SDR: Canada`), color = "blue", alpha = 0.8, size = 1) +
    labs(title = "National Currency per SDR: Canada", x = "Date", y = "CAD per SDR", subtitle = "Log of National Currency per SDR: Canada") +
    theme_bw()
canada_SDR

canada_ts_plot_CPI <- canada_ts[, 3]
canada_CPI <- ggplot(canada_ts_plot_CPI, aes(x = strDates)) +
    geom_line(aes(y=`CPI: Canada`), color = "green", alpha = 0.8, size = 1) +
    labs(title = "CPI: Canada", x = "Date", y = "CPI", subtitle = "Log of CPI: Canada") +
    theme_bw()
canada_CPI

canada_ts_plot_interest <- canada_ts[, 4]
canada_interest <- ggplot(canada_ts_plot_interest, aes(x = strDates)) +
    geom_line(aes(y=`Interest rate: Canada`), color = "yellow", alpha = 0.8, size = 1) +
    labs(title = "Interest rate: Canada", x = "Date", y = "Interest Rate", subtitle = "Interest rate: Canada") +
    theme_bw()
canada_interest

canada_ts_plot_rGDP <- canada_ts[, 5]
canada_rGDP <- ggplot(canada_ts_plot_rGDP, aes(x = strDates)) +
    geom_line(aes(y=`Real GDP: Canada`), color = "orange", alpha = 0.8, size = 1) +
    labs(title = "Real GDP: Canada", x = "Date", y = "Real GDP", subtitle = "Log of Real GDP: Canada") +
    theme_bw()
canada_rGDP

USA_ts_plot_M1 <- USA_ts[, 3]
USA_M1 <- ggplot(USA_ts_plot_M1, aes(x = strDates)) +
    geom_line(aes(y=`M1-SA: USA`), color = "darkred", alpha = 0.8, size = 1) +
    labs(title = "M1-SA: USA", x = "Date", y = "M1-SA", subtitle = "Log of M1_SA: USA") +
    theme_bw()
USA_M1

USA_ts_plot_SDR <- USA_ts[, 6]
USA_SDR <- ggplot(USA_ts_plot_SDR, aes(x = strDates)) +
    geom_line(aes(y=`National Currency per SDR: USA`), color = "blue", alpha = 0.8, size = 1) +
    labs(title = "National Currency per SDR: USA", x = "Date", y = "USD per SDR", subtitle = "Log of National Currency per SDR: USA") +
    theme_bw()
USA_SDR

USA_ts_plot_CPI <- USA_ts[, 1]
USA_CPI <- ggplot(USA_ts_plot_CPI, aes(x = strDates)) +
    geom_line(aes(y=`CPI: USA`), color = "green", alpha = 0.8, size = 1) +
    labs(title = "CPI: USA", x = "Date", y = "CPI", subtitle = "Log of CPI: USA") +
    theme_bw()
USA_CPI

USA_ts_plot_interest <- USA_ts[, 4]
USA_interest <- ggplot(USA_ts_plot_interest, aes(x = strDates)) +
    geom_line(aes(y=`Interest rate: USA`), color = "yellow", alpha = 0.8, size = 1) +
    labs(title = "Interest rate: USA", x = "Date", y = "Interest Rate", subtitle = "Interest rate: USA") +
    theme_bw()
USA_interest

USA_ts_plot_rGDP <- USA_ts[, 5]
USA_rGDP <- ggplot(USA_ts_plot_rGDP, aes(x = strDates)) +
    geom_line(aes(y=`Real GDP: USA`), color = "orange", alpha = 0.8, size = 1) +
    labs(title = "Real GDP: USA", x = "Date", y = "Real GDP", subtitle = "Log of Real GDP: USA") +
    theme_bw()
USA_rGDP

joined_canada <- ggarrange(world_oilprice, canada_CPI, canada_M1, canada_interest, canada_rGDP, canada_SDR, ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")
joined_canada

joined_USA <- ggarrange(world_oilprice, USA_CPI, USA_M1, USA_interest, USA_rGDP, USA_SDR, ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")
joined_USA
