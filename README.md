# Time-Series-Project-Metrics
Replication of "Oil prices, inflation and interest rates in a structural cointegrated VAR model for the G-7 countries"

df_fulldata = data.frame(FULLDATAPROPER)
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tibble)
pacman::p_load(tidyverse)

##Creating data set for Canada
canada_data <-  df_fulldata |> 
    filter(Country.Name == "Canada") |> 
    filter(Indicator.Code == "ENSA_XDC_XDR_RATE"|Indicator.Code == "PCPI_IX"|Indicator.Code == "NGDP_R_SA_XDC"|Indicator.Code == "FITB_PA") |>
    filter() #Filter for monetray aggregate

canada_data <- canada_data |> select(-contains("M"))
df_canada <- data.frame(canada_data)
tibble::tibble(df_canada)

##Creating data set for USA
USA_data <-  df_fulldata |> 
    filter(Country.Name == "United States") |> 
    filter(Indicator.Code == "ENSA_XDC_XDR_RATE"|Indicator.Code == "PCPI_IX"|Indicator.Code == "NGDP_R_SA_XDC"|Indicator.Code == "FITB_PA") |>
    filter() #Filter for monetray aggregate

USA_data <- USA_data |> select(-contains("M"))
df_USA <- data.frame(USA_data)
tibble::tibble(df_USA)

