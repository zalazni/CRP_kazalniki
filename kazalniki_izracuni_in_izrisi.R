setwd("C:/Users/ZalaZn/OneDrive - Univerza v Ljubljani/projekti/CRP kazalniki 2024/delo") # služba
library(graphics)
library(ismev)
library(readxl)
# library(ie2misc)
library(ggplot2)
library(openxlsx)
library(dplyr)
library(pollen)
library(tidyr)
library(chillR)
library(ChillModels)
library(tidyverse)
library(viridis)
library(cowplot)
library(ggspatial)
library(sf)
library(ncdf4)
library(ClimInd)
library(pracma)
library(spData)

library(maxnet)
library(dismo) 
library(lubridate)
library(fruclimadapt)
library(data.table)


###################################################################
#                                                                 #
# TO JE KODA ZA IZRACUN NABORA celoletnih KAZALNIKOV ZA KORUZO    #
#         in psenico ZA OBDOBJE 1981-2011                         #
#                                                                 #
###################################################################


###################################################################
# Open a connection to the first file in our list
path <- "C:/Users/ZalaZn/OneDrive - Univerza v Ljubljani/DR/podatki_hist/OPSI/" # sluzba

nuts3_mapdata <- st_read("C:/Users/ZalaZn/OneDrive - Univerza v Ljubljani/DR/NUTS3_ID.gpkg")
slovenia_nuts3_mapdata <- filter(nuts3_mapdata, cntr_code=="SI") # filtriram samo slovenske NUTS3 regije

nc_data <- nc_open(paste0(path,"evspsblpot","_12km_ARSO_v5_day_19810101_20101231.nc"))
names(nc_data$var)
print(nc_data)
lon <- ncvar_get(nc_data, "X") # drugi parameter je ime spremenljivke v datoteki
lat <- ncvar_get(nc_data, "Y")
start_lon <- 13.2 
end_lon <- 16.8

start_lat<-45.3 #+ 25/60 +18.34/3600
end_lat<-47 #+ 54/60 + 37.52/3600

time <- ncvar_get(nc_data, "time")
nx = length(lon)
ny = length(lat)
start1<-c(1,1,1)
count1<-c(nx,ny, 10957)
############################################################# 
# ET
data <- ncvar_get(nc_data,"evspsblpot",start=start1, count=count1)
data_et <- data
#############################################################
library(pracma)
library(spData)
library(sf)

pretvorba_sek_v_dan1 = 60*60*24 #?eprav imam dnevne podatke moram to dat, ker as.positxct meri v sekundah

########### 
# povpre?na dnevna T
datumi0 <- seq(0,count1[3]-1,by = 1)
datumi <- as.POSIXct((start1[3]+datumi0)*pretvorba_sek_v_dan1,origin="1980-12-31 00:00:00")
leta <- format(datumi,format="%Y")
meseci <- format(datumi,format="%m")
dnevi <- format(datumi,format="%d")

nc_data <- nc_open(paste0(path,"tas","_12km_ARSO_v5_day_19810101_20101231.nc"))
data3 <- ncvar_get(nc_data,"tas",start=start1, count=count1)
# d3 <- flipdim(dataPOVP, 2)
datum2 = format(as.POSIXct(datumi, format="%Y-%m-%d"), "%m/%d/%Y")
# dataPOVP<-structure(data3, .Names = datum2)
# data3 <- data.frame(datum = datum2, leto = leta, tg = data3[,,1:10957])
nc_close(nc_data) # konec branja

# minimalna dnevna T
nc_data_min <- nc_open(paste0(path,"tasmin","_12km_ARSO_v5_day_19810101_20101231.nc"))
data_min <- ncvar_get(nc_data_min, "tasmin", start=start1, count=count1)
dataMIN<-structure(data_min, .Names = datum2)
# data_min <- data.frame(datum = datumi, leto = leta, tn = data_min)
nc_close(nc_data_min) # konec branja

# maksimalna dnevna T
nc_data_max<- nc_open(paste0(path,"tasmax","_12km_ARSO_v5_day_19810101_20101231.nc"))
data_max <- ncvar_get(nc_data_max, "tasmax", start=start1, count=count1)
# dataMAX<-structure(data_max, .Names = datum2)
# data_max <- data.frame(datum = datumi, leto = leta, tx = data_max)
nc_close(nc_data_max) # konec branja

# padavine dnevne
nc_data_rr<- nc_open(paste0(path,"pr","_12km_ARSO_v5_day_19810101_20101231.nc"))
data_rr <- ncvar_get(nc_data_rr, "pr", start=start1, count=count1)
# dataRR<-structure(data_rr, .Names = datum2)
# data_rr <- data.frame(datum = datumi, leto = leta, rr = data_rr)
nc_close(nc_data_rr) # konec branja

# evapotranspiracija
nc_data_et<- nc_open(paste0(path,"evspsblpot","_12km_ARSO_v5_day_19810101_20101231.nc"))
data_et <- ncvar_get(nc_data_et, "evspsblpot", start=start1, count=count1)
nc_close(nc_data_et) # konec branja

# celotna bliznja okolica Slovenije, letne vrednosti agroklimatskih kazalnikov za vseh 30 let izracunamo

lats1 <- seq(1,24,by=1)
lons1 <- seq(1,40,by=1)


########################## 
j = 12
i = 20

# funkcije
calculate_late_frost <- function(df) {
  df <- df %>% arrange(Date) # Ensure data is sorted by date
  df$Year <- format(as.POSIXct(df$Date, format="%m/%d/%Y"), "%Y")
  df$LateFrost <- FALSE
  years <- unique(df$Year)
  results_frost <- data.frame(Year = integer(), LateFrostCount = integer())
  for (current_year in years) {
    year_data <- df %>% filter(Year == current_year)
    start_date <- NA        # Find start of 10°C period
    for (m in 1:(nrow(year_data) - 4)) {
      mean_temp_5days <- mean(year_data$Tmean[m:(m + 4)])
      if (mean_temp_5days >= 10) {
        start_date <- year_data$Date[m + 4] # Use the last day of the 5-day period
        break  }  }
    if (!is.na(start_date)) {
      # Check for late frost after the start date
      late_frost_days <- year_data %>%
        filter(Date > start_date, Tmin <= 0)
      if(nrow(late_frost_days) > 0){
        df$LateFrost[df$Date %in% late_frost_days$Date] <- TRUE          }
      late_frost_count <- nrow(late_frost_days)
      results_frost <- rbind(results_frost, data.frame(Year = current_year, LateFrostCount = late_frost_count))
    } else {
      results_frost <- rbind(results_frost, data.frame(Year = current_year, LateFrostCount = 0))}  #No 5 day period with temp > 10
  }
  return(list(df = df, results_frost = results_frost))
}

CSDI_optimized <- function(tmin_data) {
  below_threshold <- tmin_data < podatki_percentil_10_tmin2$Tn10p  # Uporaba `rle` (Run Length Encoding) za iskanje zaporednih nizov
  rle_result <- rle(below_threshold)
  cold_spell_lengths <- rle_result$lengths[rle_result$values == TRUE & rle_result$lengths >= 6]
  csdi_days_count <- sum(cold_spell_lengths)
  return(csdi_days_count)
}

Cold_spell_duration <- function(df) {
  df$year <- format(as.POSIXct(df$Date, format="%m/%d/%Y"), "%Y")
  annual_csdi <- df %>%
    group_by(year) %>%
    summarise(csdi_annual_sum = CSDI_optimized(Tmin)) %>%
    ungroup()
  return(annual_csdi)
}

warm_wet_days <- function(data) {
  data <- data %>% mutate(warm_wet = (Tmean > podatki_percentil_75_tmean2$T75p) & (RR > podatki_percentil_75_rr2$rr75p))
  data$year <- format(as.POSIXct(data$date, format="%m/%d/%Y"), "%Y")
  warm_wet_days_count <- data %>%
    filter(warm_wet) %>%
    group_by(year) %>%
    summarize(warm_wet_days = n(),
              .groups = "drop")
  return(warm_wet_days_count)
}             



podatki_percentil_90_tmin<-data.frame(latsi=1,lonsi=1,T90p=1)
podatki_percentil_99_tmax<-data.frame(latsi=1,lonsi=1,Tx99p=1)
podatki_percentil_90_tmax<-data.frame(latsi=1,lonsi=1,Tx90p=1)
podatki_percentil_10_tmin<-data.frame(latsi=1,lonsi=1,Tn10p=1)
podatki_percentil_75_tmean<-data.frame(latsi=1,lonsi=1,T75p=1)
podatki_percentil_75_rr<-data.frame(latsi=1,lonsi=1,rr75p=1)


vsi_podatki <- data.frame()
for(i in lons1){
  for(j in lats1){
    print(i)
    print(j)
    if(all(is.na(data3[i,j,1:10957]))){
      next
    }
    else{
      data_temp <- data.frame(Date = datum2, meseci = as.numeric(meseci), Temp = data3[i,j,1:10957]-273)
      # data_temp <- data_temp %>% filter(meseci >= 4, meseci <= 10)                        # s filtriranjem po mesecih dobimo verzijo kazalnikov za koruzo

      data_tmax <- data.frame(Date = datum2, meseci = as.numeric(meseci), Tmax = data_max[i,j,1:10957]-273)
      # data_tmax <- data_tmax %>% filter(meseci >= 4, meseci <= 10)                        # s filtriranjem po mesecih dobimo verzijo kazalnikov za koruzo

      data_tmin <- data.frame(Date = datum2, meseci = as.numeric(meseci), Tmin = data_min[i,j,1:10957]-273)
      # data_tmin <- data_tmin %>% filter(meseci >= 4, meseci <= 10)                        # s filtriranjem po mesecih dobimo verzijo kazalnikov za koruzo
      
      data_pad <- data.frame(Date = datum2, meseci = as.numeric(meseci), pad = data_rr[i,j,1:10957]*24*60*60)
      # data_pad <- data_pad %>% filter(meseci >= 4, meseci <= 10)                        # s filtriranjem po mesecih dobimo verzijo kazalnikov za koruzo)
      
      data_etp <- data.frame(Date = datum2, meseci = as.numeric(meseci), et = data_et[i,j,1:10957]*24*60*60)
      # data_etp <- data_etp %>% filter(meseci >= 4, meseci <= 10)                        # s filtriranjem po mesecih dobimo verzijo kazalnikov za koruzo

      percentil90<-data.frame(latsi=lat[j],lonsi=lon[i],T90p=as.numeric(quantile(data_tmin$Tmin,0.9))) # tega ne rabim za proj.
      #podatki_percentil_90_tmin = readrRDS("tn90p_referencno_obdobje.rds")
      podatki_percentil_90_tmin<-rbind(podatki_percentil_90_tmin,percentil90) # tega ne rabim za proj.
      podatki_percentil_90_tmin2<- subset(podatki_percentil_90_tmin, as.numeric(latsi)==lat[j]  & podatki_percentil_90_tmin$lon==as.numeric(lon[i]))
      tn90p = podatki_percentil_90_tmin2$T90p
      data_tmin_90p<-data_tmin
      # data_tmin_90p$Datum <-as.Date(data_tmin_90p$Date)
      data_tmin_90p$leto <-format(as.POSIXct(data_tmin_90p$Date, format="%m/%d/%Y"), "%Y")
      t90_test <- data_tmin_90p %>%
        mutate(tmin90 = ifelse(Tmin >= podatki_percentil_90_tmin2$T90p,1,0),ind1=1)
      t90_test <- t90_test %>%
        group_by(leto) %>% summarise(sum=sum(tmin90),
                  n=sum(ind1), tn90p = sum/n *100)

      percentil99x<-data.frame(latsi=lat[j],lonsi=lon[i],Tx99p=as.numeric(quantile(data_tmax$Tmax,0.99))) # tega ne rabim za proj.
      #podatki_percentil_99_tmax = readrRDS("tx99p_referencno_obdobje.rds")
      podatki_percentil_99_tmax<-rbind(podatki_percentil_99_tmax,percentil99x) # tega ne rabim za proj.
      podatki_percentil_99_tmax2<- subset(podatki_percentil_99_tmax, as.numeric(latsi)==lat[j]  & podatki_percentil_99_tmax$lon==as.numeric(lon[i]))
      tx99p = podatki_percentil_99_tmax2$Tx99p
      data_tmax_99p<-data_tmax
      # data_tmax_99p$Datum <-as.Date(data_tmax_99p$Date)
      data_tmax_99p$leto <-format(as.POSIXct(data_tmax_99p$Date, format="%m/%d/%Y"), "%Y")
      tx99_test <- data_tmax_99p %>%
        mutate(tmax99 = ifelse(Tmax >= podatki_percentil_99_tmax2$Tx99p,1,0),ind1=1)
      tx99_test <- tx99_test %>%
        group_by(leto) %>% summarise(sum=sum(tmax99),
                                     n=sum(ind1), tx99p = sum)

      percentil90x<-data.frame(latsi=lat[j],lonsi=lon[i],Tx90p=as.numeric(quantile(data_tmax$Tmax,0.90))) # tega ne rabim za proj.
      #podatki_percentil_90_tmax = readrRDS("tx90p_referencno_obdobje.rds")
      podatki_percentil_90_tmax<-rbind(podatki_percentil_90_tmax,percentil90x) # tega ne rabim za proj.
      podatki_percentil_90_tmin2<- subset(podatki_percentil_90_tmax, as.numeric(latsi)==lat[j]  & podatki_percentil_90_tmax$lon==as.numeric(lon[i]))
      tx90p = podatki_percentil_90_tmin2$Tx90p
      # data_tmax_90p<-data_tmax
      # data_tmax_90p$leto <-format(as.POSIXct(data_tmax_90p$Date, format="%m/%d/%Y"), "%Y")
      # tx90_test <- data_tmax_90p %>%
      #   mutate(tmax90 = ifelse(Tmax >= percentil90x$Tx90p,1,0),ind1=1)
      # tx90_test <- tx90_test %>%
      #   group_by(leto) %>% summarise(sum=sum(tmax90),
      #                                n=sum(ind1), tx90p = sum)
      data_tmax_90p <- data_tmax %>%
        mutate(leto = format(as.POSIXct(Date, format = "%m/%d/%Y"), "%Y"),
          tmax90 = ifelse(Tmax >= tx90p, 1, 0),
          ind1 = 1)
      consecutive_days_list <- data_tmax_90p %>%
        group_by(leto) %>%
        group_split() %>%
        lapply(function(year_data) {above_threshold <- year_data$tmax90
          consecutive_groups <- rle(above_threshold)
          consecutive_days <- sum(consecutive_groups$lengths[consecutive_groups$values == 1 & consecutive_groups$lengths >= 6])
          return(consecutive_days)
        })
      consecutive_days_df <- data.frame(leto = unique(data_tmax_90p$leto), consecutive_days = unlist(consecutive_days_list))
      summary_df <- data_tmax_90p %>%
        group_by(leto) %>%
        summarise(sum = sum(tmax90),n = sum(ind1))
      result_df <- left_join(summary_df, consecutive_days_df, by = "leto")

      growing_degree_days <- gd4(structure(data_temp$Temp, .Names = data_temp$Date))
      Growing_season_length <- gsl(structure(data_temp$Temp, .Names = data_temp$Date))
      End_growing_season <- ogs6(structure(data_temp$Temp, .Names = data_temp$Date)) + Growing_season_length + 90
      Sums_Tmax32 <- stx32(structure(data_tmax$Tmax, .Names = data_tmax$Date)) ###
      TG_of_warmest_quarter <- bio10(structure(data_temp$Temp, .Names = data_temp$Date)) #Kelvini
      dtr <- (data_tmax$Tmax+273)/(data_temp$Temp+273)
      dtr1 <- (data_tmin$Tmin+273)/(data_temp$Temp+273)
      Diurnal_temp_rangeN <-	dtr(structure(dtr, .Names = data_temp$Date),structure(dtr1, .Names = data_temp$Date))
      # Days_Tmax32	<- d32(structure(data_tmax$Tmax, .Names = data_tmax$Date)) ###
      cons_summer_days <- csd(structure(data_tmax$Tmax, .Names = data_tmax$Date))
      # Warm_spell_duration <- wsdi(structure(data_tmax$Tmax, .Names = data_tmax$Date))

      # Dry_days <- dd(structure(data_pad$pad, .Names = data_pad$Date))
      # Prec_deficit	<- -1*ep(structure(data_etp$et, .Names = data_etp$Date), structure(data_pad$pad, .Names = data_pad$Date))
      Max_consecutive_dry_days <- cdd(structure(data_pad$pad, .Names = data_pad$Date))
      longest_wet_period <- cwd(structure(data_pad$pad, .Names = data_pad$Date))
      # r10mm <- r10mm(structure(data_pad$pad, .Names = data_pad$Date))
      r20mm <- r20mm(structure(data_pad$pad, .Names = data_pad$Date))
      Heavy_prec_days <- d50mm(structure(data_pad$pad, .Names = data_pad$Date))
      # wet_days <- dr1mm(structure(data_pad$pad, .Names = data_pad$Date))
      SDII <- sdii(structure(data_pad$pad, .Names = data_pad$Date))
      Prec_wettest_month	<- bio13(structure(data_pad$pad, .Names = data_pad$Date))
      # Prec_warmest_quarter	<- bio18(structure(data_pad$pad, .Names = data_pad$Date),structure(data_temp$Temp, .Names = data_temp$Date))
      # Prec_coldest_quarter	<- bio19(structure(data_pad$pad, .Names = data_pad$Date),structure(data_temp$Temp, .Names = data_temp$Date))
      frost_days <- fd(structure(data_tmin$Tmin, .Names = data_tmin$Date))

      percentil10Tmin<-data.frame(latsi=lat[j],lonsi=lon[i],Tn10p=as.numeric(quantile(data_tmin$Tmin,0.10))) # tega ne rabim za proj.   OD TUKAJ
      #podatki_percentil10Tmin = readrRDS("tn10p_referencno_obdobje_psenica+koruza.rds")
      podatki_percentil_10_tmin<-rbind(podatki_percentil_10_tmin,percentil10Tmin) # tega ne rabim za proj.
      podatki_percentil_10_tmin2<- subset(podatki_percentil_10_tmin, as.numeric(latsi)==lat[j]  & podatki_percentil_10_tmin$lon==as.numeric(lon[i]))


      CSDI = Cold_spell_duration(data_tmin)

      cons_frost_days <- cfd(structure(data_tmin$Tmin, .Names = data_tmin$Date))
      ice_days <- id(structure(data_tmax$Tmax, .Names = data_tmax$Date))
      # Very_wet_days	<- d95p(structure(data_pad$pad, .Names = data_pad$Date))
      Effective_prec	<- ep(structure(data_etp$et, .Names = data_etp$Date), structure(data_pad$pad, .Names = data_pad$Date))
      Growing_season_prec	<- gsr(structure(data_pad$pad, .Names = data_pad$Date)) ###
      Nongrowing_season_prec <- ngsr(structure(data_pad$pad, .Names = data_pad$Date))
      # precip_total <- rti(structure(data_pad$pad, .Names = data_pad$Date))

      su <- su(structure(data_tmax$Tmax, .Names = data_tmax$Date))
      tr <- tr(structure(data_tmin$Tmin, .Names = data_tmin$Date))
      # tn90p <- tn90p(structure(data_tmin$Tmin, .Names = data_tmin$Date))
      # vwd <- vwd(data = structure(data_tmax$Tmax, .Names = data_tmax$Date))
      # tn90p
      Sums_Tmin10 <- stn10(structure(data_tmin$Tmin, .Names = data_tmax$Date))
      Sums_Tmin15 <- stn15(structure(data_tmin$Tmin, .Names = data_tmax$Date))

      percentil75T<-data.frame(latsi=lat[j],lonsi=lon[i],T75p=as.numeric(quantile(data_temp$Temp,0.75))) # tega ne rabim za proj.   OD TUKAJ
      #podatki_percentil_75_t = readrRDS("t75p_referencno_obdobje.rds")
      podatki_percentil_75_tmean<-rbind(podatki_percentil_75_tmean,percentil75T) # tega ne rabim za proj.
      podatki_percentil_75_tmean2<- subset(podatki_percentil_75_tmean, as.numeric(latsi)==lat[j]  & podatki_percentil_75_tmean$lon==as.numeric(lon[i]))

      percentil75rr<-data.frame(latsi=lat[j],lonsi=lon[i],rr75p=as.numeric(quantile(data_pad$pad,0.75))) # tega ne rabim za proj.
      #podatki_percentil_75_rr = readrRDS("rr75p_referencno_obdobje.rds")
      podatki_percentil_75_rr<-rbind(podatki_percentil_75_rr,percentil75rr) # tega ne rabim za proj.
      podatki_percentil_75_rr2<- subset(podatki_percentil_75_rr, as.numeric(latsi)==lat[j]  & podatki_percentil_75_rr$lon==as.numeric(lon[i]))

      data_wwd <- data.frame(date = data_tmax$Date, Tmean = data_temp$Temp, RR = data_pad$pad)
      WWD <- warm_wet_days(data_wwd)
      temp_day1 <- data.frame(Year = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%Y"),
                              Month = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%m"),
                              Day = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%d"),
                              temp = data_temp$Temp)
      x<-aggregate(temp ~  Year + Month, temp_day1, FUN = mean, na.rm=TRUE, na.action=na.pass)
      warmest_m_temp <- x %>% group_by(Year) %>%
        summarise(max = max(temp))

      WWD$Year <- WWD$year
      WWD <- left_join(warmest_m_temp,WWD,by="Year") %>%
        mutate_if(is.numeric,coalesce,0)

      df <- data.frame(Date = data_temp$Date, Tmean = data_temp$Temp, Tmin = data_tmin$Tmin)
      # df$Date <- as.Date(df$Date)
      late_frost_analysis <- calculate_late_frost(df) # df <- late_frost_analysis$df
      results_frost <- late_frost_analysis$results_frost


      flowering_heat_sum <- 703.42
      maturity_heat_sum <- 1616.8


      df_heat_stress <- data.frame(Date = data_temp$Date, Tmax = data_tmax$Tmax, temp = data_temp$Temp)
      calculate_heat_sum <- function(tmax_data) {
        gdd_daily <- pmax(0, tmax_data$temp - 10)
        return(cumsum(gdd_daily))
      }
      is_heat_stress <- function(tmax_window) {# Function to check for heat stress (2-day period above 35°C)
        all(tmax_window > 35)
      }
      df_heat_stress$Year <- format(as.POSIXct(df_heat_stress$Date, format="%m/%d/%Y"), "%Y")
      results <- data.frame(Year = unique(df_heat_stress$Year), HeatStressDaysFL = NA, HeatStressDaysMT = NA)
      for (current_year in unique(df_heat_stress$Year)) {
        year_data <- subset(df_heat_stress, Year == current_year)
        year_data <- subset(year_data, temp >=10)
        year_data$HeatSum <- calculate_heat_sum(year_data)
        start_dateFL <- min(year_data$Date)
        end_dateFL <- year_data$Date[which.min(abs(year_data$HeatSum - flowering_heat_sum))]

        end_date <- year_data$Date[which.min(abs(year_data$HeatSum - maturity_heat_sum))]
        start_date <- end_dateFL
        # print(c(start_date,end_date,start_dateFL,end_dateFL))
        if (!is.na(start_date) && !is.na(end_date)) {  # Only proceed if both dates are found
          flowering_data <- subset(year_data, Date >= start_dateFL & Date <= end_dateFL)
          maturity_data <- subset(year_data, Date >= start_date & Date <= end_date)
          if (nrow(flowering_data) >= 2){ # Check if there are at least 2 days of flowering data
            heat_stress_eventsFL <- zoo::rollapply(flowering_data$Tmax, width = 2, FUN = is_heat_stress, fill = NA, align = "left")
            heat_stress_daysFL <- sum(heat_stress_eventsFL, na.rm = TRUE) * 2
          }
          if (nrow(maturity_data) >= 2){ # Check if there are at least 2 days of flowering data
            heat_stress_events <- zoo::rollapply(maturity_data$Tmax, width = 2, FUN = is_heat_stress, fill = NA, align = "left")
            heat_stress_days <- sum(heat_stress_events, na.rm = TRUE) * 2
          }
          else {
            heat_stress_days <- 0
            heat_stress_daysFL <- 0# No heat stress days if flowering period is less than 2 days
          }
        } else {
          heat_stress_days <- 0
          heat_stress_daysFL <- 0# Or NA, if you prefer to indicate that flowering period couldn't be determined
          cat("Flowering period not found for year", current_year, "\n")
        }
        results$HeatStressDaysFL[results$Year == current_year] <- heat_stress_daysFL
        results$HeatStressDaysMT[results$Year == current_year] <- heat_stress_days
      }

      TG_of_coldest_quarter	<- bio11(structure(data_temp$Temp, .Names = data_temp$Date))
      rx1day <- rx1day(structure(data_pad$pad, .Names = data_pad$Date))
      rx5d <- rx5d(structure(data_pad$pad, .Names = data_pad$Date))
      r95tot <- r95tot(structure(data_pad$pad, .Names = data_pad$Date))

      temp_day <- data.frame(Year = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%Y"),
                             Month = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%m"),
                             Day = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%d"),
                             Tmin = data_tmin$Tmin, Tmax = data_tmax$Tmax)
      climdata <- hourly_temps(temp_day, latitude = lat[i])
      chill_portions0 <- chill_portions(climdata, Start = 214)
      chill_portions <- aggregate(Chill ~  Year, chill_portions0, FUN = sum, na.rm=TRUE, na.action=na.pass)[,2]

      BEDD0 <- head(GDD_linear(temp_day, Tb = 10, Tu = 30),-1)
      BEDD <- aggregate(GDD ~  Year, BEDD0, FUN = sum, na.rm=TRUE, na.action=na.pass)[,2]

      pod_celi <- data.frame(datum = data_temp$Date,
                             leto = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%Y"),
                             mesec = as.numeric(format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%m")),
                             tg = data_tmax$Tmax)
      st_dni0 <- pod_celi %>% filter(tg >= 30)
      st_dni30 <- st_dni0 %>% group_by(leto) %>% summarise(tg = sum(tg-30))
      warmest_m_temp$leto <- warmest_m_temp$Year
      Dnevi_30max <- left_join(warmest_m_temp,st_dni30,by="leto") %>%
        mutate_if(is.numeric,coalesce,0)
      Dnevi_30max <- Dnevi_30max[,3:4] #Plant heat stress = accumulated daily maximum temperature values above 30?C

      st_dni32_anthesis <- pod_celi %>% filter(tg >= 32, mesec >= 05, mesec <= 06)
      Dnevi_32max_anthesis <- st_dni32_anthesis %>% group_by(leto) %>% count()
      Dnevi_32max_anthesis <- left_join(warmest_m_temp[,3],Dnevi_32max_anthesis,by="leto") %>%
        mutate_if(is.numeric,coalesce,0) # Days with Tmax above 32 ?C

      podatki_tocka <- data.frame(lats = rep(lat[j], length(growing_degree_days)),
                                  lons = rep(lon[i], length(growing_degree_days)),
                                  leto = unique(WWD$Year),
                                  growing_degree_days,
                                  # Dnevi_30max = Dnevi_30max$tg,
                                  GSL = Growing_season_length,
                                  End_growing_season,
                                  BEDD,
                                  Sums_Tmax32,
                                  T_warmest_m = warmest_m_temp$max,
                                  TG_of_warmest_quarter,
                                  TG_of_coldest_quarter,
                                  Diurnal_temp_rangeN,
                                  chill_portions,
                                  Dnevi_32max_anthesis = Dnevi_32max_anthesis$n, #Days_Tmax32,
                                  Heat_stress_fl = results$HeatStressDaysFL,
                                  Heat_stress_mat = results$HeatStressDaysMT,
                                  cons_summer_days,
                                  WWD = WWD$warm_wet_days,
                                  WSDI = result_df$consecutive_days,
                                  CDD = Max_consecutive_dry_days,#DD = Dry_days, Prec_deficit,
                                  CWD = longest_wet_period,
                                  r20mm,
                                  Heavy_prec_days,# r10mm,
                                  SDII,
                                  Prec_wettest_month, #Prec_warmest_quarter, Prec_coldest_quarter,wet_days,
                                  FD = frost_days,
                                  late_frost_days = results_frost$LateFrostCount,
                                  CSDI,
                                  CFD = cons_frost_days,
                                  ice_days,
                                  Sums_Tmin10, Sums_Tmin15,
                                  Effective_prec,
                                  Growing_season_prec,#Very_wet_days,
                                  Nongrowing_season_prec,
                                  # precip_total,
                                  su,
                                  tr,
                                  tn90p = t90_test$tn90p,
                                  vwd = tx99_test$tx99p,
                                  rx5d)
      vsi_podatki <- rbind(vsi_podatki, podatki_tocka)
      }
  }
}
saveRDS(podatki_percentil_90_tmin, file = "tn90p_referencno_obdobje_psenica+koruza.rds")
saveRDS(podatki_percentil_99_tmax, file = "tx99p_referencno_obdobje_psenica+koruza.rds")
saveRDS(podatki_percentil_90_tmax, file = "tx90p_referencno_obdobje_psenica+koruza.rds")
saveRDS(podatki_percentil_75_tmean, file = "t75p_referencno_obdobje_psenica+koruza.rds")
saveRDS(podatki_percentil_75_rr, file = "rr75p_referencno_obdobje_psenica+koruza.rds")
saveRDS(podatki_percentil_10_tmin, file = "tn10p_referencno_obdobje_psenica+koruza.rds")

# vsi_podatki = subset(vsi_podatki, select = -c(Year,SPEI_3,year) )
saveRDS(vsi_podatki, file = "kazalniki_OPSI_historical_psenica+koruza.rds") # ce bi racunali za koruzo za GS, bi odkomentirali spodnjo vrstico:
# saveRDS(vsi_podatki, file = "kazalniki_OPSI_historical_GS.rds")

vsi_podatki$ID<-seq(1,nrow(vsi_podatki))


###################################################################
#                                                                 #
#         IZRAČUN PROJEKCIJ KAZALNIKOV ZA KORUZO IN PŠENICO       #
#                                                                 #
###################################################################

# risanje kart in časovnih potekov z ovojnicami se začne okoli vrstice 1200

path <- "C:/Users/ZalaZn/OneDrive - Univerza v Ljubljani/DR/podatki_hist/OPSI/" # sluzba
nuts3_mapdata <- st_read("C:/Users/ZalaZn/OneDrive - Univerza v Ljubljani/DR/NUTS3_ID.gpkg")
slovenia_nuts3_mapdata <- filter(nuts3_mapdata, cntr_code=="SI") # filtriram samo slovenske NUTS3 regije

nc_data <- nc_open(paste0(path,"evspsblpot","_12km_ARSO_v5_day_19810101_20101231.nc"))
names(nc_data$var)
print(nc_data)
lon <- ncvar_get(nc_data, "X") # drugi parameter je ime spremenljivke v datoteki
lat <- ncvar_get(nc_data, "Y")
start_lon <- 13.2 # meje https://sl.wikipedia.org/wiki/Geografija_Slovenije
end_lon <- 16.8

start_lat<-45.3 #+ 25/60 +18.34/3600
end_lat<-47 #+ 54/60 + 37.52/3600

time <- ncvar_get(nc_data, "time")
nx = length(lon)
ny = length(lat)

########################## HIST KORUZA

# Risanje kazalnikov
# vsi_podatki0 <- vsi_podatki
vsi_podatki <- readRDS(file = "C:/Users/ZalaZn/OneDrive - Univerza v Ljubljani/projekti/CRP kazalniki 2024/kazalniki_OPSI_historical_GS.rds")
vsi_podatki <- subset(vsi_podatki, select = -c(TG_of_coldest_quarter,chill_portions,Dnevi_32max_anthesis,year,Sums_Tmin10,Sums_Tmin15,Nongrowing_season_prec)) # izlocimo kazalnike za psenico

# yrs <- unique(years(datumi))
# vsi_podatki$ID<-seq(1,nrow(vsi_podatki))

kaz <- names(vsi_podatki)[4:34]
kaz
# 
# zac = 1981
# kon = 2010
# 
# 
# for(k in seq(1,length(kaz),by=1)){
#   data <- data.frame(vsi_podatki$lons, vsi_podatki$lats, vsi_podatki$leto, vsi_podatki[[kaz[k]]])
#   colnames(data) <- c("lons","lats","leto","agroclim_ind")
#   data <- data %>% filter(leto >= zac, leto <= kon)
#   data=na.omit(data)
#   min = min(na.omit(vsi_podatki[[kaz[k]]]))
#   max = max(na.omit(vsi_podatki[[kaz[k]]]))
#   
#   povp <- data %>% group_by(lats,lons) %>%
#     summarise(period_mean = mean(agroclim_ind))
#   ggplot() +
#     geom_tile(data=povp,aes(x=lons,y=lats,fill=period_mean)) +
#     geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
#     scale_fill_distiller("povp_obd",palette = "Spectral", limits = c(min,max)) +
#     # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
#     labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("Kazalnik ",kaz[k],", obdobje ",zac,"-",kon)) +
#     coord_sf(crs = st_crs(4326)) +
#     theme_light(base_size = 17)
#   ggsave(paste0("karte/hist_period_mean_",kaz[k],"_",zac,"-",kon,".png"),width = 10, height = 7)
# }

########## PROJEKCIJE KORUZA

######################################### PROJEKCIJE MEDIANA MODELOV 2071 - 2100 KORUZA

model <- c("CNRM-CERFACS-CNRM-CM5", 
           "ICHEC-EC-EARTH",
           "IPSL-IPSL-CM5A-MR",
           "MOHC-HadGEM2-ES", # samo do 30. 11. 2099 pri padavinah, zato tudi pri Tmin in Tmax .nc fajl spremenim, da gre le do 30.11.
           "MPI-M-MPI-ESM-LR",
           "MPI-M-MPI-ESM-LR")
drugo <- c("_r1i1p1_CLMcom-CCLM4-8-17_v1",
           "_r3i1p1_DMI-HIRHAM5_v1",
           "_r1i1p1_IPSL-INERIS-WRF331F_v1",
           "_r1i1p1_KNMI-RACMO22E_v2",
           "_r1i1p1_CLMcom-CCLM4-8-17_v1",
           "_r1i1p1_SMHI-RCA4_v1a")

get_nc_data <- function(path, model1, rcp, drugo1, leto_zac, leto_kon1, leto_kon_rr1, nx, ny, z){
  
  nc_data_max <- nc_open(paste0(path,"tasmax","_12km_",model1,"_",rcp,drugo1,"_day_",leto_zac,"0101_",leto_kon1,".nc"))
  time <- ncvar_get(nc_data_max, "time")
  count_len <- length(time)  
  start1<-c(1,1,1)
  count1<-c(nx,ny, count_len)  
  data_max <- ncvar_get(nc_data_max, "tasmax", start=start1, count=count1)
  lon <- ncvar_get(nc_data_max, "lon")
  lat <- ncvar_get(nc_data_max, "lat")
  nc_close(nc_data_max) # konec branja
  
  nc_data_min <- nc_open(paste0(path,"tasmin","_12km_",model1,"_",rcp,drugo1,"_day_",leto_zac,"0101_",leto_kon1,".nc"))
  data_min <- ncvar_get(nc_data_min, "tasmin", start=start1, count=count1)
  nc_close(nc_data_min) 
  
  nc_data <- nc_open(paste0(path,"tas","_12km_",model1,"_",rcp,drugo1,"_day_",leto_zac,"0101_",leto_kon1,".nc"))
  data3 <- ncvar_get(nc_data,"tas",start=start1, count=count1)
  nc_close(nc_data) # konec branja
  
  nc_data_rr<- nc_open(paste0(path,"pr","_12km_",model1,"_",rcp,drugo1,"_day_",leto_zac,"0101_",leto_kon_rr1,".nc"))
  time1 <- ncvar_get(nc_data_rr, "time")
  count_len1 <- length(time1)  
  count2<-c(nx,ny, count_len1) 
  data_rr <- ncvar_get(nc_data_rr, "pr", start=start1, count=count2)
  nc_close(nc_data_rr)
  
  nc_data_et<- nc_open(paste0(path,"evspsblpot","_12km_",model1,"_",rcp,drugo1,"_day_",leto_zac,"0101_",leto_kon1,".nc"))
  data_et <- ncvar_get(nc_data_et, "evspsblpot", start=start1, count=count1)
  nc_close(nc_data_et)
  # nc_data_time <- nc_open(paste0(path, "pr", "_12km_MOHC-HadGEM2-ES_", rcp, "_r1i1p1_KNMI-RACMO22E_v2", "_day_", leto_zac, "0101_", "20991130", ".nc")) # Use a representative file for time
  # 
  # nc_close(nc_data_time)
  
  return(list(time = time1, lon = lon, lat = lat, data_max = data_max, 
              data_min = data_min, data3 = data3, data_rr = data_rr, data_et = data_et))
  
}


drugo <- c("_r1i1p1_CLMcom-CCLM4-8-17_v1",
           "_r3i1p1_DMI-HIRHAM5_v1",
           "_r1i1p1_IPSL-INERIS-WRF331F_v1",
           "_r1i1p1_KNMI-RACMO22E_v2",
           "_r1i1p1_CLMcom-CCLM4-8-17_v1",
           "_r1i1p1_SMHI-RCA4_v1a")
leto_zac <- "2071"
leto_kon <- c("21001231","21001231","21001231","20991231","21001231","21001231")
leto_kon_rr <- c("21001231","21001231","21001231","20991130","21001231","21001231")

en_model0 <- function(rcp, leto_zac, obd,z){
  povp_obd <- data.table()
  print(c("zacetek za: ",model[z]))    
  nc_data_info <- get_nc_data(path, model[z], rcp, drugo[z], leto_zac, leto_kon[z], leto_kon_rr[z], nx, ny)
  time <- nc_data_info$time
  lon <- nc_data_info$lon
  lat <- nc_data_info$lat
  data_max <- nc_data_info$data_max
  data_min <- nc_data_info$data_min
  data3 <- nc_data_info$data3
  data_rr <- nc_data_info$data_rr
  data_et <- nc_data_info$data_et
  rm(nc_data_info) 
  
  datumi <- as.POSIXct((time) * 60 * 60 * 24, origin = paste0("1950-12-01 00:00:00")) #Directly use time
  leta <- format(datumi, format = "%Y")
  datum2 <- format(datumi, format = "%Y-%m-%d")
  
  leto = format(datumi, format = "%Y")
  leta1 <- seq(min(leto),max(leto),by=1)
  lats1 <- seq(1,ny,by=1)
  lons1 <- seq(1,nx,by=1)
  n_rows <- 24
  n_cols <- 40
  n_layers <- 19
  vsi_podatki <- data.table()
  
  for(i in lons1){
    for(j in lats1){
      if(all(is.na(data3[i,j,1:length(time)]))){
        next
      }
      else{
        data_temp <- data.table(Date = format(as.Date(datum2),"%m/%d/%Y"), meseci = as.numeric(format(datumi,format="%m")), Temp = data3[i,j,1:length(time)]-273)
        data_temp <- data_temp %>% filter(meseci >= 4, meseci <= 10)
        
        data_tmax <- data.table(Date = format(as.Date(datum2),"%m/%d/%Y"), meseci = as.numeric(format(datumi,format="%m")), Tmax = data_max[i,j,1:length(time)]-273)
        data_tmax <- data_tmax %>% filter(meseci >= 4, meseci <= 10)
        
        data_tmin <- data.table(Date = format(as.Date(datum2),"%m/%d/%Y"), meseci = as.numeric(format(datumi,format="%m")), Tmin = data_min[i,j,1:length(time)]-273)
        data_tmin <- data_tmin %>% filter(meseci >= 4, meseci <= 10)
        
        data_pad0 <- data.table(Date = format(as.Date(datum2),"%m/%d/%Y"), meseci = as.numeric(format(datumi,format="%m")), pad = data_rr[i,j,1:length(time)]*24*60*60)
        data_pad <- data_pad0 %>% filter(meseci >= 4, meseci <= 10)
        
        data_et0 <- data.table(Date = format(as.Date(datum2),"%m/%d/%Y"), meseci = as.numeric(format(datumi,format="%m")), et = data_et[i,j,1:length(time)]*24*60*60)
        data_etp <- data_et0 %>% filter(meseci >= 4, meseci <= 10)
        
        growing_degree_days <- gd4(structure(data_temp$Temp, .Names = data_temp$Date))
        Growing_season_length <- gsl(structure(data_temp$Temp, .Names = data_temp$Date)) 
        End_growing_season <- ogs6(structure(data_temp$Temp, .Names = data_temp$Date)) + Growing_season_length + 90
        Sums_Tmax32 <- stx32(structure(data_tmax$Tmax, .Names = data_tmax$Date)) ###
        TG_of_warmest_quarter <- bio10(structure(data_temp$Temp, .Names = data_temp$Date)) #Kelvini
        dtr <- (data_tmax$Tmax+273)/(data_temp$Temp+273)
        dtr1 <- (data_tmin$Tmin+273)/(data_temp$Temp+273)
        Diurnal_temp_rangeN <-	dtr(structure(dtr, .Names = data_temp$Date),structure(dtr1, .Names = data_temp$Date))
        cons_summer_days <- csd(structure(data_tmax$Tmax, .Names = data_tmax$Date))
        # Warm_spell_duration <- wsdi(structure(data_tmax$Tmax, .Names = data_tmax$Date))
        Max_consecutive_dry_days <- cdd(structure(data_pad$pad, .Names = data_pad$Date))
        longest_wet_period <- cwd(structure(data_pad$pad, .Names = data_pad$Date))    
        r20mm <- r20mm(structure(data_pad$pad, .Names = data_pad$Date))
        Heavy_prec_days <- d50mm(structure(data_pad$pad, .Names = data_pad$Date)) 
        SDII <- sdii(structure(data_pad$pad, .Names = data_pad$Date))
        Prec_wettest_month	<- bio13(structure(data_pad$pad, .Names = data_pad$Date)) 
        frost_days <- fd(structure(data_tmin$Tmin, .Names = data_tmin$Date))
        Cold_spell_duration	<- csdi(structure(data_tmin$Tmin, .Names = data_tmin$Date))
        cons_frost_days <- cfd(structure(data_tmin$Tmin, .Names = data_tmin$Date))
        ice_days <- id(structure(data_tmax$Tmax, .Names = data_tmax$Date))    
        Effective_prec	<- ep(structure(data_etp$et, .Names = data_etp$Date), structure(data_pad$pad, .Names = data_pad$Date))
        Growing_season_prec	<- gsr(structure(data_pad$pad, .Names = data_pad$Date)) ###
        su <- su(structure(data_tmax$Tmax, .Names = data_tmax$Date))
        tr <- tr(structure(data_tmin$Tmin, .Names = data_tmin$Date))
        tn90p <- tn90p(structure(data_tmin$Tmin, .Names = data_tmin$Date))
        # vwd <- vwd(data = structure(data_tmax$Tmax, .Names = data_tmax$Date))
        
        # percentil90<-data.frame(latsi=lat[j],lonsi=lon[i],T90p=as.numeric(quantile(data_tmin$Tmin,0.9)))
        podatki_percentil_90_tmin = readRDS("../tn90p_referencno_obdobje.rds")
        podatki_percentil_90_tmin2<- subset(podatki_percentil_90_tmin, as.numeric(latsi)==lat[j]  & podatki_percentil_90_tmin$lon==as.numeric(lon[i]))
        tn90p = podatki_percentil_90_tmin2$T90p
        data_tmin_90p<-data_tmin
        # data_tmin_90p$Datum <-as.Date(data_tmin_90p$Date)
        data_tmin_90p$leto <-format(as.POSIXct(data_tmin_90p$Date, format="%m/%d/%Y"), "%Y")
        t90_test <- data_tmin_90p %>% 
          mutate(tmin90 = ifelse(Tmin >= podatki_percentil_90_tmin2$T90p,1,0),ind1=1)
        t90_test <- t90_test %>%
          group_by(leto) %>% summarise(sum=sum(tmin90),
                                       n=sum(ind1), tn90p = sum/n *100)
        
        podatki_percentil_99_tmax = readRDS("../tx99p_referencno_obdobje.rds")
        podatki_percentil_99_tmax2<- subset(podatki_percentil_99_tmax, as.numeric(latsi)==lat[j]  & podatki_percentil_99_tmax$lon==as.numeric(lon[i]))
        tx99p = podatki_percentil_99_tmax2$Tx99p
        data_tmax_99p<-data_tmax
        # data_tmax_99p$Datum <-as.Date(data_tmax_99p$Date)
        data_tmax_99p$leto <-format(as.POSIXct(data_tmax_99p$Date, format="%m/%d/%Y"), "%Y")
        tx99_test <- data_tmax_99p %>% 
          mutate(tmax99 = ifelse(Tmax >= podatki_percentil_99_tmax2$Tx99p,1,0),ind1=1)
        tx99_test <- tx99_test %>%
          group_by(leto) %>% summarise(sum=sum(tmax99),
                                       n=sum(ind1), tx99p = sum)
        
        podatki_percentil_90_tmax = readRDS("../tx90p_referencno_obdobje.rds")
        podatki_percentil_90_tmax2<- subset(podatki_percentil_90_tmax, as.numeric(latsi)==lat[j]  & podatki_percentil_90_tmax$lon==as.numeric(lon[i]))
        tx90p = podatki_percentil_90_tmax2$Tx90p
        data_tmax_90p <- data_tmax %>%
          mutate(leto = format(as.POSIXct(Date, format = "%m/%d/%Y"), "%Y"),
                 tmax90 = ifelse(Tmax >= tx90p, 1, 0),
                 ind1 = 1)
        consecutive_days_list <- data_tmax_90p %>%
          group_by(leto) %>%
          group_split() %>%
          lapply(function(year_data) {above_threshold <- year_data$tmax90
          consecutive_groups <- rle(above_threshold)
          consecutive_days <- sum(consecutive_groups$lengths[consecutive_groups$values == 1 & consecutive_groups$lengths >= 6])
          return(consecutive_days)
          })
        consecutive_days_df <- data.frame(leto = unique(data_tmax_90p$leto), consecutive_days = unlist(consecutive_days_list))
        summary_df <- data_tmax_90p %>%
          group_by(leto) %>%
          summarise(sum = sum(tmax90),n = sum(ind1))
        result_df <- left_join(summary_df, consecutive_days_df, by = "leto")
        
        
        data_wwd <- data.table(date = data_tmax$Date, Tmean = data_temp$Temp, RR = data_pad$pad)
        warm_wet_days <- function(data) {
          data <- data %>% mutate(day_of_year = yday(as.Date(data$date)))
          percentiles <- data %>%
            group_by(day_of_year) %>%
            summarize(Tmean75th = quantile(Tmean, 0.75, na.rm = TRUE),
                      RR75th = quantile(RR[RR > 0], 0.75, na.rm = TRUE), .groups = "drop")
          data <- data %>% left_join(percentiles, by = "day_of_year")
          data <- data %>% mutate(warm_wet = (Tmean > Tmean75th) & (RR > RR75th))
          data$year <- format(as.POSIXct(data$date, format="%m/%d/%Y"), "%Y")
          warm_wet_days_count <- data %>%
            filter(warm_wet) %>%
            group_by(year) %>%
            summarize(warm_wet_days = n(),
                      .groups = "drop")
          return(warm_wet_days_count)
        }
        WWD <- warm_wet_days(data_wwd)
        temp_day1 <- data.table(Year = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%Y"), 
                                Month = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%m"), 
                                Day = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%d"),  
                                temp = data_temp$Temp)
        x<-aggregate(temp ~  Year + Month, temp_day1, FUN = mean, na.rm=TRUE, na.action=na.pass)
        warmest_m_temp <- x %>% group_by(Year) %>%
          summarise(max = max(temp))
        
        WWD$Year <- WWD$year
        WWD <- left_join(warmest_m_temp,WWD,by="Year") %>%
          mutate_if(is.numeric,coalesce,0)
        
        df <- data.table(Date = data_temp$Date, Tmean = data_temp$Temp, Tmin = data_tmin$Tmin)
        # df$Date <- as.Date(df$Date)
        calculate_late_frost <- function(df) {
          df <- df %>% arrange(Date) # Ensure data is sorted by date
          df$Year <- format(as.POSIXct(df$Date, format="%m/%d/%Y"), "%Y")
          df$LateFrost <- FALSE
          years <- unique(df$Year)
          results_frost <- data.table(Year = integer(), LateFrostCount = integer())
          for (current_year in years) {
            year_data <- df %>% filter(Year == current_year)
            start_date <- NA        # Find start of 10°C period
            for (m in 1:(nrow(year_data) - 4)) {
              mean_temp_5days <- mean(year_data$Tmean[m:(m + 4)])
              if (mean_temp_5days >= 10) {
                start_date <- year_data$Date[m + 4] # Use the last day of the 5-day period
                break  }  }
            if (!is.na(start_date)) {
              # Check for late frost after the start date
              late_frost_days <- year_data %>%
                filter(Date > start_date, Tmin <= 0)
              if(nrow(late_frost_days) > 0){
                df$LateFrost[df$Date %in% late_frost_days$Date] <- TRUE          }
              late_frost_count <- nrow(late_frost_days)
              results_frost <- rbind(results_frost, data.table(Year = current_year, LateFrostCount = late_frost_count))
            } else { 
              results_frost <- rbind(results_frost, data.table(Year = current_year, LateFrostCount = 0))}  #No 5 day period with temp > 10
          }
          return(list(df = df, results_frost = results_frost))
        }
        late_frost_analysis <- calculate_late_frost(df) # df <- late_frost_analysis$df
        results_frost <- late_frost_analysis$results_frost
        
        
        flowering_heat_sum <- 703.42
        maturity_heat_sum <- 1616.8
        
        
        df_heat_stress <- data.table(Date = data_temp$Date, Tmax = data_tmax$Tmax, temp = data_temp$Temp)
        calculate_heat_sum <- function(tmax_data) {
          gdd_daily <- pmax(0, tmax_data$temp - 10)
          return(cumsum(gdd_daily))
        }
        is_heat_stress <- function(tmax_window) {# Function to check for heat stress (2-day period above 35°C)
          all(tmax_window > 35)
        }
        df_heat_stress$Year <- format(as.POSIXct(df_heat_stress$Date, format="%m/%d/%Y"), "%Y")
        results <- data.table(Year = unique(df_heat_stress$Year), HeatStressDaysFL = NA, HeatStressDaysMT = NA)
        for (current_year in unique(df_heat_stress$Year)) {
          year_data <- subset(df_heat_stress, Year == current_year)
          year_data <- subset(year_data, temp >=10)
          year_data$HeatSum <- calculate_heat_sum(year_data)
          start_dateFL <- min(year_data$Date)
          end_dateFL <- year_data$Date[which.min(abs(year_data$HeatSum - flowering_heat_sum))]
          
          end_date <- year_data$Date[which.min(abs(year_data$HeatSum - maturity_heat_sum))]
          start_date <- end_dateFL
          # print(c(start_date,end_date,start_dateFL,end_dateFL))
          if (!is.na(start_date) && !is.na(end_date)) {  # Only proceed if both dates are found
            flowering_data <- subset(year_data, Date >= start_dateFL & Date <= end_dateFL) 
            maturity_data <- subset(year_data, Date >= start_date & Date <= end_date) 
            if (nrow(flowering_data) >= 2){ # Check if there are at least 2 days of flowering data
              heat_stress_eventsFL <- zoo::rollapply(flowering_data$Tmax, width = 2, FUN = is_heat_stress, fill = NA, align = "left")
              heat_stress_daysFL <- sum(heat_stress_eventsFL, na.rm = TRUE) * 2
            }
            if (nrow(maturity_data) >= 2){ # Check if there are at least 2 days of flowering data
              heat_stress_events <- zoo::rollapply(maturity_data$Tmax, width = 2, FUN = is_heat_stress, fill = NA, align = "left")
              heat_stress_days <- sum(heat_stress_events, na.rm = TRUE) * 2
            }
            else {
              heat_stress_days <- 0
              heat_stress_daysFL <- 0# No heat stress days if flowering period is less than 2 days
            }
          } else {
            heat_stress_days <- 0
            heat_stress_daysFL <- 0# Or NA, if you prefer to indicate that flowering period couldn't be determined
            cat("Flowering period not found for year", current_year, "\n")
          }
          results$HeatStressDaysFL[results$Year == current_year] <- heat_stress_daysFL
          results$HeatStressDaysMT[results$Year == current_year] <- heat_stress_days
        }
        
        rx1day <- rx1day(structure(data_pad$pad, .Names = data_pad$Date)) 
        rx5d <- rx5d(structure(data_pad$pad, .Names = data_pad$Date)) 
        r95tot <- r95tot(structure(data_pad$pad, .Names = data_pad$Date)) 
        
        temp_day <- data.table(Year = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%Y"),
                               Month = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%m"), 
                               Day = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%d"),
                               Tmin = data_tmin$Tmin, Tmax = data_tmax$Tmax)
        # climdata <- hourly_temps(temp_day, latitude = lat[i])
        BEDD0 <- head(GDD_linear(temp_day, Tb = 10, Tu = 30),-1)
        BEDD <- aggregate(GDD ~  Year, BEDD0, FUN = sum, na.rm=TRUE, na.action=na.pass)[,2]
        
        pod_celi <- data.table(datum = data_temp$Date, 
                               leto = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%Y"), 
                               mesec = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%m"), 
                               tg = data_tmax$Tmax)
        st_dni0 <- pod_celi %>% filter(tg >= 30)
        st_dni30 <- st_dni0 %>% group_by(leto) %>% summarise(tg = sum(tg-30))
        warmest_m_temp$leto <- warmest_m_temp$Year
        Dnevi_30max <- left_join(warmest_m_temp,st_dni30,by="leto") %>%
          mutate_if(is.numeric,coalesce,0)
        Dnevi_30max <- Dnevi_30max[,3:4] #Plant heat stress = accumulated daily maximum temperature values above 30?C 
        
        st_dni32_anthesis <- pod_celi %>% filter(tg >= 32, mesec >= 5, mesec <= 6) 
        Dnevi_32max_anthesis <- st_dni32_anthesis %>% group_by(leto) %>% count()
        Dnevi_32max_anthesis <- left_join(warmest_m_temp[,3],Dnevi_32max_anthesis,by="leto") %>%
          mutate_if(is.numeric,coalesce,0) # Days with Tmax above 32 ?C
        
        podatki_tocka <- data.table(lats = rep(lat[j], length(growing_degree_days)), 
                                    lons = rep(lon[i], length(growing_degree_days)), 
                                    leto = leta1,
                                    model = rep(model[z], length(growing_degree_days)),
                                    growing_degree_days, 
                                    GSL = Growing_season_length, 
                                    End_growing_season,
                                    BEDD, 
                                    Sums_Tmax32, 
                                    T_warmest_m = warmest_m_temp$max, 
                                    TG_of_warmest_quarter, 
                                    Diurnal_temp_rangeN, 
                                    Heat_stress_fl = results$HeatStressDaysFL, 
                                    Heat_stress_mat = results$HeatStressDaysMT,
                                    cons_summer_days, 
                                    WWD = WWD$warm_wet_days, 
                                    WSDI = result_df$consecutive_days,
                                    CDD = Max_consecutive_dry_days,#DD = Dry_days, 
                                    CWD = longest_wet_period, 
                                    r20mm, 
                                    Heavy_prec_days,# r10mm, 
                                    SDII, 
                                    Prec_wettest_month, #Prec_warmest_quarter, Prec_coldest_quarter,wet_days, 
                                    FD = frost_days, 
                                    late_frost_days = results_frost$LateFrostCount,
                                    CSDI = Cold_spell_duration, 
                                    CFD = cons_frost_days, 
                                    ice_days, 
                                    Effective_prec,
                                    Growing_season_prec,#Very_wet_days,
                                    # Nongrowing_season_prec, 
                                    # precip_total,
                                    su, 
                                    tr, 
                                    tn90p = t90_test$tn90p, 
                                    vwd = tx99_test$tx99p,
                                    rx5d)
        vsi_podatki <- rbind(vsi_podatki, podatki_tocka)
        
      }
    }    
    
  }
  saveRDS(vsi_podatki, file = paste0("period_mean_",rcp,"_",obd,"_",model[z],"_",drugo[z],".rds"))
  return(vsi_podatki)
}

# posamezen_model <- en_model0(rcp = "rcp45", leto_zac = "2071", obd = "2071-2100", z = 1)
# posam_model2 <- en_model0(rcp = "rcp45", leto_zac = "2071", obd = "2071-2100", z = 2)
# posam_model3 <- en_model0(rcp = "rcp45", leto_zac = "2071", obd = "2071-2100", z = 3)
# posam_model4 <- en_model0(rcp = "rcp45", leto_zac = "2071", obd = "2071-2100", z = 4)
# posam_model5 <- en_model0(rcp = "rcp45", leto_zac = "2071", obd = "2071-2100", z = 5)
# posam_model6 <- en_model0(rcp = "rcp45", leto_zac = "2071", obd = "2071-2100", z = 6)

# 
# posamezen_model <- en_model0(rcp = "rcp85", leto_zac = "2071", obd = "2071-2100", z = 1)
# posam_model2 <- en_model0(rcp = "rcp85", leto_zac = "2071", obd = "2071-2100", z = 2)
# posam_model3 <- en_model0(rcp = "rcp85", leto_zac = "2071", obd = "2071-2100", z = 3)
# posam_model4 <- en_model0(rcp = "rcp85", leto_zac = "2071", obd = "2071-2100", z = 4)
# posam_model5 <- en_model0(rcp = "rcp85", leto_zac = "2071", obd = "2071-2100", z = 5)
# posam_model6 <- en_model0(rcp = "rcp85", leto_zac = "2071", obd = "2071-2100", z = 6)


posamezen_model0 <- readRDS(paste0("period_mean_rcp45_2071-2100_", model[1],"_",drugo[1],".rds"))
posamezen_model1 <- readRDS(paste0("period_mean_rcp85_2071-2100_", model[1],"_",drugo[1],".rds"))
posam_model02 <- readRDS(paste0("period_mean_rcp45_2071-2100_", model[2],"_",drugo[2],".rds"))
posam_model12 <- readRDS(paste0("period_mean_rcp85_2071-2100_", model[2],"_",drugo[2],".rds"))
posam_model03 <- readRDS(paste0("period_mean_rcp45_2071-2100_", model[3],"_",drugo[3],".rds"))
posam_model13 <- readRDS(paste0("period_mean_rcp85_2071-2100_", model[3],"_",drugo[3],".rds"))
posam_model04 <- readRDS(paste0("period_mean_rcp45_2071-2100_", model[4],"_",drugo[4],".rds"))
posam_model14 <- readRDS(paste0("period_mean_rcp85_2071-2100_", model[4],"_",drugo[4],".rds"))
posam_model05 <- readRDS(paste0("period_mean_rcp45_2071-2100_", model[5],"_",drugo[5],".rds"))
posam_model15 <- readRDS(paste0("period_mean_rcp85_2071-2100_", model[5],"_",drugo[5],".rds"))
posam_model06 <- readRDS(paste0("period_mean_rcp45_2071-2100_", model[6],"_",drugo[6],".rds"))
posam_model16 <- readRDS(paste0("period_mean_rcp85_2071-2100_", model[6],"_",drugo[6],".rds"))

povp_obd <- data.table() 
yrs <- posamezen_model0$leto
kaz <- names(posamezen_model0)[5:35]
zac = 2071
kon = 2100
rcp="rcp45"
l = list(posamezen_model0, posam_model02, posam_model03,
         posam_model04, posam_model05, posam_model06)
vsi_podatki_70_10_rcp45 <- rbindlist(l)
saveRDS(vsi_podatki_70_10_rcp45, file = "kazalniki_71-00_rcp45.rds")


rcp="rcp85"
l = list(posamezen_model1, posam_model12, posam_model13,
         posam_model14, posam_model15, posam_model16)
vsi_podatki_70_10_rcp85 <- rbindlist(l)
saveRDS(vsi_podatki_70_10_rcp85, file = "kazalniki_71-00_rcp85.rds")

################# 2041-2070 #######################

leto_zac <- "2041"
leto_kon <- c("20701231","20701231","20701231","20701231","20701231","20701231")
leto_kon_rr <- c("20701231","20701231","20701231","20701231","20701231","20701231")
leto_zac1 <- "2040"

# 
# posamezen_model0 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 1)
# posam_model02 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 2)
# posam_model03 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 3)
# posam_model04 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 4)
# posam_model05 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 5)
# posam_model06 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 6)
# 
# 
# posamezen_model <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 1)
# posam_model2 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 2)
# posam_model3 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 3)
# posam_model4 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 4)
# posam_model5 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 5)
# posam_model6 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 6)

posamezen_model0 <- readRDS(paste0("period_mean_rcp45_2041-2070_", model[1],"_",drugo[1],".rds"))
posamezen_model1 <- readRDS(paste0("period_mean_rcp85_2041-2070_", model[1],"_",drugo[1],".rds"))
posam_model02 <- readRDS(paste0("period_mean_rcp45_2041-2070_", model[2],"_",drugo[2],".rds"))
posam_model12 <- readRDS(paste0("period_mean_rcp85_2041-2070_", model[2],"_",drugo[2],".rds"))
posam_model03 <- readRDS(paste0("period_mean_rcp45_2041-2070_", model[3],"_",drugo[3],".rds"))
posam_model13 <- readRDS(paste0("period_mean_rcp85_2041-2070_", model[3],"_",drugo[3],".rds"))
posam_model04 <- readRDS(paste0("period_mean_rcp45_2041-2070_", model[4],"_",drugo[4],".rds"))
posam_model14 <- readRDS(paste0("period_mean_rcp85_2041-2070_", model[4],"_",drugo[4],".rds"))
posam_model05 <- readRDS(paste0("period_mean_rcp45_2041-2070_", model[5],"_",drugo[5],".rds"))
posam_model15 <- readRDS(paste0("period_mean_rcp85_2041-2070_", model[5],"_",drugo[5],".rds"))
posam_model06 <- readRDS(paste0("period_mean_rcp45_2041-2070_", model[6],"_",drugo[6],".rds"))
posam_model16 <- readRDS(paste0("period_mean_rcp85_2041-2070_", model[6],"_",drugo[6],".rds"))

# posamezen_model0 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 1)
# posamezen_model1 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 1)
# posam_model02 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 2)
# posam_model12 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 2)
# posam_model03 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 3)
# posam_model13 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 3)
# posam_model04 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 4)
# posam_model14 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 4)
# posam_model05 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 5)
# posam_model15 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 5)
# posam_model06 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 6)
# posam_model16 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 6)

povp_obd <- data.table()
yrs <- posamezen_model0$leto
kaz <- names(posamezen_model0)[5:35]
zac = 2041
kon = 2070
rcp="rcp45"
l = list(posamezen_model0, posam_model02, posam_model03,
         posam_model04, posam_model05, posam_model06)
vsi_podatki_41_70_rcp45 <- rbindlist(l)
saveRDS(vsi_podatki_41_70_rcp45, file = "kazalniki_41-70_rcp45.rds")

rcp="rcp85"
l = list(posamezen_model1, posam_model12, posam_model13,
         posam_model14, posam_model15, posam_model16)
vsi_podatki_41_70_rcp85 <- rbindlist(l)
saveRDS(vsi_podatki_41_70_rcp85, file = "kazalniki_41-70_rcp85.rds")

####################################################################################################################

########################## HIST PŠENICA


# Risanje kazalnikov
# vsi_podatki0 <- vsi_podatki
vsi_podatki <- readRDS(file = "C:/Users/ZalaZn/OneDrive - Univerza v Ljubljani/projekti/CRP kazalniki 2024/kazalniki_OPSI_historical_psenica+koruza.rds")
vsi_podatki = subset(vsi_podatki, select = -c(End_growing_season,Heat_stress_fl,chill_portions,Heat_stress_mat,year) )
# izlocimo kazalnike za koruzo in nepotrebno ponovitev let 

kaz <- names(vsi_podatki)[4:36]
kaz

zac = 1981
kon = 2010
# path1 <- "C:/Users/ZalaZn/OneDrive - Univerza v Ljubljani/CRP kazalniki 2024/delo/" # sluzba


# for(k in seq(1,length(kaz),by=1)){
#   data <- data.frame(vsi_podatki$lons, vsi_podatki$lats, vsi_podatki$leto, vsi_podatki[[kaz[k]]])
#   colnames(data) <- c("lons","lats","leto","agroclim_ind")
#   data <- data %>% filter(leto >= zac, leto <= kon)
#   data=na.omit(data)
#   min = min(na.omit(vsi_podatki[[kaz[k]]]))
#   max = max(na.omit(vsi_podatki[[kaz[k]]]))
#   
#   povp <- data %>% group_by(lats,lons) %>%
#     summarise(period_mean = mean(agroclim_ind))
#   ggplot() +
#     geom_tile(data=povp,aes(x=lons,y=lats,fill=period_mean)) +
#     geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
#     scale_fill_distiller("povp_obd",palette = "Spectral", limits = c(min,max)) +
#     # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
#     labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("Kazalnik ",kaz[k],", obdobje ",zac,"-",kon)) +
#     coord_sf(crs = st_crs(4326)) +
#     theme_light(base_size = 17)
#   ggsave(paste0("karte_psenica/hist_period_mean_",kaz[k],"_",zac,"-",kon,".png"),width = 10, height = 7)
# }

########## PROJEKCIJE PŠENICA

######################################### PROJEKCIJE MEDIANA MODELOV 2071 - 2100 PŠENICA

model <- c("CNRM-CERFACS-CNRM-CM5", 
           "ICHEC-EC-EARTH",
           "IPSL-IPSL-CM5A-MR",
           "MOHC-HadGEM2-ES", # samo do 30. 11. 2099 pri padavinah, zato tudi pri Tmin in Tmax .nc fajl spremenim, da gre le do 30.11.
           "MPI-M-MPI-ESM-LR",
           "MPI-M-MPI-ESM-LR")
drugo <- c("_r1i1p1_CLMcom-CCLM4-8-17_v1",
           "_r3i1p1_DMI-HIRHAM5_v1",
           "_r1i1p1_IPSL-INERIS-WRF331F_v1",
           "_r1i1p1_KNMI-RACMO22E_v2",
           "_r1i1p1_CLMcom-CCLM4-8-17_v1",
           "_r1i1p1_SMHI-RCA4_v1a")

get_nc_data <- function(path, model1, rcp, drugo1, leto_zac, leto_kon1, leto_kon_rr1, nx, ny, z){
  
  nc_data_max <- nc_open(paste0(path,"tasmax","_12km_",model1,"_",rcp,drugo1,"_day_",leto_zac,"0101_",leto_kon1,".nc"))
  time <- ncvar_get(nc_data_max, "time")
  count_len <- length(time)  
  start1<-c(1,1,1)
  count1<-c(nx,ny, count_len)  
  data_max <- ncvar_get(nc_data_max, "tasmax", start=start1, count=count1)
  lon <- ncvar_get(nc_data_max, "lon")
  lat <- ncvar_get(nc_data_max, "lat")
  nc_close(nc_data_max) # konec branja
  
  nc_data_min <- nc_open(paste0(path,"tasmin","_12km_",model1,"_",rcp,drugo1,"_day_",leto_zac,"0101_",leto_kon1,".nc"))
  data_min <- ncvar_get(nc_data_min, "tasmin", start=start1, count=count1)
  nc_close(nc_data_min) 
  
  nc_data <- nc_open(paste0(path,"tas","_12km_",model1,"_",rcp,drugo1,"_day_",leto_zac,"0101_",leto_kon1,".nc"))
  data3 <- ncvar_get(nc_data,"tas",start=start1, count=count1)
  nc_close(nc_data) # konec branja
  
  nc_data_rr<- nc_open(paste0(path,"pr","_12km_",model1,"_",rcp,drugo1,"_day_",leto_zac,"0101_",leto_kon_rr1,".nc"))
  time1 <- ncvar_get(nc_data_rr, "time")
  count_len1 <- length(time1)  
  count2<-c(nx,ny, count_len1) 
  data_rr <- ncvar_get(nc_data_rr, "pr", start=start1, count=count2)
  nc_close(nc_data_rr)
  
  nc_data_et<- nc_open(paste0(path,"evspsblpot","_12km_",model1,"_",rcp,drugo1,"_day_",leto_zac,"0101_",leto_kon1,".nc"))
  data_et <- ncvar_get(nc_data_et, "evspsblpot", start=start1, count=count1)
  nc_close(nc_data_et)
  # nc_data_time <- nc_open(paste0(path, "pr", "_12km_MOHC-HadGEM2-ES_", rcp, "_r1i1p1_KNMI-RACMO22E_v2", "_day_", leto_zac, "0101_", "20991130", ".nc")) # Use a representative file for time
  # 
  # nc_close(nc_data_time)
  
  return(list(time = time1, lon = lon, lat = lat, data_max = data_max, 
              data_min = data_min, data3 = data3, data_rr = data_rr, data_et = data_et))
  
}


drugo <- c("_r1i1p1_CLMcom-CCLM4-8-17_v1",
           "_r3i1p1_DMI-HIRHAM5_v1",
           "_r1i1p1_IPSL-INERIS-WRF331F_v1",
           "_r1i1p1_KNMI-RACMO22E_v2",
           "_r1i1p1_CLMcom-CCLM4-8-17_v1",
           "_r1i1p1_SMHI-RCA4_v1a")
leto_zac <- "2071"
leto_kon <- c("21001231","21001231","21001231","20991231","21001231","21001231")
leto_kon_rr <- c("21001231","21001231","21001231","20991130","21001231","21001231")

en_model0 <- function(rcp, leto_zac, obd,z){
  povp_obd <- data.table()
  print(c("zacetek za: ",model[z]))    
  nc_data_info <- get_nc_data(path, model[z], rcp, drugo[z], leto_zac, leto_kon[z], leto_kon_rr[z], nx, ny)
  time <- nc_data_info$time
  lon <- nc_data_info$lon
  lat <- nc_data_info$lat
  data_max <- nc_data_info$data_max
  data_min <- nc_data_info$data_min
  data3 <- nc_data_info$data3
  data_rr <- nc_data_info$data_rr
  data_et <- nc_data_info$data_et
  rm(nc_data_info) 
  
  datumi <- as.POSIXct((time) * 60 * 60 * 24, origin = paste0("1949-12-31 00:00:00")) #Directly use time
  leta <- format(datumi, format = "%Y")
  datum2 <- format(datumi, format = "%Y-%m-%d")
  
  leto = format(datumi, format = "%Y")
  leta1 <- seq(min(leto),max(leto),by=1)
  lats1 <- seq(1,ny,by=1)
  lons1 <- seq(1,nx,by=1)
  n_rows <- 24
  n_cols <- 40
  n_layers <- 19
  vsi_podatki <- data.table()
  
  for(i in lons1){
    for(j in lats1){
      if(all(is.na(data3[i,j,1:length(time)]))){
        next
      }
      else{
        data_temp <- data.table(Date = format(as.Date(datum2),"%m/%d/%Y"), meseci = as.numeric(format(datumi,format="%m")), Temp = data3[i,j,1:length(time)]-273)
        # data_temp <- data_temp %>% filter(meseci >= 4, meseci <= 10)
        
        data_tmax <- data.table(Date = format(as.Date(datum2),"%m/%d/%Y"), meseci = as.numeric(format(datumi,format="%m")), Tmax = data_max[i,j,1:length(time)]-273)
        # data_tmax <- data_tmax %>% filter(meseci >= 4, meseci <= 10)
        
        data_tmin <- data.table(Date = format(as.Date(datum2),"%m/%d/%Y"), meseci = as.numeric(format(datumi,format="%m")), Tmin = data_min[i,j,1:length(time)]-273)
        # data_tmin <- data_tmin %>% filter(meseci >= 4, meseci <= 10)
        
        data_pad <- data.table(Date = format(as.Date(datum2),"%m/%d/%Y"), meseci = as.numeric(format(datumi,format="%m")), pad = data_rr[i,j,1:length(time)]*24*60*60)
        # data_pad <- data_pad0 %>% filter(meseci >= 4, meseci <= 10)
        
        data_etp <- data.table(Date = format(as.Date(datum2),"%m/%d/%Y"), meseci = as.numeric(format(datumi,format="%m")), et = data_et[i,j,1:length(time)]*24*60*60)
        # data_etp <- data_et0 %>% filter(meseci >= 4, meseci <= 10)
        
        growing_degree_days <- gd4(structure(data_temp$Temp, .Names = data_temp$Date))
        Growing_season_length <- gsl(structure(data_temp$Temp, .Names = data_temp$Date)) 
        End_growing_season <- ogs6(structure(data_temp$Temp, .Names = data_temp$Date)) + Growing_season_length + 90
        Sums_Tmax32 <- stx32(structure(data_tmax$Tmax, .Names = data_tmax$Date)) ###
        TG_of_warmest_quarter <- bio10(structure(data_temp$Temp, .Names = data_temp$Date)) #Kelvini
        dtr <- (data_tmax$Tmax+273)/(data_temp$Temp+273)
        dtr1 <- (data_tmin$Tmin+273)/(data_temp$Temp+273)
        Diurnal_temp_rangeN <-	dtr(structure(dtr, .Names = data_temp$Date),structure(dtr1, .Names = data_temp$Date))
        cons_summer_days <- csd(structure(data_tmax$Tmax, .Names = data_tmax$Date))
        # Warm_spell_duration <- wsdi(structure(data_tmax$Tmax, .Names = data_tmax$Date))
        
        Max_consecutive_dry_days <- cdd(structure(data_pad$pad, .Names = data_pad$Date))
        longest_wet_period <- cwd(structure(data_pad$pad, .Names = data_pad$Date))    
        r20mm <- r20mm(structure(data_pad$pad, .Names = data_pad$Date))
        Heavy_prec_days <- d50mm(structure(data_pad$pad, .Names = data_pad$Date)) 
        SDII <- sdii(structure(data_pad$pad, .Names = data_pad$Date))
        Prec_wettest_month	<- bio13(structure(data_pad$pad, .Names = data_pad$Date)) 
        frost_days <- fd(structure(data_tmin$Tmin, .Names = data_tmin$Date))
        
        podatki_percentil_10_tmin = readRDS("../tn10p_referencno_obdobje_psenica+koruza.rds")
        podatki_percentil_10_tmin2<- subset(podatki_percentil_10_tmin, as.numeric(latsi)==lat[j]  & podatki_percentil_10_tmin$lon==as.numeric(lon[i]))
        
        CSDI_optimized <- function(tmin_data) {
          below_threshold <- tmin_data < podatki_percentil_10_tmin2$Tn10p[1]  # Uporaba `rle` (Run Length Encoding) za iskanje zaporednih nizov
          rle_result <- rle(below_threshold)
          cold_spell_lengths <- rle_result$lengths[rle_result$values == TRUE & rle_result$lengths >= 6]
          csdi_days_count <- sum(cold_spell_lengths)
          return(csdi_days_count)
        }
        Cold_spell_duration <- function(df) {
          df$year <- format(as.POSIXct(df$Date, format="%m/%d/%Y"), "%Y")
          annual_csdi <- df %>%
            group_by(year) %>%
            summarise(csdi_annual_sum = CSDI_optimized(Tmin)) %>%
            ungroup() 
          return(annual_csdi)
        }
        CSDI = Cold_spell_duration(data_tmin)
        
        cons_frost_days <- cfd(structure(data_tmin$Tmin, .Names = data_tmin$Date))
        ice_days <- id(structure(data_tmax$Tmax, .Names = data_tmax$Date))    
        Effective_prec	<- ep(structure(data_etp$et, .Names = data_etp$Date), structure(data_pad$pad, .Names = data_pad$Date))
        Growing_season_prec	<- gsr(structure(data_pad$pad, .Names = data_pad$Date)) ###
        Nongrowing_season_prec <- ngsr(structure(data_pad$pad, .Names = data_pad$Date))
        su <- su(structure(data_tmax$Tmax, .Names = data_tmax$Date))
        tr <- tr(structure(data_tmin$Tmin, .Names = data_tmin$Date))
        tn90p <- tn90p(structure(data_tmin$Tmin, .Names = data_tmin$Date))
        # vwd <- vwd(data = structure(data_tmax$Tmax, .Names = data_tmax$Date))
        Sums_Tmin10 <- stn10(structure(data_tmin$Tmin, .Names = data_tmin$Date))
        Sums_Tmin15 <- stn15(structure(data_tmin$Tmin, .Names = data_tmin$Date))
        
        # percentil90<-data.frame(latsi=lat[j],lonsi=lon[i],T90p=as.numeric(quantile(data_tmin$Tmin,0.9)))
        podatki_percentil_90_tmin = readRDS("../tn90p_referencno_obdobje_psenica+koruza.rds")
        podatki_percentil_90_tmin2<- subset(podatki_percentil_90_tmin, as.numeric(latsi)==lat[j]  & podatki_percentil_90_tmin$lon==as.numeric(lon[i]))
        tn90p = podatki_percentil_90_tmin2$T90p[1]
        data_tmin_90p<-data_tmin
        # data_tmin_90p$Datum <-as.Date(data_tmin_90p$Date)
        data_tmin_90p$leto <-format(as.POSIXct(data_tmin_90p$Date, format="%m/%d/%Y"), "%Y")
        t90_test <- data_tmin_90p %>% 
          mutate(tmin90 = ifelse(Tmin >= podatki_percentil_90_tmin2$T90p,1,0),ind1=1)
        t90_test <- t90_test %>%
          group_by(leto) %>% summarise(sum=sum(tmin90),
                                       n=sum(ind1), tn90p = sum/n *100)
        
        podatki_percentil_99_tmax = readRDS("../tx99p_referencno_obdobje_psenica+koruza.rds")
        podatki_percentil_99_tmax2<- subset(podatki_percentil_99_tmax, as.numeric(latsi)==lat[j]  & podatki_percentil_99_tmax$lon==as.numeric(lon[i]))
        tx99p = podatki_percentil_99_tmax2$Tx99p[1]
        data_tmax_99p<-data_tmax
        # data_tmax_99p$Datum <-as.Date(data_tmax_99p$Date)
        data_tmax_99p$leto <-format(as.POSIXct(data_tmax_99p$Date, format="%m/%d/%Y"), "%Y")
        tx99_test <- data_tmax_99p %>% 
          mutate(tmax99 = ifelse(Tmax >= podatki_percentil_99_tmax2$Tx99p,1,0),ind1=1)
        tx99_test <- tx99_test %>%
          group_by(leto) %>% summarise(sum=sum(tmax99),
                                       n=sum(ind1), tx99p = sum)
        
        podatki_percentil_90_tmax = readRDS("../tx90p_referencno_obdobje_psenica+koruza.rds")
        podatki_percentil_90_tmax2<- subset(podatki_percentil_90_tmax, as.numeric(latsi)==lat[j]  & podatki_percentil_90_tmax$lon==as.numeric(lon[i]))
        tx90p = podatki_percentil_90_tmax2$Tx90p[1]
        data_tmax_90p <- data_tmax %>%
          mutate(leto = format(as.POSIXct(Date, format = "%m/%d/%Y"), "%Y"),
                 tmax90 = ifelse(Tmax >= tx90p, 1, 0),
                 ind1 = 1)
        consecutive_days_list <- data_tmax_90p %>%
          group_by(leto) %>%
          group_split() %>%
          lapply(function(year_data) {above_threshold <- year_data$tmax90
          consecutive_groups <- rle(above_threshold)
          consecutive_days <- sum(consecutive_groups$lengths[consecutive_groups$values == 1 & consecutive_groups$lengths >= 6])
          return(consecutive_days)
          })
        consecutive_days_df <- data.frame(leto = unique(data_tmax_90p$leto), consecutive_days = unlist(consecutive_days_list))
        summary_df <- data_tmax_90p %>%
          group_by(leto) %>%
          summarise(sum = sum(tmax90),n = sum(ind1))
        result_df <- left_join(summary_df, consecutive_days_df, by = "leto")
        
        
        podatki_percentil_75_tmean = readRDS("../t75p_referencno_obdobje_psenica+koruza.rds")
        podatki_percentil_75_tmean2<- subset(podatki_percentil_75_tmean, as.numeric(latsi)==lat[j]  & podatki_percentil_75_tmean$lon==as.numeric(lon[i]))
        
        podatki_percentil_75_rr = readRDS("../rr75p_referencno_obdobje_psenica+koruza.rds")
        podatki_percentil_75_rr2<- subset(podatki_percentil_75_rr, as.numeric(latsi)==lat[j]  & podatki_percentil_75_rr$lon==as.numeric(lon[i]))
        
        data_wwd <- data.frame(date = data_tmax$Date, Tmean = data_temp$Temp, RR = data_pad$pad)
        warm_wet_days <- function(data) {
          data <- data %>% mutate(warm_wet = (Tmean > podatki_percentil_75_tmean2$T75p[1]) & (RR > podatki_percentil_75_rr2$rr75p[1]))
          data$year <- format(as.POSIXct(data$date, format="%m/%d/%Y"), "%Y")
          warm_wet_days_count <- data %>%
            filter(warm_wet) %>%
            group_by(year) %>%
            summarize(warm_wet_days = n(),
                      .groups = "drop")
          return(warm_wet_days_count)
        }            
        
        WWD <- warm_wet_days(data_wwd)
        temp_day1 <- data.table(Year = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%Y"), 
                                Month = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%m"), 
                                Day = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%d"),  
                                temp = data_temp$Temp)
        x<-aggregate(temp ~  Year + Month, temp_day1, FUN = mean, na.rm=TRUE, na.action=na.pass)
        warmest_m_temp <- x %>% group_by(Year) %>%
          summarise(max = max(temp))
        
        WWD$Year <- WWD$year
        WWD <- left_join(warmest_m_temp,WWD,by="Year") %>%
          mutate_if(is.numeric,coalesce,0)
        
        df <- data.table(Date = data_temp$Date, Tmean = data_temp$Temp, Tmin = data_tmin$Tmin)
        # df$Date <- as.Date(df$Date)
        calculate_late_frost <- function(df) {
          df <- df %>% arrange(Date) # Ensure data is sorted by date
          df$Year <- format(as.POSIXct(df$Date, format="%m/%d/%Y"), "%Y")
          df$LateFrost <- FALSE
          years <- unique(df$Year)
          results_frost <- data.table(Year = integer(), LateFrostCount = integer())
          for (current_year in years) {
            year_data <- df %>% filter(Year == current_year)
            start_date <- NA        # Find start of 10°C period
            for (m in 1:(nrow(year_data) - 4)) {
              mean_temp_5days <- mean(year_data$Tmean[m:(m + 4)])
              if (mean_temp_5days >= 10) {
                start_date <- year_data$Date[m + 4] # Use the last day of the 5-day period
                break  }  }
            if (!is.na(start_date)) {
              # Check for late frost after the start date
              late_frost_days <- year_data %>%
                filter(Date > start_date, Tmin <= 0)
              if(nrow(late_frost_days) > 0){
                df$LateFrost[df$Date %in% late_frost_days$Date] <- TRUE          }
              late_frost_count <- nrow(late_frost_days)
              results_frost <- rbind(results_frost, data.table(Year = current_year, LateFrostCount = late_frost_count))
            } else { 
              results_frost <- rbind(results_frost, data.table(Year = current_year, LateFrostCount = 0))}  #No 5 day period with temp > 10
          }
          return(list(df = df, results_frost = results_frost))
        }
        late_frost_analysis <- calculate_late_frost(df) # df <- late_frost_analysis$df
        results_frost <- late_frost_analysis$results_frost
        
        TG_of_coldest_quarter	<- bio11(structure(data_temp$Temp, .Names = data_temp$Date))
        rx1day <- rx1day(structure(data_pad$pad, .Names = data_pad$Date)) 
        rx5d <- rx5d(structure(data_pad$pad, .Names = data_pad$Date)) 
        r95tot <- r95tot(structure(data_pad$pad, .Names = data_pad$Date)) 
        
        temp_day <- data.table(Year = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%Y"),
                               Month = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%m"), 
                               Day = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%d"),
                               Tmin = data_tmin$Tmin, Tmax = data_tmax$Tmax)
        # climdata <- hourly_temps(temp_day, latitude = lat[i])
        # chill_portions0 <- chill_portions(climdata, Start = 214)
        # chill_portions <- aggregate(Chill ~  Year, chill_portions0, FUN = sum, na.rm=TRUE, na.action=na.pass)[,2]
        BEDD0 <- head(GDD_linear(temp_day, Tb = 10, Tu = 30),-1)
        BEDD <- aggregate(GDD ~  Year, BEDD0, FUN = sum, na.rm=TRUE, na.action=na.pass)[,2]
        
        pod_celi <- data.table(datum = data_temp$Date, 
                               leto = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"),format="%Y"), 
                               mesec = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"),format="%m"), 
                               tg = data_tmax$Tmax)
        st_dni0 <- pod_celi %>% filter(tg >= 30)
        st_dni30 <- st_dni0 %>% group_by(leto) %>% summarise(tg = sum(tg-30))
        warmest_m_temp$leto <- warmest_m_temp$Year
        Dnevi_30max <- left_join(warmest_m_temp,st_dni30,by="leto") %>%
          mutate_if(is.numeric,coalesce,0)
        Dnevi_30max <- Dnevi_30max[,3:4] #Plant heat stress = accumulated daily maximum temperature values above 30?C 
        
        pod_celi$mesec <- as.integer(pod_celi$mesec)
        st_dni32_anthesis <- pod_celi %>% filter(tg >= 32, mesec >= 5, mesec <= 6) 
        Dnevi_32max_anthesis <- st_dni32_anthesis %>% group_by(leto) %>% count()
        Dnevi_32max_anthesis <- left_join(warmest_m_temp[,3],Dnevi_32max_anthesis,by="leto") %>%
          mutate_if(is.numeric,coalesce,0) # Days with Tmax above 32 ?C
        
        # flowering_heat_sum <- 703.42
        # maturity_heat_sum <- 1616.8
        # 
        # data_temp <- data_temp %>% filter(meseci >= 4, meseci <= 10)
        # data_tmax <- data_tmax %>% filter(meseci >= 4, meseci <= 10)
        # data_tmin <- data_tmin %>% filter(meseci >= 4, meseci <= 10)
        # df_heat_stress <- data.table(Date = data_temp$Date, Tmax = data_tmax$Tmax, temp = data_temp$Temp)
        # calculate_heat_sum <- function(tmax_data) {
        #   gdd_daily <- pmax(0, tmax_data$temp - 10)
        #   return(cumsum(gdd_daily))
        # }
        # is_heat_stress <- function(tmax_window) {# Function to check for heat stress (2-day period above 35°C)
        #   all(tmax_window > 35)
        # }
        # df_heat_stress$Year <- format(as.POSIXct(df_heat_stress$Date, format="%m/%d/%Y"), "%Y")
        # results <- data.table(Year = unique(df_heat_stress$Year), HeatStressDaysFL = NA, HeatStressDaysMT = NA)
        # for (current_year in unique(df_heat_stress$Year)) {
        #   year_data <- subset(df_heat_stress, Year == current_year)
        #   year_data <- subset(year_data, temp >=10)
        #   year_data$HeatSum <- calculate_heat_sum(year_data)
        #   start_dateFL <- min(year_data$Date)
        #   end_dateFL <- year_data$Date[which.min(abs(year_data$HeatSum - flowering_heat_sum))]
        #   
        #   end_date <- year_data$Date[which.min(abs(year_data$HeatSum - maturity_heat_sum))]
        #   start_date <- end_dateFL
        #   # print(c(start_date,end_date,start_dateFL,end_dateFL))
        #   if (!is.na(start_date) && !is.na(end_date)) {  # Only proceed if both dates are found
        #     flowering_data <- subset(year_data, Date >= start_dateFL & Date <= end_dateFL)
        #     maturity_data <- subset(year_data, Date >= start_date & Date <= end_date)
        #     if (nrow(flowering_data) >= 2){ # Check if there are at least 2 days of flowering data
        #       heat_stress_eventsFL <- zoo::rollapply(flowering_data$Tmax, width = 2, FUN = is_heat_stress, fill = NA, align = "left")
        #       heat_stress_daysFL <- sum(heat_stress_eventsFL, na.rm = TRUE) * 2
        #     }
        #     if (nrow(maturity_data) >= 2){ # Check if there are at least 2 days of flowering data
        #       heat_stress_events <- zoo::rollapply(maturity_data$Tmax, width = 2, FUN = is_heat_stress, fill = NA, align = "left")
        #       heat_stress_days <- sum(heat_stress_events, na.rm = TRUE) * 2
        #     }
        #     else {
        #       heat_stress_days <- 0
        #       heat_stress_daysFL <- 0# No heat stress days if flowering period is less than 2 days
        #     }
        #   } else {
        #     heat_stress_days <- 0
        #     heat_stress_daysFL <- 0# Or NA, if you prefer to indicate that flowering period couldn't be determined
        #     cat("Flowering period not found for year", current_year, "\n")
        #   }
        #   results$HeatStressDaysFL[results$Year == current_year] <- heat_stress_daysFL
        #   results$HeatStressDaysMT[results$Year == current_year] <- heat_stress_days
        # }
        podatki_tocka <- data.table(lats = rep(lat[j], length(growing_degree_days)), 
                                    lons = rep(lon[i], length(growing_degree_days)), 
                                    leto = leta1,
                                    model = rep(model[z], length(growing_degree_days)),
                                    growing_degree_days, 
                                    GSL = Growing_season_length, 
                                    # End_growing_season,
                                    BEDD, 
                                    Sums_Tmax32, 
                                    T_warmest_m = warmest_m_temp$max, 
                                    TG_of_warmest_quarter, 
                                    TG_of_coldest_quarter,
                                    Diurnal_temp_rangeN, 
                                    # chill_portions,
                                    Dnevi_32max_anthesis = Dnevi_32max_anthesis$n,
                                    # Heat_stress_fl = results$HeatStressDaysFL,
                                    # Heat_stress_mat = results$HeatStressDaysMT,
                                    cons_summer_days, 
                                    WWD = WWD$warm_wet_days, 
                                    WSDI = result_df$consecutive_days,
                                    CDD = Max_consecutive_dry_days,#DD = Dry_days, 
                                    CWD = longest_wet_period, 
                                    r20mm, 
                                    Heavy_prec_days,# r10mm, 
                                    SDII, 
                                    Prec_wettest_month, #Prec_warmest_quarter, Prec_coldest_quarter,wet_days, 
                                    FD = frost_days, 
                                    late_frost_days = results_frost$LateFrostCount,
                                    CSDI, 
                                    CFD = cons_frost_days, 
                                    ice_days, 
                                    Sums_Tmin10, Sums_Tmin15,
                                    Effective_prec,
                                    Growing_season_prec,#Very_wet_days,
                                    Nongrowing_season_prec,
                                    # precip_total,
                                    su, 
                                    tr, 
                                    tn90p = t90_test$tn90p, 
                                    vwd = tx99_test$tx99p,
                                    rx5d)
        vsi_podatki <- rbind(vsi_podatki, podatki_tocka)
        
      }
    }    
    
  }
  saveRDS(vsi_podatki, file = paste0("psenica/period_mean_",rcp,"_",obd,"_",model[z],"_",drugo[z],".rds"))
  return(vsi_podatki)
}

# posamezen_model <- en_model0(rcp = "rcp45", leto_zac = "2071", obd = "2071-2100", z = 1)
# posam_model2 <- en_model0(rcp = "rcp45", leto_zac = "2071", obd = "2071-2100", z = 2)
# posam_model3 <- en_model0(rcp = "rcp45", leto_zac = "2071", obd = "2071-2100", z = 3)
# posam_model4 <- en_model0(rcp = "rcp45", leto_zac = "2071", obd = "2071-2100", z = 4)
# posam_model5 <- en_model0(rcp = "rcp45", leto_zac = "2071", obd = "2071-2100", z = 5)
# posam_model6 <- en_model0(rcp = "rcp45", leto_zac = "2071", obd = "2071-2100", z = 6)

# posamezen_model <- en_model0(rcp = "rcp85", leto_zac = "2071", obd = "2071-2100", z = 1)
# posam_model2 <- en_model0(rcp = "rcp85", leto_zac = "2071", obd = "2071-2100", z = 2)
# posam_model3 <- en_model0(rcp = "rcp85", leto_zac = "2071", obd = "2071-2100", z = 3)
# posam_model4 <- en_model0(rcp = "rcp85", leto_zac = "2071", obd = "2071-2100", z = 4) #ne se
# posam_model5 <- en_model0(rcp = "rcp85", leto_zac = "2071", obd = "2071-2100", z = 5)
# posam_model6 <- en_model0(rcp = "rcp85", leto_zac = "2071", obd = "2071-2100", z = 6)


posamezen_model0 <- readRDS(paste0("psenica/period_mean_rcp45_2071-2100_", model[1],"_",drugo[1],".rds"))# psenica/ pove da gledam celoletne kazalnike
posamezen_model1 <- readRDS(paste0("psenica/period_mean_rcp85_2071-2100_", model[1],"_",drugo[1],".rds"))
posam_model02 <- readRDS(paste0("psenica/period_mean_rcp45_2071-2100_", model[2],"_",drugo[2],".rds"))
posam_model12 <- readRDS(paste0("psenica/period_mean_rcp85_2071-2100_", model[2],"_",drugo[2],".rds"))
posam_model03 <- readRDS(paste0("psenica/period_mean_rcp45_2071-2100_", model[3],"_",drugo[3],".rds"))
posam_model13 <- readRDS(paste0("psenica/period_mean_rcp85_2071-2100_", model[3],"_",drugo[3],".rds"))
posam_model04 <- readRDS(paste0("psenica/period_mean_rcp45_2071-2100_", model[4],"_",drugo[4],".rds"))
posam_model14 <- readRDS(paste0("psenica/period_mean_rcp85_2071-2100_", model[4],"_",drugo[4],".rds"))
posam_model05 <- readRDS(paste0("psenica/period_mean_rcp45_2071-2100_", model[5],"_",drugo[5],".rds"))
posam_model15 <- readRDS(paste0("psenica/period_mean_rcp85_2071-2100_", model[5],"_",drugo[5],".rds"))
posam_model06 <- readRDS(paste0("psenica/period_mean_rcp45_2071-2100_", model[6],"_",drugo[6],".rds"))
posam_model16 <- readRDS(paste0("psenica/period_mean_rcp85_2071-2100_", model[6],"_",drugo[6],".rds"))

povp_obd <- data.table() 
yrs <- posamezen_model0$leto
# kaz <- names(posamezen_model0)[5:35]
zac = 2071
kon = 2100
rcp="rcp45"
l = list(posamezen_model0, posam_model02, posam_model03,
         posam_model04, posam_model05, posam_model06)
vsi_podatki_70_10_rcp45 <- rbindlist(l)
vsi_podatki_70_10_rcp45 <- vsi_podatki_70_10_rcp45[,c(1:24,26:38)] # odstranimo samo stolpec "year", ki je ostal od izračuna CSDI
vsi_podatki_70_10_rcp45 <- vsi_podatki_70_10_rcp45 %>% filter(leto <= 2100, leto >= 2071)
saveRDS(vsi_podatki_70_10_rcp45, file = "psenica/kazalniki_71-00_rcp45.rds")


rcp="rcp85"
l = list(posamezen_model1, posam_model12, posam_model13,
         posam_model14, posam_model15, posam_model16)
vsi_podatki_70_10_rcp85 <- rbindlist(l)
vsi_podatki_70_10_rcp85 <- vsi_podatki_70_10_rcp85[,c(1:24,26:38)]
vsi_podatki_70_10_rcp85 <- vsi_podatki_70_10_rcp85 %>% filter(leto <= 2100, leto >= 2071)
saveRDS(vsi_podatki_70_10_rcp85, file = "psenica/kazalniki_71-00_rcp85.rds")

################# 2041-2070 #######################

leto_zac <- "2041"
leto_kon <- c("20701231","20701231","20701231","20701231","20701231","20701231")
leto_kon_rr <- c("20701231","20701231","20701231","20701231","20701231","20701231")
leto_zac1 <- "2040"


# posamezen_model0 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 1)
# posam_model02 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 2)
# posam_model03 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 3)
# posam_model04 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 4)
# posam_model05 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 5)
# posam_model06 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 6)
# 
# 
# posamezen_model <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 1)
# posam_model2 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 2)
# posam_model3 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 3)
# posam_model4 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 4)
# posam_model5 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 5)
# posam_model6 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 6)

posamezen_model1 <- readRDS(paste0("psenica/period_mean_rcp85_2041-2070_", model[1],"_",drugo[1],".rds"))# psenica/ pove da gledam celoletne kazalnike
posam_model12 <- readRDS(paste0("psenica/period_mean_rcp85_2041-2070_", model[2],"_",drugo[2],".rds"))
posam_model13 <- readRDS(paste0("psenica/period_mean_rcp85_2041-2070_", model[3],"_",drugo[3],".rds"))
posam_model14 <- readRDS(paste0("psenica/period_mean_rcp85_2041-2070_", model[4],"_",drugo[4],".rds"))
posam_model15 <- readRDS(paste0("psenica/period_mean_rcp85_2041-2070_", model[5],"_",drugo[5],".rds"))
posam_model16 <- readRDS(paste0("psenica/period_mean_rcp85_2041-2070_", model[6],"_",drugo[6],".rds"))

posamezen_model0 <- readRDS(paste0("psenica/period_mean_rcp45_2041-2070_", model[1],"_",drugo[1],".rds"))
posam_model02 <- readRDS(paste0("psenica/period_mean_rcp45_2041-2070_", model[2],"_",drugo[2],".rds"))
posam_model03 <- readRDS(paste0("psenica/period_mean_rcp45_2041-2070_", model[3],"_",drugo[3],".rds"))
posam_model04 <- readRDS(paste0("psenica/period_mean_rcp45_2041-2070_", model[4],"_",drugo[4],".rds"))
posam_model05 <- readRDS(paste0("psenica/period_mean_rcp45_2041-2070_", model[5],"_",drugo[5],".rds"))
posam_model06 <- readRDS(paste0("psenica/period_mean_rcp45_2041-2070_", model[6],"_",drugo[6],".rds"))

# posamezen_model0 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 1)
# posamezen_model1 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 1)
# posam_model02 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 2)
# posam_model12 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 2)
# posam_model03 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 3)
# posam_model13 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 3)
# posam_model04 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 4)
# posam_model14 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 4)
# posam_model05 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 5)
# posam_model15 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 5)
# posam_model06 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 6)
# posam_model16 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 6)

povp_obd <- data.table()
yrs <- posamezen_model0$leto
zac = 2041
kon = 2070
rcp="rcp45"
l = list(posamezen_model0, posam_model02, posam_model03,
         posam_model04, posam_model05, posam_model06)
vsi_podatki_41_70_rcp45 <- rbindlist(l)
vsi_podatki_41_70_rcp45 <- vsi_podatki_41_70_rcp45[,c(1:24,26:38)]
saveRDS(vsi_podatki_41_70_rcp45, file = "psenica/kazalniki_41-70_rcp45.rds")
kaz <- names(vsi_podatki_41_70_rcp45)[5:37]

rcp="rcp85"
l = list(posamezen_model1, posam_model12, posam_model13,
         posam_model14, posam_model15, posam_model16)
vsi_podatki_41_70_rcp85 <- rbindlist(l)
vsi_podatki_41_70_rcp85 <- vsi_podatki_41_70_rcp85[,c(1:24,26:38)]
saveRDS(vsi_podatki_41_70_rcp85, file = "psenica/kazalniki_41-70_rcp85.rds")


################# 2011-2040 #######################

leto_zac <- "2011"
leto_kon <- c("20401231","20401231","20401231","20401231","20401231","20401231")
leto_kon_rr <- c("20401231","20401231","20401231","20401231","20401231","20401231")
leto_zac1 <- "2010"


# posamezen_model0 <- en_model0(rcp = "rcp45", leto_zac = "2011", obd = "2011-2040", z = 1)
# posam_model02 <- en_model0(rcp = "rcp45", leto_zac = "2011", obd = "2011-2040", z = 2)
# posam_model03 <- en_model0(rcp = "rcp45", leto_zac = "2011", obd = "2011-2040", z = 3)
# posam_model04 <- en_model0(rcp = "rcp45", leto_zac = "2011", obd = "2011-2040", z = 4)
# posam_model05 <- en_model0(rcp = "rcp45", leto_zac = "2011", obd = "2011-2040", z = 5)
# posam_model06 <- en_model0(rcp = "rcp45", leto_zac = "2011", obd = "2011-2040", z = 6)
# 
# 
# posamezen_model1 <- en_model0(rcp = "rcp85", leto_zac = "2011", obd = "2011-2040", z = 1)
# posam_model12 <- en_model0(rcp = "rcp85", leto_zac = "2011", obd = "2011-2040", z = 2)
# posam_model13 <- en_model0(rcp = "rcp85", leto_zac = "2011", obd = "2011-2040", z = 3)
# posam_model14 <- en_model0(rcp = "rcp85", leto_zac = "2011", obd = "2011-2040", z = 4)
# posam_model15 <- en_model0(rcp = "rcp85", leto_zac = "2011", obd = "2011-2040", z = 5)
# posam_model16 <- en_model0(rcp = "rcp85", leto_zac = "2011", obd = "2011-2040", z = 6)

posamezen_model1 <- readRDS(paste0("psenica/period_mean_rcp85_2011-2040_", model[1],"_",drugo[1],".rds")) # psenica/ pove da gledam celoletne kazalnike
posam_model12 <- readRDS(paste0("psenica/period_mean_rcp85_2011-2040_", model[2],"_",drugo[2],".rds"))
posam_model13 <- readRDS(paste0("psenica/period_mean_rcp85_2011-2040_", model[3],"_",drugo[3],".rds"))
posam_model14 <- readRDS(paste0("psenica/period_mean_rcp85_2011-2040_", model[4],"_",drugo[4],".rds"))
posam_model15 <- readRDS(paste0("psenica/period_mean_rcp85_2011-2040_", model[5],"_",drugo[5],".rds"))
posam_model16 <- readRDS(paste0("psenica/period_mean_rcp85_2011-2040_", model[6],"_",drugo[6],".rds"))

posamezen_model0 <- readRDS(paste0("psenica/period_mean_rcp45_2011-2040_", model[1],"_",drugo[1],".rds"))
posam_model02 <- readRDS(paste0("psenica/period_mean_rcp45_2011-2040_", model[2],"_",drugo[2],".rds"))
posam_model03 <- readRDS(paste0("psenica/period_mean_rcp45_2011-2040_", model[3],"_",drugo[3],".rds"))
posam_model04 <- readRDS(paste0("psenica/period_mean_rcp45_2011-2040_", model[4],"_",drugo[4],".rds"))
posam_model05 <- readRDS(paste0("psenica/period_mean_rcp45_2011-2040_", model[5],"_",drugo[5],".rds"))
posam_model06 <- readRDS(paste0("psenica/period_mean_rcp45_2011-2040_", model[6],"_",drugo[6],".rds"))

povp_obd <- data.table()
yrs <- posamezen_model0$leto
zac = 2011
kon = 2040
rcp="rcp45"
l = list(posamezen_model0, posam_model02, posam_model03,
         posam_model04, posam_model05, posam_model06)
vsi_podatki_11_40_rcp45 <- rbindlist(l)
vsi_podatki_11_40_rcp45 <- vsi_podatki_11_40_rcp45[,c(1:24,26:38)]
vsi_podatki_11_40_rcp45 <- vsi_podatki_11_40_rcp45 %>% filter(leto <= 2040, leto >= 2011)
saveRDS(vsi_podatki_11_40_rcp45, file = "psenica/kazalniki_11-40_rcp45.rds")

rcp="rcp85"
l = list(posamezen_model1, posam_model12, posam_model13,
         posam_model14, posam_model15, posam_model16)
vsi_podatki_11_40_rcp85 <- rbindlist(l)
vsi_podatki_11_40_rcp85 <- vsi_podatki_11_40_rcp85[,c(1:24,26:38)]
vsi_podatki_11_40_rcp85 <- vsi_podatki_11_40_rcp85 %>% filter(leto <= 2040, leto >= 2011)
saveRDS(vsi_podatki_11_40_rcp85, file = "psenica/kazalniki_11-40_rcp85.rds")

###################################################################
#                                                                 #
#         IZRIS PROJEKCIJ KAZALNIKOV ZA KORUZO IN PŠENICO         #
#                                                                 #
###################################################################



########## KARTE PROJEKCIJ - PŠENICA

vsi_podatki_71_00_rcp85 = readRDS(file = "psenica/kazalniki_71-00_rcp85.rds")
vsi_podatki_71_00_rcp45 = readRDS(file = "psenica/kazalniki_71-00_rcp45.rds")
vsi_podatki_41_70_rcp85 = readRDS(file = "psenica/kazalniki_41-70_rcp85.rds")
vsi_podatki_41_70_rcp45 = readRDS(file = "psenica/kazalniki_41-70_rcp45.rds")
vsi_podatki_11_40_rcp85 = readRDS(file = "psenica/kazalniki_11-40_rcp85.rds")
vsi_podatki_11_40_rcp45 = readRDS(file = "psenica/kazalniki_11-40_rcp45.rds")

dodaj_z_in_faktorje <- function(vsi_podatki,vsi_podatki_1){
  
  M_vrednosti <- as.numeric(vsi_podatki[1, 70:102])
  SD_vrednosti <- as.numeric(vsi_podatki[1, 103:135])
  
  z_vrednosti <- sweep(vsi_podatki_1[, 5:37], 2, M_vrednosti, FUN = "-")
  z_vrednosti <- sweep(z_vrednosti, 2, SD_vrednosti, FUN = "/")
  vsi_podatki_2 <- cbind(vsi_podatki_1, z_vrednosti)
  imena_z <- paste0("z", colnames(vsi_podatki_1)[5:37])
  colnames(vsi_podatki_2)[(ncol(vsi_podatki_1) + 1):ncol(vsi_podatki_2)] <- imena_z
  
  vsi_podatki_3 <- vsi_podatki_2
  
  vsi_podatki_3$zpozeba<-(vsi_podatki_3$zCFD + vsi_podatki_3$zFD + vsi_podatki_3$zice_days + 
                            vsi_podatki_3$zlate_frost_days)/4
  
  vsi_podatki_3$zvrocinski_stres<-(vsi_podatki_3$zSums_Tmax32 + vsi_podatki_3$zDnevi_32max_anthesis + 
                                     vsi_podatki_3$zvwd + vsi_podatki_3$zWSDI)/4
  
  vsi_podatki_3$zrastna<-(vsi_podatki_3$zgrowing_degree_days + vsi_podatki_3$zGSL + 
                            vsi_podatki_3$zTG_of_warmest_quarter + vsi_podatki_3$zBEDD +
                            vsi_podatki_3$zcons_summer_days + vsi_podatki_3$zsu + vsi_podatki_3$zTG_of_coldest_quarter + 
                            vsi_podatki_3$zT_warmest_m + vsi_podatki_3$zDiurnal_temp_rangeN)/9
  
  vsi_podatki_3$zvrocinski_stres_Vlaga_noc<-(vsi_podatki_3$zWWD + vsi_podatki_3$ztn90p)/2
  
  vsi_podatki_3$zmin_padavine <- vsi_podatki_3$zCDD
  
  # vsi_podatki_3$zvisoke_padavine <- (vsi_podatki_3$zPrec_wettest_month + vsi_podatki_3$zr20mm + 
  #                                    vsi_podatki_3$zrx5d + vsi_podatki_3$zHeavy_prec_days + 
  #                                    vsi_podatki_3$zCWD + vsi_podatki_3$zSDII)/6
  
  vsi_podatki_3$zvisoke_padavine <- (vsi_podatki_3$zPrec_wettest_month + vsi_podatki_3$zr20mm + 
                                       vsi_podatki_3$zrx5d + vsi_podatki_3$zHeavy_prec_days + 
                                       vsi_podatki_3$zCWD + vsi_podatki_3$zSDII +
                                       vsi_podatki_3$zEffective_prec + vsi_podatki_3$zGrowing_season_prec + 
                                       vsi_podatki_3$zNongrowing_season_prec)/9
  
  # vsi_podatki_3$zkonec_rastne<-(vsi_podatki_3$zGSL+vsi_podatki_3$zEnd_growing_season)/2
  
  vsi_podatki_3$zprezimovanje_minT<-(vsi_podatki_3$zcsdi_annual_sum +
                                       vsi_podatki_3$zSums_Tmin10 + vsi_podatki_3$zSums_Tmin15)/3
  
  
  return(vsi_podatki_3)
}
vsi_podatki <- readRDS(file = "../kazalniki_1981-2010_psenica.rds")
vsi_podatki_1 = vsi_podatki_41_70_rcp45

faktorji_71_00_rcp45 <- dodaj_z_in_faktorje(vsi_podatki,vsi_podatki_71_00_rcp45)
# colnames(faktorji_71_00_rcp45)[13] <- "Dnevi_32max_anthesis"
# colnames(faktorji_71_00_rcp45)[46] <- "zDnevi_32max_anthesis"
faktorji_71_00_rcp85 <- dodaj_z_in_faktorje(vsi_podatki,vsi_podatki_71_00_rcp85)
# colnames(faktorji_71_00_rcp85)[13] <- "Dnevi_32max_anthesis"
# colnames(faktorji_71_00_rcp85)[46] <- "zDnevi_32max_anthesis"
faktorji_41_70_rcp45 <- dodaj_z_in_faktorje(vsi_podatki,vsi_podatki_41_70_rcp45)
# colnames(faktorji_41_70_rcp45)[13] <- "Dnevi_32max_anthesis"
# colnames(faktorji_41_70_rcp45)[46] <- "zDnevi_32max_anthesis"
faktorji_41_70_rcp85 <- dodaj_z_in_faktorje(vsi_podatki,vsi_podatki_41_70_rcp85)
# colnames(faktorji_41_70_rcp85)[13] <- "Dnevi_32max_anthesis"
# colnames(faktorji_41_70_rcp85)[46] <- "zDnevi_32max_anthesis"
faktorji_11_40_rcp45 <- dodaj_z_in_faktorje(vsi_podatki,vsi_podatki_11_40_rcp45)
# colnames(faktorji_11_40_rcp45)[13] <- "Dnevi_32max_anthesis"
# colnames(faktorji_11_40_rcp45)[46] <- "zDnevi_32max_anthesis"
faktorji_11_40_rcp85 <- dodaj_z_in_faktorje(vsi_podatki,vsi_podatki_11_40_rcp85)
# colnames(faktorji_11_40_rcp85)[13] <- "Dnevi_32max_anthesis"
# colnames(faktorji_11_40_rcp85)[46] <- "zDnevi_32max_anthesis"




zac = 2041
kon = 2070
kazalniki <- names(faktorji_11_40_rcp45)[5:37]

minimums <- c()
maximums <- c()
for(kaz in kazalniki){
  kaz_min <- min(min(faktorji_41_70_rcp45[[kaz]]),#min(faktorji_11_40_rcp45[[kaz]]), min(faktorji_11_40_rcp85[[kaz]]), 
                 min(faktorji_41_70_rcp85[[kaz]]), min(faktorji_71_00_rcp45[[kaz]])) #- min(vsi_podatki[[kaz]]), min(faktorji_71_00_rcp85[[kaz]])
  kaz_max <- max(max(faktorji_41_70_rcp45[[kaz]]),#max(faktorji_11_40_rcp45[[kaz]]), max(faktorji_11_40_rcp85[[kaz]]), 
                 max(faktorji_41_70_rcp85[[kaz]]), max(faktorji_71_00_rcp45[[kaz]])) #- max(vsi_podatki[[kaz]]), max(faktorji_71_00_rcp85[[kaz]])
  print(c(kaz,kaz_min,kaz_max))
  minimums[[kaz]] <- kaz_min
  maximums[[kaz]] <- kaz_max
}
maximums[["Nongrowing_season_prec"]] <- 2200

maximums[["Dnevi_32max_anthesis"]] <- 10
maximums[["Heavy_prec_days"]] <- 20
maximums[["csdi_annual_sum"]] <- 30

for(kaz in kazalniki){
  print(kaz)
  data <- data.frame(faktorji_41_70_rcp45$lons, faktorji_41_70_rcp45$lats, faktorji_41_70_rcp45$leto, faktorji_41_70_rcp45$model, faktorji_41_70_rcp45[[kaz]])
  colnames(data) <- c("lons","lats","leto","model","agroclim_ind")
  data=na.omit(data)
  
  povp <- aggregate(agroclim_ind ~  lats + lons + model, data, FUN = mean , na.rm=TRUE, na.action=na.pass) # najprej naredimo povpre?je obdobja
  rattler.mediana <- aggregate(agroclim_ind ~  lats + lons, povp, FUN = median , na.rm=TRUE, na.action=na.pass) # nato naredimo me min max modelov
  rattler.min <- aggregate(agroclim_ind ~  lats + lons, povp, FUN = "min" , na.rm=TRUE, na.action=na.pass)
  rattler.max <- aggregate(agroclim_ind ~  lats + lons, povp, FUN = "max" , na.rm=TRUE, na.action=na.pass)
  min = min(na.omit(rattler.min$agroclim_ind))
  max = max(na.omit(rattler.max$agroclim_ind))
  
  data1 <- data.frame(vsi_podatki$lons, vsi_podatki$lats, vsi_podatki$leto, vsi_podatki[[kaz]])
  colnames(data1) <- c("lons","lats","leto","agroclim_ind")
  data1 <- data1 %>% filter(leto >= 1981, leto <= 2010)
  data1=na.omit(data1)
  
  povp_hist <- data1 %>% group_by(lats,lons) %>%
    summarise(period_mean = mean(agroclim_ind))
  
  ano <- rattler.mediana
  ano$period_mean <- ano$agroclim_ind-povp_hist$period_mean
  
  ano <- merge(rattler.mediana,povp_hist,by=c("lats","lons"))
  ano$anomalije <- ano$agroclim_ind - ano$period_mean
  
  ggplot() +
    geom_tile(data=povp_hist,aes(x=lons,y=lats,fill=period_mean)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller("Št. dni",palette = "Spectral", limits = c(minimums[[kaz]],maximums[[kaz]])) +
    # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
    labs(x="Geografska dolzina",y="Geografska sirina",title=paste0(kaz,", obdobje ",zac,"-",kon)) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 20)
  ggsave(paste0("kazalniki_karte/psenica/1981-2010/hist_period_mean_",kaz,"_",zac,"-",kon,".png"),width = 10, height = 7)
  
  
  # ggplot() +
  #   geom_tile(data=ano,aes(x=lons,y=lats,fill=anomalije)) +
  #   geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
  #   scale_fill_distiller(palette = "Spectral")+#, limits = c(min,max)) +
  #   # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
  #   labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("Kazalnik ",kaz,", obdobje ",zac,"-",kon)) +
  #   coord_sf(crs = st_crs(4326)) +
  #   theme_light(base_size = 17)
  # ggsave(paste0("kazalniki_karte/psenica/2041-2070_rcp45/ano_period_mean_",kaz,"_",zac,"-",kon,".png"),width = 10, height = 7)
  
  
  a<-ggplot() +
    geom_tile(data=rattler.mediana,aes(x=lons,y=lats,fill=agroclim_ind)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller("Št. dni",palette = "Spectral", limits = c(minimums[[kaz]],maximums[[kaz]])) +
    # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
    labs(x="Geografska dolzina",y="Geografska sirina",title=paste0(kaz,", me, RCP4.5, ",zac,"-",kon)) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 20)
  # ggsave(paste0("kazalniki_karte/psenica/2041-2070_rcp45/mediana_",kaz,"_",zac,"-",kon,".png"),width = 10, height = 7)
  
  b<-ggplot() +
    geom_tile(data=rattler.min,aes(x=lons,y=lats,fill=agroclim_ind)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller("Št. dni",palette = "Spectral", limits = c(minimums[[kaz]],maximums[[kaz]])) +
    # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
    labs(x="Geografska dolzina",y="Geografska sirina",title=paste0(kaz,", min, RCP4.5, ",zac,"-",kon)) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 20)
  # ggsave(paste0("kazalniki_karte/psenica/2041-2070_rcp45/min_",kaz,"_",zac,"-",kon,".png"),width = 10, height = 7)
  
  c<-ggplot() +
    geom_tile(data=rattler.max,aes(x=lons,y=lats,fill=agroclim_ind)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller("Št. dni",palette = "Spectral", limits = c(minimums[[kaz]],maximums[[kaz]])) +
    # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
    labs(x="Geografska dolzina",y="Geografska sirina",title=paste0(kaz,", max, RCP4.5, ",zac,"-",kon)) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 20)
  # ggsave(paste0("kazalniki_karte/psenica/2041-2070_rcp45/max_",kaz,"_",zac,"-",kon,".png"),width = 10, height = 7)
  
  d<-ggplot() +
    geom_tile(data=povp_hist,aes(x=lons,y=lats,fill=period_mean)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller("Št. dni",palette = "Spectral", limits = c(minimums[[kaz]],maximums[[kaz]])) +
    labs(x="Geografska dolzina",y="Geografska sirina",title=paste0(kaz,", obdobje 1981-2010")) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 20)
  
  # Create the bottom row
  bottom_row <- plot_grid(b, c, labels = c("c", "d"), label_size = 30, ncol = 2)
  
  # Create the top row with spacers (NULL in cowplot)
  top_row <- plot_grid(d, a, labels = c("a", "b"), label_size = 30,  ncol = 2)
  # Combine them vertically
  g <- plot_grid(top_row, bottom_row, ncol = 1)
  
  save_plot(paste0("kazalniki_karte/psenica/2041-2070_rcp45/", kaz, ".png"), g, base_width = 24, base_height = 14)
  my_sf_points <- st_as_sf(rattler.mediana,
                           coords = c("lons", "lats"), crs = st_crs(4326),
                           remove = FALSE   # pusti lon/lat stolpce, če jih želiš obdržati
  )
  plot(my_sf_points["agroclim_ind"])
  my_sf_points_3794 <- st_transform(my_sf_points, 3794)
  plot(my_sf_points_3794["agroclim_ind"])
  output_gpkg_path <- file.path(paste0("kazalniki_karte/psenica/ARSO/2041-2070_rcp45/",kaz,"mediana.gpkg"),sep="")
  layer_name <- "my_points_layer" # Name of the layer within the GeoPackage
  
  st_write(
    obj = my_sf_points_3794,
    dsn = output_gpkg_path,
    layer = layer_name, # Specify layer name
    driver = "GPKG", # GeoPackage driver
    delete_layer = TRUE # Overwrite if layer with this name exists in the GPKG
  )
  
  cat(paste0("GeoPackage exported to: ", output_gpkg_path, "\n"))
  cat(paste0("Layer name within GeoPackage: ", layer_name, "\n"))
  
  my_sf_points <- st_as_sf(rattler.min,
                           coords = c("lons", "lats"), crs = st_crs(4326),
                           remove = FALSE   # pusti lon/lat stolpce, če jih želiš obdržati
  )
  plot(my_sf_points["agroclim_ind"])
  my_sf_points_3794 <- st_transform(my_sf_points, 3794)
  plot(my_sf_points_3794["agroclim_ind"])
  
  output_gpkg_path <- file.path(paste0("kazalniki_karte/psenica/ARSO/2041-2070_rcp45/",kaz,"min.gpkg"),sep="")
  layer_name <- "my_points_layer" # Name of the layer within the GeoPackage
  
  st_write(
    obj = my_sf_points_3794,
    dsn = output_gpkg_path,
    layer = layer_name, # Specify layer name
    driver = "GPKG", # GeoPackage driver
    delete_layer = TRUE # Overwrite if layer with this name exists in the GPKG
  )
  
  cat(paste0("GeoPackage exported to: ", output_gpkg_path, "\n"))
  cat(paste0("Layer name within GeoPackage: ", layer_name, "\n"))
  
  my_sf_points <- st_as_sf(rattler.max,
                           coords = c("lons", "lats"), crs = st_crs(4326),
                           remove = FALSE   # pusti lon/lat stolpce, če jih želiš obdržati
  )
  plot(my_sf_points["agroclim_ind"])
  my_sf_points_3794 <- st_transform(my_sf_points, 3794)
  plot(my_sf_points_3794["agroclim_ind"])
  
  output_gpkg_path <- file.path(paste0("kazalniki_karte/psenica/ARSO/2041-2070_rcp45/",kaz,"max.gpkg"),sep="")
  layer_name <- "my_points_layer" # Name of the layer within the GeoPackage
  
  st_write(
    obj = my_sf_points_3794,
    dsn = output_gpkg_path,
    layer = layer_name, # Specify layer name
    driver = "GPKG", # GeoPackage driver
    delete_layer = TRUE # Overwrite if layer with this name exists in the GPKG
  )
  
  cat(paste0("GeoPackage exported to: ", output_gpkg_path, "\n"))
  cat(paste0("Layer name within GeoPackage: ", layer_name, "\n"))
  
  
}


for(kaz in kazalniki){
  print(kaz)
  data <- data.frame(faktorji_41_70_rcp85$lons, faktorji_41_70_rcp85$lats, faktorji_41_70_rcp85$leto, faktorji_41_70_rcp85$model, faktorji_41_70_rcp85[[kaz]])
  colnames(data) <- c("lons","lats","leto","model","agroclim_ind")
  data=na.omit(data)
  
  povp <- aggregate(agroclim_ind ~  lats + lons + model, data, FUN = mean , na.rm=TRUE, na.action=na.pass) # najprej naredimo povpre?je obdobja
  rattler.mediana <- aggregate(agroclim_ind ~  lats + lons, povp, FUN = median , na.rm=TRUE, na.action=na.pass) # nato naredimo me min max modelov
  rattler.min <- aggregate(agroclim_ind ~  lats + lons, povp, FUN = "min" , na.rm=TRUE, na.action=na.pass)
  rattler.max <- aggregate(agroclim_ind ~  lats + lons, povp, FUN = "max" , na.rm=TRUE, na.action=na.pass)
  min = min(na.omit(rattler.min$agroclim_ind))
  max = max(na.omit(rattler.max$agroclim_ind))
  
  data1 <- data.frame(vsi_podatki$lons, vsi_podatki$lats, vsi_podatki$leto, vsi_podatki[[kaz]])
  colnames(data1) <- c("lons","lats","leto","agroclim_ind")
  data1 <- data1 %>% filter(leto >= 1981, leto <= 2010)
  data1=na.omit(data1)
  
  povp_hist <- data1 %>% group_by(lats,lons) %>%
    summarise(period_mean = mean(agroclim_ind))
  
  ano <- rattler.mediana
  ano$period_mean <- ano$agroclim_ind-povp_hist$period_mean
  
  ano <- merge(rattler.mediana,povp_hist,by=c("lats","lons"))
  ano$anomalije <- ano$agroclim_ind - ano$period_mean
  
  # ggplot() +
  #   geom_tile(data=ano,aes(x=lons,y=lats,fill=anomalije)) +
  #   geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
  #   scale_fill_distiller(palette = "Spectral")+#, limits = c(min,max)) +
  #   # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
  #   labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("Kazalnik ",kaz,", obdobje ",zac,"-",kon)) +
  #   coord_sf(crs = st_crs(4326)) +
  #   theme_light(base_size = 17)
  # ggsave(paste0("kazalniki_karte/psenica/2041-2070_rcp85/ano_period_mean_",kaz,"_",zac,"-",kon,".png"),width = 10, height = 7)
  
  a<-ggplot() +
    geom_tile(data=rattler.mediana,aes(x=lons,y=lats,fill=agroclim_ind)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller(palette = "Spectral", limits = c(minimums[[kaz]],maximums[[kaz]])) +
    # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
    labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("Kazalnik ",kaz,", me, RCP8.5, ",zac,"-",kon)) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 20)
  # ggsave(paste0("kazalniki_karte/psenica/2041-2070_rcp85/mediana_",kaz,"_",zac,"-",kon,".png"),width = 10, height = 7)
  
  b<-ggplot() +
    geom_tile(data=rattler.min,aes(x=lons,y=lats,fill=agroclim_ind)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller(palette = "Spectral", limits = c(minimums[[kaz]],maximums[[kaz]])) +
    # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
    labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("Kazalnik ",kaz,", min, RCP8.5, ",zac,"-",kon)) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 20)
  # ggsave(paste0("kazalniki_karte/psenica/2041-2070_rcp85/min_",kaz,"_",zac,"-",kon,".png"),width = 10, height = 7)
  
  c<-ggplot() +
    geom_tile(data=rattler.max,aes(x=lons,y=lats,fill=agroclim_ind)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller(palette = "Spectral", limits = c(minimums[[kaz]],maximums[[kaz]])) +
    # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
    labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("Kazalnik ",kaz,", max, RCP8.5, ",zac,"-",kon)) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 20)
  # ggsave(paste0("kazalniki_karte/psenica/2041-2070_rcp85/max_",kaz,"_",zac,"-",kon,".png"),width = 10, height = 7)
  
  library(cowplot)
  
  d<-ggplot() +
    geom_tile(data=povp_hist,aes(x=lons,y=lats,fill=period_mean)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller(palette = "Spectral", limits = c(minimums[[kaz]],maximums[[kaz]])) +
    labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("Kazalnik ",kaz,", obdobje 1981-2010")) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 17)
  
  # Create the bottom row
  bottom_row <- plot_grid(b, c, labels = c("c", "d"), label_size = 30, ncol = 2)
  
  # Create the top row with spacers (NULL in cowplot)
  top_row <- plot_grid(d, a, labels = c("a", "b"), label_size = 30,  ncol = 2)
  
  # Combine them vertically
  g <- plot_grid(top_row, bottom_row, ncol = 1)
  
  save_plot(paste0("kazalniki_karte/psenica/2041-2070_rcp85/", kaz, ".png"), g, base_width = 24, base_height = 14)
  my_sf_points <- st_as_sf(rattler.mediana,
                           coords = c("lons", "lats"), crs = st_crs(4326),
                           remove = FALSE   # pusti lon/lat stolpce, če jih želiš obdržati
  )
  plot(my_sf_points["agroclim_ind"])
  my_sf_points_3794 <- st_transform(my_sf_points, 3794)
  plot(my_sf_points_3794["agroclim_ind"])
  output_gpkg_path <- file.path(paste0("kazalniki_karte/psenica/ARSO/2041-2070_rcp85/",kaz,"mediana.gpkg"),sep="")
  layer_name <- "my_points_layer" # Name of the layer within the GeoPackage
  st_write(
    obj = my_sf_points_3794,
    dsn = output_gpkg_path,
    layer = layer_name, # Specify layer name
    driver = "GPKG", # GeoPackage driver
    delete_layer = TRUE )
  cat(paste0("GeoPackage exported to: ", output_gpkg_path, "\n"))
  cat(paste0("Layer name within GeoPackage: ", layer_name, "\n"))
  
  my_sf_points <- st_as_sf(rattler.min,
                           coords = c("lons", "lats"), crs = st_crs(4326),
                           remove = FALSE   # pusti lon/lat stolpce, če jih želiš obdržati
  )
  plot(my_sf_points["agroclim_ind"])
  my_sf_points_3794 <- st_transform(my_sf_points, 3794)
  plot(my_sf_points_3794["agroclim_ind"])
  output_gpkg_path <- file.path(paste0("kazalniki_karte/psenica/ARSO/2041-2070_rcp85/",kaz,"min.gpkg"),sep="")
  layer_name <- "my_points_layer" # Name of the layer within the GeoPackage
  st_write(
    obj = my_sf_points_3794,
    dsn = output_gpkg_path,
    layer = layer_name, # Specify layer name
    driver = "GPKG", # GeoPackage driver
    delete_layer = TRUE )
  cat(paste0("GeoPackage exported to: ", output_gpkg_path, "\n"))
  cat(paste0("Layer name within GeoPackage: ", layer_name, "\n"))
  
  my_sf_points <- st_as_sf(rattler.max,
                           coords = c("lons", "lats"), crs = st_crs(4326),
                           remove = FALSE   # pusti lon/lat stolpce, če jih želiš obdržati
  )
  plot(my_sf_points["agroclim_ind"])
  my_sf_points_3794 <- st_transform(my_sf_points, 3794)
  plot(my_sf_points_3794["agroclim_ind"])
  output_gpkg_path <- file.path(paste0("kazalniki_karte/psenica/ARSO/2041-2070_rcp85/",kaz,"max.gpkg"),sep="")
  layer_name <- "my_points_layer" # Name of the layer within the GeoPackage
  st_write(
    obj = my_sf_points_3794,
    dsn = output_gpkg_path,
    layer = layer_name, # Specify layer name
    driver = "GPKG", # GeoPackage driver
    delete_layer = TRUE )
  cat(paste0("GeoPackage exported to: ", output_gpkg_path, "\n"))
  cat(paste0("Layer name within GeoPackage: ", layer_name, "\n"))
  
}
######################################### PROJEKCIJE MEDIANA MODELOV 2071 - 2100


zac = 2071
kon = 2100


for(kaz in kazalniki){
  print(kaz)
  data <- data.frame(faktorji_71_00_rcp45$lons, faktorji_71_00_rcp45$lats, faktorji_71_00_rcp45$leto, faktorji_71_00_rcp45$model, faktorji_71_00_rcp45[[kaz]])
  colnames(data) <- c("lons","lats","leto","model","agroclim_ind")
  data=na.omit(data)
  
  povp <- aggregate(agroclim_ind ~  lats + lons + model, data, FUN = mean , na.rm=TRUE, na.action=na.pass) # najprej naredimo povpre?je obdobja
  rattler.mediana <- aggregate(agroclim_ind ~  lats + lons, povp, FUN = median , na.rm=TRUE, na.action=na.pass) # nato naredimo me min max modelov
  rattler.min <- aggregate(agroclim_ind ~  lats + lons, povp, FUN = "min" , na.rm=TRUE, na.action=na.pass)
  rattler.max <- aggregate(agroclim_ind ~  lats + lons, povp, FUN = "max" , na.rm=TRUE, na.action=na.pass)
  min = min(na.omit(rattler.min$agroclim_ind))
  max = max(na.omit(rattler.max$agroclim_ind))  
  
  data1 <- data.frame(vsi_podatki$lons, vsi_podatki$lats, vsi_podatki$leto, vsi_podatki[[kaz]])
  colnames(data1) <- c("lons","lats","leto","agroclim_ind")
  data1 <- data1 %>% filter(leto >= 1981, leto <= 2010)
  data1=na.omit(data1)
  
  povp_hist <- data1 %>% group_by(lats,lons) %>%
    summarise(period_mean = mean(agroclim_ind))
  
  ano <- rattler.mediana
  ano$period_mean <- ano$agroclim_ind-povp_hist$period_mean
  
  ano <- merge(rattler.mediana,povp_hist,by=c("lats","lons"))
  ano$anomalije <- ano$agroclim_ind - ano$period_mean
  
  ggplot() +
    geom_tile(data=ano,aes(x=lons,y=lats,fill=anomalije)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller(palette = "Spectral")+#, limits = c(minimums[[kaz]],maximums[[kaz]])) +
    # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
    labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("Kazalnik ",kaz,", obdobje ",zac,"-",kon)) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 17)
  ggsave(paste0("kazalniki_karte/psenica/2071-2100_rcp45/ano_period_mean_",kaz,"_",zac,"-",kon,".png"),width = 10, height = 7)
  
  a<-ggplot() +
    geom_tile(data=rattler.mediana,aes(x=lons,y=lats,fill=agroclim_ind)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller(palette = "Spectral", limits = c(minimums[[kaz]],maximums[[kaz]])) +
    # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
    labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("Kazalnik ",kaz,", me, RCP4.5, ",zac,"-",kon)) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 17)
  # ggsave(paste0("kazalniki_karte/psenica/2071-2100_rcp45/mediana_",kaz,"_",zac,"-",kon,".png"),width = 10, height = 7)
  
  b<-ggplot() +
    geom_tile(data=rattler.min,aes(x=lons,y=lats,fill=agroclim_ind)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller(palette = "Spectral", limits = c(minimums[[kaz]],maximums[[kaz]])) +
    # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
    labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("Kazalnik ",kaz,", min, RCP4.5, ",zac,"-",kon)) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 17)
  # ggsave(paste0("kazalniki_karte/psenica/2071-2100_rcp45/min_",kaz,"_",zac,"-",kon,".png"),width = 10, height = 7)
  
  c<-ggplot() +
    geom_tile(data=rattler.max,aes(x=lons,y=lats,fill=agroclim_ind)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller(palette = "Spectral", limits = c(minimums[[kaz]],maximums[[kaz]])) +
    # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
    labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("Kazalnik ",kaz,", max, RCP4.5, ",zac,"-",kon)) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 17)
  # ggsave(paste0("kazalniki_karte/psenica/2071-2100_rcp45/max_",kaz,"_",zac,"-",kon,".png"),width = 10, height = 7)
  
  d<-ggplot() +
    geom_tile(data=povp_hist,aes(x=lons,y=lats,fill=period_mean)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller(palette = "Spectral", limits = c(minimums[[kaz]],maximums[[kaz]])) +
    labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("Kazalnik ",kaz,", obdobje 1981-2010")) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 17)
  
  # Create the bottom row
  bottom_row <- plot_grid(b, c, labels = c("c", "d"), label_size = 30, ncol = 2)
  
  # Create the top row with spacers (NULL in cowplot)
  top_row <- plot_grid(d, a, labels = c("a", "b"), label_size = 30,  ncol = 2)
  # Combine them vertically
  g <- plot_grid(top_row, bottom_row, ncol = 1)
  
  save_plot(paste0("kazalniki_karte/psenica/2071-2100_rcp45/", kaz, ".png"), g, base_width = 24, base_height = 14)
  
  my_sf_points <- st_as_sf(rattler.mediana,
                           coords = c("lons", "lats"), crs = st_crs(4326),
                           remove = FALSE   # pusti lon/lat stolpce, če jih želiš obdržati
  )
  plot(my_sf_points["agroclim_ind"])
  my_sf_points_3794 <- st_transform(my_sf_points, 3794)
  plot(my_sf_points_3794["agroclim_ind"])
  
  output_gpkg_path <- file.path(paste0("kazalniki_karte/psenica/ARSO/2071-2100_rcp45/",kaz,"mediana.gpkg"),sep="")
  layer_name <- "my_points_layer" # Name of the layer within the GeoPackage
  
  st_write(
    obj = my_sf_points_3794,
    dsn = output_gpkg_path,
    layer = layer_name, # Specify layer name
    driver = "GPKG", # GeoPackage driver
    delete_layer = TRUE # Overwrite if layer with this name exists in the GPKG
  )
  
  cat(paste0("GeoPackage exported to: ", output_gpkg_path, "\n"))
  cat(paste0("Layer name within GeoPackage: ", layer_name, "\n"))
  
  my_sf_points <- st_as_sf(rattler.min,
                           coords = c("lons", "lats"), crs = st_crs(4326),
                           remove = FALSE   # pusti lon/lat stolpce, če jih želiš obdržati
  )
  plot(my_sf_points["agroclim_ind"])
  my_sf_points_3794 <- st_transform(my_sf_points, 3794)
  plot(my_sf_points_3794["agroclim_ind"])
  
  output_gpkg_path <- file.path(paste0("kazalniki_karte/psenica/ARSO/2071-2100_rcp45/",kaz,"min.gpkg"),sep="")
  layer_name <- "my_points_layer" # Name of the layer within the GeoPackage
  
  st_write(
    obj = my_sf_points_3794,
    dsn = output_gpkg_path,
    layer = layer_name, # Specify layer name
    driver = "GPKG", # GeoPackage driver
    delete_layer = TRUE # Overwrite if layer with this name exists in the GPKG
  )
  
  cat(paste0("GeoPackage exported to: ", output_gpkg_path, "\n"))
  cat(paste0("Layer name within GeoPackage: ", layer_name, "\n"))
  
  my_sf_points <- st_as_sf(rattler.max,
                           coords = c("lons", "lats"), crs = st_crs(4326),
                           remove = FALSE   # pusti lon/lat stolpce, če jih želiš obdržati
  )
  plot(my_sf_points["agroclim_ind"])
  my_sf_points_3794 <- st_transform(my_sf_points, 3794)
  plot(my_sf_points_3794["agroclim_ind"])
  
  output_gpkg_path <- file.path(paste0("kazalniki_karte/psenica/ARSO/2071-2100_rcp45/",kaz,"max.gpkg"),sep="")
  layer_name <- "my_points_layer" # Name of the layer within the GeoPackage
  
  st_write(
    obj = my_sf_points_3794,
    dsn = output_gpkg_path,
    layer = layer_name, # Specify layer name
    driver = "GPKG", # GeoPackage driver
    delete_layer = TRUE # Overwrite if layer with this name exists in the GPKG
  )
  
  cat(paste0("GeoPackage exported to: ", output_gpkg_path, "\n"))
  cat(paste0("Layer name within GeoPackage: ", layer_name, "\n"))
  
}


for(kaz in kazalniki){
  print(kaz)
  data <- data.frame(faktorji_71_00_rcp85$lons, faktorji_71_00_rcp85$lats, faktorji_71_00_rcp85$leto, faktorji_71_00_rcp85$model, faktorji_71_00_rcp85[[kaz]])
  colnames(data) <- c("lons","lats","leto","model","agroclim_ind")
  data=na.omit(data)
  
  
  povp <- aggregate(agroclim_ind ~  lats + lons + model, data, FUN = mean , na.rm=TRUE, na.action=na.pass) # najprej naredimo povpre?je obdobja
  rattler.mediana <- aggregate(agroclim_ind ~  lats + lons, povp, FUN = median , na.rm=TRUE, na.action=na.pass) # nato naredimo me min max modelov
  rattler.min <- aggregate(agroclim_ind ~  lats + lons, povp, FUN = "min" , na.rm=TRUE, na.action=na.pass)
  rattler.max <- aggregate(agroclim_ind ~  lats + lons, povp, FUN = "max" , na.rm=TRUE, na.action=na.pass)
  min = min(na.omit(rattler.min$agroclim_ind))
  max = max(na.omit(rattler.max$agroclim_ind))  
  data1 <- data.frame(vsi_podatki$lons, vsi_podatki$lats, vsi_podatki$leto, vsi_podatki[[kaz]])
  colnames(data1) <- c("lons","lats","leto","agroclim_ind")
  data1 <- data1 %>% filter(leto >= 1981, leto <= 2010)
  data1=na.omit(data1)
  
  povp_hist <- data1 %>% group_by(lats,lons) %>%
    summarise(period_mean = mean(agroclim_ind))
  
  ano <- rattler.mediana
  ano$period_mean <- ano$agroclim_ind-povp_hist$period_mean
  
  ano <- merge(rattler.mediana,povp_hist,by=c("lats","lons"))
  ano$anomalije <- ano$agroclim_ind - ano$period_mean
  
  ggplot() +
    geom_tile(data=ano,aes(x=lons,y=lats,fill=anomalije)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller(palette = "Spectral")+#, limits = c(minimums[[kaz]],maximums[[kaz]])) +
    # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
    labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("Kazalnik ",kaz,", obdobje ",zac,"-",kon)) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 17)
  ggsave(paste0("kazalniki_karte/psenica/2071-2100_rcp85/ano_period_mean_",kaz,"_",zac,"-",kon,".png"),width = 10, height = 7)
  
  a<-ggplot() +
    geom_tile(data=rattler.mediana,aes(x=lons,y=lats,fill=agroclim_ind)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller(palette = "Spectral", limits = c(minimums[[kaz]],maximums[[kaz]])) +
    # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
    labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("Kazalnik ",kaz,", me, RCP8.5, ",zac,"-",kon)) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 17)
  # ggsave(paste0("kazalniki_karte/psenica/2071-2100_rcp85/mediana_",kaz,"_",zac,"-",kon,".png"),width = 10, height = 7)
  
  b<-ggplot() +
    geom_tile(data=rattler.min,aes(x=lons,y=lats,fill=agroclim_ind)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller(palette = "Spectral", limits = c(minimums[[kaz]],maximums[[kaz]])) +
    # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
    labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("Kazalnik ",kaz,", min, RCP8.5, ",zac,"-",kon)) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 17)
  # ggsave(paste0("kazalniki_karte/psenica/2071-2100_rcp85/min_",kaz,"_",zac,"-",kon,".png"),width = 10, height = 7)
  
  c<-ggplot() +
    geom_tile(data=rattler.max,aes(x=lons,y=lats,fill=agroclim_ind)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller(palette = "Spectral", limits = c(minimums[[kaz]],maximums[[kaz]])) +
    # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
    labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("Kazalnik ",kaz,", max, RCP8.5, ",zac,"-",kon)) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 17)
  # ggsave(paste0("kazalniki_karte/psenica/2071-2100_rcp85/max_",kaz,"_",zac,"-",kon,".png"),width = 10, height = 7)
  
  
  d<-ggplot() +
    geom_tile(data=povp_hist,aes(x=lons,y=lats,fill=period_mean)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller(palette = "Spectral", limits = c(minimums[[kaz]],maximums[[kaz]])) +
    labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("Kazalnik ",kaz,", obdobje 1981-2010")) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 17)
  
  # Create the bottom row
  bottom_row <- plot_grid(b, c, labels = c("c", "d"), label_size = 30, ncol = 2)
  
  # Create the top row with spacers (NULL in cowplot)
  top_row <- plot_grid(d, a, labels = c("a", "b"), label_size = 30,  ncol = 2)
  
  # Combine them vertically
  g <- plot_grid(top_row, bottom_row, ncol = 1)
  
  save_plot(paste0("kazalniki_karte/psenica/2071-2100_rcp85/", kaz, ".png"), g, base_width = 24, base_height = 14)
  
  my_sf_points <- st_as_sf(rattler.mediana,
                           coords = c("lons", "lats"), crs = st_crs(4326),
                           remove = FALSE   # pusti lon/lat stolpce, če jih želiš obdržati
  )
  plot(my_sf_points["agroclim_ind"])
  my_sf_points_3794 <- st_transform(my_sf_points, 3794)
  plot(my_sf_points_3794["agroclim_ind"])
  
  output_gpkg_path <- file.path(paste0("kazalniki_karte/psenica/ARSO/2071-2100_rcp85/",kaz,"mediana.gpkg"),sep="")
  layer_name <- "my_points_layer" # Name of the layer within the GeoPackage
  
  st_write(
    obj = my_sf_points_3794,
    dsn = output_gpkg_path,
    layer = layer_name, # Specify layer name
    driver = "GPKG", # GeoPackage driver
    delete_layer = TRUE # Overwrite if layer with this name exists in the GPKG
  )
  
  cat(paste0("GeoPackage exported to: ", output_gpkg_path, "\n"))
  cat(paste0("Layer name within GeoPackage: ", layer_name, "\n"))
  
  my_sf_points <- st_as_sf(rattler.min,
                           coords = c("lons", "lats"), crs = st_crs(4326),
                           remove = FALSE   # pusti lon/lat stolpce, če jih želiš obdržati
  )
  plot(my_sf_points["agroclim_ind"])
  my_sf_points_3794 <- st_transform(my_sf_points, 3794)
  plot(my_sf_points_3794["agroclim_ind"])
  
  output_gpkg_path <- file.path(paste0("kazalniki_karte/psenica/ARSO/2071-2100_rcp85/",kaz,"min.gpkg"),sep="")
  layer_name <- "my_points_layer" # Name of the layer within the GeoPackage
  
  st_write(
    obj = my_sf_points_3794,
    dsn = output_gpkg_path,
    layer = layer_name, # Specify layer name
    driver = "GPKG", # GeoPackage driver
    delete_layer = TRUE # Overwrite if layer with this name exists in the GPKG
  )
  
  cat(paste0("GeoPackage exported to: ", output_gpkg_path, "\n"))
  cat(paste0("Layer name within GeoPackage: ", layer_name, "\n"))
  
  my_sf_points <- st_as_sf(rattler.max,
                           coords = c("lons", "lats"), crs = st_crs(4326),
                           remove = FALSE   # pusti lon/lat stolpce, če jih želiš obdržati
  )
  plot(my_sf_points["agroclim_ind"])
  my_sf_points_3794 <- st_transform(my_sf_points, 3794)
  plot(my_sf_points_3794["agroclim_ind"])
  
  output_gpkg_path <- file.path(paste0("kazalniki_karte/psenica/ARSO/2071-2100_rcp85/",kaz,"max.gpkg"),sep="")
  layer_name <- "my_points_layer" # Name of the layer within the GeoPackage
  
  st_write(
    obj = my_sf_points_3794,
    dsn = output_gpkg_path,
    layer = layer_name, # Specify layer name
    driver = "GPKG", # GeoPackage driver
    delete_layer = TRUE # Overwrite if layer with this name exists in the GPKG
  )
  
  cat(paste0("GeoPackage exported to: ", output_gpkg_path, "\n"))
  cat(paste0("Layer name within GeoPackage: ", layer_name, "\n"))
  
}



########## ČASOVNI POTEKI Z OVOJNICAMI

minimums <- c()
maximums <- c()
imena <- c()
i=1
kaz_min <- c(-3, -2, -2, -2, -2, -2, -2)
kaz_max <- c( 3, 10,  3, 10,  7,  3, 3)
imena_i <- c("razmere spomladanske pozebe","zmerni vročinski stres","temperaturne razmere rastne dobe",
             "nočni vročinski stres in visoka vlaga","majhna količina padavin","velika količina padavin","prezimovalne razmere")

for(kaz in names(faktorji_11_40_rcp45)[71:77]){
  minimums[[kaz]] <- kaz_min[i]
  maximums[[kaz]] <- kaz_max[i]
  imena[[kaz]] <- imena_i[i]
  print(c(kaz,kaz_min[i],kaz_max[i],imena_i[i]))
  
  i = i+1
}


for(kaz in names(faktorji_11_40_rcp45)[c(5:31,33:37)]){ #71:75,77},]){ 
  
  print(kaz)
  data0 <- data.frame(vsi_podatki$lons, vsi_podatki$lats, vsi_podatki$leto, vsi_podatki[[kaz]])
  colnames(data0) <- c("lons","lats","leto","agroclim_ind")
  data0 <- data0 %>% filter(leto >= 1981, leto <= 2010)
  data0=na.omit(data0)
  povp_1981_2010 <- data0 %>% group_by(leto) %>%
    summarise(slo_mean = mean(agroclim_ind))
  
  # RCP4.5
  data1 <- data.frame(faktorji_11_40_rcp45$lons, faktorji_11_40_rcp45$lats, faktorji_11_40_rcp45$leto, faktorji_11_40_rcp45$model, faktorji_11_40_rcp45[[kaz]])
  colnames(data1) <- c("lons","lats","leto","model","agroclim_ind")
  data1 <- data1 %>% filter(leto >= 2011, leto <= 2040)
  data1=na.omit(data1)
  povp_2011_2040 <- data1 %>% group_by(model,leto) %>%
    summarise(slo_mean = mean(agroclim_ind))
  povp_2011_2040.mediana <- aggregate(slo_mean ~  leto, povp_2011_2040, FUN = median , na.rm=TRUE, na.action=na.pass) # nato naredimo me min max modelov
  povp_2011_2040.min <- aggregate(slo_mean ~  leto, povp_2011_2040, FUN = "min" , na.rm=TRUE, na.action=na.pass)
  povp_2011_2040.max <- aggregate(slo_mean ~  leto, povp_2011_2040, FUN = "max" , na.rm=TRUE, na.action=na.pass)
  
  data2 <- data.frame(faktorji_41_70_rcp45$lons, faktorji_41_70_rcp45$lats, faktorji_41_70_rcp45$leto, faktorji_41_70_rcp45$model, faktorji_41_70_rcp45[[kaz]])
  colnames(data2) <- c("lons","lats","leto","model","agroclim_ind")
  data2 <- data2 %>% filter(leto >= 2041, leto <= 2070)
  data2=na.omit(data2)
  povp_2041_2070 <- data2 %>% group_by(model,leto) %>%
    summarise(slo_mean = mean(agroclim_ind))
  povp_2041_2070.mediana <- aggregate(slo_mean ~  leto, povp_2041_2070, FUN = median , na.rm=TRUE, na.action=na.pass) # nato naredimo me min max modelov
  povp_2041_2070.min <- aggregate(slo_mean ~  leto, povp_2041_2070, FUN = "min" , na.rm=TRUE, na.action=na.pass)
  povp_2041_2070.max <- aggregate(slo_mean ~  leto, povp_2041_2070, FUN = "max" , na.rm=TRUE, na.action=na.pass)
  
  data3 <- data.frame(faktorji_71_00_rcp45$lons, faktorji_71_00_rcp45$lats, faktorji_71_00_rcp45$leto, faktorji_71_00_rcp45$model, faktorji_71_00_rcp45[[kaz]])
  colnames(data3) <- c("lons","lats","leto","model","agroclim_ind")
  data3 <- data3 %>% filter(leto >= 2071, leto <= 2100)
  data3=na.omit(data3)
  povp_2071_2100 <- data3 %>% group_by(model,leto) %>%
    summarise(slo_mean = mean(agroclim_ind))
  povp_2071_2100.mediana <- aggregate(slo_mean ~  leto, povp_2071_2100, FUN = median , na.rm=TRUE, na.action=na.pass) # nato naredimo me min max modelov
  povp_2071_2100.min <- aggregate(slo_mean ~  leto, povp_2071_2100, FUN = "min" , na.rm=TRUE, na.action=na.pass)
  povp_2071_2100.max <- aggregate(slo_mean ~  leto, povp_2071_2100, FUN = "max" , na.rm=TRUE, na.action=na.pass)
  
  # mediana_1981_2100 <- rbind(povp_1981_2010,povp_2041_2070)
  mediana_2011_2100 <- rbind(rbind(povp_2011_2040.mediana,povp_2041_2070.mediana),povp_2071_2100.mediana)
  min_2011_2100 <- rbind(rbind(povp_2011_2040.min,povp_2041_2070.min),povp_2071_2100.min)
  max_2011_2100 <- rbind(rbind(povp_2011_2040.max,povp_2041_2070.max),povp_2071_2100.max)
  
  
  rmediana45s=loess.smooth(2011:2100, mediana_2011_2100$slo_mean, span=0.75, evaluation = 2100-2011+1, degree = 1)$y
  rmax45s=loess.smooth(2011:2100, min_2011_2100$slo_mean, span=0.75, evaluation = 2100-2011+1, degree = 1)$y
  rmin45s=loess.smooth(2011:2100, max_2011_2100$slo_mean, span=0.75, evaluation = 2100-2011+1, degree = 1)$y
  
  # RCP8.5
  data1 <- data.frame(faktorji_11_40_rcp85$lons, faktorji_11_40_rcp85$lats, faktorji_11_40_rcp85$leto, faktorji_11_40_rcp85$model, faktorji_11_40_rcp85[[kaz]])
  colnames(data1) <- c("lons","lats","leto","model","agroclim_ind")
  data1 <- data1 %>% filter(leto >= 2011, leto <= 2040)
  data1=na.omit(data1)
  povp_2011_2040 <- data1 %>% group_by(model,leto) %>%
    summarise(slo_mean = mean(agroclim_ind))
  povp_2011_2040.mediana <- aggregate(slo_mean ~  leto, povp_2011_2040, FUN = median , na.rm=TRUE, na.action=na.pass) # nato naredimo me min max modelov
  povp_2011_2040.min <- aggregate(slo_mean ~  leto, povp_2011_2040, FUN = "min" , na.rm=TRUE, na.action=na.pass)
  povp_2011_2040.max <- aggregate(slo_mean ~  leto, povp_2011_2040, FUN = "max" , na.rm=TRUE, na.action=na.pass)
  
  data2 <- data.frame(faktorji_41_70_rcp85$lons, faktorji_41_70_rcp85$lats, faktorji_41_70_rcp85$leto, faktorji_41_70_rcp85$model, faktorji_41_70_rcp85[[kaz]])
  colnames(data2) <- c("lons","lats","leto","model","agroclim_ind")
  data2 <- data2 %>% filter(leto >= 2041, leto <= 2070)
  data2=na.omit(data2)
  povp_2041_2070 <- data2 %>% group_by(model,leto) %>%
    summarise(slo_mean = mean(agroclim_ind))
  povp_2041_2070.mediana <- aggregate(slo_mean ~  leto, povp_2041_2070, FUN = median , na.rm=TRUE, na.action=na.pass) # nato naredimo me min max modelov
  povp_2041_2070.min <- aggregate(slo_mean ~  leto, povp_2041_2070, FUN = "min" , na.rm=TRUE, na.action=na.pass)
  povp_2041_2070.max <- aggregate(slo_mean ~  leto, povp_2041_2070, FUN = "max" , na.rm=TRUE, na.action=na.pass)
  
  data3 <- data.frame(faktorji_71_00_rcp85$lons, faktorji_71_00_rcp85$lats, faktorji_71_00_rcp85$leto, faktorji_71_00_rcp85$model, faktorji_71_00_rcp85[[kaz]])
  colnames(data3) <- c("lons","lats","leto","model","agroclim_ind")
  data3 <- data3 %>% filter(leto >= 2071, leto <= 2100)
  data3=na.omit(data3)
  povp_2071_2100 <- data3 %>% group_by(model,leto) %>%
    summarise(slo_mean = mean(agroclim_ind))
  povp_2071_2100.mediana <- aggregate(slo_mean ~  leto, povp_2071_2100, FUN = median , na.rm=TRUE, na.action=na.pass) # nato naredimo me min max modelov
  povp_2071_2100.min <- aggregate(slo_mean ~  leto, povp_2071_2100, FUN = "min" , na.rm=TRUE, na.action=na.pass)
  povp_2071_2100.max <- aggregate(slo_mean ~  leto, povp_2071_2100, FUN = "max" , na.rm=TRUE, na.action=na.pass)
  
  # mediana_1981_2100 <- rbind(povp_1981_2010,povp_2041_2070)
  mediana_2011_2100 <- rbind(rbind(povp_2011_2040.mediana,povp_2041_2070.mediana),povp_2071_2100.mediana)
  min_2011_2100 <- rbind(rbind(povp_2011_2040.min,povp_2041_2070.min),povp_2071_2100.min)
  max_2011_2100 <- rbind(rbind(povp_2011_2040.max,povp_2041_2070.max),povp_2071_2100.max)
  
  rmediana85s=loess.smooth(2011:2100, mediana_2011_2100$slo_mean, span=0.75, evaluation = 2100-2011+1, degree = 1)$y
  rmax85s=loess.smooth(2011:2100, min_2011_2100$slo_mean, span=0.75, evaluation = 2100-2011+1, degree = 1)$y
  rmin85s=loess.smooth(2011:2100, max_2011_2100$slo_mean, span=0.75, evaluation = 2100-2011+1, degree = 1)$y
  
  rhist=loess.smooth(1981:2009, povp_1981_2010$slo_mean, span=1, evaluation = 2010-1981+1, degree = 1)$y
  
  tabelaMinMedianMax45<- data.frame(leto=2011:2100,rmediana45s,rmin45s,rmax45s)
  tabelaMinMedianMax85<- data.frame(leto=2011:2100,rmediana85s,rmin85s,rmax85s)
  tabelahist <-data.frame(leto=1981:2010,rhist)
  
  p=ggplot()
  p=p+
    geom_line(data=tabelaMinMedianMax45,aes(leto, rmediana45s, colour="RCP4.5"), linewidth=1)+
    geom_ribbon(data=tabelaMinMedianMax45,aes(x = leto, ymin = rmin45s, ymax = rmax45s), alpha = 0.2, colour ="green4", fill = "green4")+
    geom_line(data=tabelaMinMedianMax85,aes(leto, rmediana85s, colour="RCP8.5"), linewidth=1)+
    geom_ribbon(data=tabelaMinMedianMax85,aes(x = leto, ymin = rmin85s, ymax = rmax85s), alpha = 0.2, colour ="red4", fill = "red4")+
    scale_colour_manual("",
                        breaks = c("RCP4.5", "RCP8.5"),#"1981-2010",
                        values = c("darkseagreen","red"))+ #"black",
    ylab(imena[[kaz]])+#kaz)+
    xlab("") + 
    scale_y_continuous(limits = c(minimums[[kaz]],maximums[[kaz]]))+
    scale_x_continuous(breaks = c(2010,2040,2070,2100))+
    ggtitle("")+ #NASLOV GRAF
    theme_bw() +
    theme(
      plot.title = element_text(margin = margin(b = 2),hjust=0.5,size = 20),
      aspect.ratio=1, #naredi kvadratni graf
      axis.text = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.title.x = element_text(size = 20),
      legend.text = element_text(size = 18),
      legend.position = c(0.20,0.85),
      legend.background=element_rect(fill = "transparent"))+
    geom_abline(slope = 0, intercept = 0, linetype = 1, colour = "grey40", linewidth = 0.4)
  p
  ggsave(paste0("kazalniki_ovojnice/psenica_2011-2100_rcp45_",kaz,".png"),width = 7, height = 7)
  
}

kaz = "Nongrowing_season_prec"
# kaz = "zvisoke_padavine"
print(kaz)
data0 <- data.frame(vsi_podatki$lons, vsi_podatki$lats, vsi_podatki$leto, vsi_podatki[[kaz]])
colnames(data0) <- c("lons","lats","leto","agroclim_ind")
data0 <- data0 %>% filter(leto >= 1981, leto <= 2011)
data0=na.omit(data0)
povp_1981_2010 <- data0 %>% group_by(leto) %>%
  summarise(slo_mean = mean(agroclim_ind))

# RCP4.5
data1 <- data.frame(faktorji_11_40_rcp45$lons, faktorji_11_40_rcp45$lats, faktorji_11_40_rcp45$leto, faktorji_11_40_rcp45$model, faktorji_11_40_rcp45[[kaz]])
colnames(data1) <- c("lons","lats","leto","model","agroclim_ind")
data1 <- data1 %>% filter(leto >= 2010, leto <= 2040)
data1=na.omit(data1)
povp_2011_2040 <- data1 %>% group_by(model,leto) %>%
  summarise(slo_mean = mean(agroclim_ind))
povp_2011_2040.mediana <- aggregate(slo_mean ~  leto, povp_2011_2040, FUN = median , na.rm=TRUE, na.action=na.pass) # nato naredimo me min max modelov
povp_2011_2040.min <- aggregate(slo_mean ~  leto, povp_2011_2040, FUN = "min" , na.rm=TRUE, na.action=na.pass)
povp_2011_2040.max <- aggregate(slo_mean ~  leto, povp_2011_2040, FUN = "max" , na.rm=TRUE, na.action=na.pass)

data2 <- data.frame(faktorji_41_70_rcp45$lons, faktorji_41_70_rcp45$lats, faktorji_41_70_rcp45$leto, faktorji_41_70_rcp45$model, faktorji_41_70_rcp45[[kaz]])
colnames(data2) <- c("lons","lats","leto","model","agroclim_ind")
data2 <- data2 %>% filter(leto >= 2041, leto <= 2070)
data2=na.omit(data2)
povp_2041_2070 <- data2 %>% group_by(model,leto) %>%
  summarise(slo_mean = mean(agroclim_ind))
povp_2041_2070.mediana <- aggregate(slo_mean ~  leto, povp_2041_2070, FUN = median , na.rm=TRUE, na.action=na.pass) # nato naredimo me min max modelov
povp_2041_2070.min <- aggregate(slo_mean ~  leto, povp_2041_2070, FUN = "min" , na.rm=TRUE, na.action=na.pass)
povp_2041_2070.max <- aggregate(slo_mean ~  leto, povp_2041_2070, FUN = "max" , na.rm=TRUE, na.action=na.pass)

data3 <- data.frame(faktorji_71_00_rcp45$lons, faktorji_71_00_rcp45$lats, faktorji_71_00_rcp45$leto, faktorji_71_00_rcp45$model, faktorji_71_00_rcp45[[kaz]])
colnames(data3) <- c("lons","lats","leto","model","agroclim_ind")
data3 <- data3 %>% filter(leto >= 2071, leto <= 2100)
data3=na.omit(data3)
povp_2071_2100 <- data3 %>% group_by(model,leto) %>%
  summarise(slo_mean = mean(agroclim_ind))
povp_2071_2100.mediana <- aggregate(slo_mean ~  leto, povp_2071_2100, FUN = median , na.rm=TRUE, na.action=na.pass) # nato naredimo me min max modelov
povp_2071_2100.min <- aggregate(slo_mean ~  leto, povp_2071_2100, FUN = "min" , na.rm=TRUE, na.action=na.pass)
povp_2071_2100.max <- aggregate(slo_mean ~  leto, povp_2071_2100, FUN = "max" , na.rm=TRUE, na.action=na.pass)

# mediana_1981_2100 <- rbind(povp_1981_2010,povp_2041_2070)
mediana_2011_2100 <- rbind(rbind(povp_2011_2040.mediana,povp_2041_2070.mediana),povp_2071_2100.mediana)
min_2011_2100 <- rbind(rbind(povp_2011_2040.min,povp_2041_2070.min),povp_2071_2100.min)
max_2011_2100 <- rbind(rbind(povp_2011_2040.max,povp_2041_2070.max),povp_2071_2100.max)

mediana_2011_2100 <- mediana_2011_2100[order(mediana_2011_2100$leto), ]
vsa_leta <- seq(from = min(mediana_2011_2100$leto),
                to   = max(mediana_2011_2100$leto),
                by   = 1)
manjkajoca_leta <- setdiff(vsa_leta, mediana_2011_2100$leto)
df_all <- data.frame(leto = vsa_leta)
df_all <- merge(df_all, mediana_2011_2100, by = "leto", all.x = TRUE)
df_non_na <- df_all[!is.na(df_all$slo_mean), ]
spline_model <- smooth.spline(x = df_non_na$leto,
                              y = df_non_na$slo_mean,spar = 0.7)
df_all$interp_raw <- predict(spline_model, x = df_all$leto)$y
df_all$interpolirano <- ifelse(is.na(df_all$slo_mean),
                               df_all$interp_raw,  df_all$slo_mean)

min_2011_2100 <- min_2011_2100[order(min_2011_2100$leto), ]
vsa_leta <- seq(from = min(min_2011_2100$leto),
                to   = max(min_2011_2100$leto),
                by   = 1)
manjkajoca_leta <- setdiff(vsa_leta, min_2011_2100$leto)
df_all_min <- data.frame(leto = vsa_leta)
df_all_min <- merge(df_all_min, min_2011_2100, by = "leto", all.x = TRUE)
df_non_na <- df_all_min[!is.na(df_all_min$slo_mean), ]
spline_model <- smooth.spline(x = df_non_na$leto,
                              y = df_non_na$slo_mean,spar = 0.7)
df_all_min$interp_raw <- predict(spline_model, x = df_all_min$leto)$y
df_all_min$interpolirano <- ifelse(is.na(df_all_min$slo_mean),
                                   df_all_min$interp_raw,df_all_min$slo_mean)

max_2011_2100 <- max_2011_2100[order(max_2011_2100$leto), ]
vsa_leta <- seq(from = min(max_2011_2100$leto),
                to   = max(max_2011_2100$leto),by   = 1)
manjkajoca_leta <- setdiff(vsa_leta, max_2011_2100$leto)
df_all_max <- data.frame(leto = vsa_leta)
df_all_max <- merge(df_all_max, max_2011_2100, by = "leto", all.x = TRUE)
df_non_na <- df_all_max[!is.na(df_all_max$slo_mean), ]
spline_model <- smooth.spline(x = df_non_na$leto,
                              y = df_non_na$slo_mean,spar = 0.7)
df_all_max$interp_raw <- predict(spline_model, x = df_all_max$leto)$y
df_all_max$interpolirano <- ifelse(is.na(df_all_max$slo_mean),
                                   df_all_max$interp_raw,df_all_max$slo_mean)


rmediana45s=loess.smooth(2011:2099, df_all$interpolirano, span=0.75, evaluation = 2099-2011+1, degree = 1)$y
rmax45s=loess.smooth(2011:2099, df_all_max$interpolirano, span=0.75, evaluation = 2099-2011+1, degree = 1)$y
rmin45s=loess.smooth(2011:2099, df_all_min$interpolirano, span=0.75, evaluation = 2099-2011+1, degree = 1)$y

# RCP8.5
data1 <- data.frame(faktorji_11_40_rcp85$lons, faktorji_11_40_rcp85$lats, faktorji_11_40_rcp85$leto, faktorji_11_40_rcp85$model, faktorji_11_40_rcp85[[kaz]])
colnames(data1) <- c("lons","lats","leto","model","agroclim_ind")
data1 <- data1 %>% filter(leto >= 2011, leto <= 2040)
data1=na.omit(data1)
povp_2011_2040 <- data1 %>% group_by(model,leto) %>%
  summarise(slo_mean = mean(agroclim_ind))
povp_2011_2040.mediana <- aggregate(slo_mean ~  leto, povp_2011_2040, FUN = median , na.rm=TRUE, na.action=na.pass) # nato naredimo me min max modelov
povp_2011_2040.min <- aggregate(slo_mean ~  leto, povp_2011_2040, FUN = "min" , na.rm=TRUE, na.action=na.pass)
povp_2011_2040.max <- aggregate(slo_mean ~  leto, povp_2011_2040, FUN = "max" , na.rm=TRUE, na.action=na.pass)

data2 <- data.frame(faktorji_41_70_rcp85$lons, faktorji_41_70_rcp85$lats, faktorji_41_70_rcp85$leto, faktorji_41_70_rcp85$model, faktorji_41_70_rcp85[[kaz]])
colnames(data2) <- c("lons","lats","leto","model","agroclim_ind")
data2 <- data2 %>% filter(leto >= 2041, leto <= 2070)
data2=na.omit(data2)
povp_2041_2070 <- data2 %>% group_by(model,leto) %>%
  summarise(slo_mean = mean(agroclim_ind))
povp_2041_2070.mediana <- aggregate(slo_mean ~  leto, povp_2041_2070, FUN = median , na.rm=TRUE, na.action=na.pass) # nato naredimo me min max modelov
povp_2041_2070.min <- aggregate(slo_mean ~  leto, povp_2041_2070, FUN = "min" , na.rm=TRUE, na.action=na.pass)
povp_2041_2070.max <- aggregate(slo_mean ~  leto, povp_2041_2070, FUN = "max" , na.rm=TRUE, na.action=na.pass)

data3 <- data.frame(faktorji_71_00_rcp85$lons, faktorji_71_00_rcp85$lats, faktorji_71_00_rcp85$leto, faktorji_71_00_rcp85$model, faktorji_71_00_rcp85[[kaz]])
colnames(data3) <- c("lons","lats","leto","model","agroclim_ind")
data3 <- data3 %>% filter(leto >= 2071, leto <= 2100)
data3=na.omit(data3)
povp_2071_2100 <- data3 %>% group_by(model,leto) %>%
  summarise(slo_mean = mean(agroclim_ind))
povp_2071_2100.mediana <- aggregate(slo_mean ~  leto, povp_2071_2100, FUN = median , na.rm=TRUE, na.action=na.pass) # nato naredimo me min max modelov
povp_2071_2100.min <- aggregate(slo_mean ~  leto, povp_2071_2100, FUN = "min" , na.rm=TRUE, na.action=na.pass)
povp_2071_2100.max <- aggregate(slo_mean ~  leto, povp_2071_2100, FUN = "max" , na.rm=TRUE, na.action=na.pass)

# mediana_1981_2100 <- rbind(povp_1981_2010,povp_2041_2070)
mediana_2011_2100 <- rbind(rbind(povp_2011_2040.mediana,povp_2041_2070.mediana),povp_2071_2100.mediana)
min_2011_2100 <- rbind(rbind(povp_2011_2040.min,povp_2041_2070.min),povp_2071_2100.min)
max_2011_2100 <- rbind(rbind(povp_2011_2040.max,povp_2041_2070.max),povp_2071_2100.max)

mediana_2011_2100 <- mediana_2011_2100[order(mediana_2011_2100$leto), ]
vsa_leta <- seq(from = min(mediana_2011_2100$leto),
                to   = max(mediana_2011_2100$leto),
                by   = 1)
manjkajoca_leta <- setdiff(vsa_leta, mediana_2011_2100$leto)
df_all <- data.frame(leto = vsa_leta)
df_all <- merge(df_all, mediana_2011_2100, by = "leto", all.x = TRUE)
df_non_na <- df_all[!is.na(df_all$slo_mean), ]
spline_model <- smooth.spline(x = df_non_na$leto,
                              y = df_non_na$slo_mean,spar = 0.7)
df_all$interp_raw <- predict(spline_model, x = df_all$leto)$y
df_all$interpolirano <- ifelse(is.na(df_all$slo_mean),
                               df_all$interp_raw,  df_all$slo_mean)

min_2011_2100 <- min_2011_2100[order(min_2011_2100$leto), ]
vsa_leta <- seq(from = min(min_2011_2100$leto),
                to   = max(min_2011_2100$leto),
                by   = 1)
manjkajoca_leta <- setdiff(vsa_leta, min_2011_2100$leto)
df_all_min <- data.frame(leto = vsa_leta)
df_all_min <- merge(df_all_min, min_2011_2100, by = "leto", all.x = TRUE)
df_non_na <- df_all_min[!is.na(df_all_min$slo_mean), ]
spline_model <- smooth.spline(x = df_non_na$leto,
                              y = df_non_na$slo_mean,spar = 0.7)
df_all_min$interp_raw <- predict(spline_model, x = df_all_min$leto)$y
df_all_min$interpolirano <- ifelse(is.na(df_all_min$slo_mean),
                                   df_all_min$interp_raw,df_all_min$slo_mean)

max_2011_2100 <- max_2011_2100[order(max_2011_2100$leto), ]
vsa_leta <- seq(from = min(max_2011_2100$leto),
                to   = max(max_2011_2100$leto),by   = 1)
manjkajoca_leta <- setdiff(vsa_leta, max_2011_2100$leto)
df_all_max <- data.frame(leto = vsa_leta)
df_all_max <- merge(df_all_max, max_2011_2100, by = "leto", all.x = TRUE)
df_non_na <- df_all_max[!is.na(df_all_max$slo_mean), ]
spline_model <- smooth.spline(x = df_non_na$leto,
                              y = df_non_na$slo_mean,spar = 0.7)
df_all_max$interp_raw <- predict(spline_model, x = df_all_max$leto)$y
df_all_max$interpolirano <- ifelse(is.na(df_all_max$slo_mean),
                                   df_all_max$interp_raw,df_all_max$slo_mean)

rmediana85s=loess.smooth(2011:2099, df_all$interpolirano, span=0.75, evaluation = 2099-2011+1, degree = 1)$y
rmax85s=loess.smooth(2011:2099, df_all_max$interpolirano, span=0.75, evaluation = 2099-2011+1, degree = 1)$y
rmin85s=loess.smooth(2011:2099, df_all_min$interpolirano, span=0.75, evaluation = 2099-2011+1, degree = 1)$y

rhist=loess.smooth(1981:2009, povp_1981_2010$slo_mean, span=1, evaluation = 2010-1981+1, degree = 1)$y

tabelaMinMedianMax45<- data.frame(leto=2011:2099,rmediana45s,rmin45s,rmax45s)
tabelaMinMedianMax85<- data.frame(leto=2011:2099,rmediana85s,rmin85s,rmax85s)
tabelahist <-data.frame(leto=1981:2010,rhist)

p=ggplot()
p=p+
  geom_line(data=tabelaMinMedianMax45,aes(leto, rmediana45s, colour="RCP4.5"), linewidth=1)+
  geom_ribbon(data=tabelaMinMedianMax45,aes(x = leto, ymin = rmin45s, ymax = rmax45s), alpha = 0.2, colour ="green4", fill = "green4")+
  geom_line(data=tabelaMinMedianMax85,aes(leto, rmediana85s, colour="RCP8.5"), linewidth=1)+
  geom_ribbon(data=tabelaMinMedianMax85,aes(x = leto, ymin = rmin85s, ymax = rmax85s), alpha = 0.2, colour ="red4", fill = "red4")+
  scale_colour_manual("",
                      breaks = c("RCP4.5", "RCP8.5"),#"1981-2010",
                      values = c("darkseagreen","red"))+ #"black",
  xlab("") + 
  scale_x_continuous(breaks = c(2010,2040,2070,2100))+
  ggtitle("")+ #NASLOV GRAF
  theme_bw() +
  scale_y_continuous(kaz)+#imena[[kaz]],limits = c(-2,4))+
  theme(
    plot.title = element_text(margin = margin(b = 2),hjust=0.5,size = 20),
    aspect.ratio=1, #naredi kvadratni graf
    axis.text = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.position = c(0.20,0.85),
    legend.background=element_rect(fill = "transparent"))+
  geom_abline(slope = 0, intercept = 0, linetype = 1, colour = "grey40", linewidth = 0.4)
p
ggsave(paste0("kazalniki_ovojnice/psenica_2011-2100_rcp45_",kaz,".png"),width = 7, height = 7)

