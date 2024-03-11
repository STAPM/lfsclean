library(curl)
library(readxl)
library(data.table)
library(lubridate)
library(tidyverse)

base <- c(1,2022)

#######################################
###### CPIH Monthly Inflation #########

### download data direct from ONS

temp <- tempfile()
url <- "https://www.ons.gov.uk/generator?format=xls&uri=/economy/inflationandpriceindices/timeseries/l522/mm23"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

### load in the data and clean

data <- read_excel(temp, range="A9:B2000", col_names = FALSE) %>% setDT()

setnames(data, names(data), c("time","cpih"))

data[, year := as.numeric(substr(time,1,4))]
data[, month := substr(time,6,8)]

### keep only monthly data and recode to numeric

data <- data[!(month %in% c(NA,"","Q1","Q2","Q3","Q4")),]

data[.(month = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"),
       to = c(1:12)), on = "month", month := i.to]

data[, month := as.numeric(month)]

cpih <- data[, c("year","month","cpih")]


######################################
###### RPI Monthly Inflation #########

### download data direct from ONS

temp <- tempfile()
url <- "https://www.ons.gov.uk/generator?format=xls&uri=/economy/inflationandpriceindices/timeseries/chaw/mm23"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

### load in the data and clean

data <- read_excel(temp, range="A9:B2000", col_names = FALSE) %>% setDT()

setnames(data, names(data), c("time","rpi"))

data[, year := as.numeric(substr(time,1,4))]
data[, month := substr(time,6,8)]

### keep only monthly data and recode to numeric

data <- data[!(month %in% c(NA,"","Q1","Q2","Q3","Q4")),]

data[.(month = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"),
       to = c(1:12)), on = "month", month := i.to]

data[, month := as.numeric(month)]

data <- data[, c("year","month","rpi")]

rpi <- data[, c("year","month","rpi")]

#########################################
##### Combine and rebase to Jan 2022 ####

inflation <- merge(rpi, cpih, by = c("year","month"), all = F)

cpih_base <- as.numeric( inflation[month == base[1] & year == base[2], "cpih"] )
rpi_base  <- as.numeric( inflation[month == base[1] & year == base[2], "rpi"] )

inflation[, cpih := (cpih/cpih_base)*100]
inflation[, rpi := (rpi/rpi_base)*100]

### create a monthly time variable

inflation[, mth := as.character(month)]
inflation[, mth := ifelse(str_length(mth) == 2, mth, paste0("0",mth))]
inflation[, time := paste0(mth, "-", as.character(year))]
inflation[, date := lubridate::my(time)]

inflation <- inflation[, c("date","year","month","rpi","cpih")]

inflation <- melt(inflation,
             id.vars = c("date","year","month"),
             variable.name = "measure",
             value.name = "index")

usethis::use_data(inflation, overwrite = TRUE)

#### plot the inflation data



ggplot(inflation) +
  geom_line(aes(y = index, x = date, color = measure)) +
  scale_x_date(limits=c(as.Date("2006-01-01"), as.Date("2024-01-01") )) +
  theme_classic() +
  geom_hline(yintercept = 100) +
  labs(y = "Index (base = January 2022)",
       x = "",
       title = "RPI and CPIH Inflation 2006 - January 2024")





