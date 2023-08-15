library(curl)
library(readxl)
library(data.table)
library(magrittr)

##################################################
###### SOC2010 4 digit code lookup table #########

### download data direct from ONS

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/methodology/classificationsandstandards/standardoccupationalclassificationsoc/soc2010/soc2010volume2thestructureandcodingindex/soc2010indexversion705june2018.xls"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

### load in the data and clean

data <- read_excel(temp, range="A1:E710", sheet = "SOC2010 Structure",col_names = TRUE) %>% setDT()

setnames(data, names(data), c("major","submajor","minor","unit","title"))

########################
### 4-digit

soc2010_4dig_lookup <- copy(data)

soc2010_4dig_lookup <- soc2010_4dig_lookup[!is.na(unit)]

soc2010_4dig_lookup <- soc2010_4dig_lookup[, c("title","unit")]

setnames(soc2010_4dig_lookup, names(soc2010_4dig_lookup), c("soc2010_4dig_title","l_soc2010code"))

######################
#### 3-digit #########

soc2010_3dig_lookup <- copy(data)

soc2010_3dig_lookup <- soc2010_3dig_lookup[!is.na(minor)]

setnames(soc2010_3dig_lookup, "title", "soc2010_3dig_title")

soc2010_3dig_lookup <- soc2010_3dig_lookup[, c("soc2010_3dig_title","minor")]

soc2010_3dig_lookup[, minor := as.character(minor)]

######################
#### 2-digit #########

soc2010_2dig_lookup <- copy(data)

soc2010_2dig_lookup <- soc2010_2dig_lookup[!is.na(submajor)]

setnames(soc2010_2dig_lookup, "title", "soc2010_2dig_title")

soc2010_2dig_lookup <- soc2010_2dig_lookup[, c("soc2010_2dig_title","submajor")]

soc2010_2dig_lookup[, submajor := as.character(submajor)]

######################
#### 1-digit #########

soc2010_1dig_lookup <- copy(data)

soc2010_1dig_lookup <- soc2010_1dig_lookup[!is.na(major)]

setnames(soc2010_1dig_lookup, "title", "soc2010_1dig_title")

soc2010_1dig_lookup <- soc2010_1dig_lookup[, c("soc2010_1dig_title","major")]

soc2010_1dig_lookup[, major := as.character(major)]

######################

soc2010_4dig_lookup[, minor := substr(l_soc2010code, 1, 3)]
soc2010_4dig_lookup[, submajor := substr(l_soc2010code, 1, 2)]
soc2010_4dig_lookup[, major := substr(l_soc2010code, 1, 1)]

### merge in data
soc2010_4dig_lookup <- merge(soc2010_4dig_lookup, soc2010_3dig_lookup, by = "minor")
soc2010_4dig_lookup <- merge(soc2010_4dig_lookup, soc2010_2dig_lookup, by = "submajor")
soc2010_4dig_lookup <- merge(soc2010_4dig_lookup, soc2010_1dig_lookup, by = "major")

soc2010_lookup <- copy(soc2010_4dig_lookup)

soc2010_lookup[, c("minor", "submajor", "major") := NULL]

soc2010_lookup <- soc2010_lookup[, c("l_soc2010code", "soc2010_4dig_title", "soc2010_3dig_title", "soc2010_2dig_title", "soc2010_1dig_title")]

soc2010_lookup[, l_soc2010code := as.character(l_soc2010code)]

usethis::use_data(soc2010_lookup, overwrite = TRUE)
