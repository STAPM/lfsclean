# https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/unemployment/datasets/vacanciesbyindustryvacs02
# based on the ONS Vacancy Survey

library(data.table)
library(readxl)
library(curl)

### JULY 2023

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peoplenotinwork/unemployment/datasets/vacanciesbyindustryvacs02/current/vacs02jul2023.xls"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

### vacancies by industry

data <- read_excel(temp,
                   sheet = "levels",
                   range = "A9:U273",
                   col_names = FALSE)

setDT(data)

################################
### Construct month and year variables. Data is based on a rolling
### 3 month window, so take the middle month

data[, c("...2","...3") := NULL]
setnames(data, "...1", "t")

data[substring(t,1,7) == "Dec-Feb", month := 1]
data[substring(t,1,7) == "Jan-Mar", month := 2]
data[substring(t,1,7) == "Feb-Apr", month := 3]
data[substring(t,1,7) == "Mar-May", month := 4]
data[substring(t,1,7) == "Apr-Jun", month := 5]
data[substring(t,1,7) == "May-Jul", month := 6]
data[substring(t,1,7) == "Jun-Aug", month := 7]
data[substring(t,1,7) == "Jul-Sep", month := 8]
data[substring(t,1,7) == "Aug-Oct", month := 9]
data[substring(t,1,7) == "Sep-Nov", month := 10]
data[substring(t,1,7) == "Oct-Dec", month := 11]
data[substring(t,1,7) == "Nov-Jan", month := 12]

data[, year := substring(t,9,12)]


# mislabelled typos in the spreadsheet
data[substring(t,1,8) == "Nov- Jan", month := 12]

data[substring(t,1,8) == "Nov- Jan", year := substring(t,10,13)]
data[substring(t,1,11) == "Apr-Jun2008", year := substring(t,8,11)]

data[, year := as.numeric(year)]

data[, t := NULL]

setcolorder(data, c("month","year"))


##################################
### Label industries and reshape

setnames(data, names(data),
         c("month","year","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"))

vacancy_by_industry <- melt(data,
                            id.vars = c("month","year"),
                            value.name = "vacancies_by_ind",
                            variable.name = "ind_section")

usethis::use_data(vacancy_by_industry, overwrite = TRUE)

