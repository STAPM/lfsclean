#' Monthly Inflation
#'
#' Time series of the CPIH (consumer price index including housing costs) inflation index and the
#' RPI (retail price index) on a monthly basis with a base month of January 2022 Data are obtained
#' from the Office for National Statistics for \href{https://www.ons.gov.uk/generator?format=xls&uri=/economy/inflationandpriceindices/timeseries/l522/mm23}{CPIH}
#' and \href{https://www.ons.gov.uk/generator?format=xls&uri=/economy/inflationandpriceindices/timeseries/chaw/mm23}{RPI}
#' last accessed 19/08/2022.
#'
#'
"inflation"

#' SOC-2010 Lookup
#'
#' Lookup of SOC-2010 4-digit codes matched to unit (4-digit), minor (3-digit), submajor (2-digit) and
#' major (1-digit) major group occupation titles.
#'
#' @source \href{https://www.ons.gov.uk/methodology/classificationsandstandards/standardoccupationalclassificationsoc/soc2010/soc2010volume2thestructureandcodingindex}{Office for National Statistics}
#'
"soc2010_lookup"


#' Vacancies and unemployment
#'
#' A rolling three-month time series of aggregate vacancies data and unemployment. Estimated from the ONS Vacancy Survey
#' and Labour Force Survey by the ONS and published on the
#' \href{https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/unemployment/datasets/vacanciesandunemploymentvacs01}{ONS website}.
#' Vacancies data exclude the agriculture, forestry and fishing sector.
#'
#' @format A data frame with 4,518 observations and 4 variables.
#' \describe{
#'     \item{year}{year}
#'     \item{month}{month}
#'     \item{vacancies}{Numeric variable. Number of vacancies excluding agriculture, forestry, and fishing (thousands)}
#'     \item{unemployment}{Numeric variable. Number of unemployed (thousands)}
#'     \item{unemp_per_vacancy}{Numeric variable. Number of unemployed per vacancy}
#' }
"vacancy"

#' Vacancies by Industry
#'
#' A rolling three-month time series of vacancy data by broad industrial group. Estimated from the ONS Vacancy Survey and published
#' \href{https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/unemployment/datasets/vacanciesbyindustryvacs02}{by sector by the ONS}.
#' Covers all sectors except for agriculture, forestry and fishing.
#'
#' @format A data frame with 4,518 observations and 4 variables.
#' \describe{
#'     \item{year}{year}
#'     \item{month}{month}
#'     \item{sic_1dig}{Factor variable. Broad SIC industry sector (excluding A)}
#'     \item{vacancies_by_ind}{Numeric variable. Number of vacancies (thousands)}
#' }
"vacancy_by_industry"


