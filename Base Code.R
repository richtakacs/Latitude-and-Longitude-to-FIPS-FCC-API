library(tictoc)
library(dplyr)
library(zipcode)

#' Try/catch with exponential backoff
#'
#' Attempts the expression in \code{expr} up to the number of tries specified in
#' \code{max_attempts}. Each time a failure results, the functions sleeps for a
#' random amount of time before re-attempting the expression. The upper bound of
#' the backoff increases exponentially after each failure.
#'
#' For details on exponential backoff, see:
#' \url{http://en.wikipedia.org/wiki/Exponential_backoff}
#'
#' @param expr an R expression to try.
#' @param silent logical: should the report of error messages be suppressed?
#' @param max_tries the maximum number of times to attempt the expression
#' \code{expr}
#' @param verbose logical: Should detailed messages be reported regarding each
#' attempt? Default: no.
#' @return the value of the expression in \code{expr}. If the final attempt was
#' a failure, the objected returned will be of class try-error".

CH2011 <- read.csv("/Users/Rich/Downloads/City Data Without FIPS/CHI_311_2011.csv")
CH2011 <- CH2011[!CH2011$Latitude==0,]
CH2011 <- CH2011[apply(CH2011,1,function(x)any(!is.na(x))),]


try_backoff <- function(expr, silent=FALSE, max_attempts=10, verbose=FALSE) {
  for (attempt_i in seq_len(max_attempts)) {
    results <- try(expr=expr, silent=silent)
    if (class(results) == "try-error") {
      backoff <- runif(n=1, min=0, max=2^attempt_i - 1)
      if (verbose) {
        message("Backing off for ", backoff, " seconds.")
      }
      Sys.sleep(backoff)
    } else {
      if (verbose) {
        message("Succeeded after ", attempt_i, " attempts.")
      }
      break
    }
  }
  results
}

# FCC's Census Block Conversions API
# http://www.fcc.gov/developers/census-block-conversions-api
latlong2fips <- function(latitude, longitude) {
  out <- try({
    url <- "https://geo.fcc.gov/api/census/block/find?format=json&latitude=%f&longitude=%f&showall=true"
    url <- sprintf(url, latitude, longitude)
    json <- try_backoff(RCurl::getURL(url), verbose=TRUE)
    json <- RJSONIO::fromJSON(json)
    as.character(json$Block['FIPS'])
  })
  out
}

# Gets unique latitude/longitude pairs
lat_long <- NYCJanJune11[c("Latitude", "Longitude")]
lat_long <- lat_long[!duplicated(lat_long), ]

# Gets FIPS codes for each latitude/longitude pair

lat_long <- within(lat_long, {
  tic()
  fips <- mapply(latlong2fips, latitude=lat_long$Latitude, longitude=lat_long$Longitude)
  toc()
})

lat_long$fips <- factor(lat_long$fips)


CH2011_fips <- merge(CH2011, lat_long, by=c("Latitude","Longitude"))

