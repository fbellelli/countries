% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_countries_api.R
\name{check_countries_api}
\alias{check_countries_api}
\title{Check if the connection to Countries REST API is working}
\usage{
check_countries_api(warnings = TRUE, timeout = 4)
}
\arguments{
\item{warnings}{Logical value indicating whether to output a warning when there is no connection. Default is \code{TRUE}.}

\item{timeout}{Numeric value giving the timeout in seconds for attempting connection to the API. Default is \code{4} second.}
}
\value{
Returns a logical value: \code{TRUE} if there is a connection, \code{FALSE} if there is no connection.
}
\description{
Check if the connection to \href{https://restcountries.com/}{REST Countries API} is working. The function checks if the user has an internet connection and if any answer is returned from the Countries REST API.
}
\examples{
check_countries_api()
}
\seealso{
\link[countries]{list_fields}, \link[countries]{country_info}
}
