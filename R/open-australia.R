#' Retrieve summary data Hansard debates for a given day from the Open Australia API
#'
#' @param date Retrieve debates for the given day. Defaults to the current day.
#'   If given as a character, this function will attempt to convert it into a
#'   date.
#' @param type Either "representatives" or "senate". Defaults to
#'   "representatives".
#' @param api_key The Open Australia API key, available at
#'   \url{https://www.openaustralia.org.au/api/}. Defaults to the value stored
#'   in the "OPEN_AUSTRALIA_KEY" environment variable.
#'
#' @return
#'
#' @importFrom glue glue
#'
#' @export
#'
get_debates <- function(date = Sys.Date(),
                        type = c("representatives", "senate"),
                        api_key = Sys.getenv("OPEN_AUSTRALIA_KEY")) {
  # Don't trust the status code: errors can have status code 200.
  # Instead, look for the "error" key in the resulting JSON.
  if (api_key == "") {
      stop("Must provide an Open Australia API key, available at ",
           "https://www.openaustralia.org.au/api/ --- or store the key in ",
           "the \"OPEN_AUSTRALIA_KEY\" environment variable")
  }
  type <- match.arg(type)
  endpoint <- "http://www.openaustralia.org/api/getDebates"
  if (is.character(date)) date <- as.Date(date)
  formatted_date <- format(date, "%Y-%m-%d")
  response <- httr::GET(glue(
    "{endpoint}?key={api_key}",
    "&date={formatted_date}",
    "&type={type}"
  ))
  httr::content(response)
}







