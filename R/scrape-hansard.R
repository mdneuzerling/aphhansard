links_on_page <- function(url) {
  page <- xml2::read_html(url)
  rvest::html_attr(rvest::html_nodes(page, "a"), "href")
}

#' Return lists of URLs for individual days of Hansard
#'
#' @param url
#'
#' @return
#' @export
scrape_house_links <- function(url) {
  links <- links_on_page(url)
  links[grepl(hansard_link_pattern(), links)]
}

#' @export
scrape_reps_links <- function() {
  reps_index_url <- glue("https://www.aph.gov.au/",
                         "Parliamentary_Business/Hansard/Hansreps_2011")
  scrape_house_links(reps_index_url)
}

#' @export
scrape_senate_links <- function() {
  senate_index_url <- glue("https://www.aph.gov.au/",
                           "Parliamentary_Business/Hansard/Hanssen261110")
  scrape_house_links(senate_index_url)
}

#' Define a regular expression pattern for matching links to Hansard pages
#'
#' @return Character
#'
#' @import RVerbalExpressions
#' @importFrom dplyr %>%
#'
#' @export
#'
hansard_link_pattern <- function() {
  rx_start_of_line() %>%
    rx_anything_but(' ') %>%
    rx_find("aph.gov.au") %>%
    rx_anything_but(' ') %>%
    rx_find("hansard") %>%
    rx_anything_but(' ') %>%
    rx_end_of_line() %>%
    rx_with_any_case()
}

find_hansard_xml_link <- function(url) {
  links <- links_on_page(url)
  xml_links <- links[grepl("xml", links)]
  if (length(xml_links) == 0) {
    stop("Could not find any XML links at ", url)
  } else if (length(xml_links) > 1) {
    stop("Found multiple XML links at ", url)
  }
  xml_link <- xml_links[[1]]
  if (substr(xml_link, 1, 1) == "/") {
    domain_regex <- rx_start_of_line() %>%
      rx_anything_but(' ') %>%
      rx_find("aph.gov.au")
    domain <- stringr::str_extract(url, domain_regex)
    paste0(domain, xml_link)
  } else {
    xml_link
  }
}

scrape_hansard_plan <- function() {
  drake::drake_plan(
    representatives_links = scrape_reps_links(),
    senate_links = scrape_senate_links(),
    hansard_links_df = rbind(
      data.frame(link = representatives_links, house = "representatives"),
      data.frame(link = senate_links, house = "senate")
    )
  )
}
