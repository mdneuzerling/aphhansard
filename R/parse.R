#' Parse an XML node and its children
#'
#' @param x
#' @param ...
#'
#' @import xml2
#' @import dplyr
#'
#' @export
#'
parse_node <- function(x, ...) {
  UseMethod("parse_node", x)
}

#' @export
parse_node.xml_node <- function(x, ...) {
  name <- xml_name(x)
  if (name %in% class(x)) {
    stop("Detected circular dispatch loop for ", name, " node.")
  }
  class(x) <- c(name, class(x))
  parse_node(x, ...)
}

parse_node.para <- function(x, ...) {
  list(text = xml_text(x), text_type = NA_character_)
}

xml_children_names <- function(node) {
  purrr::map_chr(xml_children(node), xml_name)
}

#' Title
#'
#' @param node
#' @param necessary
#' @param optional
#'
#' @importFrom glue glue
#' @importFrom assertthat assert_that
#'
#' @export
#'
assert_children_names <- function(node, necessary = c(), optional = c()) {
  children_names <- xml_children_names(node) %>% unique
  for (name in necessary) {
    assert_that(
      name %in% children_names,
      msg = glue("Could not find {name} in children of {xml_name(node)}")
    )
  }
  for (name in children_names) {
    assert_that(
      name %in% c(necessary, optional),
      msg = glue("Unexpected {name} node in children of {xml_name(node)}")
    )
  }
  invisible(TRUE)
}

has_xml_child <- function(node, child) {
  child %in% xml_children_names(node)
}

xml_time <- function(x) {
  xml_text(x) #//TODO
}

#' Title
#'
#' @param xml
#'
#' @return
#'
#' @import xml2
#'
#' @export
parse_hansard <- function(xml) {
  stop("Not yet implemented")
  doc <- xml2::read_xml(xml)
  header <- xml_child(doc, "session.header")
  chamber <- xml_child(doc, "chamber.xscript")
  maincomm <- xml_child(doc, "maincomm.xscript")
  answers <- xml_child(doc, "answers.to.questions")

  date <- header %>% xml_child("date") %>% xml_text() %>% as.Date()
  parliament <- header %>% xml_child("parliament") %>% xml_integer()
  session <- header %>% xml_child("session") %>% xml_integer()
  chamber <- header %>% xml_child("chamber") %>% xml_text
}
