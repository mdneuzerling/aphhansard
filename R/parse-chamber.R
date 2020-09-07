#' @export
parse_node.chamber.xscript <- function(x, ...) {
  assert_children_names(
    x,
    necessary = c("business.start", "debate", "adjournment")
  )
  header <- x %>% xml_child("business.start") %>% parse_node() %>% as_tibble()
  content <- x %>%
    xml_children() %>%
    purrr::keep(~xml_name(.x) == "debate") %>%
    purrr::map_dfr(parse_node)
  footer <- x %>% xml_child("adjournment") %>% parse_node() %>% as_tibble()
  bind_rows(header, content, footer)  %>%
    tidyr::fill(!any_of(colnames(content)), .direction = "down") %>%
    tidyr::fill(page, time, .direction = "down")%>%
    tidyr::fill(page, time, .direction = "up")
}

#' @export
parse_node.business.start <- function(x, ...) {
  assert_children_names(
    x,
    necessary = c("day.start", "para"),
    optional = "separator"
  )
  date <- x %>% xml_child("day.start") %>% xml_text() %>% as.Date()
  text <- x %>% xml_child("para") %>% xml_text()
  tibble(date = date, text = text, text_type = "business start")
}

#' @export
parse_node.debate <- function(x, ...) {
  content_nodes <- c("subdebate.1", "speech", "division",
                     "interjection", "continue", "motion", "motionnospeech",
                     "para", "question", "answer")
  assert_children_names(
    x,
    necessary = "debateinfo",
    optional = content_nodes
  )
  # The header contains a page number, which will conflict with the page numbers
  # in the content nodes. We disregard it.
  full_header <- x %>% xml_child("debateinfo") %>% parse_node() %>% as_tibble()
  header <- full_header %>% select(-page)
  content <- x %>%
    xml_children() %>%
    purrr::keep(~xml_name(.x) %in% content_nodes) %>%
    purrr::map_dfr(parse_node)
  bind_cols(header, content) %>%
    tidyr::fill(!any_of(colnames(content)), .direction = "down")
}

#' @export
parse_node.debateinfo <- function(x, ...) {
  title <- x %>% xml_child("title") %>% xml_text()
  page <- x %>% xml_child("page.no") %>% xml_integer()
  type <- x %>% xml_child("type") %>% xml_text()
  id <- x %>% xml_child("id") %>% xml_text()
  list(debate = title, page = page, type = type, debate_id = id)
}

#' @export
parse_node.subdebate.1 <- function(x, ...) {
  content_nodes <- c("speech", "para", "motion", "interjection", "division",
                     "motionnospeech", "question", "answer", "quote",
                     "subdebate.2", "amendments")
  assert_children_names(
    x,
    necessary = "subdebateinfo",
    optional = content_nodes
  )
  header <- x %>% xml_child("subdebateinfo") %>% parse_node()
  content <- x %>%
    xml_children() %>%
    purrr::keep(~xml_name(.x) %in% content_nodes) %>%
    purrr::map_dfr(parse_node)
  bind_rows(header, content) %>%
    tidyr::fill(!any_of(colnames(content)), .direction = "down")
}

#' @export
parse_node.subdebate.2 <- function(x, ...) {
  parse_node.subdebate.1(x)
}

parse_node.subdebateinfo <- function(x, ...) {
  title <- x %>% xml_child("title") %>% xml_text()
  page <- x %>% xml_child("page.no") %>% xml_integer()
  list(subdebate = title, page = page)
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#'
#' @import dplyr
#' @export
#'
#' @examples
parse_node.speech <- function(x, ...) {
  content_nodes <- c("motion", "quote", "interjection", "continue",
                     "amendments", "table", "para", "list")
  assert_children_names(
    x,
    necessary = c("talk.start"),
    optional = content_nodes
  )
  header <- x %>% xml_child("talk.start") %>% parse_node()
  content <- x %>%
    xml_children() %>%
    purrr::keep(~xml_name(.x) %in% content_nodes) %>%
    purrr::map_dfr(parse_node) %>%
    replace_missing_text_type("speech")
  bind_rows(header, content) %>%
    tidyr::fill(!any_of(colnames(content)), .direction = "down") %>%
    tibble::rowid_to_column("paragraph") %>%
    # speeches have a line of text in the header. Usually this is just "I move"
    # for the motion. I call this paragraph 0.
    mutate(paragraph = as.integer(paragraph - 1)) %>%
    add_secondary_name_columns() %>%
    mutate(
      electorate = ifelse(secondary_name_overwrite(name, secondary_name),
                          NA_character_, electorate),
      party = ifelse(secondary_name_overwrite(name, secondary_name),
                     NA_character_, party),
      in_gov = ifelse(secondary_name_overwrite(name, secondary_name),
                      NA, in_gov),
      first_speech = ifelse(secondary_name_overwrite(name, secondary_name),
                            FALSE, first_speech),
      # These have to go last because the impact the result of the
      # secondary_name_overwrite function
      name = coalesce(secondary_name, name),
      name_id = coalesce(secondary_name_id, name_id)
    ) %>%
    select(-starts_with("secondary"))
}

parse_node.question <- function(x, ...) {
  content_nodes <- c("para", "quote", "interjection", "continue")
  assert_children_names(x, necessary = "talk.start", optional = content_nodes)
  header <- x %>% xml_child("talk.start") %>% parse_node()
  content <- x %>%
    xml_children() %>%
    purrr::keep(~xml_name(.x) %in% content_nodes) %>%
    purrr::map_dfr(parse_node)
  bind_rows(header, content) %>%
    tidyr::fill(!any_of(colnames(content)), .direction = "down") %>%
    tibble::rowid_to_column("paragraph") %>%
    replace_missing_text_type("question") %>%
    mutate(first_speech = tidyr::replace_na(first_speech, FALSE))
}

parse_node.answer <- function(x, ...) {
  x %>% parse_node.question() %>% mutate(
    text_type = ifelse(text_type == "question", "answer", text_type)
  )
}

secondary_name_overwrite <- function(name, secondary_name) {
  is.na(name) | (!is.na(secondary_name) & name != secondary_name)
}

parse_node.amendments <- function(x, ...) {
  assert_children_names(x, necessary = "amendment")
  purrr::map_dfr(
    seq(xml_length(x)),
    ~tibble(
      text = parse_node(xml_child(x, .x)),
      text_type = "amendment",
      amendment = .x
    )
  )
}

parse_node.amendment <- function(x, ...) {
  assert_children_names(x, necessary = "para")
  x %>%
    xml_children() %>%
    purrr::map_chr(xml_text)
}

parse_node.table <- function(x, ...) {
  # I have no idea how to parse XML tables in a reliable way. For now, as a
  # placeholder, return the raw XML as text
  tibble(text = x %>% as.character(), text_type = "table")
}

parse_node.motion <- function(x, ...) {
  text = x %>% xml_child("para") %>% xml_text()
  tibble(text = text, text_type = "motion")
}

#' @export
parse_node.quote <- function(x, ...) {
  text = x %>% xml_child("para") %>% xml_text()
  tibble(text = text, text_type = "quote")
}

#' @export
parse_node.interjection <- function(x, ...) {
  assert_children_names(x, necessary = "talk.start")
  # There's already a parse_node.talk.start, but it's aimed at the more fully
  # detailed talker metadata. Interjections contain only a subset of that
  # information
  talk_start <- x %>% xml_child("talk.start")
  assert_children_names(talk_start, necessary = c("talker", "para"))
  talker <- talk_start %>% xml_child("talker")
  assert_children_names(talker, necessary = c("name.id", "name"))
  name = talk_start %>% xml_child("talker") %>% xml_child("name") %>%
    xml_text()
  name_id = talk_start %>% xml_child("talker") %>% xml_child("name.id") %>%
    xml_text()
  text = talk_start %>% xml_child("para") %>% xml_text()
  tibble(text = text, text_type = "interjection",
         secondary_name = name, secondary_name_id = name_id)
}

parse_node.continue <- function(x, ...) {
  # There's already a parse_node.talk.start, but it's aimed at the more fully
  # detailed talker metadata. Continuations contain only a subset of that
  # information
  talk_start <- x %>% xml_child("talk.start")
  name = talk_start %>% xml_child("talker") %>% xml_child("name") %>%
    xml_text()
  name_id = talk_start %>% xml_child("talker") %>% xml_child("name.id") %>%
    xml_text()
  text = talk_start %>% xml_child("para") %>% xml_text()
  tibble(text = text, text_type = "continue",
         secondary_name = name, secondary_name_id = name_id)
}

parse_node.motionnospeech <- function(x, ...) {
  assert_children_names(
    x,
    necessary = c("name", "electorate", "role", "time.stamp", "inline",
                  "motion", "para")
  )
  header <- tibble(
    name = x %>% xml_child("name") %>% xml_text(),
    electorate = x %>% xml_child("electorate") %>% xml_text() %>%
      trim_punctuation(),
    role = x %>% xml_child("role") %>% xml_text() %>%
      trim_punctuation(),
    time = x %>% xml_child("time.stamp") %>% xml_time(),
    text = x %>% xml_child("inline") %>% xml_text()
  )
  content <- x %>%
    xml_children %>%
    purrr::keep(~xml_name(.x) %in% c("motion", "para")) %>%
    purrr::map_dfr(parse_node)
  bind_rows(header, content) %>%
    tidyr::fill(!any_of(colnames(content)), .direction = "down") %>%
    tibble::rowid_to_column("paragraph") %>%
    # speeches have a line of text in the header. Usually this is just "I move"
    # for the motion. I call this paragraph 0.
    mutate(paragraph = as.integer(paragraph - 1)) %>%
    mutate(text_type = tidyr::replace_na(text_type, "motion"))
}

trim_punctuation <- function(x, ...) {
  leading_punctuation_regex <- rx_start_of_line() %>%
    rx_punctuation() %>%
    rx_one_or_more()
  trailing_punctuation_regex <- rx_punctuation() %>%
    rx_one_or_more() %>%
    rx_end_of_line()
  x %>%
    stringr::str_remove(leading_punctuation_regex) %>%
    stringr::str_remove(trailing_punctuation_regex)
}

#' @export
parse_node.division <- function(x, ...) {
  # unimplemented: which of the voices are tellers
  # The information is kept as an asterisk next to the name
  assert_children_names(
    x,
    necessary = c("division.header", "para",
                  "division.data", "division.result")
  )
  division_header <- x %>% xml_child("division.header") %>% parse_node()
  header_supplement <- x %>% xml_child("para") %>% xml_text()
  division_result <- x %>% xml_child("division.result") %>%
    xml_child("para") %>% xml_text
  header <- division_header %>% mutate(
    text = paste(text, header_supplement),
    division_result = division_result
  )
  division_data <- x %>% xml_child("division.data") %>% parse_node()
  dplyr::bind_rows(header, division_data) %>%
    tidyr::fill(!any_of(colnames(division_data)), .direction = "down")
}

parse_node.division.header <- function(x, ...) {
  assert_children_names(x, necessary = c("time.stamp", "para"))
  tibble(
    time = x %>% xml_child("time.stamp") %>% xml_time(),
    text = x %>% xml_child("para") %>% xml_text(),
    text_type = "business"
  )
}

parse_node.division.data <- function(x, ...) {
  assert_children_names(x, necessary = c("ayes", "noes"))
  ayes <- x %>% xml_child("ayes")
  noes <- x %>% xml_child("noes")
  tibble(
    aye_votes = ayes %>% xml_child("num.votes") %>% xml_integer,
    aye_names = list(ayes %>% xml_child("names") %>% parse_node()),
    no_votes = noes %>% xml_child("num.votes") %>% xml_integer,
    no_names = list(noes %>% xml_child("names") %>% parse_node())
  ) %>% mutate(votes = aye_votes + no_votes)
}

parse_node.names <- function(x, ...) {
  assert_children_names(x, necessary = "name")
  x %>% xml_children %>% purrr::map_chr(xml_text)
}

#' @export
parse_node.adjournment <- function(x, ...) {
  assert_children_names(x, necessary = c("adjournmentinfo", "para"))
  page <- x %>% xml_child("adjournmentinfo") %>% xml_child("page.no") %>%
    xml_integer()
  time <- x %>% xml_child("adjournmentinfo") %>% xml_child("time.stamp") %>%
    xml_time()
  text <- x %>% xml_child("para") %>% xml_text()
  tibble(page = page, time = time, text = text, text_type = "adjournment")
}

parse_node.talk.start <- function(x, ...) {
  assert_children_names(x, necessary= c("talker", "para"))
  talker <- x %>% xml_child("talker")
  page <- talker %>% xml_child("page.no") %>% xml_integer()
  time <- talker %>% xml_child("time.stamp") %>% xml_time()
  # Two names: this will pick up on role="metadata", not role="display"
  name <- talker %>% xml_child("name") %>% xml_text()
  name_id <- talker %>% xml_child("name.id") %>% xml_text()
  electorate <- talker %>% xml_child("electorate") %>% xml_text() %>%
    trim_punctuation()
  party_raw <- talker %>% xml_child("party") %>% xml_text()
  party <- if (party_raw == "N/A") NA_character_ else party_raw
  in_gov <- talker %>% xml_child("in.gov") %>% xml_integer() %>% as.logical()
  first_speech <- talker %>% xml_child("first.speech") %>% xml_integer() %>%
    as.logical()
  text <- x %>% xml_child("para") %>% parse_node()
  c(
    list(page = page, time = time, name = name, name_id = name_id,
         electorate = electorate, party = party, in_gov = in_gov,
         first_speech = first_speech),
    text
  ) %>%
    as_tibble()
}

parse_node.list <- function(x, ...) {
  list_type <- x %>% xml_attr("type")
  assertthat::assert_that(list_type %in% c("bullet", "decimal"))
  elements <- if (list_type == "decimal") {
    x %>% xml_children %>% purrr::map_chr(function(item) {
      label <- item %>% xml_attr("label")
      content <- item %>% xml_child("para") %>% xml_text()
      paste(label, content)
    })
  } else {
    x %>% xml_children %>% purrr::map_chr(function(item) {
      label <- "\U2022" # bullet point
      content <- item %>% xml_child("para") %>% xml_text()
      paste(label, content)
    })
  }
  tibble(text = elements)
}

replace_missing_text_type <- function(df, replacement_text_type) {
  if ("text_type" %in% colnames(df)) {
    df %>%
      mutate(text_type = tidyr::replace_na(text_type, replacement_text_type))
  } else {
    df %>% mutate(text_type = replacement_text_type)
  }
}

add_secondary_name_columns <- function(df) {
  if ("secondary_name" %in% colnames(df)) {
    df
  } else {
    df %>% mutate(secondary_name = NA, secondary_name_id = NA)
  }
}

