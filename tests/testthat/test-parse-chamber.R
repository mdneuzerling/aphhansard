test_that("assertion function properly checks node children", {
  xml <- read_xml("<doc> <foo> </foo> <bar> </bar> <baz> </baz> </doc>")
  expect_true(assert_children_names(xml, necessary = c("foo", "bar", "baz")))
  expect_true(assert_children_names(xml, optional = c("foo", "bar", "baz")))
  expect_true(
    assert_children_names(xml, necessary = c("foo", "bar"), optional = "baz")
  )
  expect_true(
    assert_children_names(xml, necessary = "foo", optional = c("baz", "bar"))
  )
  expect_error(
    assert_children_names(xml, necessary = c("foo", "bar", "baz", "qux")),
    "Could not find qux in children of doc"
  )
  expect_true(
    assert_children_names(xml, optional = c("foo", "bar", "baz", "qux"))
  )
  expect_error(
    assert_children_names(xml, necessary = "foo", optional = "bar"),
    "Unexpected baz node in children of doc"
  )
})

test_that("business.start parses", {
  business.start <- xml2::read_xml(
    "<business.start>,
    <day.start>2005-03-16</day.start>,
    <separator/>,
    <para><inline font-weight=\"bold\">The SPEAKER took the chair</inline></para>,
    </business.start>"
  )
  expected <- tibble::tribble(
    ~date, ~text, ~text_type,
      as.Date("2005-03-16"),
      paste0("The SPEAKER took the chair"),
      "business start"
  )
  expect_equal(parse_node(business.start), expected)
})

test_that("division.header parses", {
  division.header <- xml2::read_xml(
    "<division.header>
    <time.stamp>12:15:00</time.stamp>
    <para>The House divided.</para>
    </division.header>"
  )
  expected <- tibble::tribble(
    ~time, ~text, ~text_type,
    "12:15:00", "The House divided.", "business"
  )
  expect_equal(parse_node(division.header), expected)
})

test_that("names parses", {
  division_names <- xml2::read_xml("
    <names>
    <name>Abbott, A.J.</name>
    <name>Andrews, K.J.</name>
    <name>Bailey, F.E.</name>
    </names>")
  expected <- c("Abbott, A.J.", "Andrews, K.J.", "Bailey, F.E.")
  expect_equal(parse_node(division_names), expected)
})

test_that("division.data parses", {
  division.data <- xml2::read_xml("
    <division.data>
    <ayes>
    <num.votes>3</num.votes>
    <title>AYES</title>
    <names>
    <name>Abbott, A.J.</name>
    <name>Andrews, K.J.</name>
    <name>Bailey, F.E.</name>
    </names>
    </ayes>
    <noes>
    <num.votes>2</num.votes>
    <title>NOES</title>
    <names>
    <name>Adams, D.G.H.</name>
    <name>Albanese, A.N.</name>
    </names>
    </noes>
    </division.data>")
  expected <- tibble::tribble(
    ~aye_votes, ~aye_names, ~no_votes, ~no_names, ~votes,
    3L,
    c("Abbott, A.J.", "Andrews, K.J.", "Bailey, F.E."),
    2L,
    c("Adams, D.G.H.", "Albanese, A.N."),
    5L
  )
  expect_equal(parse_node(division.data), expected)
})

test_that("division parses", {
  division <- xml2::read_xml("<division>
    <division.header>
    <time.stamp>12:15:00</time.stamp>
    <para>The House divided.</para>
    </division.header>
    <para>(The Deputy Speaker—Mr Hatton)</para>
    <division.data>
    <ayes>
    <num.votes>3</num.votes>
    <title>AYES</title>
    <names>
    <name>Abbott, A.J.</name>
    <name>Andrews, K.J.</name>
    <name>Bailey, F.E.</name>
    </names>
    </ayes>
    <noes>
    <num.votes>2</num.votes>
    <title>NOES</title>
    <names>
    <name>Adams, D.G.H.</name>
    <name>Albanese, A.N.</name>
    </names>
    </noes>
    </division.data>
    <para>* denotes teller</para>
    <division.result>
    <para>Question agreed to.</para>
    </division.result>
    </division>")
  expected <- tibble::tribble(
    ~time, ~text, ~text_type, ~division_result,
    ~aye_votes, ~aye_names, ~no_votes, ~no_names, ~votes,
    "12:15:00",
    "The House divided. (The Deputy Speaker—Mr Hatton)",
    "business",
    "Question agreed to.",
    3L,
    c("Abbott, A.J.", "Andrews, K.J.", "Bailey, F.E."),
    2L,
    c("Adams, D.G.H.", "Albanese, A.N."),
    5L
  )
})

test_that("interjection parses", {
  interjection <- xml2::read_xml("
    <interjection>
      <talk.start>
        <talker>
          <name.id>HX4</name.id>
          <name role=\"metadata\">Katter, Bob, MP</name>
          <name role=\"display\">Mr Katter</name>
        </talker>
        <para>—Peter, you can’t be serious! Some principles are timeless.</para>
      </talk.start>
    </interjection>")
  expected <- tibble::tribble(
    ~text, ~text_type, ~secondary_name, ~secondary_name_id,
    "—Peter, you can’t be serious! Some principles are timeless.",
    "interjection",
    "Katter, Bob, MP",
    "HX4"
  )
  expect_equal(parse_node(interjection), expected)
})

test_that("continue parses", {
  continue <- xml2::read_xml("
    <continue>
      <talk.start>
        <talker>
          <name.id>XH4</name.id>
          <name role=\"metadata\">McGauran, Peter, MP</name>
          <name role=\"display\">Mr McGAURAN</name>
        </talker>
        <para>—No, you are the one out of date</para>
      </talk.start>
    </continue>")
  expected <- tibble::tribble(
    ~text, ~text_type, ~secondary_name, ~secondary_name_id,
    "—No, you are the one out of date",
    "continue",
    "McGauran, Peter, MP",
    "XH4"
  )
  expect_equal(parse_node(continue), expected)
})

test_that("speech parses", {
  speech <- xml2::read_xml("
    <speech>
      <talk.start>
        <talker>
          <page.no>3</page.no>
          <time.stamp>09:11:00</time.stamp>
          <name role=\"metadata\">Nelson, Dr Brendan, MP</name>
          <name.id>RW5</name.id>
          <electorate>Bradfield</electorate>
          <party>LP</party>
          <role>Minister for Education, Science and Training</role>
          <in.gov>1</in.gov>
          <first.speech>1</first.speech>
          <name role=\"display\">Dr NELSON</name>
        </talker>
        <para>—I move:</para>
      </talk.start>
      <motion>
        <para>That the bill be now read a second time.</para>
      </motion>
      <para class=\"block\">This is a paragraph.</para>
      <interjection>
        <talk.start>
          <talker>
            <name.id>HX4</name.id>
            <name role=\"metadata\">Katter, Bob, MP</name>
            <name role=\"display\">Mr Katter</name>
          </talker>
          <para>This is an interjection.</para>
        </talk.start>
      </interjection>
      <continue>
        <talk.start>
          <talker>
            <name.id>RW5</name.id>
            <name role=\"metadata\">Nelson, Dr Brendan, MP</name>
            <name role=\"display\">Dr NELSON</name>
          </talker>
          <para>This is a continue.</para>
        </talk.start>
      </continue>
      <continue>
        <talk.start>
          <talker>
            <name.id>HX4</name.id>
            <name role=\"metadata\">Katter, Bob, MP</name>
            <name role=\"display\">Mr Katter</name>
          </talker>
          <para>This is a continue with a different speaker.</para>
        </talk.start>
      </continue>
      <para>This is a paragraph with an <inline ref=\"R2283\">inline reference</inline> in it.</para>
    </speech>")
  expected <- tibble::tribble(
    ~paragraph, ~page, ~time, ~name, ~name_id, ~electorate, ~party, ~in_gov, ~first_speech, ~text, ~text_type,
    0L, 3L, "09:11:00", "Nelson, Dr Brendan, MP", "RW5", "Bradfield", "LP", TRUE, TRUE, "—I move:", NA_character_,
    1L, 3L, "09:11:00", "Nelson, Dr Brendan, MP", "RW5", "Bradfield", "LP", TRUE, TRUE, "That the bill be now read a second time.", "motion",
    2L, 3L, "09:11:00", "Nelson, Dr Brendan, MP", "RW5", "Bradfield", "LP", TRUE, TRUE, "This is a paragraph.", "speech",
    3L, 3L, "09:11:00", "Katter, Bob, MP", "HX4", NA_character_, NA_character_, NA, FALSE, "This is an interjection.", "interjection",
    4L, 3L, "09:11:00", "Nelson, Dr Brendan, MP", "RW5", "Bradfield", "LP", TRUE, TRUE, "This is a continue.", "continue",
    5L, 3L, "09:11:00", "Katter, Bob, MP", "HX4", NA_character_, NA_character_, NA, FALSE, "This is a continue with a different speaker.", "continue",
    6L, 3L, "09:11:00", "Nelson, Dr Brendan, MP", "RW5", "Bradfield", "LP", TRUE, TRUE, "This is a paragraph with an inline reference in it.", "speech"
  )
  expect_equal(parse_node(speech), expected)

  # Now consider a simpler case with no interjections/continues. In this case,
  # the inner functions don't generate the secondary_name and secondary_name_id
  # columns, and we need the parse function to be able to handle that scenario.
  speech <- xml2::read_xml("
    <speech>
      <talk.start>
        <talker>
          <page.no>3</page.no>
          <time.stamp>09:11:00</time.stamp>
          <name role=\"metadata\">Nelson, Dr Brendan, MP</name>
          <name.id>RW5</name.id>
          <electorate>Bradfield</electorate>
          <party>LP</party>
          <role>Minister for Education, Science and Training</role>
          <in.gov>1</in.gov>
          <first.speech>1</first.speech>
          <name role=\"display\">Dr NELSON</name>
        </talker>
        <para>—I move:</para>
      </talk.start>
      <motion>
        <para>That the bill be now read a second time.</para>
      </motion>
      <para class=\"block\">This is a paragraph.</para>
      <list type=\"decimal\">
      <item label=\"(1)\">
        <para>Item 1</para>
      </item>
      <item label=\"(2)\">
        <para>Item 2</para>
      </item>
    </list>
    </speech>")
  expected <- tibble::tribble(
    ~paragraph, ~page, ~time, ~name, ~name_id, ~electorate, ~party, ~in_gov, ~first_speech, ~text, ~text_type,
    0L, 3L, "09:11:00", "Nelson, Dr Brendan, MP", "RW5", "Bradfield", "LP", TRUE, TRUE, "—I move:", NA_character_,
    1L, 3L, "09:11:00", "Nelson, Dr Brendan, MP", "RW5", "Bradfield", "LP", TRUE, TRUE, "That the bill be now read a second time.", "motion",
    2L, 3L, "09:11:00", "Nelson, Dr Brendan, MP", "RW5", "Bradfield", "LP", TRUE, TRUE, "This is a paragraph.", "speech",
    3L, 3L, "09:11:00", "Nelson, Dr Brendan, MP", "RW5", "Bradfield", "LP", TRUE, TRUE, "(1) Item 1", "speech",
    4L, 3L, "09:11:00", "Nelson, Dr Brendan, MP", "RW5", "Bradfield", "LP", TRUE, TRUE, "(2) Item 2", "speech"
  )
  expect_equal(parse_node(speech), expected)
})

test_that("motionnospeech parses", {
  motionnospeech <- xml2::read_xml("<motionnospeech>
    <name>Mr RUDDOCK</name>
    <electorate>(Berowra</electorate>
    <role>—Attorney-General)</role>
    <time.stamp>17:25:00</time.stamp>
    <inline>—by leave—I move:</inline>
    <motion>
      <para>That the bill be now read a third time.</para>
    </motion>
    <para>Question agreed to.</para>
    <para>Bill read a third time.</para>
  </motionnospeech>")
  expected <-tibble::tribble(
    ~paragraph,        ~name, ~electorate,               ~role,      ~time,                                     ~text, ~text_type,
    0L, "Mr RUDDOCK",   "Berowra", "Attorney-General", "17:25:00",                       "—by leave—I move:",   "motion",
    1L, "Mr RUDDOCK",   "Berowra", "Attorney-General", "17:25:00", "That the bill be now read a third time.",   "motion",
    2L, "Mr RUDDOCK",   "Berowra", "Attorney-General", "17:25:00",                     "Question agreed to.",   "motion",
    3L, "Mr RUDDOCK",   "Berowra", "Attorney-General", "17:25:00",                 "Bill read a third time.",   "motion"
  )
  expect_equal(parse_node(motionnospeech), expected)
})

test_that("amendment parses", {
  amendment <- xml2::read_xml(
    "<amendment>
      <para class=\"subsection\">(5) Amendment 1</para>
      <para class=\"indenta\">(a) Amendment 2</para>
    </amendment>"
  )
  expected <- c("(5) Amendment 1", "(a) Amendment 2")
  expect_equal(parse_node(amendment), expected)
})

test_that("amendments parses", {
  amendments <- xml2::read_xml("<amendments>
    <amendment>
      <para class=\"subsection\">(5) Amendment 1</para>
      <para class=\"indenta\">(a) Amendment 2</para>
    </amendment>
    <amendment>
      <para class=\"ParlAmend\">(8) Amendment 3</para>
    </amendment>
  </amendments>")
  expected <- tibble::tribble(
    ~text,  ~text_type, ~amendment,
    "(5) Amendment 1", "amendment", 1L,
    "(a) Amendment 2", "amendment", 1L,
    "(8) Amendment 3", "amendment", 2L
  )
  expect_equal(parse_node(amendments), expected)
})

test_that("table parses", {
  table <- xml2::read_xml("<table>placeholder</table>")
  expected <- tibble::tribble(
    ~text, ~text_type,
    "placeholder", "table"
  )
})

test_that("question parses", {
  # The first test case has only one paragraph within the talk.start header
  question <- xml2::read_xml(
    "<question>
      <talk.start>
        <talker>
          <time.stamp>14:00:00</time.stamp>
          <page.no>66</page.no>
          <name role=\"metadata\">Beazley, Kim, MP</name>
          <name.id>PE4</name.id>
          <electorate>Brand</electorate>
          <party>ALP</party>
          <role>Leader of the Opposition</role>
          <in.gov>0</in.gov>
          <name role=\"display\">Mr BEAZLEY</name>
        </talker>
      <para>My question is to the Prime Minister</para>
      </talk.start>
    </question>"
  )
  expected <- tibble::tribble(
    ~paragraph, ~page,      ~time,              ~name, ~name_id, ~electorate, ~party, ~in_gov, ~first_speech,                                  ~text, ~text_type,
    1L,          66L, "14:00:00", "Beazley, Kim, MP",    "PE4",     "Brand",  "ALP",   FALSE,         FALSE, "My question is to the Prime Minister", "question"
  )
  expect_equal(parse_node(question), expected)

  # The second test case has a follow-on paragraph
  question <- xml2::read_xml(
    "<question>
      <talk.start>
        <talker>
          <time.stamp>14:00:00</time.stamp>
          <page.no>66</page.no>
          <name role=\"metadata\">Beazley, Kim, MP</name>
          <name.id>PE4</name.id>
          <electorate>Brand</electorate>
          <party>ALP</party>
          <role>Leader of the Opposition</role>
          <in.gov>0</in.gov>
          <name role=\"display\">Mr BEAZLEY</name>
        </talker>
      <para>My question is to the Prime Minister</para>
      </talk.start>
      <para>and is in two parts</para>
    </question>"
  )
  expected <- tibble::tribble(
    ~paragraph, ~page,      ~time,              ~name, ~name_id, ~electorate, ~party, ~in_gov, ~first_speech,                                  ~text, ~text_type,
    1L,   66L, "14:00:00", "Beazley, Kim, MP",    "PE4",     "Brand",  "ALP",   FALSE,         FALSE, "My question is to the Prime Minister", "question",
    2L,   66L, "14:00:00", "Beazley, Kim, MP",    "PE4",     "Brand",  "ALP",   FALSE,         FALSE,                  "and is in two parts", "question"
  )
  expect_equal(parse_node(question), expected)
})

# Answers are very similar to questions, so the parse.answer simply calls
# parse.question and changes text_type from "question" to "answer"
test_that("answer parses", {
  answer <- xml2::read_xml(
    "<answer>
      <talk.start>
        <talker>
          <time.stamp>14:00:00</time.stamp>
          <page.no>66</page.no>
          <name role=\"metadata\">Beazley, Kim, MP</name>
          <name.id>PE4</name.id>
          <electorate>Brand</electorate>
          <party>ALP</party>
          <role>Leader of the Opposition</role>
          <in.gov>0</in.gov>
          <name role=\"display\">Mr BEAZLEY</name>
        </talker>
      <para>My answer</para>
      </talk.start>
      <para>is in two parts</para>
      <quote pgwide=\"yes\">
        <para pgwide=\"yes\">And here's a quote</para>
      </quote>
    </answer>"
  )
  expected <- tibble::tribble(
    ~paragraph, ~page,      ~time,              ~name, ~name_id, ~electorate, ~party, ~in_gov, ~first_speech, ~text, ~text_type,
    1L,          66L, "14:00:00", "Beazley, Kim, MP",    "PE4",     "Brand",  "ALP",   FALSE,         FALSE,       "My answer", "answer",
    2L,          66L, "14:00:00", "Beazley, Kim, MP",    "PE4",     "Brand",  "ALP",   FALSE,         FALSE, "is in two parts", "answer",
    3L,          66L, "14:00:00", "Beazley, Kim, MP",    "PE4",     "Brand",  "ALP",   FALSE,         FALSE, "And here's a quote", "quote"
  )
  expect_equal(parse_node(answer), expected)
})

test_that("quote parses", {
  quote <- xml2::read_xml(
    "<quote pgwide=\"yes\">
      <para pgwide=\"yes\">Here's a quote</para>
    </quote>"
  )
  expected <- tibble::tribble(
    ~text, ~text_type,
    "Here's a quote",    "quote"
  )
  expect_equal(parse_node(quote), expected)
})

test_that("adjournment parses", {
  adjournment <- xml2::read_xml(
    "<adjournment>
    <adjournmentinfo>\n  <page.no>148</page.no>\n  ,
    <time.stamp>20:00:00</time.stamp>\n</adjournmentinfo>,
    <para>House adjourned at 8.00 p.m.</para>,
    </adjournment>"
  )
  expected <- tibble::tribble(
     ~page, ~time, ~text, ~text_type,
     148L, "20:00:00", "House adjourned at 8.00 p.m.", "adjournment"
  )
  expect_equal(parse_node(adjournment), expected)
})

test_that("list parses", {
  decimal_list <- xml2::read_xml(
    "<list type=\"decimal\">
      <item label=\"(1)\">
        <para>Item 1</para>
      </item>
      <item label=\"(2)\">
        <para>Item 2</para>
      </item>
    </list>"
  )
  decimal_expected <- tibble::tribble(
    ~text,
    "(1) Item 1",
    "(2) Item 2"
  )
  expect_equal(parse_node(decimal_list), decimal_expected)

  bullet_list <- xml2::read_xml(
    "<list type=\"bullet\">
      <item>
        <para>Item 1</para>
      </item>
      <item>
        <para>Item 2</para>
      </item>
    </list>"
  )
  bullet_expected <- tibble::tribble(
    ~text,
    "\U2022 Item 1",
    "\U2022 Item 2"
  )
  expect_equal(parse_node(bullet_list), bullet_expected)
})

test_that("we can trim punctuation", {
  expect_equal(trim_punctuation("giraffe"), "giraffe")
  expect_equal(trim_punctuation("-giraffe"), "giraffe")
  expect_equal(trim_punctuation("--giraffe"), "giraffe")
  expect_equal(trim_punctuation("giraffe-"), "giraffe")
  expect_equal(trim_punctuation("giraffe--"), "giraffe")
  expect_equal(trim_punctuation("-giraffe-"), "giraffe")
  expect_equal(trim_punctuation("--giraffe--"), "giraffe")
  expect_equal(trim_punctuation("(giraffe)"), "giraffe")
})
