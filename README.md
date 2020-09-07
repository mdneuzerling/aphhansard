
# aphhansard

<!-- badges: start -->
[![Last commit](https://img.shields.io/github/last-commit/mdneuzerling/aphhansard/main.svg)](https://github.com/mdneuzerling/aphhansard/tree/main)
[![R build status](https://github.com/mdneuzerling/aphhansard/workflows/R-CMD-check/badge.svg)](https://github.com/mdneuzerling/aphhansard/actions)
[![license](https://img.shields.io/badge/license-MIT-lightgrey.svg)](https://choosealicense.com/licenses/mit/)
<!-- badges: end -->

The aim of this package is to parse [the Hansard of Australian Parliament House](https://www.aph.gov.au/Parliamentary_Business/Hansard) from XML into rectangular data frames. The Hansard is a written record of the proceedings of the Australian federal parliament.

This is a work in progress, and the vast majority of the Hansard cannot yet be parsed.

The Hansard is [provided by the Australian Parliament House](https://www.aph.gov.au/Help/Disclaimer_Privacy_Copyright) under a [Creative Commons Attribution-NonCommercial-NoDerivs 3.0 Australia](creativecommons.org/licenses/by-nc-nd/3.0/au) licence.

## Installation

You can install the latest version from GitHub with:

``` r
remotes::install_packages("mdneuzerling/aphhansard")
```

## Example

The largest functional component implemented thus far is the parsing of the "chamber" node of a single day of Hansard. This functionality has only been tested for the day below:

``` r
library(aphhansard)
xml_link <- "https://parlinfo.aph.gov.au/parlInfo/download/chamber/hansardr/2005-03-16/toc_unixml/3766-5.xml;fileType=text%2Fxml"
xml <- xml2::read_xml(xml_link)
chamber <- xml2::xml_child(xml, "chamber.xscript")
parse_node(chamber)
```

