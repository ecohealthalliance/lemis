
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lemis

[![CircleCI](https://circleci.com/gh/ecohealthalliance/lemis.svg?style=shield&circle-token=23cd13e8d5276a8100a83984982d065d1773fd77)](https://circleci.com/gh/ecohealthalliance/lemis)

Authors: *Noam Ross, Allison M. White, Carlos Zambrana-Torrelio, Evan A.
Eskew*

The **lemis** package provides access to United States Fish and Wildlife
Service (USFWS) data on wildlife and wildlife product imports to the
U.S. This data was obtained via more than 14 years of Freedom of
Information Act (FOIA) requests by EcoHealth Alliance.

## Installation

Install the **lemis** package with this command:

``` r
devtools::install_github("ecohealthalliance/lemis")
```

As this is currently a private repository, you must have a GitHub
personal access token set up to install and use the package.
Instructions for this can be found
[here](http://happygitwithr.com/github-pat.html#step-by-step).

## Usage

The main function in **lemis** is `lemis_data()`. This returns the
cleaned LEMIS database as a **dplyr** tibble.

**lemis** makes use of
[**datastorr**](https://github.com/ropenscilabs/datastorr) to manage
data download. The first time you run `lemis_data()`, the package will
download the most recent version of the database (~160 MB at present).
Subsequent calls will load the database from storage on your computer.

The LEMIS database is stored as an efficiently compressed [`.fst`
file](https://github.com/fstpackage/fst), and loading it loads it a
[remote dplyr source](https://github.com/krlmlr/fstplyr). This means
that it does not load fully into memory but can be filtered and
manipulated on-disk. If you wish to manipulate it as a data frame,
simply call `dplyr::collect()` to load it fully into memory, like so:

``` r
all_lemis <- lemis_data() %>% 
  collect()
```

Note that the full database will be ~1 GB in memory.

`lemis_metadata()` provides a brief description of each of the data
fields in `lemis_data()`, while `lemis_codes()` returns a data frame
with the codes (abbreviations) used by USFWS in the various columns.
This is useful for lookup or joining with the main data for more
descriptive outputs. The `?lemis_codes` help file also has a searchable
table of USFWS codes.

Our [paper (Smith et.
al. 2017)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5357285/)
provides a broad introduction to this data and its relevance to
infectious disease. See the
[vignette](https://github.com/ecohealthalliance/lemis/tree/master/inst/doc/the-lemis-database.md)
for a more in-depth tutorial and example use cases for the package. See
the [developer
README](https://github.com/ecohealthalliance/lemis/tree/master/data-raw/README.md)
for more on the data cleaning process.

## About

Please give us feedback or ask questions by filing
[issues](https://github.com/ecohealthalliance/lemis/issues).

**lemis** is developed at [EcoHealth
Alliance](https://github.com/ecohealthalliance). Please note that this
project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project, you
agree to abide by its
terms.

[![http://www.ecohealthalliance.org/](inst/figs/eha-footer.png)](http://www.ecohealthalliance.org/)
