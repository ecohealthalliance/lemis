
<!-- README.md is generated from README.Rmd. Please edit that file -->
lemis
=====

Authors: *Noam Ross, Allison White, Carlos Zambrana-Torrelio*

The **lemis** package provides access U.S. Fish and Wildlife Service data on wildlife and wildlife product imports to and exports from the United States. This data was obtained via more than 14 years of FOIA requests by EcoHealth Alliance.

Installation
------------

Install the **lemis** package with this command:

``` r
source("https://install-github.me/ecohealthalliance/lemis")
```

As this is currently a private repository, you must have a GitHub personal access token set up to install and use the package. Instructions for this can be found [here](http://happygitwithr.com/github-pat.html#step-by-step).

Usage
-----

The main function in **lemis** is `lemis_data()`. This returns the main cleaned LEMIS database as a **dplyr** tibble.

**lemis** makes use of [**datastorr**](https://github.com/ropenscilabs/datastorr) to manage data download. The first time you run `lemis_data()` the package will download the most recent version of the database (~160MB). Subsequent calls will load the database from storage on your computer.

The LEMIS database is stored as an efficiently compressed [`.fst` file](https://github.com/fstpackage/fst), and loading it loads it a a [remote dplyr source](https://github.com/krlmlr/fstplyr). This means that it does not load fully into memory, but can be filtered and manipulated on-disk. If you wish to manipulate it as a data frame, simply call `dplyr::collect()` to load it fully into memory, like so:

``` r
all_lemis <- lemis_data() %>% 
  collect()
```

Note that the full database will be approximately 1 GB in memory.

`lemis_codes()` returns a data frame with descriptions of the codes used by USFWS in the various columns of `lemis_data()`. This is useful for lookup or joining with the main data for more descriptive outputs. The `?lemis_code` help file also has a searchable table of these codes.

See the [vignette](https://github.com/ecohealthalliance/lemis/tree/master/inst/doc/the-lemis-database.md) for a more in-depth tutorial and example use cases for the package.

About
-----

**lemis** is developed at [EcoHealth Alliance](https://github.com/ecohealthalliance). Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

[![http://www.ecohealthalliance.org/](inst/figs/eha-footer.png)](http://www.ecohealthalliance.org/)
