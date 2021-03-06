# lemis 1.1.0

This data release has the same temporal scope as the v1.0.0 data (2000-2014) but with minor revisions (mainly related to taxonomy calling) resulting from peer review feedback.

# lemis 1.0.0

This is the first version of the **lemis** data intended for public release. Changes include:

* Taxonomic data overhaul, with major efforts to clean existing taxonomic names and the addition of class-level information using the `taxadb` package
* Significant updates to the data cleaning workflow generally, including the filtering of apparent duplicate records
* In previous data versions, `control_number` 2006798504 was problematic as it contained multiple records erroneously embedded within a single data field. This issue has been resolved

# lemis 0.2.0

This is the first update to the **lemis** package. Changes include:

* Addition of data from late 2013 and all of 2014
* A major reorganization of **lemis** data processing. Briefly, all data importation and cleaning steps are now automated with scripts located in the `data-raw/` subdirectory. While this should be mostly irrelevant to the end user, it means that all data processing code is now fully contained within the **lemis** package repository. This should make it easier to incorporate future data into the pipeline
* Improved error handling for non-standard data values. Previously, some records with non-standard values in specific fields were dropped from the data. Now the **lemis** cleaning workflow incorporates error checking for non-standard values across all the fields of data for which valid values are described in USFWS spreadsheets. Non-standard values are converted to `non-standard value` (as opposed to being converted to `NA` or dropped) and a `cleaning_notes` column has been added to describe the original value
* Note that data versions from previous releases can still be had with `lemis_data("0.1.0")`

## Bug fixes

* Browsable tables in HTML should now work under systems with all pandoc versions

## Minor changes

* Reduced dependencies by removing `tidyverse` package
* Updated test infrastructure to R 3.5.1

# lemis 0.1.0

* Initial pre-release version of the **lemis** data
