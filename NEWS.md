# lemis 2.0.0

This is the first major update to the **lemis** package. Changes include:

* Addition of data from late 2013 and all of 2014.
* A major reorganization of **lemis** data processing for release. Briefly, all data importation and cleaning steps are now automated with scripts located in the `data-raw/` subdirectory. While this should be mostly irrelevant to the end user, it means that all data processing code is now fully contained within the **lemis** package repository. This should make it easier to incorporate future data into the pipeline.
* Improved error handling for non-standard data values. Previously, some records with non-standard values in specific fields were dropped from the data. Now the **lemis** cleaning workflow incorporates error checking for non-standard values across all the fields of data for which valid values are described in USFWS spreadsheets. Non-standard values are converted to `non-standard value` (as opposed to being converted to `NA` or dropped) and a `cleaning_notes` column has been added to describe the original value.

# lemis 1.0.0

* Initial release
