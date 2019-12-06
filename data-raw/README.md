# **lemis** data processing and cleaning 

The scripts in this directory import, clean, and process the data for the **lemis** package. While the initial pre-release version of **lemis** relied on the related [WildDB](https://github.com/ecohealthalliance/WildDB/) repository for data preparation, as of **lemis** v0.2.0 these processing steps have been moved to the `data-raw/` subdirectory of the **lemis** package repository for consistency and ease of use. The core **lemis** data preparation workflow requires execution of four scripts in succession:

1) `import_lemis.R` downloads a local copy of all raw LEMIS data files, which are stored on an [Amazon Web Services S3 bucket](https://s3.console.aws.amazon.com/s3/buckets/eha.wild.db/). These data consist of multiple Microsoft Excel files for a given year of FOIA requests. In addition, each spreadsheet file may contain different numbers of sheets with relevant LEMIS data. `import_lemis.R` imports the raw data (in `data-raw/raw_data/`) and merges data into yearly CSV files (in `data-raw/csv_by_year/`).

2) `clean_lemis.R` merges the yearly LEMIS CSV files into a single data frame and performs various data cleaning steps. Following generation of the single cleaned LEMIS data file, the local copies of intermediate files in `data-raw/raw_data/` and `data-raw/csv_by_year/` can be safely deleted (since they can always be regenerated from `import_lemis.R`).

3) `taxonomy_lemis.R` cleans the LEMIS taxonomic data and adds higher level taxonomic information, making use of the [`taxadb`](https://cboettig.github.io/taxadb/) package. The output of this script represents the final, cleaned LEMIS data file in CSV format.

4) `process_lemis.R` processes the cleaned LEMIS data file for use in a **lemis** package release by compressing the data into an `.fst` file.
  
    Once the `.fst` file is generated, it can be attached to the package as a release using `datastorr::github_release_create()`. Please read the help file for this function before doing so. Also, before release, one should update the package version in `DESCRIPTION` and commit all changes to GitHub.

    v1.1.0 of **lemis** has the 2000-2014 data set.

Finally, `data-raw/` contains two supplemental scripts related to **lemis** package development:

1) `scrape_codes.R` uses the [`tabulizer`](https://github.com/ropensci/tabulizer) package to extract text from USFWS codebooks (in PDF form) and generates `lemis_codes()` and `lemis_metadata()`. 

2) `validation_plots.R` generates visualizations (in `data-raw/validation_plots/`) that are useful for confirming data quality prior to a data release.
