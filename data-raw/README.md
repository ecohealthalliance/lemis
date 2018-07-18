# Data processing for **lemis**

The scripts in this directory import, clean, and process the data for the **lemis** package. While the initial version of **lemis** relied on the related [WildDB](https://github.com/ecohealthalliance/WildDB/) repository for data cleaning, as of **lemis** vX.X.X, these processing steps have been moved to the `data-raw/` subdirectory of the **lemis** package repository for consistency and ease of use. The **lemis** data preparation workflow requires execution of three scripts in succession:

- `import_lemis.R` downloads a local copy of all raw LEMIS files, which are kept on an [Amazon Web Services S3 bucket](https://s3.console.aws.amazon.com/s3/buckets/eha.wild.db/). These data consist of multiple Excel files for a given year of FOIA requests. In addition, each spreadsheet file may contain different numbers of sheets with relevant LEMIS data. `import_lemis.R` imports all of this raw data (in `data-raw/by_year/`) and merges all data into yearly CSV files (in `data-raw/csv_by_year/`).

- `clean_lemis.R` merges the yearly LEMIS CSV files into a single dataframe of all LEMIS data and performs various cleaning steps. Following generation of the single cleaned LEMIS data file, the local copies of intermediate files in `data-raw/by_year/` and `data-raw/csv_by_year/` can be safely deleted (since they can always be regenerated from `import_lemis.R` and `clean_lemis.R`).

- `process_lemis.R` processes the cleaned LEMIS data for use in a **lemis** package release by compressing the data into an `.fst` file.

In addition, there is the script `scrape_codes.R`, which uses the **tabulizer** package to extract codes from
the PDF codebook to generate `lemis_codes()` and `lemis_metadata()`. 

Once the `.fst` file is generated, it can be attached to the package as a release using `datastorr::github_release_create()`. Please read the help file for this function before doing so. Also, before release, one should update the package version in `DESCRIPTION` and commit all changes to GitHub.

v1.0.0 of **lemis** has the 2000-2013 data set.
