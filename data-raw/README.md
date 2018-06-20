# Data processing for **lemis**

The scripts in this directory process the data for the **lemis** package.

`scrape_codes.R` uses the **tabulizer** package to extract codes from
the PDF codebook to generate `lemis_codes()` and `lemis_metadata()`. 

Raw LEMIS files are kept on AWS S3 at <https://s3.console.aws.amazon.com/s3/buckets/eha.wild.db/>. The `cleaned_data` directory there contains the data after extracting from XLS and going through some basic processing. The processing changes from year to year and so is not automated for all files. It is described in detail at <https://github.com/ecohealthalliance/WildDB/tree/master/scripts/data_cleaning>.

`process_lemis.R` imports the cleaned LEMIS data and performs a few more processing tasks before compressing the data into an `.fst` file. These should be moved upstream into the base data cleaning at the next iteration.

Once the `.fst` file is generated, it can be attached to this package as a release using `datastorr::github_release_create`. Please read the help file for this function before doing so. Also, before release, one should update the package version in `DESCRIPTION` and commit all changes to GitHub.

v1.0.0 of **lemis** has the 2000-2013 data set.
