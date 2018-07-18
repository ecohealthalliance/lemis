# LEMIS import and merging script based on WildDB script originally written
# by Carlos Zambrana-Torrelio

# The goal of this script is to convert raw LEMIS Excel files to CSVs and
# merge them to create yearly files


# Load packages
library(dplyr)
library(tidyr)
library(stringr)
library(readxl) # Need devtools::install_version("readxl", version = "1.0.0")
library(assertthat)
library(aws.s3)

h <- here::here
aws.signature::use_credentials()

#==============================================================================


# Download raw LEMIS data from the Amazon bucket to a local copy


# Get keys (filenames) for relevant objects from the Amazon bucket
keys <- get_bucket_df(bucket = "eha.wild.db",
                      prefix = "Original_data/by_year/") %>%
  pull(Key)

# Some object keys only list a folder, not the files themselves, and this
# causes a problem with saving the objects (some weird AWS thing?)
# In any case, get rid of these folder names since we want the actual files
keys <- grep("/$", keys, value = T, invert = T)

# Save the files, essentially making a local copy of what's on the bucket
# under the "Original_data/by_year" subdirectory
sapply(seq_along(keys), function(i)
  save_object(keys[i], bucket = "eha.wild.db",
              file = h("data-raw", str_replace(keys[i], "Original_data/", ""))
  )
)

#==============================================================================


# Generate yearly CSV files


# Review the number of worksheets per Excel file
file.list <- dir(path = "data-raw/by_year",
                 full.names = TRUE, recursive = TRUE)

for (i in file.list){
  print(i)
  print(readxl::excel_sheets(i))
}
# Note that all files have multiple sheets, with the last sheet being
# a "Qry" sheet, which is metadata, not core LEMIS data


# Create a dataframe to hold information that will be used in the loop
# to generate yearly-level LEMIS CSV files
# The information we need are the years of data and the Excel filenames
# associated with each year
df_for_looping <- data.frame(dir(path = "data-raw/by_year/"),
                             stringsAsFactors = FALSE)
colnames(df_for_looping) <- "year"

df_for_looping <- df_for_looping %>%
  group_by(year) %>%
  summarize(files = list(dir(path = paste0("data-raw/by_year/", year),
                             full.names = TRUE, recursive = TRUE)))


# Since there are some issues with column headings from different years
# or files of data not matching, record the desired column headings
# as well as problematic alternatives that arise. These are used for
# error checking during the loop below
desired_header <-
  read_excel("data-raw/by_year/2000/DecDetail1_Q1_2000.FOIA-SmithK.2013.08.06.xlsx", sheet = 1) %>%
  colnames()

problem_header_1 <-
  read_excel("data-raw/by_year/2008/DecDetail.Smith.2008JanMar.2011.08.02.xls", sheet = 1) %>%
  colnames()

problem_header_2 <-
  read_excel("data-raw/by_year/2013/Copy of DecDetail_Jun-Sep_2013.FOIA-Smith.2015.04.13_redacted.xlsx", sheet = 1) %>%
  colnames()


# Create a directory to hold yearly-level CSV files
if(!dir.exists("data-raw/csv_by_year/")) dir.create("data-raw/csv_by_year/")

# Generate the yearly-level CSV files
for (i in seq_along(df_for_looping$year)) {

  # Initialize a year-level dataframe
  year_df <- data.frame()

  for (file_number in seq_along(df_for_looping$files[[i]])) {

    file_name <- df_for_looping$files[[i]][file_number]
    # How many worksheets are in the Excel file?
    n_sheets <- length(readxl::excel_sheets(file_name))
    # Since the last worksheet in every file is always a "QRY" file,
    # we don't want to record data from that
    n_sheets <- n_sheets - 1

    # Initialize an Excel file-level dataframe
    file_df <- data.frame()

    for (sheet_number in seq_len(n_sheets)) {

      # Read in the sheet-level data
      sheet_df <- read_excel(file_name, sheet = sheet_number)

      # Error checking for column heading names (issue 1)
      if(identical(colnames(sheet_df), problem_header_1)) {

        # Assign column names
        colnames(sheet_df) <- c("Control\r\nNumber", "Genus", "Species", "Subspecies",
                                "Species\r\nCode", "Generic\r\nName", "Specific\r\nName", "Wildlf\r\nDesc",
                                "Qty", "Unit", "Ctry\r\nOrg", "Ctry\r\nIE",
                                "Purp", "Src", "Act", "Dp\r\nCd",
                                "Disp\r\nDate", "Ship\r\nDate", "I\r\nE", "Pt\r\nCd",
                                "U.S.Importer/\r\nExporter", "Foreign Importer/\r\nForeign Exporter")

        # Add a "Value" column
        sheet_df$Value <- rep(NA, nrow(sheet_df))

        # Reorder column names to match previous years
        sheet_df <- sheet_df %>%
          select("Control\r\nNumber", "Species\r\nCode", "Genus", "Species",
                 "Subspecies", "Specific\r\nName", "Generic\r\nName", "Wildlf\r\nDesc",
                 "Qty", "Unit", "Value", "Ctry\r\nOrg",
                 "Ctry\r\nIE", "Purp", "Src", "Act",
                 "Dp\r\nCd", "Disp\r\nDate", "Ship\r\nDate", "I\r\nE",
                 "Pt\r\nCd", "U.S.Importer/\r\nExporter", "Foreign Importer/\r\nForeign Exporter")
      }

      # Error checking for column heading names (issue 2)
      if(identical(colnames(sheet_df), problem_header_2)) {

        # Add a "Value" column
        sheet_df$Value <- rep(NA, nrow(sheet_df))

        # Reorder column names to match previous years
        sheet_df <- sheet_df %>%
          select("Control\r\nNumber", "Species\r\nCode", "Genus", "Species",
                 "Subspecies", "Specific\r\nName", "Generic\r\nName", "Wildlf\r\nDesc",
                 "Qty", "Unit", "Value", "Ctry\r\nOrg",
                 "Ctry\r\nIE", "Purp", "Src", "Act",
                 "Dp\r\nCd", "Disp\r\nDate", "Ship\r\nDate", "I\r\nE",
                 "Pt\r\nCd", "U.S.Importer/\r\nExporter", "Foreign Importer/\r\nForeign Exporter")
      }

      # This is a critical check: do the sheet column headings match with
      # the desired headings? If not, rbind() will be thrown off, so we
      # have to make sure this is the case
      assert_that(identical(colnames(sheet_df), desired_header))

      # Combine the sheet-level data with other data from the same Excel file
      file_df <- rbind(file_df, sheet_df)

      # Print processing status
      print(paste0("Year ", i,
                   ", File ", file_number,
                   ", Sheet ", sheet_number, " Processed"))
    }

    # Combine the Excel file-level data with other data from the same year
    year_df <- rbind(year_df, file_df)
  }

  # Write out a merged CSV file for each year of the LEMIS data
  write.csv(year_df,
            paste0("data-raw/csv_by_year/lemis_", df_for_looping$year[i], ".csv"),
            row.names = FALSE)
}
