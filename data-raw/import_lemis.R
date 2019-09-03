# LEMIS import and merging script based on WildDB script originally written
# by Carlos Zambrana-Torrelio

# The goal of this script is to convert raw LEMIS Excel files to CSVs and
# merge them to create yearly LEMIS files

# Access to the EHA Amazon Web Services bucket is required, and it is
# expected that credentials are in .aws/credentials


# Load packages
library(assertthat)
library(aws.s3)
library(dplyr)
library(readxl)
library(stringr)

h <- here::here
aws.signature::use_credentials()

#==============================================================================


# Download raw LEMIS data from the Amazon bucket to a local copy


# Get keys (filenames) for relevant objects from the Amazon bucket
keys <-
  get_bucket_df(bucket = "eha.wild.db", prefix = "Original_data/by_year/") %>%
  pull(Key)

# Some object keys only list a folder, not the files themselves, and this
# causes a problem with saving the objects (some AWS idiosyncrasy?)
# In any case, get rid of these folder names since we want the actual files
keys.mod <- grep("/$", keys, value = T, invert = T)

# Save the files, making a local copy in the "data-raw/raw_data" subdirectory
# of what's on the bucket in the "Original_data/by_year" subdirectory
sapply(seq_along(keys.mod), function(i)

  save_object(keys.mod[i], bucket = "eha.wild.db",
              file = h("data-raw", str_replace(keys.mod[i],
                                               "Original_data/by_year",
                                               "raw_data")
              )
  )
)

#==============================================================================


# Generate yearly CSV files


# Review the number of worksheets per Excel file
file.list <- dir(
  path = h("data-raw", "raw_data"),
  full.names = TRUE, recursive = TRUE
)

for (i in file.list) {
  print(i)
  print(excel_sheets(i))
}
# Note that all files have multiple sheets, with the last sheet being
# a "Qry" sheet, which is metadata, not core LEMIS data


# Create a data frame to hold information that will be used in the loop
# to generate yearly-level LEMIS CSV files
# The information we need are the years of data and the Excel filenames
# associated with each year
df.for.looping <-
  data.frame(
    year = dir(path = h("data-raw", "raw_data")),
    stringsAsFactors = FALSE
  ) %>%
  group_by(year) %>%
  summarize(
    files = list(
      dir(path = h("data-raw", "raw_data", year),
          full.names = TRUE, recursive = TRUE
      )
    )
  )


# Since there are some issues with column headings from different years
# or files of data not matching, record the desired column headings
# as well as problematic alternatives that arise. These are used for
# error checking in the loop below
desired.header <-
  read_excel("data-raw/raw_data/2000/DecDetail1_Q1_2000.FOIA-SmithK.2013.08.06.xlsx", sheet = 1) %>%
  colnames()

problem.header.1 <-
  read_excel("data-raw/raw_data/2008/DecDetail.Smith.2008JanMar.2011.08.02.xls", sheet = 1) %>%
  colnames()

problem.header.2 <-
  read_excel("data-raw/raw_data/2013/Copy of DecDetail_Jun-Sep_2013.FOIA-Smith.2015.04.13_redacted.xlsx", sheet = 1) %>%
  colnames()


# Create a directory to hold yearly-level CSV files
if (!dir.exists(h("data-raw", "csv_by_year"))) dir.create(h("data-raw", "csv_by_year"))

# Generate the yearly-level CSV files
for (i in seq_along(df.for.looping$year)) {

  # Initialize a year-level data frame
  year.df <- data.frame()

  for (file.number in seq_along(df.for.looping$files[[i]])) {

    file.name <- df.for.looping$files[[i]][file.number]
    # How many worksheets are in the Excel file?
    n.sheets <- length(excel_sheets(file.name))
    # Since the last worksheet in every file is always a "QRY" file,
    # we don't want to record data from that
    n.sheets <- n.sheets - 1

    # Initialize an Excel file-level data frame
    file.df <- data.frame()

    for (sheet.number in seq_len(n.sheets)) {

      # Read in the sheet-level data
      sheet.df <- read_excel(file.name, sheet = sheet.number)

      # Error checking for column heading names (issue 1)
      if (identical(colnames(sheet.df), problem.header.1)) {

        # Assign column names
        colnames(sheet.df) <- c(
          "Control\r\nNumber", "Genus", "Species", "Subspecies",
          "Species\r\nCode", "Generic\r\nName", "Specific\r\nName", "Wildlf\r\nDesc",
          "Qty", "Unit", "Ctry\r\nOrg", "Ctry\r\nIE",
          "Purp", "Src", "Act", "Dp\r\nCd",
          "Disp\r\nDate", "Ship\r\nDate", "I\r\nE", "Pt\r\nCd",
          "U.S.Importer/\r\nExporter", "Foreign Importer/\r\nForeign Exporter"
        )

        # Add a "Value" column
        sheet.df$Value <- rep(NA, nrow(sheet.df))

        # Reorder column names to match previous years
        sheet.df <- sheet.df %>%
          select(
            "Control\r\nNumber", "Species\r\nCode", "Genus", "Species",
            "Subspecies", "Specific\r\nName", "Generic\r\nName", "Wildlf\r\nDesc",
            "Qty", "Unit", "Value", "Ctry\r\nOrg",
            "Ctry\r\nIE", "Purp", "Src", "Act",
            "Dp\r\nCd", "Disp\r\nDate", "Ship\r\nDate", "I\r\nE",
            "Pt\r\nCd", "U.S.Importer/\r\nExporter", "Foreign Importer/\r\nForeign Exporter"
          )
      }

      # Error checking for column heading names (issue 2)
      if (identical(colnames(sheet.df), problem.header.2)) {

        # Add a "Value" column
        sheet.df$Value <- rep(NA, nrow(sheet.df))

        # Reorder column names to match previous years
        sheet.df <- sheet.df %>%
          select(
            "Control\r\nNumber", "Species\r\nCode", "Genus", "Species",
            "Subspecies", "Specific\r\nName", "Generic\r\nName", "Wildlf\r\nDesc",
            "Qty", "Unit", "Value", "Ctry\r\nOrg",
            "Ctry\r\nIE", "Purp", "Src", "Act",
            "Dp\r\nCd", "Disp\r\nDate", "Ship\r\nDate", "I\r\nE",
            "Pt\r\nCd", "U.S.Importer/\r\nExporter", "Foreign Importer/\r\nForeign Exporter"
          )
      }

      # This is a critical check: do the sheet column headings match with
      # the desired headings? If not, rbind() will be thrown off, so we
      # have to make sure this is the case
      assert_that(identical(colnames(sheet.df), desired.header))

      # Combine the sheet-level data with other data from the same Excel file
      file.df <- rbind(file.df, sheet.df)

      # Print processing status
      print(paste0(
        "Year ", i,
        ", File ", file.number,
        ", Sheet ", sheet.number, " Processed"
      ))
    }

    # Create a file number variable for error checking during data cleaning
    file.df$file_num <- file.number

    # Combine the Excel file-level data with other data from the same year
    year.df <- rbind(year.df, file.df)
  }

  # Write out a merged CSV file for each year of the LEMIS data
  write.csv(
    year.df,
    h("data-raw", "csv_by_year", paste0("lemis_", df.for.looping$year[i], ".csv")),
    row.names = FALSE
  )
}
