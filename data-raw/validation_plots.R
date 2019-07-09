

# Load packages and the cleaned LEMIS dataset

library(dplyr)
library(readr)
library(stringr)

h <- here::here

lemis_to_save <- read_csv(
  h("data-raw", "lemis_2000_2014_cleaned.csv"),
  col_types = cols(
    .default = col_character(),
    control_number = col_integer(),
    quantity = col_integer(),
    value = col_integer(),
    disposition_date = col_date(format = "%Y-%m-%d"),
    disposition_year = col_integer(),
    shipment_date = col_date(format = "%Y-%m-%d"),
    shipment_year = col_integer()
  )
)

#==============================================================================


# Generate a validation plot directory, if needed

plot.dir <- h("data-raw", "validation_plots")
if(!file.exists(plot.dir)) dir.create(plot.dir)

# Generate validation plots for all LEMIS fields besides 'control_number'

for (i in colnames(lemis_to_save)[-1]) {

  print(i)

  png(
    filename = h("data-raw", "validation_plots", paste0(i, "_plot1.png")),
    width = 1200, height = 600
  )

  plot(
    table(lemis_to_save[i], useNA = "ifany"),
    main = paste0(i, " values"), ylab = "Frequency"
  )

  dev.off()

  png(
    filename = h("data-raw", "validation_plots", paste0(i, "_plot2.png")),
    width = 1200, height = 600
  )

  plot(
    table(str_length(unlist(lemis_to_save[i])), useNA = "ifany"),
    main = paste0(i, " string length"), ylab = "Frequency"
  )

  dev.off()
}
