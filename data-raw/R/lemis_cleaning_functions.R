

compareNA <- function(x1, x2) {

  # This function returns TRUE wherever elements are the same, including NAs,
  # and FALSE everywhere else
  same <- (x1 == x2) | (is.na(x1) & is.na(x2))
  same[is.na(same)] <- FALSE
  return(same)
}


# Function to clean the intermediate LEMIS dataset fields given valid codes

get_cleaned_lemis <- function(field, valid.values) {

  # Get the column index of the field to clean
  index <- which(colnames(lemis) == field)

  lemis %>%
    # Add cleaning notes based on the values in the field
    mutate(
      cleaning_notes = case_when(
        !(.[[index]] %in% valid.values) & !is.na(.[[index]]) & is.na(cleaning_notes) ~
          paste0("Original value in '", field, "' column: ", .[[index]]),
        !(.[[index]] %in% valid.values) & !is.na(.[[index]]) & !is.na(cleaning_notes) ~
          paste0(cleaning_notes, ", '", field, "' column: ", .[[index]]),
        TRUE ~ cleaning_notes
      )
    ) %>%
    # Add non-standard values to the field in question where appropriate
    mutate(
      UQ(rlang::sym(field)) :=
        ifelse(!(UQ(rlang::sym(field)) %in% valid.values) & !is.na(UQ(rlang::sym(field))),
               "non-standard value", UQ(rlang::sym(field))),
      UQ(rlang::sym(field)) := as.factor(UQ(rlang::sym(field)))
    )
}


# Function to produce cleaning notes for taxonomic data fields

get_taxonomic_cleaning_notes <- function(dataframe) {

  taxonomic.cols <-
    c("genus", "species", "subspecies", "specific_name", "generic_name")

  for(x in taxonomic.cols) {

    # Get the column index of the variable and its new version
    index <- which(colnames(dataframe) == x)
    index.new <- which(colnames(dataframe) == paste0("new_", x))

    # Do the two values match?
    matching <- compareNA(dataframe[, index], dataframe[, index.new])

    dataframe <- dataframe %>%
      # Add cleaning notes based on the values of the taxonomic variable
      mutate(
        cleaning_notes = case_when(
          !matching & is.na(cleaning_notes) ~
            paste0("Original value in '", x, "' column: ", .[[index]]),
          !matching & !is.na(cleaning_notes) ~
            paste0(cleaning_notes, ", '", x, "' column: ", .[[index]]),
          TRUE ~ cleaning_notes
        )
      )
  }

  return(dataframe)
}
