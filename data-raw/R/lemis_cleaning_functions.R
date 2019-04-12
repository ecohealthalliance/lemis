

compareNA <- function(x1, x2) {

  # This function returns TRUE wherever elements are the same, including NAs,
  # and FALSE everywhere else
  same <- (x1 == x2) | (is.na(x1) & is.na(x2))
  same[is.na(same)] <- FALSE
  return(same)
}

# Function to clean the intermediate LEMIS dataset variables given valid codes

get_cleaned_lemis <- function(variable, valid.values) {

  # Get the column index of the variable to clean
  index <- which(colnames(lemis) == variable)

  lemis %>%
    # Add cleaning notes based on the values in the variable
    mutate(
      cleaning_notes = case_when(
        !(.[[index]] %in% valid.values) & !is.na(.[[index]]) & is.na(cleaning_notes) ~
          paste0("Original value in ", variable, " column: ", .[[index]]),
        !(.[[index]] %in% valid.values) & !is.na(.[[index]]) & !is.na(cleaning_notes) ~
          paste0(cleaning_notes, ", ", variable, " column: ", .[[index]]),
        TRUE ~ cleaning_notes
      )
    ) %>%
    # Add non-standard values to the variable in question where appropriate
    mutate(
      UQ(rlang::sym(variable)) :=
        ifelse(!(UQ(rlang::sym(variable)) %in% valid.values) & !is.na(UQ(rlang::sym(variable))),
               "non-standard value", UQ(rlang::sym(variable))),
      UQ(rlang::sym(variable)) := as.factor(UQ(rlang::sym(variable)))
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
            paste0("Original value in ", x, " column: ", .[[index]]),
          !matching & !is.na(cleaning_notes) ~
            paste0(cleaning_notes, ", ", x, " column: ", .[[index]]),
          TRUE ~ cleaning_notes
        )
      )
  }
  return(dataframe)
}
