

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
