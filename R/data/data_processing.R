# Data loading and processing functions

# Updated function to standardize institute names
standardize_institute_names <- function(df) {
  # First detect if institute column exists
  if (!"institute" %in% names(df)) {
    return(df)  # Return unchanged if institute column doesn't exist
  }
  
  # Create lookup table for institute name standardization
  institute_mapping <- c(
    # Common NIH institute variations
    "NIMH - National Institute of Mental Health" = "NIMH",
    "NIMH-National Institute of Mental Health" = "NIMH",
    "National Institute of Mental Health" = "NIMH",
    
    "NIMHD - National Institute on Minority Health and Health Disparities" = "NIMHD",
    "NIMHD-National Institute on Minority Health and Health Disparities" = "NIMHD",
    "National Institute on Minority Health and Health Disparities" = "NIMHD",
    
    "NIAID - National Institute of Allergy and Infectious Diseases" = "NIAID",
    "NIAID-National Institute of Allergy and Infectious Diseases" = "NIAID",
    "National Institute of Allergy and Infectious Diseases" = "NIAID",
    "Allergy and Infectious Diseases" = "NIAID",
    
    "NCI - National Cancer Institute" = "NCI",
    "NCI-National Cancer Institute" = "NCI",
    "National Cancer Institute" = "NCI",
    
    "NIGMS - National Institute of General Medical Sciences" = "NIGMS",
    "NIGMS-National Institute of General Medical Sciences" = "NIGMS",
    "National Institute of General Medical Sciences" = "NIGMS",
    
    "NICHD - National Institute of Child Health and Human Development" = "NICHD",
    "NICHD-National Institute of Child Health and Human Development" = "NICHD",
    "National Institute of Child Health and Human Development" = "NICHD",
    "Eunice Kennedy Shriver National Institute of Child Health and Human Development" = "NICHD",
    
    "OD - NIH Office of the Director" = "OD",
    "OD-NIH Office of the Director" = "OD",
    "NIH Office of the Director" = "OD",
    "Office of the Director" = "OD"
  )
  
  # Apply mapping to standardize names
  df <- df %>%
    mutate(
      # Create a standardized institute name
      institute_std = case_when(
        institute %in% names(institute_mapping) ~ institute_mapping[institute],
        # For any other entries, try to extract the acronym only (assume it's at the beginning)
        grepl(" - ", institute) ~ str_extract(institute, "^[A-Z0-9]+(?= - )"),
        grepl("-", institute) ~ str_extract(institute, "^[A-Z0-9]+(?=-)"),
        # If all else fails, keep the original
        TRUE ~ institute
      )
    )
  
  # Replace the original institute column with standardized version
  df$institute <- df$institute_std
  df$institute_std <- NULL
  
  return(df)
}



# Load and process data
load_data <- function(trigger_terms_path, nih_taggs_path, nih_airtable_path, nsf_grants_path) {
  # Load trigger terms
  trigger_terms <- read_csv(trigger_terms_path, show_col_types = FALSE) |>
    rename(term = `Trigger Terms`) |>
    mutate(term = tolower(term),
           term = trimws(term))

  # Load and process new NIH grants (Airtable)
  nih_airtable <- read_csv(nih_airtable_path, show_col_types = FALSE) |>
    mutate(
      # Create combined text with whatever fields are available
      combined_text = case_when(
        !is.na(abstract_text) & !is.na(terms) ~ paste(project_title, abstract_text, terms, sep = " "),
        !is.na(abstract_text) ~ paste(project_title, abstract_text, sep = " "),
        !is.na(terms) ~ paste(project_title, terms, sep = " "),
        TRUE ~ project_title
      ),
      combined_text = tolower(combined_text),
      termination_date = ymd(termination_date),
      year = year(termination_date),
      month = month(termination_date),
      month_name = month(termination_date, label = TRUE, abbr = FALSE),
      quarter = quarter(termination_date),
      fiscal_quarter = case_when(
        month %in% c(10, 11, 12) ~ "Q1",
        month %in% c(1, 2, 3) ~ "Q2",
        month %in% c(4, 5, 6) ~ "Q3",
        month %in% c(7, 8, 9) ~ "Q4"
      ),
      agency = "NIH",
      state = org_state,
      institute = program_office,
      # Clean award number for joining
      clean_award_number = str_trim(str_replace_all(full_award_number, "[^0-9A-Za-z]", "")),
      # Mark data source
      data_source = "Airtable"
    )
  
  cat("NIH dataset has", nrow(nih_airtable), "grants\n")
  
  # Load and process NSF grants
  nsf_grants <- read_csv(nsf_grants_path, show_col_types = FALSE) |>
    mutate(
      combined_text = paste(project_title, abstract, sep = " "),
      combined_text = tolower(combined_text),
      termination_date = ymd(termination_letter_date),
      nsf_startdate = ymd(nsf_startdate),
      nsf_expected_end_date = ymd(nsf_expected_end_date),
      year = year(termination_date),
      month = month(termination_date),
      month_name = month(termination_date, label = TRUE, abbr = FALSE),
      quarter = quarter(termination_date),
      fiscal_quarter = case_when(
        month %in% c(10, 11, 12) ~ "Q1",
        month %in% c(1, 2, 3) ~ "Q2",
        month %in% c(4, 5, 6) ~ "Q3",
        month %in% c(7, 8, 9) ~ "Q4"
      ),
      agency = "NSF",
      state = org_state,
      # Mark data source
      data_source = "NSF"
    )
  
  # Print date diagnostics
  cat("NIH grants with valid dates:", sum(!is.na(nih_airtable$termination_date)), "out of", nrow(nih_airtable), "\n")
  cat("NSF grants with valid dates:", sum(!is.na(nsf_grants$termination_date)), "out of", nrow(nsf_grants), "\n")
  
  # Apply trigger term detection
  nih_airtable <- detect_trigger_terms(nih_airtable, trigger_terms)
  nsf_grants <- detect_trigger_terms(nsf_grants, trigger_terms)
  
  # Add administration based on date
  nih_airtable <- add_administration(nih_airtable)
  nsf_grants <- add_administration(nsf_grants)
  
  # Combine datasets
  nih_airtable <- standardize_institute_names(nih_airtable)
  all_grants <- bind_rows(nih_airtable, nsf_grants)
  
  # Count occurrences of each trigger term
  nih_term_counts <- count_term_occurrences(nih_airtable)
  nsf_term_counts <- count_term_occurrences(nsf_grants)
  
  # Combine and sort
  all_term_counts <- bind_rows(
    mutate(nih_term_counts, agency = "NIH"),
    mutate(nsf_term_counts, agency = "NSF")
  ) |>
    group_by(term) |>
    summarize(frequency = sum(count), .groups = "drop") |>
    arrange(desc(frequency))
  
  return(list(
    nih_grants = nih_airtable,
    nsf_grants = nsf_grants,
    trigger_terms = trigger_terms,
    all_grants = all_grants,
    nih_term_counts = nih_term_counts,
    nsf_term_counts = nsf_term_counts,
    all_term_counts = all_term_counts
  ))
}
