# Data loading and processing functions

# Load and process data
# Updated load_data function to incorporate both NIH datasets
load_data <- function(trigger_terms_path, nih_taggs_path, nih_airtable_path, nsf_grants_path) {
  # Load trigger terms
  trigger_terms <- read_csv(trigger_terms_path, show_col_types = FALSE) |>
    rename(term = `Trigger Terms`) |>
    mutate(term = tolower(term),
           term = trimws(term))
  
  # Load and process original NIH grants (TAGGS)
  nih_taggs <- read_csv(nih_taggs_path, show_col_types = FALSE) |>
    mutate(
      combined_text = paste(`Award Title`, `Project Terms`, sep = " "),
      combined_text = tolower(combined_text),
      termination_date = ymd(`Action Date (Date Terminated)`),
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
      state = `Organization State`,
      institute = str_extract(`Funding Institutes`, "^[^,]+"),
      # Clean award number for joining
      clean_award_number = str_trim(str_replace_all(`Award Number`, "[^0-9A-Za-z]", "")),
      # Mark data source
      data_source = "TAGGS"
    )
  
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
  
  # Identify duplicates based on award number
  cat("Checking for potential duplicate awards between TAGGS and Airtable datasets...\n")
  
  # Get counts of each award number
  award_counts <- bind_rows(
    nih_taggs |> select(clean_award_number) |> filter(!is.na(clean_award_number)),
    nih_airtable |> select(clean_award_number) |> filter(!is.na(clean_award_number))
  ) |>
    count(clean_award_number) |>
    filter(n > 1)
  
  cat("Found", nrow(award_counts), "potential duplicate award numbers\n")
  
  # Determine which records to keep (preferring Airtable when duplicates exist)
  nih_airtable <- nih_airtable |>
    mutate(keep = TRUE)
  
  nih_taggs <- nih_taggs |>
    mutate(keep = !clean_award_number %in% nih_airtable$clean_award_number | is.na(clean_award_number))
  
  # Combine NIH datasets, keeping only non-duplicate records
  combined_nih <- bind_rows(
    nih_airtable |> filter(keep),
    nih_taggs |> filter(keep)
  ) |>
    select(-keep) |>
    # Unify column names for institute (use program_office from Airtable if available)
    mutate(institute = coalesce(institute, program_office))
  
  cat("Combined NIH dataset has", nrow(combined_nih), "grants\n")
  cat("  - From Airtable:", sum(combined_nih$data_source == "Airtable"), "\n")
  cat("  - From TAGGS:", sum(combined_nih$data_source == "TAGGS"), "\n")
  
  # Load and process NSF grants
  nsf_grants <- read_csv(nsf_grants_path, show_col_types = FALSE) |>
    mutate(
      combined_text = paste(project_title, abstract, sep = " "),
      combined_text = tolower(combined_text),
      termination_date = ymd(termination_date),
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
  cat("NIH grants with valid dates:", sum(!is.na(combined_nih$termination_date)), "out of", nrow(combined_nih), "\n")
  cat("NSF grants with valid dates:", sum(!is.na(nsf_grants$termination_date)), "out of", nrow(nsf_grants), "\n")
  
  # Apply trigger term detection
  combined_nih <- detect_trigger_terms(combined_nih, trigger_terms)
  nsf_grants <- detect_trigger_terms(nsf_grants, trigger_terms)
  
  # Add administration based on date
  combined_nih <- add_administration(combined_nih)
  nsf_grants <- add_administration(nsf_grants)
  
  # Combine datasets
  all_grants <- bind_rows(combined_nih, nsf_grants)
  
  # Count occurrences of each trigger term
  nih_term_counts <- count_term_occurrences(combined_nih)
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
    nih_grants = combined_nih,
    nsf_grants = nsf_grants,
    trigger_terms = trigger_terms,
    all_grants = all_grants,
    nih_term_counts = nih_term_counts,
    nsf_term_counts = nsf_term_counts,
    all_term_counts = all_term_counts
  ))
}
