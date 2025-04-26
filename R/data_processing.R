# data_processing.R - Data loading and processing functions
source("R/utils.R")

# Load and process data
load_data <- function(trigger_terms_path, nih_grants_path, nsf_grants_path) {
  # Load trigger terms
  trigger_terms <- read_csv(trigger_terms_path, show_col_types = FALSE) |>
    rename(term = `Trigger Terms`) |>
    mutate(term = tolower(term),
           term = trimws(term))
  
  # Load and process NIH grants
  nih_grants <- read_csv(nih_grants_path, show_col_types = FALSE) |>
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
      institute = str_extract(`Funding Institutes`, "^[^,]+")
    )
  
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
      state = org_state
    )
  
  # Print date diagnostics
  cat("NIH grants with valid dates:", sum(!is.na(nih_grants$termination_date)), "out of", nrow(nih_grants), "\n")
  cat("NSF grants with valid dates:", sum(!is.na(nsf_grants$termination_date)), "out of", nrow(nsf_grants), "\n")
  
  # Apply trigger term detection
  nih_grants <- detect_trigger_terms(nih_grants, trigger_terms)
  nsf_grants <- detect_trigger_terms(nsf_grants, trigger_terms)
  
  # Add administration based on date
  nih_grants <- add_administration(nih_grants)
  nsf_grants <- add_administration(nsf_grants)
  
  # Combine datasets
  all_grants <- bind_rows(nih_grants, nsf_grants)
  
  # Count occurrences of each trigger term
  nih_term_counts <- count_term_occurrences(nih_grants)
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
    nih_grants = nih_grants,
    nsf_grants = nsf_grants,
    trigger_terms = trigger_terms,
    all_grants = all_grants,
    nih_term_counts = nih_term_counts,
    nsf_term_counts = nsf_term_counts,
    all_term_counts = all_term_counts
  ))
}