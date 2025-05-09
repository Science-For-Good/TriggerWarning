# institution_visualizations.R - Functions for institution-based analyses

# Create institution type distribution visualization
create_institution_type_plot <- function(grants_df) {
  # Extract institution types - for NIH grants
  if ("org_type" %in% names(grants_df)) {
    inst_counts <- grants_df |>
      filter(!is.na(org_type)) |>
      group_by(institution_type = org_type) |>
      summarize(
        terminations = n(),
        with_terms = sum(has_trigger_term, na.rm = TRUE),
        percent_with_terms = round(100 * mean(has_trigger_term, na.rm = TRUE)),
        .groups = "drop"
      ) |>
      arrange(desc(terminations))
  } else {
    # For NSF or combined grants, create basic classification based on org_name
    inst_counts <- grants_df |>
      mutate(
        institution_type = case_when(
          str_detect(org_name, "(?i)university|college") ~ "Academic Institution",
          str_detect(org_name, "(?i)hospital|medical|health") ~ "Medical Institution",
          str_detect(org_name, "(?i)research|institute|laboratory") ~ "Research Institute",
          str_detect(org_name, "(?i)foundation|non-profit|nonprofit") ~ "Non-Profit",
          str_detect(org_name, "(?i)company|inc\\.|corporation|llc") ~ "Industry",
          TRUE ~ "Other"
        )
      ) |>
      group_by(institution_type) |>
      summarize(
        terminations = n(),
        with_terms = sum(has_trigger_term, na.rm = TRUE),
        percent_with_terms = round(100 * mean(has_trigger_term, na.rm = TRUE)),
        .groups = "drop"
      ) |>
      arrange(desc(terminations))
  }
  
  # Create visualization
  inst_type_plot <- ggplot(inst_counts |> 
                             mutate(institution_type = reorder(institution_type, terminations)), 
                           aes(x = institution_type, y = terminations, fill = percent_with_terms)) +
    geom_col() +
    scale_fill_gradient(low = "#5EECC2", high = "#FF7400", 
                        name = "% with\ntrigger terms") +
    geom_text(aes(label = terminations),
              vjust = -0.5,
              color = "#13151F",
              fontface = "bold") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
      title = "Grant Terminations by Institution Type",
      subtitle = "Color indicates percentage with trigger terms",
      x = "Institution Type",
      y = "Number of Terminations"
    )
  
  return(inst_type_plot)
}

# Analyze academic institution classifications using CSV data
create_academic_classification_plot <- function(grants_df, 
                                                classifications_file = "data/ace-institutional-classifications.csv") {
  # Load required packages
  if (!requireNamespace("readr", quietly = TRUE))
    stop("Package 'readr' is required. Install with install.packages('readr')")
  
  # Read the CSV file containing university classifications
  university_data <- readr::read_csv(classifications_file, show_col_types = FALSE)
  
  # Standardize the classifications as requested: map to R1, R2, and Other
  university_data <- university_data |>
    dplyr::mutate(
      academic_tier = dplyr::case_when(
        `Research Activity Designation` == "Research 1: Very High Research Spending and Doctorate Production" ~ "R1 University",
        `Research Activity Designation` == "Research 2: High Research Spending and Doctorate Production" ~ "R2 University",
        `Research Activity Designation` == "Research Colleges and Universities" ~ "Other Academic Institution",
        TRUE ~ "Other Academic Institution"
      )
    )
  
  # Identify the institution name column in the classification data
  # This assumes there's a column with "institution" or "name" in it
  institution_col <- names(university_data)[grep("institution|name|college|university", 
                                                 names(university_data), ignore.case = TRUE)][1]
  
  if (is.na(institution_col))
    stop("Could not identify institution name column in classification data")
  
  # Create dataframes for each type of institution
  r1_institutions <- university_data |> 
    dplyr::filter(academic_tier == "R1 University") |> 
    dplyr::pull(.data[[institution_col]])
  
  r2_institutions <- university_data |> 
    dplyr::filter(academic_tier == "R2 University") |> 
    dplyr::pull(.data[[institution_col]])
  
  # Get institution name column from grants data
  inst_col <- ifelse("org_name" %in% names(grants_df), "org_name", 
                     ifelse("Recipient Name" %in% names(grants_df), "Recipient Name", 
                            NA_character_))
  
  if (is.na(inst_col)) {
    stop("No institution name column found in the grants data")
  }
  
  # Classify grants by institution type
  academic_grants <- grants_df |>
    dplyr::filter(stringr::str_detect(.data[[inst_col]], "(?i)university|college")) |>
    dplyr::mutate(
      academic_tier = dplyr::case_when(
        stringr::str_detect(.data[[inst_col]], paste(r1_institutions, collapse = "|")) ~ "R1 University",
        stringr::str_detect(.data[[inst_col]], paste(r2_institutions, collapse = "|")) ~ "R2 University",
        TRUE ~ "Other Academic Institution"
      )
    ) |>
    dplyr::group_by(academic_tier) |>
    dplyr::summarize(
      terminations = dplyr::n(),
      with_terms = sum(has_trigger_term, na.rm = TRUE),
      percent_with_terms = round(100 * mean(has_trigger_term, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # Create visualization
  academic_plot <- ggplot2::ggplot(academic_grants, 
                                   ggplot2::aes(x = academic_tier, y = terminations, fill = percent_with_terms)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_gradient(low = "#5EECC2", high = "#FF7400", 
                                 name = "% with\ntrigger terms") +
    ggplot2::geom_text(ggplot2::aes(label = terminations),
                       vjust = -0.5,
                       color = "#13151F",
                       fontface = "bold") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 12, face = "bold")
    ) +
    ggplot2::labs(
      title = "Terminated Grants by Academic Institution Classification",
      subtitle = "Comparing R1, R2, and other academic institutions",
      x = "Academic Classification",
      y = "Number of Terminations"
    )
  
  return(academic_plot)
}