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

# Analyze top institutions with most terminated grants
create_top_institutions_plot <- function(grants_df, top_n = 15) {
  # Get institution name column based on available data
  inst_col <- ifelse("org_name" %in% names(grants_df), "org_name", 
                     ifelse("Recipient Name" %in% names(grants_df), "Recipient Name", 
                            NA_character_))
  
  if (is.na(inst_col)) {
    stop("No institution name column found in the data")
  }
  
  # Count institutions
  inst_counts <- grants_df |>
    group_by(institution = .data[[inst_col]]) |>
    summarize(
      terminations = n(),
      with_terms = sum(has_trigger_term, na.rm = TRUE),
      percent_with_terms = round(100 * mean(has_trigger_term, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    arrange(desc(terminations)) |>
    head(top_n)
  
  # Create visualization
  top_inst_plot <- ggplot(inst_counts |> 
                            mutate(institution = reorder(institution, terminations)), 
                          aes(x = institution, y = terminations, fill = percent_with_terms)) +
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
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
    ) +
    labs(
      title = paste("Top", top_n, "Institutions with Most Terminated Grants"),
      subtitle = "Color indicates percentage with trigger terms",
      x = "Institution",
      y = "Number of Terminations"
    )
  
  return(top_inst_plot)
}

# Analyze R1/R2/other classification for academic institutions
create_academic_classification_plot <- function(grants_df) {
  # Define R1/R2 universities (simplified list)
  r1_universities <- c(
    "Harvard", "Stanford", "MIT", "Yale", "Princeton", "Columbia", "Berkeley", 
    "Johns Hopkins", "UCLA", "Michigan", "Washington", "Penn", "Duke", "Cornell",
    "Northwestern", "Chicago", "UCSF", "UNC Chapel Hill", "Wisconsin", "Vanderbilt"
    # Add more as needed
  )
  
  r2_universities <- c(
    "Northeastern", "Drexel", "Temple", "DePaul", "Fordham", "Hofstra", 
    "Clemson", "Baylor", "Oklahoma State", "UNLV", "San Diego State", "Portland State"
    # Add more as needed
  )
  
  # Get institution name column
  inst_col <- ifelse("org_name" %in% names(grants_df), "org_name", 
                     ifelse("Recipient Name" %in% names(grants_df), "Recipient Name", 
                            NA_character_))
  
  if (is.na(inst_col)) {
    stop("No institution name column found in the data")
  }
  
  # Classify institutions
  academic_grants <- grants_df |>
    filter(str_detect(.data[[inst_col]], "(?i)university|college")) |>
    mutate(
      academic_tier = case_when(
        str_detect(.data[[inst_col]], paste(r1_universities, collapse = "|")) ~ "R1 University",
        str_detect(.data[[inst_col]], paste(r2_universities, collapse = "|")) ~ "R2 University",
        TRUE ~ "Other Academic Institution"
      )
    ) |>
    group_by(academic_tier) |>
    summarize(
      terminations = n(),
      with_terms = sum(has_trigger_term, na.rm = TRUE),
      percent_with_terms = round(100 * mean(has_trigger_term, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # Create visualization
  academic_plot <- ggplot(academic_grants, 
                          aes(x = academic_tier, y = terminations, fill = percent_with_terms)) +
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
      axis.title = element_text(size = 12, face = "bold")
    ) +
    labs(
      title = "Terminated Grants by Academic Institution Classification",
      subtitle = "Comparing R1, R2, and other academic institutions",
      x = "Academic Classification",
      y = "Number of Terminations"
    )
  
  return(academic_plot)
}