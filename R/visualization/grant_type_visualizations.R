# Fixed grant_type_visualizations.R - Functions for analyzing grant types

# Extract NIH activity code (grant type)
extract_activity_code <- function(nih_grants) {
  # Check if we have the activity_code column directly
  if ("activity_code" %in% names(nih_grants)) {
    # If we already have activity_code but not grant_type, add grant_type
    if (!"grant_type" %in% names(nih_grants)) {
      nih_grants <- nih_grants |>
        mutate(
          # Extract mechanism type (first letter)
          grant_type = case_when(
            str_detect(activity_code, "^R") ~ "Research Project Grants (R)",
            str_detect(activity_code, "^K") ~ "Career Development Awards (K)",
            str_detect(activity_code, "^F") ~ "Fellowship (F)",
            str_detect(activity_code, "^P") ~ "Program Project/Center Grants (P)",
            str_detect(activity_code, "^T") ~ "Training Grants (T)",
            str_detect(activity_code, "^U") ~ "Cooperative Agreements (U)",
            str_detect(activity_code, "^S") ~ "Supplement (S)",
            TRUE ~ "Other"
          )
        )
    }
    return(nih_grants)
  }
  
  # Otherwise extract from award number
  nih_grants <- nih_grants |>
    mutate(
      # Extract activity code from award number
      activity_code = case_when(
        # From full_award_number if it exists
        "full_award_number" %in% names(nih_grants) ~ 
          str_extract(full_award_number, "(?<=\\d)[A-Z]\\d+(?:[A-Z]\\d*)?"),
        # From Award Number if it exists
        "Award Number" %in% names(nih_grants) ~ 
          str_extract(`Award Number`, "(?<=\\d)[A-Z]\\d+(?:[A-Z]\\d*)?"),
        # From FAIN if it exists
        "FAIN" %in% names(nih_grants) ~ 
          str_extract(FAIN, "(?<=\\d)[A-Z]\\d+(?:[A-Z]\\d*)?"),
        TRUE ~ NA_character_
      ),
      # Extract mechanism type (first letter)
      grant_type = case_when(
        str_detect(activity_code, "^R") ~ "Research Project Grants (R)",
        str_detect(activity_code, "^K") ~ "Career Development Awards (K)",
        str_detect(activity_code, "^F") ~ "Fellowship (F)",
        str_detect(activity_code, "^P") ~ "Program Project/Center Grants (P)",
        str_detect(activity_code, "^T") ~ "Training Grants (T)",
        str_detect(activity_code, "^U") ~ "Cooperative Agreements (U)",
        str_detect(activity_code, "^S") ~ "Supplement (S)",
        TRUE ~ "Other"
      )
    )
  
  return(nih_grants)
}

# Create visualization of grant types
create_grant_type_plot <- function(nih_grants) {
  # Ensure we have activity codes - assign the result back to nih_grants
  nih_grants <- extract_activity_code(nih_grants)
  
  # Add a debugging statement to check if grant_type exists
  cat("Columns in nih_grants:", paste(names(nih_grants), collapse=", "), "\n")
  cat("Number of grants before filtering:", nrow(nih_grants), "\n")
  
  # Count by grant type - changed to use dplyr::filter explicitly
  grant_type_counts <- nih_grants |>
    dplyr::filter(!is.na(grant_type)) |>  # Explicit package reference
    group_by(grant_type) |>
    summarize(
      terminations = n(),
      with_terms = sum(has_trigger_term, na.rm = TRUE),
      percent_with_terms = round(100 * mean(has_trigger_term, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    arrange(desc(terminations))
  
  cat("Number of grant types found:", nrow(grant_type_counts), "\n")
  
  # Create visualization
  grant_type_plot <- ggplot(grant_type_counts |> 
                              mutate(grant_type = reorder(grant_type, terminations)), 
                            aes(x = grant_type, y = terminations, fill = percent_with_terms)) +
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
      title = "NIH Terminated Grants by Grant Type",
      subtitle = "Color indicates percentage with trigger terms",
      x = "Grant Type",
      y = "Number of Terminations"
    )
  
  return(grant_type_plot)
}

# Create visualization of specific activity codes (more detailed)
create_activity_code_plot <- function(nih_grants, top_n = 15) {
  # Ensure we have activity codes - assign the result back to nih_grants
  nih_grants <- extract_activity_code(nih_grants)
  
  # Count by activity code
  activity_counts <- nih_grants |>
    dplyr::filter(!is.na(activity_code)) |>  # Explicit package reference
    group_by(activity_code) |>
    summarize(
      terminations = n(),
      with_terms = sum(has_trigger_term, na.rm = TRUE),
      percent_with_terms = round(100 * mean(has_trigger_term, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    arrange(desc(terminations)) |>
    head(top_n)
  
  # Create visualization
  activity_plot <- ggplot(activity_counts |> 
                            mutate(activity_code = reorder(activity_code, terminations)), 
                          aes(x = activity_code, y = terminations, fill = percent_with_terms)) +
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
      title = paste("Top", top_n, "NIH Activity Codes in Terminated Grants"),
      subtitle = "Color indicates percentage with trigger terms",
      x = "Activity Code",
      y = "Number of Terminations"
    )
  
  return(activity_plot)
}

# Identify diversity supplements and F31 grants - FIXED VERSION
create_diversity_supplement_analysis <- function(nih_grants) {
  # Ensure we have activity codes
  nih_grants <- extract_activity_code(nih_grants)
  
  # Check for column existence OUTSIDE of mutate
  has_foa_column <- "foa" %in% names(nih_grants)
  
  # Identify F31 grants and diversity supplements
  diversity_grants <- nih_grants |>
    mutate(
      is_f31 = ifelse(!is.na(activity_code), str_detect(activity_code, "^F31"), FALSE),
      is_diversity_f31 = case_when(
        # First case: If we have FOA column and it's an F31
        has_foa_column & is_f31 ~ 
          ifelse(!is.na(foa), str_detect(foa, "PA-23-271|PA-21-52"), FALSE),
        # Second case: If it's an F31 but no FOA column, check text
        is_f31 ~ ifelse(!is.na(combined_text), 
                        str_detect(combined_text, "(?i)diversity|underrepresented|minority"), 
                        FALSE),
        # Default case
        TRUE ~ FALSE
      ),
      is_supplement = ifelse(!is.na(activity_code), str_detect(activity_code, "^S"), FALSE),
      is_diversity_supplement = case_when(
        is_supplement ~ ifelse(!is.na(combined_text), 
                               str_detect(combined_text, "(?i)diversity|underrepresented|minority"), 
                               FALSE),
        TRUE ~ FALSE
      ),
      diversity_category = case_when(
        is_diversity_f31 ~ "F31 Diversity Fellowship",
        is_f31 & !is_diversity_f31 ~ "Other F31 Fellowship",
        is_diversity_supplement ~ "Diversity Supplement",
        is_supplement & !is_diversity_supplement ~ "Other Supplement",
        TRUE ~ "Other Grant Type"
      )
    )
  
  # Count by diversity category
  diversity_counts <- diversity_grants |>
    group_by(diversity_category) |>
    summarize(
      terminations = n(),
      with_terms = sum(has_trigger_term, na.rm = TRUE),
      percent_with_terms = round(100 * mean(has_trigger_term, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # Filter to categories of interest (if they exist)
  categories_of_interest <- c("F31 Diversity Fellowship", "Diversity Supplement", 
                              "Other F31 Fellowship", "Other Supplement")
  matching_categories <- intersect(diversity_counts$diversity_category, categories_of_interest)
  
  if (length(matching_categories) > 0) {
    diversity_counts <- diversity_counts |>
      filter(diversity_category %in% matching_categories)
  } else {
    # If no matching categories, create a placeholder for the plot
    diversity_counts <- data.frame(
      diversity_category = "No matching fellowships/supplements found",
      terminations = 0,
      with_terms = 0,
      percent_with_terms = 0
    )
  }
  
  # Create visualization
  diversity_plot <- ggplot(diversity_counts, 
                           aes(x = diversity_category, y = terminations, fill = percent_with_terms)) +
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
      title = "Diversity Fellowship and Supplement Analysis",
      subtitle = "Comparing diversity-focused grants with other similar mechanisms",
      x = "Grant Category",
      y = "Number of Terminations"
    )
  
  # Return both the plot and the data with diversity categories
  return(list(
    plot = diversity_plot,
    data = diversity_grants
  ))
}