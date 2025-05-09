# Fixed grant_type_visualizations.R - Functions for analyzing grant types

# Extract NIH activity code (grant type)
extract_activity_code <- function(nih_grants) {
  # Add debug output
  cat("Starting extract_activity_code function\n")
  cat("Dataset has", nrow(nih_grants), "rows\n")
  cat("Available columns:", paste(names(nih_grants), collapse=", "), "\n")
  
  # Check if we have the activity_code column directly
  if ("activity_code" %in% names(nih_grants)) {
    cat("activity_code column already exists\n")
    
    # If we already have activity_code but not grant_type, add grant_type
    if (!"grant_type" %in% names(nih_grants)) {
      cat("Adding grant_type based on existing activity_code\n")
      nih_grants <- nih_grants |>
        dplyr::mutate(
          # Extract mechanism type (first letter)
          grant_type = dplyr::case_when(
            stringr::str_detect(activity_code, "^R") ~ "Research Project Grants (R)",
            stringr::str_detect(activity_code, "^K") ~ "Career Development Awards (K)",
            stringr::str_detect(activity_code, "^F") ~ "Fellowship (F)",
            stringr::str_detect(activity_code, "^P") ~ "Program Project/Center Grants (P)",
            stringr::str_detect(activity_code, "^T") ~ "Training Grants (T)",
            stringr::str_detect(activity_code, "^D") ~ "Development & Diversity Grants (D)",
            stringr::str_detect(activity_code, "^U") ~ "Cooperative Agreements (U)",
            stringr::str_detect(activity_code, "^S") ~ "Supplement (S)",
            stringr::str_detect(activity_code, "^OT") ~ "Other Transactions (OT)",
            stringr::str_detect(activity_code, "^G") ~ "Resource Grants (G)",
            stringr::str_detect(activity_code, "^C") ~ "Construction Grants (C)",
            TRUE ~ "Other"
          )
        )
    }
    
    cat("Returning data with existing activity_code\n")
    return(nih_grants)
  }
  
  # Now for datasets without activity_code column, extract from available fields
  cat("No activity_code column, extracting from award numbers\n")
  
  # Create an empty activity_code column to start with
  nih_grants$activity_code <- NA_character_
  
  # Extract from FAIN if available
  if ("FAIN" %in% names(nih_grants)) {
    cat("Extracting from FAIN column\n")
    activity_codes <- stringr::str_extract(nih_grants$FAIN, "^[A-Z]\\d+")
    nih_grants$activity_code <- ifelse(is.na(nih_grants$activity_code), activity_codes, nih_grants$activity_code)
    cat("Extracted", sum(!is.na(activity_codes), na.rm=TRUE), "activity codes from FAIN\n")
  }
  
  # Now assign grant types based on the extracted activity codes
  cat("Assigning grant types based on extracted activity codes\n")
  nih_grants <- nih_grants |>
    dplyr::mutate(
      # Categorize based on first letter of activity code
      grant_type = dplyr::case_when(
        stringr::str_detect(activity_code, "^R") ~ "Research Project Grants (R)",
        stringr::str_detect(activity_code, "^K") ~ "Career Development Awards (K)",
        stringr::str_detect(activity_code, "^F") ~ "Fellowship (F)",
        stringr::str_detect(activity_code, "^P") ~ "Program Project/Center Grants (P)",
        stringr::str_detect(activity_code, "^T") ~ "Training Grants (T)",
        stringr::str_detect(activity_code, "^D") ~ "Development & Diversity Grants (D)",
        stringr::str_detect(activity_code, "^U") ~ "Cooperative Agreements (U)",
        stringr::str_detect(activity_code, "^S") ~ "Supplement (S)",
        stringr::str_detect(activity_code, "^OT") ~ "Other Transactions (OT)",
        stringr::str_detect(activity_code, "^G") ~ "Resource Grants (G)",
        stringr::str_detect(activity_code, "^C") ~ "Construction Grants (C)",
        TRUE ~ "Other"
      )
    )
  
  # Print summary of grant types
  if (!is.null(nih_grants$grant_type)) {
    cat("Grant type distribution:\n")
    grant_counts <- table(nih_grants$grant_type, useNA = "ifany")
    print(grant_counts)
  }
  
  return(nih_grants)
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

modify_grant_type_plot <- function(grant_type_plot) {
  grant_type_plot + 
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
}

# Function to update any ggplot to use angled x-axis labels
# This is a utility function you can use for any plot
add_angled_x_labels <- function(plot) {
  plot + 
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
}