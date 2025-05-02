# regional_visualizations.R - Regional visualization functions

# Create regional analysis visualizations
create_regional_analysis_plots <- function(all_grants) {
  # Define region mappings
  region_mapping <- list(
    Northeast = c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "PA"),
    Midwest = c("OH", "IN", "IL", "MI", "WI", "MN", "IA", "MO", "ND", "SD", "NE", "KS"),
    South = c("DE", "MD", "DC", "VA", "WV", "NC", "SC", "GA", "FL", "KY", "TN", "AL", "MS", "AR", "LA", "OK", "TX"),
    West = c("MT", "ID", "WY", "CO", "NM", "AZ", "UT", "NV", "WA", "OR", "CA", "AK", "HI")
  )
  
  # Add region to grants
  all_grants_with_region <- all_grants |>
    mutate(region = case_when(
      state %in% region_mapping$Northeast ~ "Northeast",
      state %in% region_mapping$Midwest ~ "Midwest",
      state %in% region_mapping$South ~ "South",
      state %in% region_mapping$West ~ "West",
      TRUE ~ NA_character_
    ))
  
  # Count grants by region
  region_counts <- all_grants_with_region |>
    filter(!is.na(region)) |>
    group_by(region) |>
    summarize(
      total_grants = n(),
      grants_with_terms = sum(has_trigger_term, na.rm = TRUE),
      percent_with_terms = round(100 * mean(has_trigger_term, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    arrange(desc(total_grants))
  
  # Debug
  cat("Regional counts:\n")
  print(region_counts)
  
  # Create regional plot
  region_plot <- ggplot(region_counts, 
                        aes(x = reorder(region, -total_grants), y = total_grants, fill = percent_with_terms)) +
    geom_col() +
    scale_fill_gradient(low = "#5EECC2", high = "#FF7400", 
                        name = "% with\ntrigger terms") +
    geom_text(aes(label = paste0(total_grants)),
              position = position_stack(vjust = 0.5),
              color = "#13151F",
              fontface = "bold") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.title = element_text(size = 12, face = "bold")
    ) +
    labs(
      title = "Grant Terminations by Region",
      subtitle = "Color indicates percentage with trigger terms",
      x = "Region",
      y = "Number of Terminations"
    )
  
  # Return the plot and the data with regions
  return(list(
    plot = region_plot,
    data = all_grants_with_region
  ))
}

# Create regional term distribution
create_regional_term_distribution_plot <- function(all_grants_with_region, all_term_counts) {
  # Get top terms
  top_terms <- head(all_term_counts, 5)$term
  
  # Create and fill regional term matrix
  regions <- c("Northeast", "Midwest", "South", "West")
  region_term_matrix <- matrix(0, nrow = length(regions), 
                               ncol = length(top_terms))
  rownames(region_term_matrix) <- regions
  colnames(region_term_matrix) <- top_terms
  
  # Fill the matrix with counts
  for (i in seq_along(regions)) {
    reg <- regions[i]
    # Get grants for this region
    reg_grants <- all_grants_with_region |> filter(region == reg)
    
    for (j in seq_along(top_terms)) {
      term <- top_terms[j]
      # Count occurrences of this term in this region
      count <- sum(sapply(reg_grants$detected_terms, function(terms) term %in% terms))
      region_term_matrix[i, j] <- count
    }
  }
  
  # Convert to data frame for plotting
  region_term_distribution <- region_term_matrix |>
    as.data.frame() |>
    rownames_to_column("region") |>
    pivot_longer(cols = -region, names_to = "term", values_to = "count")
  
  # Debug
  cat("Regional term distribution data:\n")
  print(region_term_distribution)
  
  # Create heatmap
  region_term_heatmap <- ggplot(region_term_distribution, 
                                aes(x = region, y = term, fill = count)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "#5EECC2", name = "Frequency") +
    geom_text(aes(label = count),
              color = "#13151F",
              fontface = "bold") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.title = element_text(size = 12, face = "bold")
    ) +
    labs(
      title = "Term Distribution by Region",
      subtitle = "Shows geographical patterns in trigger term usage",
      x = "Region",
      y = "Term"
    )
  
  return(region_term_heatmap)
}

# Create regional term misclassification analysis from actual data
create_regional_misclassification_plot <- function(all_grants_with_region) {
  # Get regions
  regions <- unique(all_grants_with_region$region[!is.na(all_grants_with_region$region)])
  
  # Get top terms for analysis
  top_terms <- names(sort(table(unlist(all_grants_with_region$detected_terms)), decreasing = TRUE))[1:5]
  
  # Initialize data frame for misclassification rates
  region_misclass_data <- data.frame()
  
  # Calculate regional misclassification rates for each term
  for (region in regions) {
    # Get grants for this region
    region_grants <- all_grants_with_region[all_grants_with_region$region == region, ]
    
    for (term in top_terms) {
      # Get grants with this term in this region
      term_grants <- region_grants[sapply(region_grants$detected_terms, function(x) term %in% x), ]
      
      if (nrow(term_grants) > 0) {
        # Estimate scientific context occurrences
        scientific_context <- sum(
          grepl("research", term_grants$combined_text, ignore.case = TRUE) |
            grepl("study", term_grants$combined_text, ignore.case = TRUE) |
            grepl("experiment", term_grants$combined_text, ignore.case = TRUE) |
            grepl("laboratory", term_grants$combined_text, ignore.case = TRUE)
        )
        
        # Calculate false positive rate
        false_positive_pct <- 100 * scientific_context / nrow(term_grants)
      } else {
        false_positive_pct <- NA  # No data for this term in this region
      }
      
      # Add to data frame
      region_misclass_data <- rbind(region_misclass_data,
                                    data.frame(
                                      region = region,
                                      term = term,
                                      false_positive_pct = false_positive_pct
                                    ))
    }
  }
  
  # Remove rows with NA values
  region_misclass_data <- region_misclass_data[!is.na(region_misclass_data$false_positive_pct), ]
  
  # Create heatmap
  region_misclass_plot <- ggplot(region_misclass_data, 
                                 aes(x = region, y = term, fill = false_positive_pct)) +
    geom_tile() +
    scale_fill_gradient(low = "#F5F5F5", high = "#5EECC2", name = "False Positive %") +
    geom_text(aes(label = paste0(round(false_positive_pct, 0), "%")),
              color = "#13151F") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.title = element_text(size = 12, face = "bold")
    ) +
    labs(
      title = "Regional Patterns in Term Misclassification",
      subtitle = "Based on automated analysis of actual grant text",
      x = "Region",
      y = "Term"
    )
  
  return(list(plot = region_misclass_plot, data = region_misclass_data))
}