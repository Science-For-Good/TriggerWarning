# congressional_district_visualizations.R - Congressional district visualization functions

# Create congressional district visualizations
create_congressional_district_plot <- function(nsf_grants) {
  # Calculate district counts
  district_counts <- nsf_grants |>
    filter(!is.na(org_district)) |>
    group_by(district = org_district) |>
    summarize(
      terminations = n(),
      with_terms = sum(has_trigger_term, na.rm = TRUE),
      percent_with_terms = round(100 * mean(has_trigger_term, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    arrange(desc(terminations)) |>
    head(15) # Top 15 districts
  
  # Debug
  cat("Congressional district counts:\n")
  print(district_counts)
  
  # Create visualization
  district_plot <- ggplot(district_counts |> 
                            mutate(district = reorder(district, terminations)), 
                          aes(x = district, y = terminations, fill = percent_with_terms)) +
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
      title = "Top Congressional Districts with Terminated NSF Grants",
      subtitle = "Color indicates percentage with trigger terms",
      x = "Congressional District",
      y = "Number of Terminations"
    )
  
  return(district_plot)
}

# Create term distribution by congressional district
create_district_term_distribution_plot <- function(nsf_grants, all_term_counts) {
  # Get top districts and top terms
  top_districts <- nsf_grants |>
    filter(!is.na(org_district)) |>
    count(org_district, sort = TRUE) |>
    head(6) |>
    pull(org_district)
  
  top_terms <- head(all_term_counts, 5)$term
  
  # Create term counts matrix
  district_term_matrix <- matrix(0, nrow = length(top_districts), ncol = length(top_terms))
  rownames(district_term_matrix) <- top_districts
  colnames(district_term_matrix) <- top_terms
  
  # Fill the matrix
  for (i in seq_along(top_districts)) {
    district <- top_districts[i]
    # Get grants for this district
    district_grants <- nsf_grants |> filter(org_district == district)
    
    for (j in seq_along(top_terms)) {
      term <- top_terms[j]
      # Count occurrences of this term in this district
      count <- sum(sapply(district_grants$detected_terms, function(terms) term %in% terms))
      district_term_matrix[i, j] <- count
    }
  }
  
  # Convert to data frame for plotting
  district_term_distribution <- district_term_matrix |>
    as.data.frame() |>
    rownames_to_column("district") |>
    pivot_longer(cols = -district, names_to = "term", values_to = "count")
  
  # Debug
  cat("District term distribution data (first few rows):\n")
  print(head(district_term_distribution))
  
  # Create heatmap
  district_term_heatmap <- ggplot(district_term_distribution, 
                                  aes(x = district, y = term, fill = count)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "#5EECC2", name = "Frequency") +
    geom_text(aes(label = count),
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
      title = "Term Distribution by Congressional District",
      subtitle = "Shows distribution of top trigger terms across top districts",
      x = "Congressional District",
      y = "Term"
    )
  
  return(district_term_heatmap)
}