# agency_visualizations.R - Agency-specific visualization functions

# Create visualization of NIH institutes
create_nih_institute_plot <- function(nih_grants) {
  # Count by institute
  institute_counts <- nih_grants %>%
    filter(!is.na(institute)) %>%
    group_by(institute) %>%
    summarize(
      terminations = n(),
      with_terms = sum(has_trigger_term, na.rm = TRUE),
      percent_with_terms = round(100 * mean(has_trigger_term, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    arrange(desc(terminations)) %>%
    # Limit to top institutes to avoid crowding
    head(10)
  
  # Create visualization with angled x-axis labels
  institute_plot <- ggplot(institute_counts %>% 
                             mutate(institute = reorder(institute, terminations)), 
                           aes(x = institute, y = terminations, fill = percent_with_terms)) +
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
      # Add these settings for angled x-axis labels
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1) 
    ) +
    labs(
      title = "NIH Terminations by Institute",
      subtitle = "Color indicates percentage with trigger terms",
      x = "Institute",
      y = "Number of Terminations"
    )
  
  return(institute_plot)
}

# Create term distribution by NIH institute
create_nih_term_distribution_plot <- function(nih_grants, all_term_counts) {
  # Get top institutes and top terms
  top_institutes <- nih_grants |>
    filter(!is.na(institute)) |>
    count(institute, sort = TRUE) |>
    head(6) |>
    pull(institute)
  
  top_terms <- head(all_term_counts, 5)$term
  
  # Create term counts matrix
  institute_term_matrix <- matrix(0, nrow = length(top_institutes), ncol = length(top_terms))
  rownames(institute_term_matrix) <- top_institutes
  colnames(institute_term_matrix) <- top_terms
  
  # Fill the matrix
  for (i in seq_along(top_institutes)) {
    inst <- top_institutes[i]
    # Get grants for this institute
    inst_grants <- nih_grants |> filter(institute == inst)
    
    for (j in seq_along(top_terms)) {
      term <- top_terms[j]
      # Count occurrences of this term in this institute
      count <- sum(sapply(inst_grants$detected_terms, function(terms) term %in% terms))
      institute_term_matrix[i, j] <- count
    }
  }
  
  # Convert to data frame for plotting
  nih_term_distribution <- institute_term_matrix |>
    as.data.frame() |>
    rownames_to_column("institute") |>
    pivot_longer(cols = -institute, names_to = "term", values_to = "count")
  
  # Debug
  cat("NIH term distribution data (first few rows):\n")
  print(head(nih_term_distribution))
  
  # Create heatmap
  nih_term_heatmap <- ggplot(nih_term_distribution, 
                             aes(x = institute, y = term, fill = count)) +
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
      title = "Term Distribution by NIH Institute",
      subtitle = "Shows distribution of top trigger terms across top NIH institutes",
      x = "Institute",
      y = "Term"
    )
  
  return(nih_term_heatmap)
}

# Create NSF directorate visualizations
create_nsf_directorate_plot <- function(nsf_grants) {
  # Count by directorate
  directorate_counts <- nsf_grants %>%
    filter(!is.na(directorate)) %>%
    group_by(directorate) %>%
    summarize(
      terminations = n(),
      with_terms = sum(has_trigger_term, na.rm = TRUE),
      percent_with_terms = round(100 * mean(has_trigger_term, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    arrange(desc(terminations))
  
  # Create visualization with angled labels
  directorate_plot <- ggplot(directorate_counts %>% 
                               mutate(directorate = reorder(directorate, terminations)), 
                             aes(x = directorate, y = terminations, fill = percent_with_terms)) +
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
      # Add these settings for angled x-axis labels
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    ) +
    labs(
      title = "NSF Terminations by Directorate",
      subtitle = "Color indicates percentage with trigger terms",
      x = "Directorate",
      y = "Number of Terminations"
    )
  
  return(directorate_plot)
}

# Create term distribution by NSF directorate
create_nsf_term_distribution_plot <- function(nsf_grants, all_term_counts) {
  # Get top directorates and top terms
  top_directorates <- nsf_grants |>
    filter(!is.na(directorate_abbrev)) |>
    count(directorate_abbrev, sort = TRUE) |>
    head(6) |>
    pull(directorate_abbrev)
  
  top_terms <- head(all_term_counts, 5)$term
  
  # Create term counts matrix
  directorate_term_matrix <- matrix(0, nrow = length(top_directorates), ncol = length(top_terms))
  rownames(directorate_term_matrix) <- top_directorates
  colnames(directorate_term_matrix) <- top_terms
  
  # Fill the matrix
  for (i in seq_along(top_directorates)) {
    dir <- top_directorates[i]
    # Get grants for this directorate
    dir_grants <- nsf_grants |> filter(directorate_abbrev == dir)
    
    for (j in seq_along(top_terms)) {
      term <- top_terms[j]
      # Count occurrences of this term in this directorate
      count <- sum(sapply(dir_grants$detected_terms, function(terms) term %in% terms))
      directorate_term_matrix[i, j] <- count
    }
  }
  
  # Convert to data frame for plotting
  nsf_term_distribution <- directorate_term_matrix |>
    as.data.frame() |>
    rownames_to_column("directorate") |>
    pivot_longer(cols = -directorate, names_to = "term", values_to = "count")
  
  # Debug
  cat("NSF term distribution data (first few rows):\n")
  print(head(nsf_term_distribution))
  
  # Create heatmap
  nsf_term_heatmap <- ggplot(nsf_term_distribution, 
                             aes(x = directorate, y = term, fill = count)) +
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
      title = "Term Distribution by NSF Directorate",
      subtitle = "Shows clear pattern of term usage varying by scientific field",
      x = "Directorate",
      y = "Term"
    )
  
  return(nsf_term_heatmap)
}

# Create NSF directorates with trigger terms visualization
create_nsf_directorates_terms_plot <- function(nsf_grants) {
  # Calculate data
  nsf_dir_terms <- nsf_grants |>
    filter(!is.na(directorate_abbrev)) |>
    group_by(directorate = directorate_abbrev, has_trigger = has_trigger_term) |>
    summarize(count = n(), .groups = "drop")
  
  # Debug
  cat("NSF directorate trigger terms data:\n")
  print(nsf_dir_terms)
  
  # Create visualization
  nsf_terms_plot <- ggplot(nsf_dir_terms |>
                             mutate(has_trigger = factor(has_trigger, levels = c(TRUE, FALSE),
                                                         labels = c("With Trigger Terms", "Without Trigger Terms"))), 
                           aes(x = directorate, y = count, fill = has_trigger)) +
    geom_col() +
    scale_fill_manual(values = c(
      "With Trigger Terms" = "#FF7400",
      "Without Trigger Terms" = "#5EECC2"
    )) +
    geom_text(aes(label = count),
              position = position_stack(vjust = 0.5),
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
      title = "NSF Directorates: Grants With Trigger Terms",
      subtitle = "Showing proportion of grants with trigger terms by directorate",
      x = "Directorate",
      y = "Number of Grants",
      fill = ""
    )
  
  return(nsf_terms_plot)
}