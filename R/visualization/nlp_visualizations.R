# nlp_visualizations.R - Simplified version that focuses on summary data
# This version avoids the complex joining operations that were causing errors

library(tidyverse)
library(ggplot2)
library(RColorBrewer)

# 1. Create a heatmap of term classifications
create_term_classification_heatmap <- function(nlp_summary, top_n = 20) {
  # Ensure we have the required data
  if (!"classification" %in% names(nlp_summary)) {
    stop("NLP summary data must contain a 'classification' column")
  }
  
  # Prepare data for heatmap
  heatmap_data <- nlp_summary %>%
    group_by(term) %>%
    mutate(total_count = sum(count)) %>%
    ungroup() %>%
    # Calculate percentage within each term
    mutate(percentage = count / total_count * 100) %>%
    # Select top terms by total count
    arrange(desc(total_count)) %>%
    filter(term %in% unique(term)[1:min(top_n, length(unique(term)))]) %>%
    # Pivot to wide format for processing
    pivot_wider(
      id_cols = term,
      names_from = classification,
      values_from = percentage,
      values_fill = 0
    ) %>%
    # Convert back to long for ggplot
    pivot_longer(
      cols = c("SCIENTIFIC", "POLITICAL", "AMBIGUOUS"),
      names_to = "classification",
      values_to = "percentage"
    )
  
  # Create the heatmap
  heatmap_plot <- ggplot(heatmap_data, aes(x = classification, y = reorder(term, percentage), fill = percentage)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "#5EECC2", high = "#FF7400", name = "Percentage") +
    geom_text(aes(label = round(percentage, 1)), color = "white", size = 3, fontface = "bold") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14)
    ) +
    labs(
      title = "Term Classification Distribution",
      subtitle = paste("Top", top_n, "most frequent terms"),
      x = "Classification",
      y = "Term"
    )
  
  return(heatmap_plot)
}

# 2. Create a faceted bar chart showing top terms by classification
create_top_terms_by_classification <- function(nlp_summary, top_n = 5) {
  # Get top terms for each classification
  top_terms <- nlp_summary %>%
    group_by(classification) %>%
    arrange(desc(count)) %>%
    slice_head(n = top_n) %>%
    ungroup()
  
  # Create the faceted bar chart
  terms_plot <- ggplot(top_terms, aes(x = reorder(term, count), y = count, fill = classification)) +
    geom_col() +
    facet_wrap(~classification, scales = "free_y") +
    coord_flip() +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      legend.position = "none",
      strip.text = element_text(size = 12, face = "bold")
    ) +
    labs(
      title = paste("Top", top_n, "Terms by Classification"),
      subtitle = "Most frequent terms in each classification category",
      x = NULL,
      y = "Count"
    )
  
  return(terms_plot)
}

# 3. Create a scientific classification rate chart (false positive rates)
create_scientific_rate_chart <- function(nlp_summary, top_n = 20) {
  # Calculate scientific classification rates
  sci_rates <- nlp_summary %>%
    group_by(term) %>%
    mutate(
      total_count = sum(count),
      rate = ifelse(classification == "SCIENTIFIC", 100 * count / total_count, 0)
    ) %>%
    filter(classification == "SCIENTIFIC") %>%
    arrange(desc(rate)) %>%
    head(top_n) %>%
    ungroup()
  
  # Create bar chart
  sci_plot <- ggplot(sci_rates, aes(x = reorder(term, rate), y = rate)) +
    geom_col(fill = "#5EECC2") +
    coord_flip() +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14)
    ) +
    labs(
      title = paste("Top", top_n, "Terms with Highest Scientific Classification Rates"),
      subtitle = "Percentage of contexts classified as scientific/technical",
      x = NULL,
      y = "Scientific Classification Rate (%)"
    )
  
  return(sci_plot)
}

# 4. Create a political classification rate chart
create_political_rate_chart <- function(nlp_summary, top_n = 20) {
  # Calculate political classification rates
  pol_rates <- nlp_summary %>%
    group_by(term) %>%
    mutate(
      total_count = sum(count),
      rate = ifelse(classification == "POLITICAL", 100 * count / total_count, 0)
    ) %>%
    filter(classification == "POLITICAL") %>%
    arrange(desc(rate)) %>%
    head(top_n) %>%
    ungroup()
  
  # Create bar chart
  pol_plot <- ggplot(pol_rates, aes(x = reorder(term, rate), y = rate)) +
    geom_col(fill = "#FF7400") +
    coord_flip() +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14)
    ) +
    labs(
      title = paste("Top", top_n, "Terms with Highest Political Classification Rates"),
      subtitle = "Percentage of contexts classified as political/social",
      x = NULL,
      y = "Political Classification Rate (%)"
    )
  
  return(pol_plot)
}

# 5. Create a stacked bar chart showing overall classification distribution
create_classification_distribution <- function(nlp_summary) {
  # Summarize classifications
  class_summary <- nlp_summary %>%
    group_by(classification) %>%
    summarize(count = sum(count), .groups = "drop") %>%
    mutate(percentage = 100 * count / sum(count))
  
  # Create pie chart
  pie_chart <- ggplot(class_summary, aes(x = "", y = percentage, fill = classification)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(round(percentage), "%")), 
              position = position_stack(vjust = 0.5),
              color = "white", 
              fontface = "bold") +
    scale_fill_brewer(palette = "Set2") +
    theme_void() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      legend.title = element_blank(),
      legend.position = "bottom"
    ) +
    labs(
      title = "Overall Classification Distribution",
      subtitle = "Percentage of term contexts by classification category"
    )
  
  return(pie_chart)
}

# Create comprehensive NLP visualizations using only summary data
create_nlp_visualizations <- function(nlp_results_dir, output_dir) {
  cat("Generating NLP analysis visualizations...\n")
  
  # Create the output directory if it doesn't exist
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Load the NLP results data
  cat("Loading NLP classification summary data...\n")
  
  # Load summary data
  nlp_summary_path <- file.path(nlp_results_dir, "term_classification_summary.csv")
  if (!file.exists(nlp_summary_path)) {
    stop("NLP summary data file not found at: ", nlp_summary_path)
  }
  
  nlp_summary <- read_csv(nlp_summary_path, show_col_types = FALSE)
  cat("Loaded summary data with", nrow(nlp_summary), "records\n")
  
  # List to store all generated plots
  plots_list <- list()
  
  # 1. Generate term classification heatmap
  cat("Creating term classification heatmap...\n")
  heatmap_plot <- create_term_classification_heatmap(nlp_summary, top_n = 20)
  # Save both PNG and PDF
  ggsave(file.path(output_dir, "term_classification_heatmap.png"), heatmap_plot, width = 12, height = 10)
  ggsave(file.path(output_dir, "term_classification_heatmap.pdf"), heatmap_plot, width = 12, height = 10)
  plots_list$heatmap_plot <- heatmap_plot
  
  # 2. Generate top terms by classification
  cat("Creating top terms by classification chart...\n")
  top_terms_plot <- create_top_terms_by_classification(nlp_summary, top_n = 5)
  # Save both PNG and PDF
  ggsave(file.path(output_dir, "top_terms_by_classification.png"), top_terms_plot, width = 12, height = 8)
  ggsave(file.path(output_dir, "top_terms_by_classification.pdf"), top_terms_plot, width = 12, height = 8)
  plots_list$top_terms_plot <- top_terms_plot
  
  # 3. Generate scientific rate chart
  cat("Creating scientific classification rate chart...\n")
  sci_plot <- create_scientific_rate_chart(nlp_summary, top_n = 20)
  # Save both PNG and PDF
  ggsave(file.path(output_dir, "scientific_classification_rates.png"), sci_plot, width = 12, height = 10)
  ggsave(file.path(output_dir, "scientific_classification_rates.pdf"), sci_plot, width = 12, height = 10)
  plots_list$sci_plot <- sci_plot
  
  # 4. Generate political rate chart
  cat("Creating political classification rate chart...\n")
  pol_plot <- create_political_rate_chart(nlp_summary, top_n = 20)
  # Save both PNG and PDF
  ggsave(file.path(output_dir, "political_classification_rates.png"), pol_plot, width = 12, height = 10)
  ggsave(file.path(output_dir, "political_classification_rates.pdf"), pol_plot, width = 12, height = 10)
  plots_list$pol_plot <- pol_plot
  
  # 5. Generate overall distribution pie chart
  cat("Creating overall classification distribution chart...\n")
  pie_chart <- create_classification_distribution(nlp_summary)
  # Save both PNG and PDF
  ggsave(file.path(output_dir, "classification_distribution.png"), pie_chart, width = 8, height = 8)
  ggsave(file.path(output_dir, "classification_distribution.pdf"), pie_chart, width = 8, height = 8)
  plots_list$pie_chart <- pie_chart
  
  cat("NLP visualizations complete!\n")
  
  # Return a list of all generated plots
  return(plots_list)
}

# Save plot function with improved file naming (supporting both PNG and PDF)
save_nlp_plot <- function(filename, plot, width = 10, height = 6, output_dir) {
  # Create directory if it doesn't exist
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Save as PNG
  png_path <- file.path(output_dir, paste0(filename, ".png"))
  ggsave(png_path, plot, width = width, height = height)
  
  # Save as PDF
  pdf_path <- file.path(output_dir, paste0(filename, ".pdf"))
  ggsave(pdf_path, plot, width = width, height = height)
  
  # Print a message
  cat("Saved", filename, "as PNG and PDF\n")
  
  # Return the paths for reference
  return(list(png = png_path, pdf = pdf_path))
}