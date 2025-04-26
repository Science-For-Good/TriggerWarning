# Term-related visualization functions

# Create top trigger terms visualization
create_top_terms_plot <- function(all_term_counts) {
  # Get top terms
  top_terms <- head(all_term_counts, 15)
  
  # Debug
  cat("Top terms data:\n")
  print(top_terms)
  
  # Create visualization
  top_terms_plot <- ggplot(top_terms, aes(x = reorder(term, frequency), y = frequency)) +
    geom_col(fill = "#5EECC2") +
    geom_text(aes(label = frequency),
              vjust = -0.5,
              color = "#13151F",
              fontface = "bold",
              size = 4) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
    ) +
    labs(
      title = "Most Frequently Flagged Trigger Terms",
      subtitle = "Top terms found in terminated grants",
      x = "Term",
      y = "Frequency"
    )
  
  return(top_terms_plot)
}

# Extract context for terms
extract_context <- function(text, term, window_size = 150) {
  # Handle NA case first
  if (is.na(text)) {
    return(NA_character_)
  }
  
  # Handle case where term isn't found
  if (!str_detect(text, fixed(term))) {
    return(NA_character_)
  }
  
  # Find first occurrence of the term
  term_position <- str_locate(text, fixed(term))[1, ]
  
  # Extract context around the term
  start_pos <- max(1, term_position[1] - window_size)
  end_pos <- min(nchar(text), term_position[2] + window_size)
  context <- str_sub(text, start_pos, end_pos)
  
  return(context)
}

# Extract contexts for a specific term
extract_term_contexts <- function(grants_df, term, window_size = 150) {
  # Find grants containing the term
  grants_with_term <- grants_df |>
    filter(str_detect(combined_text, fixed(term)))
  
  cat(paste("Found", nrow(grants_with_term), "grants containing term:", term, "\n"))
  
  if (nrow(grants_with_term) == 0) {
    return(tibble())
  }
  
  # Process one row at a time
  result <- grants_with_term |>
    mutate(
      id = row_number(),
      term = term,
      context = map_chr(combined_text, function(text) {
        tryCatch({
          extract_context(text, term, window_size)
        }, error = function(e) {
          NA_character_
        })
      }),
      title = if("Award Title" %in% names(grants_df)) `Award Title` else project_title
    ) |>
    select(id, term, context, title) |>
    filter(!is.na(context))
  
  return(result)
}

# Analyze term context
analyze_term_context <- function(all_grants, term) {
  # Get grants containing this term
  term_grants <- all_grants[sapply(all_grants$detected_terms, function(x) term %in% x), ]
  
  # Count total occurrences
  total <- nrow(term_grants)
  
  if (total == 0) {
    return(data.frame(
      category = c("Scientific/Technical", "Political/Social", "Ambiguous"),
      count = c(0, 0, 0)
    ))
  }
  
  # Categorize contexts based on text analysis
  scientific_count <- sum(
    grepl("research", term_grants$combined_text, ignore.case = TRUE) |
      grepl("study", term_grants$combined_text, ignore.case = TRUE) |
      grepl("experiment", term_grants$combined_text, ignore.case = TRUE) |
      grepl("laboratory", term_grants$combined_text, ignore.case = TRUE)
  )
  
  political_count <- sum(
    grepl("policy", term_grants$combined_text, ignore.case = TRUE) |
      grepl("advocate", term_grants$combined_text, ignore.case = TRUE) |
      grepl("social", term_grants$combined_text, ignore.case = TRUE) |
      grepl("community", term_grants$combined_text, ignore.case = TRUE)
  )
  
  # Ensure total matches sum of categories
  ambiguous_count <- total - scientific_count - political_count
  
  # Adjust if there are overlapping counts
  if (ambiguous_count < 0) {
    if (scientific_count >= political_count) {
      scientific_count <- scientific_count + ambiguous_count
    } else {
      political_count <- political_count + ambiguous_count
    }
    ambiguous_count <- 0
  }
  
  # Create data frame for analysis
  return(data.frame(
    category = c("Scientific/Technical", "Political/Social", "Ambiguous"),
    count = c(scientific_count, political_count, ambiguous_count)
  ))
}

# Create term context analysis visualizations
create_term_context_donuts <- function(all_grants) {
  # Analyze context for key terms
  trans_context_data <- analyze_term_context(all_grants, "trans")
  climate_context_data <- analyze_term_context(all_grants, "climate")
  
  # Analyze all trigger terms
  all_term_grants <- all_grants[all_grants$has_trigger_term, ]
  
  scientific_count <- sum(
    grepl("research", all_term_grants$combined_text, ignore.case = TRUE) |
    grepl("study", all_term_grants$combined_text, ignore.case = TRUE) |
    grepl("experiment", all_term_grants$combined_text, ignore.case = TRUE) |
    grepl("laboratory", all_term_grants$combined_text, ignore.case = TRUE)
  )
  
  political_count <- sum(
    grepl("policy", all_term_grants$combined_text, ignore.case = TRUE) |
    grepl("advocate", all_term_grants$combined_text, ignore.case = TRUE) |
    grepl("social", all_term_grants$combined_text, ignore.case = TRUE) |
    grepl("community", all_term_grants$combined_text, ignore.case = TRUE)
  )
  
  # Calculate ambiguous count
  total <- nrow(all_term_grants)
  ambiguous_count <- total - scientific_count - political_count
  
  # Adjust if there are overlapping counts
  if (ambiguous_count < 0) {
    if (scientific_count >= political_count) {
      scientific_count <- scientific_count + ambiguous_count
    } else {
      political_count <- political_count + ambiguous_count
    }
    ambiguous_count <- 0
  }
  
  # Create overall context data
  overall_context_data <- data.frame(
    category = c("Scientific Context (False Positive)", 
                "Political/Social Context (True Hit)", 
                "Ambiguous"),
    count = c(scientific_count, political_count, ambiguous_count)
  )
  
  # Create donut charts
  trans_donut <- create_donut_chart(trans_context_data, 
                                  "Context Analysis for 'trans' in Actual Grant Data")
  climate_donut <- create_donut_chart(climate_context_data, 
                                    "Context Analysis for 'climate' in Actual Grant Data")
  overall_donut <- create_donut_chart(overall_context_data, 
                                    "Analysis of Trigger Term Contexts in All Grants")
  
  return(list(
    trans_donut = trans_donut,
    climate_donut = climate_donut,
    overall_donut = overall_donut,
    trans_data = trans_context_data,
    climate_data = climate_context_data,
    overall_data = overall_context_data
  ))
}