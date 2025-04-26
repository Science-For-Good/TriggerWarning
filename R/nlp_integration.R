# nlp_integration.R - Integration with NLP analysis for term context classification
source("R/utils.R")
source("R/data_processing.R")

# Create output directories
setup_nlp_analysis <- function(base_output_dir = "images/R_analysis/nlp_analysis") {
  dir.create(base_output_dir, recursive = TRUE, showWarnings = FALSE)
  log_dir <- file.path(base_output_dir, "logs")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Set up logging
  log_file <- file.path(log_dir, paste0("analysis_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
  
  return(list(
    output_dir = base_output_dir,
    log_dir = log_dir,
    log_file = log_file
  ))
}

# Log message function
log_message <- function(msg, log_file) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message <- paste0("[", timestamp, "] ", msg)
  cat(message, "\n")
  cat(message, "\n", file = log_file, append = TRUE)
}

# Integrated function to analyze term contexts with NLP
analyze_term_contexts_nlp <- function(all_grants, terms_to_analyze, 
                                      api_function, output_dir, log_file,
                                      max_contexts_per_term = 50) {
  # Create progress tracking
  progress_file <- file.path(output_dir, "analysis_progress.csv")
  
  # Initialize progress tracking
  if (!file.exists(progress_file)) {
    progress_data <- tibble(
      term = terms_to_analyze,
      status = "pending",
      start_time = NA,
      end_time = NA,
      contexts_found = NA,
      contexts_analyzed = NA
    )
    write_csv(progress_data, progress_file)
    log_message("Created new progress tracking file", log_file)
  } else {
    progress_data <- read_csv(progress_file, show_col_types = FALSE)
    log_message(paste("Loaded progress file with", nrow(progress_data), "terms"), log_file)
  }
  
  # Function to extract contexts for a term
  extract_contexts_for_term <- function(term) {
    log_message(paste("Extracting contexts for term:", term), log_file)
    
    # Get grants containing this term
    term_grants <- all_grants[sapply(all_grants$detected_terms, function(x) term %in% x), ]
    contexts_found <- nrow(term_grants)
    
    if (contexts_found == 0) {
      log_message(paste("No contexts found for term:", term), log_file)
      return(list(
        contexts = tibble(),
        count = 0
      ))
    }
    
    # Sample if needed
    if (contexts_found > max_contexts_per_term) {
      log_message(paste("Sampling", max_contexts_per_term, "contexts from", 
                        contexts_found, "for term:", term), log_file)
      term_grants <- term_grants |>
        sample_n(max_contexts_per_term)
    }
    
    # Extract contexts
    contexts <- term_grants |>
      mutate(
        id = row_number(),
        term = term,
        context = map_chr(combined_text, function(text) {
          tryCatch({
            extract_context(text, term)
          }, error = function(e) {
            NA_character_
          })
        }),
        title = if("Award Title" %in% names(.)) `Award Title` else project_title,
        agency = agency
      ) |>
      select(id, term, context, title, agency) |>
      filter(!is.na(context))
    
    return(list(
      contexts = contexts,
      count = nrow(contexts)
    ))
  }
  
  # Process each term
  results_list <- list()
  
  for (term in terms_to_analyze) {
    # Check if already processed
    term_status <- progress_data |>
      filter(term == !!term)
    
    if (nrow(term_status) > 0 && term_status$status == "completed") {
      log_message(paste("Term already processed, loading results:", term), log_file)
      
      # Load existing results
      term_file <- file.path(output_dir, paste0(term, "_results.csv"))
      if (file.exists(term_file)) {
        term_results <- read_csv(term_file, show_col_types = FALSE)
        results_list[[term]] <- term_results
      } else {
        log_message(paste("Results file not found for completed term:", term), log_file)
      }
      
      next
    }
    
    # Update status to in-progress
    progress_data <- progress_data |>
      mutate(
        status = ifelse(term == !!term, "in-progress", status),
        start_time = ifelse(term == !!term, as.character(Sys.time()), start_time)
      )
    write_csv(progress_data, progress_file)
    
    # Extract contexts
    contexts_result <- extract_contexts_for_term(term)
    contexts <- contexts_result$contexts
    contexts_found <- contexts_result$count
    
    if (contexts_found == 0) {
      # Update progress with zero counts
      progress_data <- progress_data |>
        mutate(
          status = ifelse(term == !!term, "completed", status),
          end_time = ifelse(term == !!term, as.character(Sys.time()), end_time),
          contexts_found = ifelse(term == !!term, 0, contexts_found),
          contexts_analyzed = ifelse(term == !!term, 0, contexts_analyzed)
        )
      write_csv(progress_data, progress_file)
      next
    }
    
    # Process contexts with NLP
    log_message(paste("Processing", nrow(contexts), "contexts for term:", term), log_file)
    
    # This is where the API call function would be used
    # Replace with your actual API call implementation
    classifications <- vector("character", nrow(contexts))
    explanations <- vector("character", nrow(contexts))
    
    for (i in 1:nrow(contexts)) {
      # Call API function
      result <- api_function(
        term = contexts$term[i],
        context = contexts$context[i],
        grant_title = contexts$title[i]
      )
      
      classifications[i] <- result$category
      explanations[i] <- result$explanation
      
      # Log progress periodically
      if (i %% 10 == 0 || i == nrow(contexts)) {
        log_message(paste("  Processed", i, "of", nrow(contexts), "contexts for term:", term), log_file)
      }
    }
    
    # Combine results
    term_results <- contexts |>
      mutate(
        classification = classifications,
        explanation = explanations
      )
    
    # Save term results
    term_file <- file.path(output_dir, paste0(term, "_results.csv"))
    write_csv(term_results, term_file)
    log_message(paste("Saved results for term:", term), log_file)
    
    # Update progress
    progress_data <- progress_data |>
      mutate(
        status = ifelse(term == !!term, "completed", status),
        end_time = ifelse(term == !!term, as.character(Sys.time()), end_time),
        contexts_found = ifelse(term == !!term, contexts_found, contexts_found),
        contexts_analyzed = ifelse(term == !!term, nrow(term_results), contexts_analyzed)
      )
    write_csv(progress_data, progress_file)
    
    # Store results
    results_list[[term]] <- term_results
  }
  
  # Create summary
  create_nlp_summary(results_list, output_dir, log_file)
  
  return(results_list)
}

# Create summary visualizations from NLP analysis
create_nlp_summary <- function(results_list, output_dir, log_file) {
  log_message("Creating summary visualizations from NLP analysis", log_file)
  
  # Combine all results
  all_results <- bind_rows(results_list)
  
  if (nrow(all_results) == 0) {
    log_message("No results to summarize", log_file)
    return(NULL)
  }
  
  # Save combined results
  write_csv(all_results, file.path(output_dir, "all_term_classifications.csv"))
  
  # Calculate overall percentages
  overall <- all_results |>
    group_by(classification) |>
    summarize(count = n()) |>
    mutate(percentage = 100 * count / sum(count))
  
  # Create summary by term
  term_summary <- all_results |>
    group_by(term, classification) |>
    summarize(count = n(), .groups = "drop") |>
    group_by(term) |>
    mutate(percentage = 100 * count / sum(count)) |>
    ungroup()
  
  # Save term summary
  write_csv(term_summary, file.path(output_dir, "term_classification_summary.csv"))
  
  # Calculate false positive rates (SCIENTIFIC classification)
  fp_rates <- term_summary |>
    filter(classification == "SCIENTIFIC") |>
    select(term, false_positive_rate = percentage) |>
    arrange(desc(false_positive_rate))
  
  # Save false positive rates
  write_csv(fp_rates, file.path(output_dir, "false_positive_rates.csv"))
  
  # Create visualizations
  
  # Overall distribution
  p1 <- ggplot(overall, aes(x = "", y = percentage, fill = classification)) +
    geom_col(width = 1) +
    coord_polar("y", start = 0) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Overall Distribution of Term Classifications",
      subtitle = paste("Analysis date:", format(Sys.time(), "%Y-%m-%d")),
      fill = "Classification"
    ) +
    theme_void() +
    theme(legend.position = "right") +
    geom_text(aes(label = paste0(round(percentage), "%")),
              position = position_stack(vjust = 0.5))
  
  ggsave(file.path(output_dir, "overall_distribution.png"), p1, width = 8, height = 8)
  
  # Top terms with highest false positive rates
  top_fp_terms <- head(fp_rates, 15)
  
  p2 <- ggplot(top_fp_terms, aes(x = reorder(term, false_positive_rate), y = false_positive_rate)) +
    geom_col(fill = "#5EECC2") +
    coord_flip() +
    labs(
      title = "Top 15 Terms with Highest False Positive Rates",
      subtitle = "Percentage of contexts classified as scientific/technical",
      x = "Trigger Term",
      y = "False Positive Rate (%)"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  ggsave(file.path(output_dir, "top_false_positives.png"), p2, width = 10, height = 8)
  
  # Agency comparison
  agency_comparison <- all_results |>
    group_by(agency, classification) |>
    summarize(count = n(), .groups = "drop") |>
    group_by(agency) |>
    mutate(percentage = 100 * count / sum(count)) |>
    ungroup()
  
  p3 <- ggplot(agency_comparison, aes(x = agency, y = percentage, fill = classification)) +
    geom_col() +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Classification Distribution by Agency",
      subtitle = "Comparing NIH and NSF grant contexts",
      x = "Agency",
      y = "Percentage",
      fill = "Classification"
    ) +
    theme_minimal() +
    geom_text(aes(label = paste0(round(percentage), "%")),
              position = position_stack(vjust = 0.5))
  
  ggsave(file.path(output_dir, "agency_comparison.png"), p3, width = 8, height = 6)
  
  log_message("Saved summary visualizations", log_file)
  
  return(list(p1 = p1, p2 = p2, p3 = p3))
}