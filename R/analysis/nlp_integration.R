# nlp_integration.R - Integration with NLP analysis for term context classification

extract_context <- function(text, term, window_size = 300) {
  # Handle NA case first
  if (is.na(text)) {
    return(NA_character_)
  }
  
  # Use gregexpr which is designed for multiple matches
  matches <- gregexpr(pattern = term, text = text, fixed = TRUE)
  
  # If no matches found, return NA
  if (matches[[1]][1] == -1) {
    return(NA_character_)
  }
  
  # Take only the first match
  start_match <- matches[[1]][1]
  match_length <- attr(matches[[1]], "match.length")[1]
  
  # Calculate window positions
  start_pos <- max(1, start_match - window_size)
  end_pos <- min(nchar(text), start_match + match_length - 1 + window_size)
  
  # Extract context
  context <- substr(text, start_pos, end_pos)
  
  return(context)
}

# Log message function
log_message <- function(msg, log_file, category = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message <- paste0("[", timestamp, "][", category, "] ", msg)
  cat(message, "\n")
  cat(message, "\n", file = log_file, append = TRUE)
}

# Integrated function to analyze term contexts with NLP
analyze_term_contexts_nlp <- function(all_grants, terms_to_analyze, 
                                      api_function, output_dir, log_file,
                                      max_contexts_per_term = 50, 
                                      batch_size = 10,
                                      cooldown_period = 60) {
  # Create progress tracking
  progress_file <- file.path(output_dir, "analysis_progress.csv")
  
  # Initialize progress tracking
  if (!file.exists(progress_file)) {
    log_message("Creating new progress tracking file", log_file)
    progress_data <- tibble::tibble(
      term = terms_to_analyze,
      status = "pending",
      start_time = NA_character_,
      end_time = NA_character_,
      contexts_found = NA_integer_,
      contexts_analyzed = NA_integer_,
      off_target_count = NA_integer_,
      on_target_count = NA_integer_,
      ambiguous_count = NA_integer_
    )
    write_csv(progress_data, progress_file)
  } else {
    log_message("Loading existing progress file", log_file)
    progress_data <- readr::read_csv(progress_file, show_col_types = FALSE)
    log_message(paste("Loaded progress file with", nrow(progress_data), "terms"), log_file)
  }
  
  # Function to extract contexts for a specific term
  extract_contexts_for_term <- function(term) {
    log_message(paste("Extracting contexts for term:", term), log_file)
    
    # Get grants containing this term
    term_grants <- all_grants[sapply(all_grants$detected_terms, function(x) term %in% x), ]
    contexts_found <- nrow(term_grants)
    
    if (contexts_found == 0) {
      log_message(paste("No contexts found for term:", term), log_file, "WARNING")
      return(list(
        contexts = tibble::tibble(),
        count = 0
      ))
    }
    
    # Sample if needed
    if (contexts_found > max_contexts_per_term) {
      log_message(paste("Sampling", max_contexts_per_term, "contexts from", 
                        contexts_found, "for term:", term), log_file)
      term_grants <- dplyr::sample_n(term_grants, max_contexts_per_term)
    }
    
    # Extract contexts - FIXED APPROACH
    contexts <- term_grants %>%
      dplyr::mutate(
        id = dplyr::row_number(),
        term = term,
        # Extract context for each grant
        context = sapply(combined_text, function(text) {
          tryCatch({
            extract_context(text, term)
          }, error = function(e) {
            log_message(paste("Error extracting context:", e$message), log_file, "ERROR")
            NA_character_
          })
        })
      )
    
    # Handle title assignment separately to avoid vector length issues
    if ("project_title" %in% names(contexts)) {
      contexts$title <- contexts$project_title
    } else if ("Award Title" %in% names(contexts)) {
      contexts$title <- contexts[["Award Title"]]
    } else {
      contexts$title <- rep("Unknown Title", nrow(contexts))
    }
    
    # Complete the processing
    contexts <- contexts %>%
      dplyr::select(id, term, context, title, agency) %>%
      dplyr::filter(!is.na(context))
    
    log_message(paste("Extracted", nrow(contexts), "valid contexts for term:", term), log_file)
    return(list(
      contexts = contexts,
      count = nrow(contexts)
    ))
  }
  
  # Process in batches to better handle rate limits
  pending_terms <- progress_data %>%
    dplyr::filter(status == "pending") %>%
    dplyr::pull(term)
  
  log_message(paste(length(pending_terms), "terms pending analysis"), log_file)
  
  # Check if there are any pending terms to process
  if (length(pending_terms) == 0) {
    log_message("No pending terms to analyze. Processing complete!", log_file)
    
    # Load results to return
    results_list <- list()
    completed_terms <- progress_data %>%
      dplyr::filter(status == "completed") %>%
      dplyr::pull(term)
    
    for (term in completed_terms) {
      term_file <- file.path(output_dir, paste0(term, "_results.csv"))
      if (file.exists(term_file)) {
        results_list[[term]] <- readr::read_csv(term_file, show_col_types = FALSE)
      }
    }
    
    # Create summary if not already created
    summary_file <- file.path(output_dir, "term_classification_summary.csv")
    if (!file.exists(summary_file)) {
      log_message("Creating summary from existing results", log_file)
      create_nlp_summary(results_list, output_dir, log_file)
    }
    
    return(results_list)
  }
  
  # Process in batches
  batch_count <- ceiling(length(pending_terms) / batch_size)
  results_list <- list()
  
  for (batch_idx in 1:batch_count) {
    batch_start <- (batch_idx - 1) * batch_size + 1
    batch_end <- min(batch_idx * batch_size, length(pending_terms))
    current_batch <- pending_terms[batch_start:batch_end]
    
    log_message(paste("Processing batch", batch_idx, "of", batch_count, 
                      ":", paste(current_batch, collapse = ", ")), log_file)
    
    # Process each term in the batch
    for (term in current_batch) {
      # Update status to in-progress
      progress_data <- progress_data %>%
        dplyr::mutate(
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
        progress_data <- progress_data %>%
          dplyr::mutate(
            status = ifelse(term == !!term, "completed", status),
            end_time = ifelse(term == !!term, as.character(Sys.time()), end_time),
            contexts_found = ifelse(term == !!term, 0, contexts_found),
            contexts_analyzed = ifelse(term == !!term, 0, contexts_analyzed),
            off_target_count = ifelse(term == !!term, 0, off_target_count),
            on_target_count = ifelse(term == !!term, 0, on_target_count),
            ambiguous_count = ifelse(term == !!term, 0, ambiguous_count)
          )
        write_csv(progress_data, progress_file)
        next
      }
      
      # Process contexts with NLP
      log_message(paste("Processing", nrow(contexts), "contexts for term:", term), log_file)
      
      # Prepare result vectors
      classifications <- vector("character", nrow(contexts))
      explanations <- vector("character", nrow(contexts))
      
      for (i in 1:nrow(contexts)) {
        # Call API function with enhanced logging
        result <- api_function(
          term = contexts$term[i],
          context = contexts$context[i],
          grant_title = contexts$title[i],
          log_file = log_file
        )
        
        classifications[i] <- result$category
        explanations[i] <- result$explanation
        
        # Log progress periodically
        if (i %% 5 == 0 || i == nrow(contexts)) {
          log_message(paste("  Processed", i, "of", nrow(contexts), "contexts for term:", term), log_file)
        }
        
        # Add a small delay between requests to avoid rate limiting
        if (i < nrow(contexts)) {
          Sys.sleep(runif(1, 0.2, 0.5))
        }
      }
      
      # Count classification results
      off_target_count <- sum(classifications == "off-target")
      on_target_count <- sum(classifications == "on-target")
      ambiguous_count <- sum(classifications == "ambiguous")
      
      # Combine results
      term_results <- contexts %>%
        dplyr::mutate(
          classification = classifications,
          explanation = explanations
        )
      
      # Save term results
      term_file <- file.path(output_dir, paste0(term, "_results.csv"))
      write_csv(term_results, term_file)
      log_message(paste("Saved results for term:", term), log_file)
      
      # Update progress
      progress_data <- progress_data %>%
        dplyr::mutate(
          status = ifelse(term == !!term, "completed", status),
          end_time = ifelse(term == !!term, as.character(Sys.time()), end_time),
          contexts_found = ifelse(term == !!term, contexts_found, contexts_found),
          contexts_analyzed = ifelse(term == !!term, nrow(term_results), contexts_analyzed),
          off_target_count = ifelse(term == !!term, off_target_count, off_target_count),
          on_target_count = ifelse(term == !!term, on_target_count, on_target_count),
          ambiguous_count = ifelse(term == !!term, ambiguous_count, ambiguous_count)
        )
      write_csv(progress_data, progress_file)
      
      # Store results
      results_list[[term]] <- term_results
    }
    
    # Add cooldown period between batches to avoid rate limits
    if (batch_idx < batch_count) {
      log_message(paste("Batch", batch_idx, "completed. Cooling down for", 
                        cooldown_period, "seconds before next batch."), log_file)
      
      # Create a mini-summary after each batch
      completed <- sum(progress_data$status == "completed")
      in_progress <- sum(progress_data$status == "in-progress")
      pending <- sum(progress_data$status == "pending")
      
      log_message(paste("Progress so far:", completed, "completed,", 
                        in_progress, "in progress,", pending, "pending"), log_file)
      
      Sys.sleep(cooldown_period)
    }
  }
  
  # Create summary
  create_nlp_summary(results_list, output_dir, log_file)
  
  return(results_list)
}

# Create summary visualizations from NLP analysis
create_nlp_summary <- function(results_list, output_dir, log_file) {
  log_message("Creating summary visualizations from NLP analysis", log_file)
  
  # Combine all results
  all_results <- dplyr::bind_rows(results_list)
  
  if (nrow(all_results) == 0) {
    log_message("No results to summarize", log_file)
    return(NULL)
  }
  
  # Create figures directory if it doesn't exist
  figures_dir <- file.path(output_dir, "figures")
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Save combined results
  write_csv(all_results, file.path(output_dir, "all_term_classifications.csv"))
  
  # Calculate overall percentages
  overall <- all_results %>%
    dplyr::group_by(classification) %>%
    dplyr::summarize(count = n()) %>%
    dplyr::mutate(percentage = 100 * count / sum(count))
  
  # Create summary by term
  term_summary <- all_results %>%
    dplyr::group_by(term, classification) %>%
    dplyr::summarize(count = n(), .groups = "drop") %>%
    dplyr::group_by(term) %>%
    dplyr::mutate(percentage = 100 * count / sum(count)) %>%
    dplyr::ungroup()
  
  # Save term summary
  write_csv(term_summary, file.path(output_dir, "term_classification_summary.csv"))
  
  # Calculate false positive rates (off-target classification)
  fp_rates <- term_summary %>%
    dplyr::filter(classification == "off-target") %>%
    dplyr::select(term, false_positive_rate = percentage) %>%
    dplyr::arrange(desc(false_positive_rate))
  
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
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      legend.position = "right"
    ) +
    geom_text(aes(label = paste0(round(percentage), "%")),
              position = position_stack(vjust = 0.5),
              fontface = "bold")
  
  ggsave(file.path(figures_dir, "overall_distribution.png"), p1, width = 8, height = 8)
  
  # Top terms with highest false positive rates
  top_fp_terms <- head(fp_rates, 15)
  
  p2 <- ggplot(top_fp_terms, aes(x = reorder(term, false_positive_rate), y = false_positive_rate)) +
    geom_col(fill = "#5EECC2") +
    coord_flip() +
    labs(
      title = "Top 15 Terms with Highest False Positive Rates",
      subtitle = "Percentage of contexts classified as off-target",
      x = "Trigger Term",
      y = "False Positive Rate (%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  ggsave(file.path(figures_dir, "top_false_positives.png"), p2, width = 10, height = 8)
  
  # Agency comparison
  agency_comparison <- all_results %>%
    dplyr::group_by(agency, classification) %>%
    dplyr::summarize(count = n(), .groups = "drop") %>%
    dplyr::group_by(agency) %>%
    dplyr::mutate(percentage = 100 * count / sum(count)) %>%
    dplyr::ungroup()
  
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
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14)
    ) +
    geom_text(aes(label = paste0(round(percentage), "%")),
              position = position_stack(vjust = 0.5),
              fontface = "bold")
  
  ggsave(file.path(figures_dir, "agency_comparison.png"), p3, width = 8, height = 6)
  
  # Create a term breakdown visualization
  term_breakdown <- term_summary %>%
    dplyr::filter(term %in% head(fp_rates$term, 10)) %>%
    dplyr::mutate(term = factor(term, levels = head(fp_rates$term, 10)))
  
  p4 <- ggplot(term_breakdown, aes(x = term, y = percentage, fill = classification)) +
    geom_col() +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Classification Breakdown for Top 10 Terms",
      subtitle = "Terms ordered by false positive rate",
      x = "Term",
      y = "Percentage",
      fill = "Classification"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  ggsave(file.path(figures_dir, "term_breakdown.png"), p4, width = 12, height = 8)
  
  log_message("Saved summary visualizations", log_file)
  
  return(list(
    overall_distribution = p1,
    top_false_positives = p2,
    agency_comparison = p3,
    term_breakdown = p4
  ))
}