# nlp_integration.R - Integration with NLP analysis for term context classification
# Optimized for large-scale processing (6000+ contexts per term)

library(dplyr)
library(tibble)
library(readr)
library(ggplot2)
library(future)
library(furrr)
library(progressr)

# Context extraction function - unchanged
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

# Enhanced log message function with timestamps and categories
log_message <- function(msg, log_file, category = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message <- paste0("[", timestamp, "][", category, "] ", msg)
  cat(message, "\n")
  cat(message, "\n", file = log_file, append = TRUE)
}

# Function to estimate remaining time
estimate_remaining_time <- function(start_time, processed, total) {
  if (processed == 0) return("calculating...")
  
  elapsed <- difftime(Sys.time(), start_time, units = "mins")
  est_total <- (elapsed / processed) * total
  est_remaining <- est_total - elapsed
  
  hours <- floor(as.numeric(est_remaining) / 60)
  mins <- round(as.numeric(est_remaining) %% 60)
  
  if (hours > 0) {
    return(sprintf("%d hours, %d minutes", hours, mins))
  } else {
    return(sprintf("%d minutes", mins))
  }
}

# Main analysis function - optimized for large-scale processing
analyze_term_contexts_nlp <- function(all_grants, terms_to_analyze, 
                                      api_function, output_dir, log_file,
                                      max_contexts_per_term = 6312, 
                                      batch_size = 2,  # Reduced from 10 to 2
                                      cooldown_period = 300,  # Increased from 60 to 300
                                      parallel_workers = 4) {  # New parameter for parallel processing
  
  # Create output directory if it doesn't exist
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Initialize log file if it doesn't exist
  if (!file.exists(log_file)) {
    cat("", file = log_file)
  }
  
  log_message("Starting analysis with optimized parameters:", log_file)
  log_message(sprintf("- max_contexts_per_term: %d", max_contexts_per_term), log_file)
  log_message(sprintf("- batch_size: %d", batch_size), log_file)
  log_message(sprintf("- cooldown_period: %d seconds", cooldown_period), log_file)
  log_message(sprintf("- parallel_workers: %d", parallel_workers), log_file)
  
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
  
  # Modified function to extract contexts for a specific term - optimized for large datasets
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
      # Set seed for reproducibility
      set.seed(42)
      term_grants <- dplyr::sample_n(term_grants, max_contexts_per_term)
    }
    
    # Memory optimization: Force garbage collection before large operation
    gc()
    
    # Extract contexts - with improved error handling
    log_message("Extracting context windows for selected grants", log_file)
    contexts <- term_grants |>
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
    contexts <- contexts |>
      dplyr::select(id, term, context, title, agency) |>
      dplyr::filter(!is.na(context))
    
    log_message(paste("Extracted", nrow(contexts), "valid contexts for term:", term), log_file)
    return(list(
      contexts = contexts,
      count = nrow(contexts)
    ))
  }
  
  # Process in batches to better handle rate limits
  pending_terms <- progress_data |>
    dplyr::filter(status == "pending") |>
    dplyr::pull(term)
  
  log_message(paste(length(pending_terms), "terms pending analysis"), log_file)
  
  # Check if there are any pending terms to process
  if (length(pending_terms) == 0) {
    log_message("No pending terms to analyze. Processing complete!", log_file)
    
    # Load results to return
    results_list <- list()
    completed_terms <- progress_data |>
      dplyr::filter(status == "completed") |>
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
  
  # Process in batches with optimized parameters
  batch_count <- ceiling(length(pending_terms) / batch_size)
  results_list <- list()
  
  # Set up parallel processing if requested
  if (parallel_workers > 1) {
    log_message(sprintf("Setting up parallel processing with %d workers", parallel_workers), log_file)
    plan(multisession, workers = parallel_workers)
    # Enable progress reporting in parallel mode
    handlers(global = TRUE)
    handlers("progress")
  }
  
  for (batch_idx in 1:batch_count) {
    batch_start <- (batch_idx - 1) * batch_size + 1
    batch_end <- min(batch_idx * batch_size, length(pending_terms))
    current_batch <- pending_terms[batch_start:batch_end]
    
    log_message(paste("Processing batch", batch_idx, "of", batch_count, 
                      ":", paste(current_batch, collapse = ", ")), log_file)
    
    # Process each term in the batch
    for (term in current_batch) {
      # Update status to in-progress
      progress_data <- progress_data |>
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
        progress_data <- progress_data |>
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
      
      # Track processing time for estimation
      term_start_time <- Sys.time()
      
      # Determine processing approach based on context size
      if (parallel_workers > 1 && nrow(contexts) > 100) {
        # --- Parallel processing approach for large context sets ---
        log_message("Using parallel processing for large context set", log_file)
        
        # Create batch sizes for parallel processing with rate limit consideration
        # Process in mini-batches to respect API rate limits while using parallelism
        mini_batch_size <- 50
        mini_batches <- split(seq_len(nrow(contexts)), ceiling(seq_len(nrow(contexts))/mini_batch_size))
        
        # Process each mini-batch
        all_classifications <- list()
        all_explanations <- list()
        
        for (batch_num in seq_along(mini_batches)) {
          batch_indices <- mini_batches[[batch_num]]
          batch_contexts <- contexts[batch_indices, ]
          
          # Progress reporting
          if (batch_num %% 5 == 0 || batch_num == length(mini_batches)) {
            processed_so_far <- (batch_num - 1) * mini_batch_size
            progress_pct <- round(100 * processed_so_far / nrow(contexts), 1)
            remaining <- estimate_remaining_time(term_start_time, processed_so_far, nrow(contexts))
            log_message(sprintf("  Batch %d/%d (%.1f%%). Est. remaining: %s", 
                                batch_num, length(mini_batches), progress_pct, remaining), log_file)
          }
          
          # Process this mini-batch in parallel
          with_progress({
            p <- progressor(steps = length(batch_indices))
            
            batch_results <- future_map(seq_len(nrow(batch_contexts)), function(i) {
              result <- api_function(
                term = batch_contexts$term[i],
                context = batch_contexts$context[i],
                grant_title = batch_contexts$title[i],
                log_file = log_file
              )
              
              # In-worker delay to respect rate limits (shorter because of parallel overhead)
              Sys.sleep(runif(1, 0.1, 0.2))
              p()
              return(result)
            }, .options = furrr_options(seed = TRUE))
          })
          
          # Extract classifications and explanations from batch results
          batch_classifications <- sapply(batch_results, function(x) x$category)
          batch_explanations <- sapply(batch_results, function(x) x$explanation)
          
          # Store the results for this batch
          all_classifications[[batch_num]] <- batch_classifications
          all_explanations[[batch_num]] <- batch_explanations
          
          # Save checkpoint after each mini-batch
          if (batch_num %% 5 == 0 || batch_num == length(mini_batches)) {
            # Combine results processed so far
            processed_indices <- unlist(mini_batches[1:batch_num])
            processed_contexts <- contexts[processed_indices, ]
            processed_classifications <- unlist(all_classifications)
            processed_explanations <- unlist(all_explanations)
            
            # Create interim results
            interim_results <- processed_contexts |>
              dplyr::mutate(
                classification = processed_classifications,
                explanation = processed_explanations
              )
            
            # Save checkpoint
            checkpoint_file <- file.path(output_dir, paste0(term, "_checkpoint.csv"))
            write_csv(interim_results, checkpoint_file)
            log_message(sprintf("  Saved checkpoint with %d/%d contexts processed", 
                                length(processed_indices), nrow(contexts)), log_file)
          }
          
          # Cooldown between mini-batches
          if (batch_num < length(mini_batches)) {
            Sys.sleep(runif(1, 1, 3))  # Shorter cooldown between mini-batches
          }
          
          # Periodic garbage collection
          if (batch_num %% 10 == 0) {
            gc()
          }
        }
        
        # Combine all results
        classifications <- unlist(all_classifications)
        explanations <- unlist(all_explanations)
        
      } else {
        # --- Sequential processing approach for smaller context sets ---
        log_message("Using sequential processing", log_file)
        
        # Prepare result vectors
        classifications <- vector("character", nrow(contexts))
        explanations <- vector("character", nrow(contexts))
        
        # Adaptive approach based on context size
        if (nrow(contexts) > 1000) {
          # For very large sets, process in batches
          context_batches <- split(seq_len(nrow(contexts)), ceiling(seq_len(nrow(contexts))/100))
          
          for (batch_num in seq_along(context_batches)) {
            batch_indices <- context_batches[[batch_num]]
            
            # Progress reporting
            if (batch_num %% 2 == 0 || batch_num == length(context_batches)) {
              processed_so_far <- (batch_num - 1) * 100
              progress_pct <- round(100 * processed_so_far / nrow(contexts), 1)
              remaining <- estimate_remaining_time(term_start_time, processed_so_far, nrow(contexts))
              log_message(sprintf("  Batch %d/%d (%.1f%%). Est. remaining: %s", 
                                  batch_num, length(context_batches), progress_pct, remaining), log_file)
            }
            
            # Process contexts in this batch
            for (i in batch_indices) {
              result <- api_function(
                term = contexts$term[i],
                context = contexts$context[i],
                grant_title = contexts$title[i],
                log_file = log_file
              )
              
              classifications[i] <- result$category
              explanations[i] <- result$explanation
            }
            
            # Add a delay between batches
            if (batch_num < length(context_batches)) {
              Sys.sleep(runif(1, 2, 4))
            }
            
            # Save checkpoint periodically
            if (batch_num %% 5 == 0 || batch_num == length(context_batches)) {
              # Get the indices of all processed contexts so far
              processed_indices <- unlist(context_batches[1:batch_num])
              
              # Create interim results
              interim_results <- contexts[processed_indices, ] |>
                dplyr::mutate(
                  classification = classifications[processed_indices],
                  explanation = explanations[processed_indices]
                )
              
              # Save checkpoint
              checkpoint_file <- file.path(output_dir, paste0(term, "_checkpoint.csv"))
              write_csv(interim_results, checkpoint_file)
              log_message(sprintf("  Saved checkpoint with %d/%d contexts processed", 
                                  length(processed_indices), nrow(contexts)), log_file)
            }
            
            # Periodic garbage collection
            if (batch_num %% 10 == 0) {
              gc()
            }
          }
        } else {
          # Original approach for smaller context sets
          for (i in 1:nrow(contexts)) {
            # Call API function
            result <- api_function(
              term = contexts$term[i],
              context = contexts$context[i],
              grant_title = contexts$title[i],
              log_file = log_file
            )
            
            classifications[i] <- result$category
            explanations[i] <- result$explanation
            
            # Log progress periodically
            if (i %% 10 == 0 || i == nrow(contexts)) {
              progress_pct <- round(100 * i / nrow(contexts), 1)
              remaining <- estimate_remaining_time(term_start_time, i, nrow(contexts))
              log_message(sprintf("  Processed %d/%d (%.1f%%). Est. remaining: %s", 
                                  i, nrow(contexts), progress_pct, remaining), log_file)
            }
            
            # Add a small delay between requests to avoid rate limiting
            if (i < nrow(contexts)) {
              Sys.sleep(runif(1, 0.2, 0.5))
            }
            
            # Save checkpoint periodically
            if (i %% 100 == 0 || i == nrow(contexts)) {
              interim_results <- contexts[1:i, ] |>
                dplyr::mutate(
                  classification = classifications[1:i],
                  explanation = explanations[1:i]
                )
              
              checkpoint_file <- file.path(output_dir, paste0(term, "_checkpoint.csv"))
              write_csv(interim_results, checkpoint_file)
            }
            
            # Periodic garbage collection
            if (i %% 500 == 0) {
              gc()
            }
          }
        }
      }
      
      # Count classification results
      off_target_count <- sum(classifications == "off-target")
      on_target_count <- sum(classifications == "on-target")
      ambiguous_count <- sum(classifications == "ambiguous")
      
      # Combine results
      term_results <- contexts |>
        dplyr::mutate(
          classification = classifications,
          explanation = explanations
        )
      
      # Save term results
      term_file <- file.path(output_dir, paste0(term, "_results.csv"))
      write_csv(term_results, term_file)
      log_message(paste("Saved results for term:", term), log_file)
      
      # Update progress
      progress_data <- progress_data |>
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
      
      # Force garbage collection after completing a term
      gc()
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
  
  # Reset to sequential processing
  if (parallel_workers > 1) {
    plan(sequential)
  }
  
  # Create summary
  create_nlp_summary(results_list, output_dir, log_file)
  
  return(results_list)
}

# Create summary visualizations from NLP analysis - unchanged
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
  overall <- all_results |>
    dplyr::group_by(classification) |>
    dplyr::summarize(count = n()) |>
    dplyr::mutate(percentage = 100 * count / sum(count))
  
  # Create summary by term
  term_summary <- all_results |>
    dplyr::group_by(term, classification) |>
    dplyr::summarize(count = n(), .groups = "drop") |>
    dplyr::group_by(term) |>
    dplyr::mutate(percentage = 100 * count / sum(count)) |>
    dplyr::ungroup()
  
  # Save term summary
  write_csv(term_summary, file.path(output_dir, "term_classification_summary.csv"))
  
  # Calculate false positive rates (off-target classification)
  fp_rates <- term_summary |>
    dplyr::filter(classification == "off-target") |>
    dplyr::select(term, false_positive_rate = percentage) |>
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
  agency_comparison <- all_results |>
    dplyr::group_by(agency, classification) |>
    dplyr::summarize(count = n(), .groups = "drop") |>
    dplyr::group_by(agency) |>
    dplyr::mutate(percentage = 100 * count / sum(count)) |>
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
  term_breakdown <- term_summary |>
    dplyr::filter(term %in% head(fp_rates$term, 10)) |>
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