  # term_analysis.R - Term-related visualization functions with NLP integration
  
  #' Create top trigger terms visualization
  #' 
  #' @param all_term_counts Dataframe with term frequency counts
  #' @return ggplot object with the visualization
  create_top_terms_plot <- function(all_term_counts) {
    # Get top terms
    top_terms <- head(all_term_counts, 15)
    
    # Debug
    cat("Top terms data:\n")
    print(top_terms)
    
    # Create visualization
    top_terms_plot <- ggplot2::ggplot(top_terms, 
                                      ggplot2::aes(x = reorder(term, frequency), y = frequency)) +
      ggplot2::geom_col(fill = "#5EECC2") +
      ggplot2::geom_text(ggplot2::aes(label = frequency),
                         vjust = -0.5,
                         color = "#13151F",
                         fontface = "bold",
                         size = 4) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 14),
        axis.title = ggplot2::element_text(size = 12, face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)
      ) +
      ggplot2::labs(
        title = "Most Frequently Flagged Trigger Terms",
        subtitle = "Top terms found in terminated grants",
        x = "Term",
        y = "Frequency"
      )
    
    return(top_terms_plot)
  }
  
  #' Extract context for terms using the improved NLP extraction method
  #' 
  #' @param text Text to extract context from
  #' @param term Term to find in the text
  #' @param window_size Number of characters to include before and after the term
  #' @return String with the context
  extract_context <- function(text, term, window_size = 300) {
    # Handle NA case first
    if (is.na(text)) {
      return(NA_character_)
    }
    
    # Use gregexpr for robust matching (from the NLP script)
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
  
  #' Extract contexts for a specific term
  #' 
  #' @param grants_df Dataframe with grant data
  #' @param term Term to extract contexts for
  #' @param window_size Number of characters to include before and after the term
  #' @return Dataframe with extracted contexts
  extract_term_contexts <- function(grants_df, term, window_size = 300) {
    # Find grants containing the term
    grants_with_term <- grants_df |>
      dplyr::filter(stringr::str_detect(combined_text, stringr::fixed(term)))
    
    cat(paste("Found", nrow(grants_with_term), "grants containing term:", term, "\n"))
    
    if (nrow(grants_with_term) == 0) {
      return(tibble::tibble())
    }
    
    # Process one row at a time
    result <- grants_with_term |>
      dplyr::mutate(
        id = dplyr::row_number(),
        term = term,
        context = purrr::map_chr(combined_text, function(text) {
          tryCatch({
            extract_context(text, term, window_size)
          }, error = function(e) {
            NA_character_
          })
        }),
        title = if("Award Title" %in% names(grants_df)) `Award Title` else project_title,
        # Include agency if available
        agency = if("agency" %in% names(grants_df)) agency else "Unknown"
      ) |>
      dplyr::select(id, term, context, title, agency) |>
      dplyr::filter(!is.na(context))
    
    return(result)
  }
  
  #' Create a donut chart for term context analysis
  #' 
  #' @param context_data Dataframe with classification counts
  #' @param title Chart title
  #' @return ggplot object with the visualization
  create_donut_chart <- function(context_data, title) {
    # Calculate percentages
    context_data <- context_data |>
      dplyr::mutate(percentage = 100 * count / sum(count))
    
    # Create donut chart
    donut_chart <- ggplot2::ggplot(
      context_data, 
      ggplot2::aes(x = 2, y = percentage, fill = category)
    ) +
      ggplot2::geom_col(width = 1) +
      ggplot2::coord_polar("y", start = 0) +
      ggplot2::xlim(0.5, 2.5) +  # Create donut hole
      ggplot2::scale_fill_brewer(palette = "Set2") +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(round(percentage), "%\n(", count, ")")),
        position = position_stack(vjust = 0.5),
        color = "white", 
        fontface = "bold"
      ) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = ggplot2::element_text(size = 14, hjust = 0.5),
        legend.title = ggplot2::element_blank(),
        legend.position = "bottom"
      ) +
      ggplot2::labs(title = title)
    
    return(donut_chart)
  }
  
  #' Get NLP analysis results from the summary file
  #' 
  #' @param nlp_results_dir Directory containing NLP results
  #' @return Dataframe with classification summary or NULL if not found
  get_nlp_summary <- function(nlp_results_dir) {
    if (is.null(nlp_results_dir) || !dir.exists(nlp_results_dir)) {
      cat("Warning: NLP results directory not found\n")
      return(NULL)
    }
    
    # Check for summary file
    summary_file <- file.path(nlp_results_dir, "term_classification_summary.csv")
    if (!file.exists(summary_file)) {
      cat("Warning: NLP summary file not found:", summary_file, "\n")
      return(NULL)
    }
    
    # Read summary data
    nlp_summary <- tryCatch({
      readr::read_csv(summary_file, show_col_types = FALSE)
    }, error = function(e) {
      cat("Error reading NLP summary file:", e$message, "\n")
      return(NULL)
    })
    
    return(nlp_summary)
  }
  
  #' Find the top off-target terms from NLP analysis
  #' 
  #' @param nlp_results_dir Directory containing NLP results
  #' @param n Number of top terms to return
  #' @return Vector of top off-target terms
  get_top_off_target_terms <- function(nlp_results_dir, n = 20) {
    # Get NLP summary data
    nlp_summary <- get_nlp_summary(nlp_results_dir)
    
    if (is.null(nlp_summary)) {
      cat("Warning: Could not get NLP summary data\n")
      return(character(0))
    }
    
    # Calculate off-target rates for each term
    off_target_rates <- nlp_summary |>
      dplyr::group_by(term) |>
      dplyr::mutate(total = sum(count)) |>
      dplyr::filter(
        tolower(classification) %in% c("off-target", "scientific")
      ) |>
      dplyr::mutate(off_target_rate = count / total * 100) |>
      dplyr::select(term, off_target_rate, count, total) |>
      dplyr::arrange(dplyr::desc(off_target_rate))
    
    # Get top terms with highest off-target rates
    # Only include terms with sufficient data
    top_terms <- off_target_rates |>
      dplyr::filter(total >= 5) |>  # At least 5 occurrences to be meaningful
      dplyr::distinct(term, .keep_all = TRUE) |>
      dplyr::slice_head(n = n) |>
      dplyr::pull(term)
    
    # Print the top terms
    cat("Top", length(top_terms), "terms with highest off-target rates:\n")
    for (i in seq_along(top_terms)) {
      term_data <- off_target_rates[off_target_rates$term == top_terms[i], ]
      cat(sprintf("%2d. %s: %.1f%% (%d/%d)\n", 
                  i, top_terms[i], 
                  term_data$off_target_rate[1],
                  term_data$count[1],
                  term_data$total[1]))
    }
    
    return(top_terms)
  }
  
  #' Get NLP analysis results for a specific term
  #' 
  #' @param term Term to get results for
  #' @param nlp_summary NLP summary dataframe
  #' @return Dataframe with classification results
  get_term_classification <- function(term, nlp_summary) {
    # Filter for this term
    term_data <- nlp_summary |>
      dplyr::filter(term == !!term) |>
      dplyr::select(classification, count)
    
    # Map classification labels to match the standard categories
    term_data <- term_data |>
      dplyr::mutate(category = dplyr::case_when(
        tolower(classification) == "off-target" ~ "Off-Target",
        tolower(classification) == "scientific" ~ "Off-Target",
        tolower(classification) == "on-target" ~ "On-Target",
        tolower(classification) == "political" ~ "On-Target",
        tolower(classification) == "ambiguous" ~ "Ambiguous",
        TRUE ~ classification
      )) |>
      dplyr::select(category, count) |>
      dplyr::group_by(category) |>
      dplyr::summarize(count = sum(count)) |>
      dplyr::ungroup()
    
    # Make sure all categories are present
    all_categories <- c("Off-Target", "On-Target", "Ambiguous")
    for (cat in all_categories) {
      if (!cat %in% term_data$category) {
        term_data <- rbind(term_data, data.frame(category = cat, count = 0))
      }
    }
    
    return(term_data)
  }
  
  #' Create term context analysis visualizations using top off-target terms from NLP results
  #' 
  #' @param all_grants Dataframe with all grants (used for context only)
  #' @param nlp_results_dir Directory containing NLP analysis results
  #' @param num_terms Number of top off-target terms to analyze
  #' @return List with visualization objects and data
  create_term_context_donuts <- function(all_grants, nlp_results_dir, num_terms = 20) {
    cat("Creating term context visualizations for top off-target terms...\n")
    
    # Get top off-target terms
    top_terms <- get_top_off_target_terms(nlp_results_dir, n = num_terms)
    
    if (length(top_terms) == 0) {
      cat("Error: No off-target terms found\n")
      return(list(
        top_terms = character(0),
        overall_donut = NULL,
        overall_data = NULL
      ))
    }
    
    # Get NLP summary data
    nlp_summary <- get_nlp_summary(nlp_results_dir)
    
    # Process each term - create visualization and store data
    result_list <- list(top_terms = top_terms)
    
    for (term in top_terms) {
      # Get classification data for this term
      term_data <- get_term_classification(term, nlp_summary)
      
      # Create and store the donut chart
      title <- paste("Context Analysis for '", term, "' in Grant Data", sep = "")
      donut <- create_donut_chart(term_data, title)
      
      # Add subtitle to reflect the analysis method
      donut <- donut + labs(subtitle = "Based on NLP analysis")
      
      # Store in results - use sanitized term name for keys
      safe_term <- gsub("[^a-zA-Z0-9]", "_", term)
      result_list[[paste0(safe_term, "_donut")]] <- donut
      result_list[[paste0(safe_term, "_data")]] <- term_data
    }
    
    # Create overall context analysis from all terms
    cat("Creating overall context distribution from NLP results\n")
    
    # Aggregate by classification
    overall_data <- nlp_summary |>
      dplyr::group_by(classification) |>
      dplyr::summarize(count = sum(count)) |>
      dplyr::ungroup() |>
      # Map classifications to categories
      dplyr::mutate(category = dplyr::case_when(
        tolower(classification) == "off-target" ~ "Off-Target (False Positive)",
        tolower(classification) == "scientific" ~ "Off-Target (False Positive)",
        tolower(classification) == "on-target" ~ "On-Target (True Hit)",
        tolower(classification) == "political" ~ "On-Target (True Hit)",
        tolower(classification) == "ambiguous" ~ "Ambiguous",
        TRUE ~ classification
      )) |>
      dplyr::select(category, count) |>
      dplyr::group_by(category) |>
      dplyr::summarize(count = sum(count)) |>
      dplyr::ungroup()
    
    # Create the donut chart for overall distribution
    overall_donut <- create_donut_chart(overall_data, 
                                        "Analysis of Trigger Term Contexts in All Grants")
    overall_donut <- overall_donut + labs(subtitle = "Based on NLP analysis")
    
    # Add overall results to the list
    result_list[["overall_donut"]] <- overall_donut
    result_list[["overall_data"]] <- overall_data
    
    return(result_list)
  }