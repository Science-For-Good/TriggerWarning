# main.R - Main execution script for comprehensive analysis

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)
library(RColorBrewer)
library(future)     
library(furrr)      
library(progressr)  

# Define base paths
data_dir <- "data"
output_dir <- "output"
figures_dir <- file.path(output_dir, "figures")
reports_dir <- file.path(output_dir, "reports")
processed_data_dir <- file.path(output_dir, "data")

# Create output directories if they don't exist
create_dirs <- c(
  file.path(figures_dir, "agency"),
  file.path(figures_dir, "terms"),
  file.path(figures_dir, "regional"),
  file.path(figures_dir, "time"),
  file.path(processed_data_dir, "context_analysis"),
  file.path(processed_data_dir, "term_counts"),
  file.path(processed_data_dir, "grant_type"),
  file.path(processed_data_dir, "institution"),
  file.path(processed_data_dir, "congressional_district")
)

for (dir in create_dirs) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}

# Source necessary scripts
# Data processing
source("R/data/utils.R")
source("R/data/data_processing.R")

# Visualization scripts
source("R/visualization/agency_visualizations.R")
source("R/visualization/term_analysis.R") 
source("R/visualization/regional_visualizations.R")
source("R/visualization/time_visualizations.R")
source("R/visualization/congressional_district_visualizations.R")
source("R/visualization/institution_visualizations.R")
source("R/visualization/grant_type_visualizations.R")
source("R/visualization/nlp_visualizations.R")

# NLP integration
source("R/analysis/nlp_integration.R")

# Security setup for API keys
setup_api_keys <- function() {
  # Check if environment variable exists
  if (Sys.getenv("GEMINI_API_KEY") == "") {
    # Try to load from .Renviron file
    if (file.exists(".Renviron")) {
      readRenviron(".Renviron")
    }
    
    # If still not set, check for config file
    if (Sys.getenv("GEMINI_API_KEY") == "") {
      config_file <- file.path(Sys.getenv("HOME"), ".gemini_config")
      
      # If config file doesn't exist, create it with instructions
      if (!file.exists(config_file)) {
        cat("# Store your Gemini API key on the first line of this file\n",
            "# Replace this line with your actual API key\n",
            "AIzaSyC6I5namK158XBonFIco08JXPNH6HPfVgY\n",
            file = config_file)
        
        # Set permissions to be readable only by the user
        Sys.chmod(config_file, mode = "0600")
        
        cat("Created API key config file at:", config_file, "\n")
        cat("Please edit this file to update your API key for better security.\n")
      }
      
      # Read the config file
      key_data <- readLines(config_file, warn = FALSE)
      
      # Set environment variable for this session
      if (length(key_data) > 0) {
        api_key <- key_data[1]
        if (!startsWith(api_key, "#")) {
          Sys.setenv(GEMINI_API_KEY = api_key)
          cat("Loaded API key from config file.\n")
        }
      }
    }
  } else {
    cat("Using GEMINI_API_KEY from environment.\n")
  }
}

# Call this function early in main.R
setup_api_keys()

# Source the Gemini API integration
source("R/analysis/gemini_api_integration.R")

# Load datasets
cat("Loading and processing data...\n")
data_list <- load_data(
  trigger_terms_path = file.path(data_dir, "triggerterms.csv"),
  nih_airtable_path = file.path(data_dir, "nih_terminations_airtable_20250508.csv"),
  nsf_grants_path = file.path(data_dir, "nsf_terminations_airtable_20250508.csv")
)

# Extract datasets
nih_grants <- data_list$nih_grants
nsf_grants <- data_list$nsf_grants
trigger_terms <- data_list$trigger_terms
all_grants <- data_list$all_grants
all_term_counts <- data_list$all_term_counts

# Print basic stats
cat("Number of NIH grants:", nrow(nih_grants), "\n")
cat("NIH grants with trigger terms:", sum(nih_grants$has_trigger_term, na.rm = TRUE), 
    sprintf("(%.1f%%)\n", 100 * mean(nih_grants$has_trigger_term, na.rm = TRUE)))

cat("Number of NSF grants:", nrow(nsf_grants), "\n")
cat("NSF grants with trigger terms:", sum(nsf_grants$has_trigger_term, na.rm = TRUE), 
    sprintf("(%.1f%%)\n", 100 * mean(nsf_grants$has_trigger_term, na.rm = TRUE)))

#######################################################
# Generate all visualizations
#######################################################

# Create time-related visualizations
cat("\nCreating time-related visualizations...\n")
time_dir <- file.path(figures_dir, "time")

# Create daily time series
daily_ts_plot <- create_daily_timeseries_plot(all_grants, days_to_show = 30)
save_plot("terminations_by_day", daily_ts_plot, width = 12, height = 7, output_dir = time_dir)

# Monthly plot
month_plot <- create_monthly_plot(all_grants)
save_plot("terminations_by_month", month_plot, width = 12, height = 7, output_dir = time_dir)

# Fiscal quarter plot
quarter_plot <- create_quarterly_plot(all_grants)
save_plot("terminations_by_quarter", quarter_plot, width = 8, height = 6, output_dir = time_dir)

# Create term-related visualizations
cat("\nCreating term-related visualizations...\n")
terms_dir <- file.path(figures_dir, "terms")

# Find the most recent NLP analysis directory with validation
nlp_base_dir <- file.path(processed_data_dir, "nlp_analysis")

# Create the directory if it doesn't exist yet
if (!dir.exists(nlp_base_dir)) {
  dir.create(nlp_base_dir, recursive = TRUE, showWarnings = FALSE)
  cat("Created NLP analysis directory:", nlp_base_dir, "\n")
}

# Find the term_classification_summary.csv file
summary_files <- list.files(nlp_base_dir, pattern = "term_classification_summary.csv", 
                            recursive = TRUE, full.names = TRUE)

if (length(summary_files) > 0) {
  cat("Found", length(summary_files), "summary files:\n")
  for (file in summary_files) {
    cat(" -", file, "\n")
  }
  
  # Sort by directory name (which should be dates)
  summary_files <- sort(summary_files, decreasing = TRUE)
  
  # Use the parent directory of the most recent file as nlp_dir
  nlp_dir <- dirname(summary_files[1])
  cat("Using NLP results from:", nlp_dir, "\n")
  
  # Create term context donuts using all off-target terms
  term_donuts <- create_term_context_donuts(all_grants, nlp_results_dir = nlp_dir, num_terms = 197)
  
  # Save the overall donut chart
  save_plot("overall_context_donut", term_donuts$overall_donut, width = 8, height = 8, output_dir = terms_dir)
  
  # Save results for each top term
  cat("Saving visualizations for top off-target terms...\n")
  
  # Create a subdirectory for the individual term visualizations
  top_terms_dir <- file.path(terms_dir, "top_off_target_terms")
  dir.create(top_terms_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Save context analysis data
  context_dir <- file.path(processed_data_dir, "context_analysis")
  write.csv(term_donuts$overall_data, file.path(context_dir, "overall_context_analysis.csv"), row.names = FALSE)
  
  # Save top terms list
  write.csv(data.frame(term = term_donuts$top_terms), 
            file.path(context_dir, "top_off_target_terms.csv"), 
            row.names = FALSE)
  
  # Process each term
  for (term in term_donuts$top_terms) {
    # Create safe filename from term
    safe_term <- gsub("[^a-zA-Z0-9]", "_", term)
    
    # Check if visualization exists for this term
    if (paste0(safe_term, "_donut") %in% names(term_donuts)) {
      # Save the visualization
      save_plot(
        paste0(safe_term, "_context"), 
        term_donuts[[paste0(safe_term, "_donut")]], 
        width = 8, height = 8, 
        output_dir = top_terms_dir
      )
      
      # Save the data
      if (paste0(safe_term, "_data") %in% names(term_donuts)) {
        write.csv(
          term_donuts[[paste0(safe_term, "_data")]], 
          file.path(context_dir, paste0(safe_term, "_context_analysis.csv")),
          row.names = FALSE
        )
      }
    }
  }
  
  # Create top terms frequency visualization - ONLY do this once, AFTER processing NLP results
  top_terms_plot <- create_top_terms_plot(all_term_counts)
  save_plot("top_flagged_terms", top_terms_plot, width = 12, height = 7, output_dir = terms_dir)
  
  cat("Term context visualizations complete. Saved", length(term_donuts$top_terms), "term visualizations.\n")
} else {
  cat("Warning: No term_classification_summary.csv files found. Term context visualizations will be skipped.\n")
  
  # Still create basic term frequency visualization even without NLP
  top_terms_plot <- create_top_terms_plot(all_term_counts)
  save_plot("top_flagged_terms", top_terms_plot, width = 12, height = 7, output_dir = terms_dir)
}

# Monthly term prevalence plot
monthly_term_plot <- create_monthly_term_prevalence_plot(all_grants)
save_plot("monthly_term_prevalence", monthly_term_plot, width = 12, height = 6, output_dir = time_dir)

# Agency-specific visualizations
cat("\nCreating agency-specific visualizations...\n")
agency_dir <- file.path(figures_dir, "agency")

# NIH visualizations
nih_inst_plot <- create_nih_institute_plot(nih_grants)
save_plot("nih_institute_terminations", nih_inst_plot, width = 12, height = 6, output_dir = agency_dir)

nih_term_plot <- create_nih_term_distribution_plot(nih_grants, all_term_counts)
save_plot("nih_term_distribution", nih_term_plot, width = 10, height = 8, output_dir = agency_dir)

# NSF visualizations
nsf_dir_plot <- create_nsf_directorate_plot(nsf_grants)
save_plot("nsf_directorate_terminations", nsf_dir_plot, width = 10, height = 6, output_dir = agency_dir)

nsf_terms_plot <- create_nsf_directorates_terms_plot(nsf_grants)
save_plot("nsf_directorates_with_terms", nsf_terms_plot, width = 12, height = 7, output_dir = agency_dir)

nsf_term_plot <- create_nsf_term_distribution_plot(nsf_grants, all_term_counts)
save_plot("nsf_term_distribution", nsf_term_plot, width = 10, height = 8, output_dir = agency_dir)

# Regional visualizations
cat("\nCreating regional visualizations...\n")
regional_dir <- file.path(figures_dir, "regional")

regional_analysis <- create_regional_analysis_plots(all_grants)
save_plot("terminations_by_region", regional_analysis$plot, width = 10, height = 6, output_dir = regional_dir)

regional_term_plot <- create_regional_term_distribution_plot(regional_analysis$data, all_term_counts)
save_plot("regional_term_distribution", regional_term_plot, width = 10, height = 6, output_dir = regional_dir)

regional_misclass <- create_regional_misclassification_plot(regional_analysis$data)
save_plot("regional_misclassification", regional_misclass$plot, width = 10, height = 6, output_dir = regional_dir)
write.csv(regional_misclass$data, file.path(context_dir, "regional_misclassification.csv"), row.names = FALSE)

# Congressional district visualizations
district_plot <- create_congressional_district_plot(nsf_grants)
save_plot("congressional_districts", district_plot, width = 12, height = 7, output_dir = regional_dir)

district_term_plot <- create_district_term_distribution_plot(nsf_grants, all_term_counts)
save_plot("district_term_distribution", district_term_plot, width = 12, height = 7, output_dir = regional_dir)

# Institution visualizations
cat("\nCreating institution-based visualizations...\n")
institution_dir <- file.path(figures_dir, "institutions")
dir.create(institution_dir, recursive = TRUE, showWarnings = FALSE)

inst_type_plot <- create_institution_type_plot(all_grants)
save_plot("institution_types", inst_type_plot, width = 10, height = 6, output_dir = institution_dir)

top_inst_plot <- create_top_institutions_plot(all_grants)
save_plot("top_institutions", top_inst_plot, width = 12, height = 7, output_dir = institution_dir)

academic_plot <- create_academic_classification_plot(all_grants)
save_plot("academic_classifications", academic_plot, width = 10, height = 6, output_dir = institution_dir)

# Grant type visualizations (NIH-specific)
cat("\nCreating grant type visualizations...\n")
grant_type_dir <- file.path(figures_dir, "grant_types")
dir.create(grant_type_dir, recursive = TRUE, showWarnings = FALSE)

grant_type_plot <- create_grant_type_plot(nih_grants) |> 
  add_angled_x_labels()

save_plot("nih_grant_types", grant_type_plot, width = 10, height = 6, output_dir = grant_type_dir)

activity_code_plot <- create_activity_code_plot(nih_grants)
save_plot("nih_activity_codes", activity_code_plot, width = 10, height = 6, output_dir = grant_type_dir)

# Diversity supplement analysis
diversity_analysis <- create_diversity_supplement_analysis(nih_grants)
save_plot("diversity_grants", diversity_analysis$plot, width = 12, height = 6, output_dir = grant_type_dir)

# Save diversity data for further analysis - with additional list column handling
if (is.data.frame(diversity_analysis$data)) {
  # Identify and convert any list columns before writing to CSV
  data_to_save <- diversity_analysis$data
  
  # Check for list columns
  list_cols <- sapply(data_to_save, is.list)
  if (any(list_cols)) {
    cat("Converting", sum(list_cols), "list columns to character format\n")
    
    # Convert list columns to character representations
    for (col_name in names(data_to_save)[list_cols]) {
      data_to_save[[col_name]] <- sapply(data_to_save[[col_name]], 
                                         function(x) {
                                           if (length(x) == 0) return(NA_character_)
                                           paste(x, collapse = ", ")
                                         })
    }
  }
  
  # Now write the cleaned data
  write.csv(data_to_save, file.path(processed_data_dir, "diversity_grants.csv"), row.names = FALSE)
  cat("Diversity grants analysis saved to CSV\n")
} else {
  cat("Warning: diversity_analysis$data is not a data frame, skipping CSV export\n")
}

# NLP Analysis with all terms
run_nlp_analysis <- TRUE
if (run_nlp_analysis) {
  cat("\nRunning NLP analysis of all trigger terms using Gemini API...\n")
  nlp_dirs <- setup_nlp_analysis(file.path(processed_data_dir, "nlp_analysis"))
  
  # Use all trigger terms without limitation
  terms_to_analyze <- trigger_terms$term
  
  # Determine optimal parallel workers based on available cores
  # Leave 1 core free for system processes
  available_cores <- parallel::detectCores()
  recommended_workers <- max(1, available_cores - 1)
  
  cat("System information:\n")
  cat("- Available CPU cores:", available_cores, "\n")
  cat("- Recommended parallel workers:", recommended_workers, "\n")
  
  cat("\nStarting large-scale NLP processing with the following parameters:\n")
  cat("- Terms to analyze:", length(terms_to_analyze), "\n")
  cat("- Max contexts per term: 6312\n")
  cat("- Batch size: 2 terms at a time\n")
  cat("- Cooldown period: 300 seconds (5 minutes) between batches\n")
  cat("- Parallel workers:", recommended_workers, "\n")
  
  # Set up progress reporting for parallel processing
  handlers(global = TRUE)
  handlers("progress")
  
  # Run the analysis with optimized configuration for large-scale processing
  nlp_results <- analyze_term_contexts_nlp(
    all_grants = all_grants,
    terms_to_analyze = terms_to_analyze,
    api_function = gemini_api_function,
    output_dir = nlp_dirs$output_dir,
    log_file = nlp_dirs$log_file,
    max_contexts_per_term = 6312,  # Analyze up to 6312 contexts per term (expanded)
    batch_size = 2,                # Process in small batches of 2 terms (reduced from 10)
    cooldown_period = 300,         # Cool down for 5 minutes between batches (increased from 60s)
    parallel_workers = recommended_workers  # Use parallel processing
  )
  
  # Create combined visualizations of NLP results
  cat("\nCreating NLP analysis visualizations...\n")
  nlp_viz_dir <- file.path(figures_dir, "nlp_analysis")
  dir.create(nlp_viz_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Load the summary data created by analyze_term_contexts_nlp
  nlp_summary <- read_csv(file.path(nlp_dirs$output_dir, "term_classification_summary.csv"), 
                          show_col_types = FALSE)
  
  # Create and save additional visualizations
  # Top false positives plot (off-target)
  fp_rates <- read_csv(file.path(nlp_dirs$output_dir, "false_positive_rates.csv"), 
                       show_col_types = FALSE)
  
  top_fp_plot <- ggplot(head(fp_rates, 20), 
                        aes(x = reorder(term, false_positive_rate), y = false_positive_rate)) +
    geom_col(fill = "#5EECC2") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Top 20 Terms with Highest False Positive Rates",
         subtitle = "Percentage of contexts classified as off-target",
         x = "Term", y = "False Positive Rate (%)")
  
  save_plot("top_false_positives", top_fp_plot, width = 10, height = 8, output_dir = nlp_viz_dir)
}


# After the NLP analysis is complete:
if (run_nlp_analysis) {
  # Create NLP visualization directory
  nlp_viz_dir <- file.path(figures_dir, "nlp_analysis")
  dir.create(nlp_viz_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Generate all NLP visualizations from summary data only
  cat("\nGenerating comprehensive NLP visualizations...\n")
  
  # Generate visualizations using the simplified approach
  source("R/visualization/nlp_visualizations.R")
  
  cat("NLP visualization complete!\n")
}


# Final summary message
cat("\nComprehensive analysis completed!\n")
cat("Created visualizations in:", figures_dir, "\n")
cat("Processed data saved to:", processed_data_dir, "\n")