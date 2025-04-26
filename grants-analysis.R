# grants_analysis.R - Main script for comprehensive analysis

# Load necessary libraries and common functions
source("R/utils.R")
source("R/data_processing.R")
source("R/visualizations/time_visualizations.R")
source("R/visualizations/agency_visualizations.R")
source("R/visualizations/congressional_district_visualizations.R")
source("R/visualizations/regional_visualizations.R")
source("R/visualizations/term_analysis.R")
source("R/nlp_integration.R")

# Create output directory
output_dir <- "output/comprehensive_analysis"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Load required libraries
load_required_libs()

# Load datasets
data_list <- load_data(
  trigger_terms_path = "data/triggerterms.csv",
  nih_grants_path = "data/Terminated_Grants_Explorer_TAGGS.csv",
  nsf_grants_path = "data/nsf_terminations_airtable_20250425.csv"
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

# Annual terminations plot
annual_plot <- create_annual_terminations_plot(all_grants)
save_plot("terminations_by_year", annual_plot, width = 12, height = 7, output_dir = output_dir)

# Administration plot
admin_plot <- create_administration_plot(all_grants)
save_plot("terminations_by_administration", admin_plot, width = 10, height = 6, output_dir = output_dir)

# Monthly plot
month_plot <- create_monthly_plot(all_grants)
save_plot("terminations_by_month", month_plot, width = 12, height = 7, output_dir = output_dir)

# Recent terminations plot
recent_plot <- create_recent_terminations_plot(all_grants)
save_plot("recent_terminations", recent_plot, width = 12, height = 7, output_dir = output_dir)

# Fiscal quarter plot
quarter_plot <- create_quarterly_plot(all_grants)
save_plot("terminations_by_quarter", quarter_plot, width = 8, height = 6, output_dir = output_dir)

# Monthly term prevalence plot
monthly_term_plot <- create_monthly_term_prevalence_plot(all_grants)
save_plot("monthly_term_prevalence", monthly_term_plot, width = 12, height = 6, output_dir = output_dir)

# Yearly term prevalence plot
yearly_term_plot <- create_yearly_term_prevalence_plot(all_grants)
save_plot("yearly_term_prevalence", yearly_term_plot, width = 10, height = 6, output_dir = output_dir)

# Create term-related visualizations
cat("\nCreating term-related visualizations...\n")

# Top terms
top_terms_plot <- create_top_terms_plot(all_term_counts)
save_plot("top_flagged_terms", top_terms_plot, width = 12, height = 7, output_dir = output_dir)

# Term context donuts
term_donuts <- create_term_context_donuts(all_grants)
save_plot("trans_context_donut", term_donuts$trans_donut, width = 8, height = 8, output_dir = output_dir)
# save_plot("climate_context_donut", term_donuts$climate_donut, width = 8, height = 8, output_dir = output_dir)
save_plot("overall_context_donut", term_donuts$overall_donut, width = 8, height = 8, output_dir = output_dir)

# Save the context analysis results
write.csv(term_donuts$trans_data, file.path(output_dir, "trans_context_analysis.csv"), row.names = FALSE)
# write.csv(term_donuts$climate_data, file.path(output_dir, "climate_context_analysis.csv"), row.names = FALSE)
write.csv(term_donuts$overall_data, file.path(output_dir, "overall_context_analysis.csv"), row.names = FALSE)

# Final summary message
cat("\nComprehensive analysis completed!\n")
cat("Created", length(list.files(output_dir, pattern = ".png|.pdf")), "visualizations\n")
cat("All files saved to:", output_dir, "\n")
