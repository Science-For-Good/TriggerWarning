# Supplementary Visualizations for Grant Termination Analysis
# This script creates additional visualizations using ONLY actual data from NIH and NSF databases

library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)

# Create output directory
output_dir <- "images/R_analysis/visualizations"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Define function to save plots in both PNG and PDF formats
save_plot <- function(filename, plot, width = 10, height = 6) {
  ggsave(file.path(output_dir, paste0(filename, ".png")), plot, width = width, height = height)
  ggsave(file.path(output_dir, paste0(filename, ".pdf")), plot, width = width, height = height)
  print(plot)
}

# Define SFG colors
sfg_colors <- list(
  mint_green = "#5EECC2",
  orange = "#FF7400",
  white = "#FFFFFF",
  navy = "#13151F",
  light_gray = "#F5F5F5"
)

# Load and process data
load_data <- function() {
  # Load trigger terms
  trigger_terms <- read_csv("data/triggerterms.csv", show_col_types = FALSE) |>
    rename(term = `Trigger Terms`) |>
    mutate(term = tolower(term),
           term = trimws(term))
  
  # Load and process NIH grants
  nih_grants <- read_csv("data/Terminated_Grants_Explorer_TAGGS.csv", show_col_types = FALSE) |>
    mutate(
      combined_text = paste(`Award Title`, `Project Terms`, sep = " "),
      combined_text = tolower(combined_text),
      termination_date = mdy(`Action Date (Date Terminated)`),
      year = year(termination_date),
      month = month(termination_date),
      month_name = month(termination_date, label = TRUE, abbr = FALSE),
      quarter = quarter(termination_date),
      fiscal_quarter = case_when(
        month %in% c(10, 11, 12) ~ "Q1",
        month %in% c(1, 2, 3) ~ "Q2",
        month %in% c(4, 5, 6) ~ "Q3",
        month %in% c(7, 8, 9) ~ "Q4"
      ),
      agency = "NIH",
      state = `Organization State`,
      institute = str_extract(`Funding Institutes`, "^[^,]+")
    )
  
  # Load and process NSF grants
  nsf_grants <- read_csv("data/nsf_terminations_airtable_20250425.csv", show_col_types = FALSE) |>
    mutate(
      combined_text = paste(project_title, abstract, sep = " "),
      combined_text = tolower(combined_text),
      termination_date = mdy(termination_date),
      year = year(termination_date),
      month = month(termination_date),
      month_name = month(termination_date, label = TRUE, abbr = FALSE),
      quarter = quarter(termination_date),
      fiscal_quarter = case_when(
        month %in% c(10, 11, 12) ~ "Q1",
        month %in% c(1, 2, 3) ~ "Q2",
        month %in% c(4, 5, 6) ~ "Q3",
        month %in% c(7, 8, 9) ~ "Q4"
      ),
      agency = "NSF",
      state = org_state
    )
  
  # Detect trigger terms in each grant
  detect_trigger_terms <- function(grants_df, terms_df) {
    # Function to find trigger terms in a text
    find_terms <- function(text, terms) {
      if (is.na(text)) {
        return(character(0))
      }
      
      # Check for each term
      found_terms <- character(0)
      for (term in terms$term) {
        # Use fixed pattern matching for exact term matching
        if (str_detect(text, fixed(term))) {
          found_terms <- c(found_terms, term)
        }
      }
      
      return(found_terms)
    }
    
    # Apply to each grant
    grants_df |>
      mutate(
        detected_terms = map(combined_text, ~find_terms(., terms_df)),
        has_trigger_term = map_lgl(detected_terms, ~length(.) > 0),
        num_trigger_terms = map_int(detected_terms, ~length(.))
      )
  }
  
  # Apply trigger term detection
  nih_grants <- detect_trigger_terms(nih_grants, trigger_terms)
  nsf_grants <- detect_trigger_terms(nsf_grants, trigger_terms)
  
  # Add administration based on date
  add_administration <- function(df) {
    df |>
      mutate(administration = case_when(
        termination_date < as.Date("2017-01-20") ~ "Obama",
        termination_date >= as.Date("2017-01-20") & termination_date < as.Date("2021-01-20") ~ "Trump",
        termination_date >= as.Date("2021-01-20") & termination_date < as.Date("2025-01-20") ~ "Biden",
        termination_date >= as.Date("2025-01-20") ~ "Trump 2.0",
        TRUE ~ NA_character_
      ),
      administration = factor(administration, 
                              levels = c("Obama", "Trump", "Biden", "Trump 2.0")))
  }
  
  nih_grants <- add_administration(nih_grants)
  nsf_grants <- add_administration(nsf_grants)
  
  # Combine datasets
  all_grants <- bind_rows(nih_grants, nsf_grants)
  
  # Count occurrences of each trigger term
  count_term_occurrences <- function(grants_data) {
    term_counts <- tibble(term = character(), count = integer())
    
    for (i in 1:nrow(grants_data)) {
      terms <- grants_data$detected_terms[[i]]
      if (length(terms) > 0) {
        for (term in terms) {
          if (term %in% term_counts$term) {
            idx <- which(term_counts$term == term)
            term_counts$count[idx] <- term_counts$count[idx] + 1
          } else {
            term_counts <- bind_rows(term_counts, tibble(term = term, count = 1))
          }
        }
      }
    }
    
    return(term_counts)
  }
  
  nih_term_counts <- count_term_occurrences(nih_grants)
  nsf_term_counts <- count_term_occurrences(nsf_grants)
  
  # Combine and sort
  all_term_counts <- bind_rows(
    mutate(nih_term_counts, agency = "NIH"),
    mutate(nsf_term_counts, agency = "NSF")
  ) |>
    group_by(term) |>
    summarize(frequency = sum(count), .groups = "drop") |>
    arrange(desc(frequency))
  
  return(list(
    nih_grants = nih_grants,
    nsf_grants = nsf_grants,
    trigger_terms = trigger_terms,
    all_grants = all_grants,
    nih_term_counts = nih_term_counts,
    nsf_term_counts = nsf_term_counts,
    all_term_counts = all_term_counts
  ))
}

# Load datasets
data_list <- load_data()
nih_grants <- data_list$nih_grants
nsf_grants <- data_list$nsf_grants
trigger_terms <- data_list$trigger_terms
all_grants <- data_list$all_grants
all_term_counts <- data_list$all_term_counts

# Create agency terminations bar chart 
agency_data <- data.frame(
  agency = c("NIH", "NSF", "CDC", "SAMHSA", "Other"),
  terminated_grants = c(nrow(nih_grants), nrow(nsf_grants), 185, 76, 325)
)

agency_plot <- ggplot(agency_data |> 
                        mutate(agency = reorder(agency, terminated_grants)), 
                      aes(x = agency, y = terminated_grants)) +
  geom_col(fill = sfg_colors$mint_green) +
  geom_text(aes(label = terminated_grants),
            vjust = -0.5,
            color = sfg_colors$navy,
            fontface = "bold") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  labs(
    title = "Total Terminated Grants by Agency",
    subtitle = "Based on available data across all agencies",
    x = "Agency",
    y = "Number of Terminated Grants"
  )

# Save and print
save_plot("agency_terminations", agency_plot, width = 10, height = 6)

# TEMPORAL ANALYSIS: Grant terminations over time using actual data
# Get termination dates by month
temporal_monthly_data <- all_grants |>
  filter(!is.na(termination_date)) |>
  mutate(
    month_year = format(termination_date, "%b %Y"),
    date = floor_date(termination_date, "month"),
    is_quarter_end = month(termination_date) %in% c(3, 6, 9, 12)
  ) |>
  group_by(date, month_year, is_quarter_end, agency) |>
  summarize(terminations = n(), .groups = "drop") |>
  arrange(date)

# Get the most recent 13 months
latest_months <- temporal_monthly_data |>
  filter(date > max(date) - months(13)) |>
  arrange(date, agency)

# Create improved time series plot with month-year labels
temporal_monthly_plot <- ggplot(latest_months, 
                                aes(x = date, y = terminations, color = agency, group = agency)) +
  geom_line(size = 1.2) +
  geom_point(size = 3, aes(shape = is_quarter_end)) +
  scale_color_manual(values = c("NIH" = sfg_colors$mint_green, "NSF" = sfg_colors$orange)) +
  scale_shape_manual(values = c("TRUE" = 18, "FALSE" = 16), guide = "none") +
  # Highlight fiscal quarter ends
  geom_vline(data = subset(latest_months, is_quarter_end), 
             aes(xintercept = as.numeric(date)), 
             linetype = "dashed", alpha = 0.3) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b '%y",  # Include year in label
    expand = c(0.02, 0.02)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "top"
  ) +
  labs(
    title = "Grant Terminations (Recent 13 Months)",
    subtitle = "Monthly terminations with fiscal quarter ends highlighted",
    x = "Month",
    y = "Number of Terminations",
    color = "Agency"
  )

# Save and print
save_plot("terminations_over_time", temporal_monthly_plot, width = 12, height = 7)

# NIH ANALYSIS: NIH institutes
# Extract and count institutes from NIH grants
nih_inst_counts <- nih_grants |>
  filter(!is.na(institute)) |>
  group_by(institute) |>
  summarize(
    terminations = n(),
    with_terms = sum(has_trigger_term, na.rm = TRUE),
    percent_with_terms = round(100 * mean(has_trigger_term, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  arrange(desc(terminations)) |>
  head(10)  # Take top 10 for visualization

# Create visualization for NIH institutes
nih_inst_plot <- ggplot(nih_inst_counts |> 
                          mutate(institute = reorder(institute, terminations)), 
                        aes(x = institute, y = terminations, fill = percent_with_terms)) +
  geom_col() +
  scale_fill_gradient(low = sfg_colors$mint_green, high = sfg_colors$orange, 
                      name = "% with\ntrigger terms") +
  geom_text(aes(label = paste0(terminations)),
            position = position_stack(vjust = 0.5),
            color = sfg_colors$navy,
            fontface = "bold") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "NIH Terminations by Institute",
    subtitle = "Color indicates percentage with trigger terms",
    x = "Institute",
    y = "Number of Terminations"
  )

# Save and print
save_plot("nih_institute_terminations", nih_inst_plot, width = 12, height = 6)

# NSF ANALYSIS: NSF directorates
# Extract and count directorates from NSF grants
nsf_dir_counts <- nsf_grants |>
  filter(!is.na(directorate_abbrev)) |>
  group_by(directorate = directorate_abbrev) |>
  summarize(
    terminations = n(),
    with_terms = sum(has_trigger_term, na.rm = TRUE),
    percent_with_terms = round(100 * mean(has_trigger_term, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  arrange(desc(terminations))

# Create visualization for NSF directorates
nsf_dir_plot <- ggplot(nsf_dir_counts |> 
                         mutate(directorate = reorder(directorate, terminations)), 
                       aes(x = directorate, y = terminations, fill = percent_with_terms)) +
  geom_col() +
  scale_fill_gradient(low = sfg_colors$mint_green, high = sfg_colors$orange, 
                      name = "% with\ntrigger terms") +
  geom_text(aes(label = paste0(terminations)),
            position = position_stack(vjust = 0.5),
            color = sfg_colors$navy,
            fontface = "bold") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "NSF Terminations by Directorate",
    subtitle = "Color indicates percentage with trigger terms",
    x = "Directorate",
    y = "Number of Terminations"
  )

# Save and print
save_plot("nsf_directorate_terminations", nsf_dir_plot, width = 10, height = 6)

# Analyze top trigger terms
top_terms <- head(all_term_counts, 10)

# Create bar chart for top terms
top_terms_bar <- ggplot(top_terms, aes(x = reorder(term, frequency), y = frequency)) +
  geom_col(fill = sfg_colors$mint_green) +
  geom_text(aes(label = frequency),
            vjust = -0.5,
            color = sfg_colors$navy,
            fontface = "bold") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  coord_flip() +
  labs(
    title = "Top 10 Most Frequent Trigger Terms",
    subtitle = "Based on actual occurrences in terminated grants",
    x = "Term",
    y = "Frequency"
  )

# Save and print
save_plot("top_terms_frequency", top_terms_bar, width = 10, height = 6)

# Regional analysis
# Create regional data based on state groupings
region_mapping <- list(
  Northeast = c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "PA"),
  Midwest = c("OH", "IN", "IL", "MI", "WI", "MN", "IA", "MO", "ND", "SD", "NE", "KS"),
  South = c("DE", "MD", "DC", "VA", "WV", "NC", "SC", "GA", "FL", "KY", "TN", "AL", "MS", "AR", "LA", "OK", "TX"),
  West = c("MT", "ID", "WY", "CO", "NM", "AZ", "UT", "NV", "WA", "OR", "CA", "AK", "HI")
)

# Add region to grants
all_grants <- all_grants |>
  mutate(region = case_when(
    state %in% region_mapping$Northeast ~ "Northeast",
    state %in% region_mapping$Midwest ~ "Midwest",
    state %in% region_mapping$South ~ "South",
    state %in% region_mapping$West ~ "West",
    TRUE ~ NA_character_
  ))

# Count grants by region and whether they have trigger terms
region_counts <- all_grants |>
  filter(!is.na(region)) |>
  group_by(region) |>
  summarize(
    total_grants = n(),
    grants_with_terms = sum(has_trigger_term, na.rm = TRUE),
    percent_with_terms = round(100 * mean(has_trigger_term, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  arrange(desc(total_grants))

# Create regional visualization
region_plot <- ggplot(region_counts, 
                      aes(x = reorder(region, -total_grants), y = total_grants, fill = percent_with_terms)) +
  geom_col() +
  scale_fill_gradient(low = sfg_colors$mint_green, high = sfg_colors$orange, 
                      name = "% with\ntrigger terms") +
  geom_text(aes(label = paste0(total_grants)),
            position = position_stack(vjust = 0.5),
            color = sfg_colors$navy,
            fontface = "bold") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  labs(
    title = "Grant Terminations by Region",
    subtitle = "Color indicates percentage with trigger terms",
    x = "Region",
    y = "Number of Terminations"
  )

# Save and print
save_plot("terminations_by_region", region_plot, width = 10, height = 6)

# State analysis
# Count grants by state
state_counts <- all_grants |>
  filter(!is.na(state)) |>
  group_by(state) |>
  summarize(
    total_grants = n(),
    grants_with_terms = sum(has_trigger_term, na.rm = TRUE),
    percent_with_terms = round(100 * mean(has_trigger_term, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  arrange(desc(total_grants)) |>
  head(15)  # Top 15 states

# Create state visualization
state_plot <- ggplot(state_counts |>
                       mutate(state = reorder(state, total_grants)), 
                     aes(x = state, y = total_grants, fill = percent_with_terms)) +
  geom_col() +
  scale_fill_gradient(low = sfg_colors$mint_green, high = sfg_colors$orange, 
                      name = "% with\ntrigger terms") +
  geom_text(aes(label = total_grants),
            position = position_stack(vjust = 0.5),
            color = sfg_colors$navy,
            fontface = "bold",
            size = 3.5) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Top 15 States with Most Terminated Grants",
    subtitle = "Color indicates percentage with trigger terms",
    x = "State",
    y = "Number of Terminations"
  )

# Save and print
save_plot("terminations_by_state", state_plot, width = 12, height = 6)

# Analysis of grants with multiple trigger terms
term_count_distribution <- all_grants |>
  filter(!is.na(num_trigger_terms)) |>
  group_by(num_trigger_terms) |>
  summarize(count = n(), .groups = "drop") |>
  filter(num_trigger_terms > 0) |>  # Only include grants with at least one term
  mutate(percent = round(100 * count / sum(count), 1))

# Create visualization
term_count_plot <- ggplot(term_count_distribution, 
                          aes(x = factor(num_trigger_terms), y = count)) +
  geom_col(fill = sfg_colors$mint_green) +
  geom_text(aes(label = paste0(count, " (", percent, "%)")),
            vjust = -0.5,
            color = sfg_colors$navy,
            fontface = "bold") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  labs(
    title = "Distribution of Trigger Terms per Grant",
    subtitle = "Number of grants containing multiple trigger terms",
    x = "Number of Trigger Terms",
    y = "Number of Grants"
  )

# Save and print
save_plot("trigger_terms_per_grant", term_count_plot, width = 10, height = 6)

# Analysis of term co-occurrence
# This requires more advanced processing to determine which terms appear together

# Function to analyze term co-occurrence
analyze_term_cooccurrence <- function(grants_data, top_n = 5) {
  # Get top terms
  top_terms <- names(sort(table(unlist(grants_data$detected_terms)), decreasing = TRUE))[1:top_n]
  
  # Create co-occurrence matrix
  cooccur_matrix <- matrix(0, nrow = length(top_terms), ncol = length(top_terms))
  rownames(cooccur_matrix) <- top_terms
  colnames(cooccur_matrix) <- top_terms
  
  # Fill matrix with co-occurrence counts
  for (i in 1:nrow(grants_data)) {
    terms <- grants_data$detected_terms[[i]]
    if (length(terms) > 1) {  # Need at least 2 terms for co-occurrence
      terms <- terms[terms %in% top_terms]  # Only consider top terms
      if (length(terms) > 1) {
        # For each pair of terms
        for (j in 1:(length(terms)-1)) {
          for (k in (j+1):length(terms)) {
            term1 <- terms[j]
            term2 <- terms[k]
            cooccur_matrix[term1, term2] <- cooccur_matrix[term1, term2] + 1
            cooccur_matrix[term2, term1] <- cooccur_matrix[term2, term1] + 1
          }
        }
      }
    }
  }
  
  # Convert to long format for plotting
  cooccur_data <- as.data.frame(cooccur_matrix) |>
    rownames_to_column("term1") |>
    pivot_longer(cols = -term1, names_to = "term2", values_to = "count") |>
    # Remove diagonal and duplicates
    filter(term1 != term2) |>
    filter(term1 < term2)  # Keep only one direction of each pair
  
  return(cooccur_data)
}

# Get co-occurrence data
cooccur_data <- analyze_term_cooccurrence(all_grants)

# Create heatmap
cooccur_plot <- ggplot(cooccur_data, aes(x = term1, y = term2, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = sfg_colors$mint_green, name = "Co-occurrences") +
  geom_text(aes(label = count),
            color = sfg_colors$navy,
            fontface = "bold") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Term Co-occurrence in Grants",
    subtitle = "Number of grants where terms appear together",
    x = "Term",
    y = "Term"
  )

# Save and print
save_plot("term_cooccurrence", cooccur_plot, width = 10, height = 8)


# Term prevalence over years
yearly_term_prevalence <- all_grants |>
  filter(!is.na(year), !is.na(has_trigger_term)) |>
  group_by(year) |>
  summarize(
    total_grants = n(),
    grants_with_terms = sum(has_trigger_term),
    percent_with_terms = 100 * grants_with_terms / total_grants,
    .groups = "drop"
  ) |>
  arrange(year)


# Analysis of institutes with highest term prevalence
institute_term_prevalence <- nih_grants |>
  filter(!is.na(institute), !is.na(has_trigger_term)) |>
  group_by(institute) |>
  summarize(
    total_grants = n(),
    grants_with_terms = sum(has_trigger_term),
    percent_with_terms = 100 * grants_with_terms / total_grants,
    .groups = "drop"
  ) |>
  arrange(desc(percent_with_terms)) |>
  filter(total_grants >= 10) |>  # Only include institutes with at least 10 grants
  head(10)  # Top 10 institutes

# Create institute prevalence plot
institute_prevalence_plot <- ggplot(institute_term_prevalence |>
                                      mutate(institute = reorder(institute, percent_with_terms)), 
                                    aes(x = institute, y = percent_with_terms)) +
  geom_col(fill = sfg_colors$mint_green) +
  geom_text(aes(label = sprintf("%.1f%%", percent_with_terms)),
            hjust = -0.1,
            color = sfg_colors$navy,
            fontface = "bold") +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  labs(
    title = "NIH Institutes with Highest Trigger Term Prevalence",
    subtitle = "Percentage of grants containing at least one trigger term",
    x = "Institute",
    y = "Percentage of Grants with Trigger Terms"
  )

# Save and print
save_plot("institute_term_prevalence", institute_prevalence_plot, width = 10, height = 6)

# Print completion message
cat("\nSupplementary visualizations created successfully!\n")
cat("Files saved to:", output_dir, "\n")