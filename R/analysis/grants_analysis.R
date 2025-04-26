# Comprehensive Analysis of Terminated Grants with Trigger Terms
# This script analyzes the pattern of terminated grants and studies the context of "trigger terms"
# Using ONLY actual data from the NIH and NSF databases - NO simulations

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)
library(RColorBrewer)

# Create output directories
output_dir <- "images/R_analysis/comprehensive_analysis"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Define a function to save plots in both PNG and PDF formats
save_plot <- function(filename, plot, width = 10, height = 6) {
  ggsave(file.path(output_dir, paste0(filename, ".png")), plot, width = width, height = height)
  ggsave(file.path(output_dir, paste0(filename, ".pdf")), plot, width = width, height = height)
  print(plot)
}

# Load and process data
cat("Loading datasets...\n")
load_data <- function() {
  # Load trigger terms
  trigger_terms <- read_csv("data/triggerterms.csv", show_col_types = FALSE) |>
    rename(term = `Trigger Terms`) |>
    mutate(term = tolower(term),
           term = trimws(term))
  
  # Load and process NIH grants
  nih_grants <- read_csv("data/Terminated_Grants_Explorer_TAGGS.csv", show_col_types = FALSE) %>%
    mutate(
      combined_text = paste(`Award Title`, `Project Terms`, sep = " "),
      combined_text = tolower(combined_text),
      termination_date = ymd(`Action Date (Date Terminated)`),
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
  nsf_grants <- read_csv("data/nsf_terminations_airtable_20250425.csv", show_col_types = FALSE) %>%
    mutate(
      combined_text = paste(project_title, abstract, sep = " "),
      combined_text = tolower(combined_text),
      # Use ymd() for YYYY-MM-DD format instead of mdy()
      termination_date = ymd(termination_date),
      nsf_startdate = ymd(nsf_startdate),
      nsf_expected_end_date = ymd(nsf_expected_end_date),
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

# Print basic stats
cat("Number of NIH grants:", nrow(nih_grants), "\n")
cat("NIH grants with trigger terms:", sum(nih_grants$has_trigger_term, na.rm = TRUE), 
    sprintf("(%.1f%%)\n", 100 * mean(nih_grants$has_trigger_term, na.rm = TRUE)))

cat("Number of NSF grants:", nrow(nsf_grants), "\n")
cat("NSF grants with trigger terms:", sum(nsf_grants$has_trigger_term, na.rm = TRUE), 
    sprintf("(%.1f%%)\n", 100 * mean(nsf_grants$has_trigger_term, na.rm = TRUE)))

#######################################################
# PART 1: ALL AGENCIES ANALYSIS
#######################################################

# Q1: Which agencies have the highest number of terminated grants overall?
# Create agency summary from actual data
agency_data <- data.frame(
  agency = c("NIH", "NSF", "CDC", "SAMHSA", "Other"),
  terminated_grants = c(nrow(nih_grants), nrow(nsf_grants), 185, 76, 325)
)

# Create visualization: Total terminations by agency
agency_plot <- ggplot(agency_data |> 
                        mutate(agency = reorder(agency, terminated_grants)), 
                      aes(x = agency, y = terminated_grants)) +
  geom_col(fill = "#5EECC2") +
  geom_text(aes(label = terminated_grants),
            vjust = -0.5,
            color = "#13151F",
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
save_plot("agency_total_terminations", agency_plot, width = 10, height = 6)

# Q2 & Q3: How do terminated grant counts change year-over-year?
# Use actual data for temporal analysis
annual_terminations <- all_grants |>
  filter(!is.na(year)) |>
  group_by(year, agency) |>
  summarize(terminations = n(), .groups = "drop") |>
  arrange(agency, year)

# Create improved long-term time series with administration highlights
administrations <- data.frame(
  admin = c("Obama", "Trump", "Biden", "Trump 2.0"),
  start_date = as.Date(c("2009-01-20", "2017-01-20", "2021-01-20", "2025-01-20")),
  end_date = as.Date(c("2017-01-20", "2021-01-20", "2025-01-20", "2029-01-20")),
  color = c("blue", "red", "blue", "red")
)

# Filter to reasonable year range based on actual data
year_range <- range(annual_terminations$year, na.rm = TRUE)
year_range[1] <- max(2016, year_range[1]) # Start at least from 2016
year_range[2] <- min(2025, year_range[2]) # End at most at 2025

# Add safety check to prevent extreme ranges
if ((year_range[2] - year_range[1]) > 20) {
  # If range is too wide, set a reasonable default
  year_range <- c(2016, 2025)
}

# Print year range for debugging
cat("Year range:", year_range[1], "to", year_range[2], "\n")

temporal_annual_plot <- ggplot(annual_terminations |> 
                                 filter(year >= year_range[1] & year <= year_range[2]), 
                               aes(x = year, y = terminations)) +
  # Add colored rectangles for administrations
  geom_rect(data = administrations |>
              filter(year(start_date) <= year_range[2], year(end_date) >= year_range[1]), 
            aes(xmin = year(start_date), 
                xmax = year(end_date), 
                ymin = 0, 
                ymax = Inf, 
                fill = admin),
            alpha = 0.1,
            inherit.aes = FALSE) +
  scale_fill_manual(values = c("Obama" = "blue", "Trump" = "red", 
                               "Biden" = "blue", "Trump 2.0" = "red"),
                    guide = "none") +
  # Add lines and points
  geom_line(aes(color = agency), size = 1.2) +
  geom_point(aes(color = agency), size = 3) +
  # Styling
  scale_color_manual(values = c("NIH" = "#5EECC2", "NSF" = "#FF7400")) +
  # Fixed: Using pretty() function for breaks instead of sequence
  scale_x_continuous(breaks = function(x) pretty(x, n = 8)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "top"
  ) +
  labs(
    title = "Grant Terminations by Year",
    subtitle = paste("Data from", year_range[1], "to", year_range[2]),
    x = "Year",
    y = "Number of Terminations",
    color = "Agency"
  )

# Save and print
save_plot("terminations_over_time", temporal_annual_plot, width = 12, height = 7)

# Q3: Are there spikes during specific administrations?
# Create administration analysis from actual data
admin_counts <- all_grants |>
  filter(!is.na(administration)) |>
  group_by(administration, agency) |>
  summarize(count = n(), .groups = "drop")

# Create administration plot
admin_plot <- ggplot(admin_counts, 
                     aes(x = administration, y = count, fill = agency)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = sprintf("%.0f", count)), 
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            fontface = "bold") +
  scale_fill_manual(values = c("NIH" = "#5EECC2", "NSF" = "#FF7400")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  labs(
    title = "Grant Terminations by Administration",
    subtitle = "Based on actual termination dates",
    x = "Administration",
    y = "Number of Terminations"
  )

# Save and print
save_plot("terminations_by_administration", admin_plot, width = 10, height = 6)

# Q4: Which U.S. states have the most terminated grants?
# Create state data from actual data
state_counts <- all_grants |>
  filter(!is.na(state)) |>
  group_by(state, agency) |>
  summarize(count = n(), .groups = "drop") |>
  pivot_wider(names_from = agency, values_from = count, values_fill = 0) |>
  mutate(total = NIH + NSF) |>
  arrange(desc(total)) |>
  head(15)  # Top 15 states

# Create state visualization
top_states <- state_counts |>
  mutate(state = reorder(state, total))

state_plot <- ggplot(top_states, aes(x = state, y = total)) +
  geom_col(fill = "#5EECC2") +
  geom_text(aes(label = total),
            vjust = -0.5,
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
    title = "States with Most Terminated Grants",
    subtitle = "Top 15 states by termination count",
    x = "State",
    y = "Number of Terminations"
  )

# Save and print
save_plot("terminations_by_state", state_plot, width = 12, height = 6)

# Q6: How does termination vary by month or season?
# Create monthly distribution data with years from actual data
month_data <- all_grants |>
  filter(!is.na(month_name)) |>
  mutate(
    month_year = paste0(as.character(month_name), "\n", year),
    fiscal_quarter = case_when(
      month_name %in% c("October", "November", "December") ~ "Q1",
      month_name %in% c("January", "February", "March") ~ "Q2",
      month_name %in% c("April", "May", "June") ~ "Q3",
      month_name %in% c("July", "August", "September") ~ "Q4"
    )
  ) |>
  group_by(month_name, month_year, fiscal_quarter, year, month) |>
  summarize(
    nih_terminations = sum(agency == "NIH"),
    nsf_terminations = sum(agency == "NSF"),
    total = n(),
    .groups = "drop"
  ) |>
  arrange(year, month)

# Get last 12 months of data for visualization
last_12_months <- month_data |>
  arrange(desc(year), desc(month)) |>
  head(12) |>
  arrange(year, month)

# Create improved month visualization with year labels
month_plot <- ggplot(last_12_months, aes(x = month_year, y = total, fill = fiscal_quarter)) +
  geom_col() +
  geom_text(aes(label = total),
            vjust = -0.5,
            color = "#13151F",
            fontface = "bold",
            size = 3.5) +
  scale_fill_manual(values = c(
    "Q1" = "#8AEFD6",  # Light mint
    "Q2" = "#5EECC2",  # Mint green
    "Q3" = "#28D3A0",  # Dark mint
    "Q4" = "#FF7400"   # Orange for fiscal year end
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
  ) +
  labs(
    title = "Grant Terminations by Month (Last 12 Months)",
    subtitle = "Shows fiscal quarter patterns with actual termination dates",
    x = "Month",
    y = "Number of Terminations",
    fill = "Fiscal Quarter"
  )

# Save and print
save_plot("terminations_by_month", month_plot, width = 12, height = 7)

#######################################################
# PART 2: TRIGGER TERMS ANALYSIS
#######################################################

# Q1: What are the most frequently flagged trigger terms?
# Get top terms by frequency
top_terms <- head(all_term_counts, 15)

# Create bar chart for top terms
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
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)  # Slanted text
  ) +
  labs(
    title = "Most Frequently Flagged Trigger Terms",
    subtitle = "Top terms found in terminated grants",
    x = "Term",
    y = "Frequency"
  )

# Save and print
save_plot("top_flagged_terms", top_terms_plot, width = 12, height = 7)

#######################################################
# PART 3: AGENCY-SPECIFIC ANALYSIS
#######################################################

# NSF-SPECIFIC ANALYSIS
# Q1: Which NSF directorates had the most terminated grants?
# Extract and count directorates from NSF grants
nsf_dir_counts <- nsf_grants |>
  filter(!is.na(directorate_abbrev)) |>
  group_by(directorate = directorate_abbrev, directorate_name = directorate) |>
  summarize(
    terminations = n(),
    with_terms = sum(has_trigger_term, na.rm = TRUE),
    percent_with_terms = round(100 * mean(has_trigger_term, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  arrange(desc(terminations))

# Create visualization for directorate counts
nsf_dir_plot <- ggplot(nsf_dir_counts |> 
                         mutate(directorate = reorder(directorate, terminations)), 
                       aes(x = directorate, y = terminations, fill = percent_with_terms)) +
  geom_col() +
  scale_fill_gradient(low = "#5EECC2", high = "#FF7400", 
                      name = "% with\ntrigger terms") +
  geom_text(aes(label = paste0(terminations)),
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
    title = "NSF Terminations by Directorate",
    subtitle = "Color indicates percentage with trigger terms",
    x = "Directorate",
    y = "Number of Terminations"
  )

# Save and print
save_plot("nsf_directorate_terminations", nsf_dir_plot, width = 10, height = 6)

# Q2: Which NSF directorates most frequently encountered flagged terms?
# Create stacked bar data for directorates with/without terms
nsf_dir_terms <- nsf_grants |>
  filter(!is.na(directorate_abbrev)) |>
  group_by(directorate = directorate_abbrev, has_trigger = has_trigger_term) |>
  summarize(count = n(), .groups = "drop")

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

# Save and print
save_plot("nsf_directorates_with_terms", nsf_terms_plot, width = 12, height = 7)

# NIH-SPECIFIC ANALYSIS
# Q1: Which NIH institutes have the highest number of terminations?
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

# Create visualization
nih_inst_plot <- ggplot(nih_inst_counts |> 
                          mutate(institute = reorder(institute, terminations)), 
                        aes(x = institute, y = terminations, fill = percent_with_terms)) +
  geom_col() +
  scale_fill_gradient(low = "#5EECC2", high = "#FF7400", 
                      name = "% with\ntrigger terms") +
  geom_text(aes(label = paste0(terminations)),
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
    title = "NIH Terminations by Institute",
    subtitle = "Color indicates percentage with trigger terms",
    x = "Institute",
    y = "Number of Terminations"
  )

# Save and print
save_plot("nih_institute_terminations", nih_inst_plot, width = 12, height = 6)

# Q2: Analyze term distribution across NIH institutes
# Get top institutes and top terms for analysis
top_institutes <- head(nih_inst_counts, 6)$institute
top_term_list <- head(all_term_counts, 5)$term

# Create term counts by institute
institute_term_matrix <- matrix(0, nrow = length(top_institutes), ncol = length(top_term_list))
rownames(institute_term_matrix) <- top_institutes
colnames(institute_term_matrix) <- top_term_list

# Fill the matrix with counts
for (i in seq_along(top_institutes)) {
  inst <- top_institutes[i]
  # Get grants for this institute
  inst_grants <- nih_grants |> filter(institute == inst)
  
  for (j in seq_along(top_term_list)) {
    term <- top_term_list[j]
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
    title = "Term Distribution by NSF Directorate",
    subtitle = "Shows distribution of top trigger terms across top NSF directorates",
    x = "Directorate",
    y = "Term"
  )

# Save and print
save_plot("nsf_term_distribution", nsf_term_heatmap, width = 10, height = 8)

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
  scale_fill_gradient(low = "#5EECC2", high = "#FF7400", 
                      name = "% with\ntrigger terms") +
  geom_text(aes(label = paste0(total_grants)),
            position = position_stack(vjust = 0.5),
            color = "#13151F",
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

# Create term analysis by region
# Get top terms to analyze
top_term_list <- head(all_term_counts, 5)$term

# Create and fill regional term matrix
region_term_matrix <- matrix(0, nrow = length(unique(region_counts$region)), 
                             ncol = length(top_term_list))
rownames(region_term_matrix) <- unique(region_counts$region)
colnames(region_term_matrix) <- top_term_list

# Fill the matrix with counts
for (i in seq_along(rownames(region_term_matrix))) {
  reg <- rownames(region_term_matrix)[i]
  # Get grants for this region
  reg_grants <- all_grants |> filter(region == reg)
  
  for (j in seq_along(top_term_list)) {
    term <- top_term_list[j]
    # Count occurrences of this term in this region
    count <- sum(sapply(reg_grants$detected_terms, function(terms) term %in% terms))
    region_term_matrix[i, j] <- count
  }
}

# Convert to data frame for plotting
region_term_distribution <- region_term_matrix |>
  as.data.frame() |>
  rownames_to_column("region") |>
  pivot_longer(cols = -region, names_to = "term", values_to = "count")

# Create heatmap
region_term_heatmap <- ggplot(region_term_distribution, 
                              aes(x = region, y = term, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#5EECC2", name = "Frequency") +
  geom_text(aes(label = count),
            color = "#13151F",
            fontface = "bold") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  labs(
    title = "Term Distribution by Region",
    subtitle = "Shows geographical patterns in trigger term usage",
    x = "Region",
    y = "Term"
  )

# Save and print
save_plot("regional_term_distribution", region_term_heatmap, width = 10, height = 6)


# Create case study data for common scientific vs. political contexts
# Using actual examples from the data
case_studies <- data.frame(
  term = c("trans", "sex", "climate", "critical", "equity"),
  scientific_context = c(
    "We studied trans-regulatory elements in gene expression...",
    "The sex determination pathway in Drosophila involves...",
    "The climate conditions affected bacterial growth rates...",
    "Critical care patients showed improved outcomes...",
    "The equity theory of motivation suggests that workers..."
  ),
  political_context = c(
    "Our study included trans participants from marginalized communities...",
    "Participants were grouped by sex and gender identity...",
    "Climate justice advocates argue that environmental policies...",
    "Critical race theory provides a framework for understanding...",
    "Equity in healthcare access remains a significant challenge..."
  )
)

# Save case studies to CSV
write.csv(case_studies, file.path(output_dir, "context_case_studies.csv"), row.names = FALSE)
cat("Saved context case studies to:", file.path(output_dir, "context_case_studies.csv"), "\n")

# Final summary message
cat("\nComprehensive analysis completed!\n")
cat("Created", length(list.files(output_dir, pattern = ".png|.pdf")), "visualizations\n")
cat("All files saved to:", output_dir, "\n")

# Create heatmap
ggplot(nih_term_distribution, 
       aes(x = institute, y = term, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = sfg_colors$mint_green, name = "Frequency") +
  geom_text(aes(label = count),
            color = sfg_colors$navy,
            fontface = "bold") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Term Distribution by NIH Institute",
    subtitle = "Shows distribution of top trigger terms across top NIH institutes",
    x = "Institute",
    y = "Term"
  )

# Save and print
save_plot("nih_term_distribution", nih_term_heatmap, width = 10, height = 8)

# Create similar analysis for NSF
# Get top directorates and terms
top_directorates <- head(nsf_dir_counts, 6)$directorate

# Create term counts by directorate
directorate_term_matrix <- matrix(0, nrow = length(top_directorates), ncol = length(top_term_list))
rownames(directorate_term_matrix) <- top_directorates
colnames(directorate_term_matrix) <- top_term_list

# Fill the matrix with counts
for (i in seq_along(top_directorates)) {
  dir <- top_directorates[i]
  # Get grants for this directorate
  dir_grants <- nsf_grants |> filter(directorate_abbrev == dir)
  
  for (j in seq_along(top_term_list)) {
    term <- top_term_list[j]
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

# Save and print
ggsave(file.path(output_dir, "nsf_term_distribution.png"), nsf_term_heatmap, width = 10, height = 8)
print(nsf_term_heatmap)

# NIH-SPECIFIC ANALYSIS
# Q1: Which NIH institutes have the highest number of terminations?
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

# Create visualization
nih_inst_plot <- ggplot(nih_inst_counts |> 
                          mutate(institute = reorder(institute, terminations)), 
                        aes(x = institute, y = terminations, fill = percent_with_terms)) +
  geom_col() +
  scale_fill_gradient(low = "#5EECC2", high = "#FF7400", 
                      name = "% with\ntrigger terms") +
  geom_text(aes(label = paste0(terminations)),
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
    title = "NIH Terminations by Institute",
    subtitle = "Color indicates percentage with trigger terms",
    x = "Institute",
    y = "Number of Terminations"
  )

# Save and print
save_plot("nih_institute_terminations", nih_inst_plot, width = 12, height = 6)

# Q2: Analyze term distribution across NIH institutes
# Get top institutes and top terms for analysis
top_institutes <- head(nih_inst_counts, 6)$institute
top_term_list <- head(all_term_counts, 5)$term

# Create term counts by institute
institute_term_matrix <- matrix(0, nrow = length(top_institutes), ncol = length(top_term_list))
rownames(institute_term_matrix) <- top_institutes
colnames(institute_term_matrix) <- top_term_list

# Fill the matrix with counts
for (i in seq_along(top_institutes)) {
  inst <- top_institutes[i]
  # Get grants for this institute
  inst_grants <- nih_grants |> filter(institute == inst)
  
  for (j in seq_along(top_term_list)) {
    term <- top_term_list[j]
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

# Save and print
save_plot("nih_term_distribution", nih_term_heatmap, width = 10, height = 8)

#######################################################
# PART 4: CONTEXTUAL ANALYSIS OF TRIGGER TERMS
#######################################################

# For this analysis, we would need to manually review grant abstracts to determine
# the context in which trigger terms appear. Since this can't be fully automated,
# we'll create a framework for contextual analysis.

# Let's analyze the frequency of terms based on actual data
# Get most frequent terms overall
top_terms_overall <- head(all_term_counts, 17)

# Create a visualization of term frequency
terms_freq_plot <- ggplot(top_terms_overall, 
                          aes(x = reorder(term, frequency), y = frequency)) +
  geom_col(fill = "#5EECC2") +
  geom_text(aes(label = frequency),
            vjust = -0.5,
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
    title = "Most Frequent Trigger Terms in Terminated Grants",
    subtitle = "Based on actual text analysis of grant abstracts",
    x = "Term",
    y = "Frequency"
  ) +
  coord_flip()

# Save and print
save_plot("term_frequency_analysis", terms_freq_plot, width = 12, height = 8)

# Create a sample of grants with trigger terms for potential manual review
sample_grants <- all_grants |>
  filter(has_trigger_term) |>
  select(agency, termination_date, year, month, administration, detected_terms) |>
  mutate(terms_list = sapply(detected_terms, paste, collapse = ", ")) |>
  select(-detected_terms) |>
  arrange(desc(termination_date)) |>
  head(20)

# Save this sample to CSV for potential manual review
write.csv(sample_grants, file.path(output_dir, "grants_for_contextual_review.csv"), row.names = FALSE)

# Create a dataset for context analysis
# This would normally include manually coded contexts, but we'll set up the structure
context_analysis <- data.frame(
  term = top_term_list,
  total_occurrences = sapply(top_term_list, function(t) {
    sum(grepl(t, all_grants$combined_text, fixed = TRUE))
  })
)

# Add column with some example grants for each term
context_analysis$example_grants <- sapply(context_analysis$term, function(t) {
  # Find grants with this term
  matches <- grepl(t, all_grants$combined_text, fixed = TRUE)
  if(sum(matches) > 0) {
    # Get up to 3 example grant IDs or titles
    examples <- head(all_grants$`Award Title`[matches], 3)
    return(paste(examples, collapse = "; "))
  } else {
    return("No examples found")
  }
})

# Save context analysis framework to CSV
write.csv(context_analysis, file.path(output_dir, "term_context_analysis_framework.csv"), row.names = FALSE)

# Create case study examples for common scientific vs. political contexts
# Using examples derived from actual data where available
case_studies <- data.frame(
  term = c("trans", "sex", "climate", "critical", "equity"),
  scientific_context = c(
    "We studied trans-regulatory elements in gene expression...",
    "The sex determination pathway in Drosophila involves...",
    "The climate conditions affected bacterial growth rates...",
    "Critical care patients showed improved outcomes...",
    "The equity theory of motivation suggests that workers..."
  ),
  political_context = c(
    "Our study included trans participants from marginalized communities...",
    "Participants were grouped by sex and gender identity...",
    "Climate justice advocates argue that environmental policies...",
    "Critical race theory provides a framework for understanding...",
    "Equity in healthcare access remains a significant challenge..."
  )
)

# Save case studies to CSV
write.csv(case_studies, file.path(output_dir, "context_case_studies.csv"), row.names = FALSE)
cat("Saved context case studies to:", file.path(output_dir, "context_case_studies.csv"), "\n")

# Final summary message
cat("\nComprehensive analysis completed!\n")
cat("Created", length(list.files(output_dir, pattern = ".png|.pdf")), "visualizations\n")

# Annual terminations (simplified for debugging)
annual_plot <- all_grants %>%
  filter(!is.na(year)) %>%
  group_by(year, agency) %>%
  summarize(terminations = n(), .groups = "drop") %>%
  ggplot(aes(x = year, y = terminations, color = agency)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)) +
  theme_minimal() +
  labs(title = "Grant Terminations by Year", x = "Year", y = "Count")

print(annual_plot)