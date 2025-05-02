# Create annual terminations plot
create_annual_terminations_plot <- function(all_grants) {
  # Calculate annual terminations
  annual_terminations <- all_grants |>
    filter(!is.na(year)) |>
    group_by(year, agency) |>
    summarize(terminations = n(), .groups = "drop") |>
    arrange(agency, year)
  
  # Print debugging info
  cat("Annual terminations data summary:\n")
  print(summary(annual_terminations))
  
  # Administration backgrounds
  administrations <- data.frame(
    admin = c("Obama", "Trump", "Biden", "Trump 2.0"),
    start_date = as.Date(c("2009-01-20", "2017-01-20", "2021-01-20", "2025-01-20")),
    end_date = as.Date(c("2017-01-20", "2021-01-20", "2025-01-20", "2029-01-20")),
    color = c("blue", "red", "blue", "red")
  )
  
  # Filter to reasonable year range
  year_range <- range(annual_terminations$year, na.rm = TRUE)
  year_range[1] <- max(2016, year_range[1]) # Start at least from 2016
  year_range[2] <- min(2025, year_range[2]) # End at most at 2025
  
  # Debug year range
  cat("Year range:", year_range[1], "to", year_range[2], "\n")
  
  # Create plot
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
  
  return(temporal_annual_plot)
}

# Create administration-based analysis
create_administration_plot <- function(all_grants) {
  # Calculate data
  admin_counts <- all_grants |>
    filter(!is.na(administration)) |>
    group_by(administration, agency) |>
    summarize(count = n(), .groups = "drop")
  
  # Debug data
  cat("Administration counts:\n")
  print(admin_counts)
  
  # Create plot
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
  
  return(admin_plot)
}

# Create monthly analysis
create_monthly_plot <- function(all_grants) {
  # Calculate monthly data
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
  
  # Get last 12 months of data
  last_12_months <- month_data |>
    arrange(desc(year), desc(month)) |>
    head(12) |>
    arrange(year, month)
  
  # Debug
  cat("Last 12 months data (first few rows):\n")
  print(head(last_12_months))
  
  # Create plot
  month_plot <- ggplot(last_12_months, aes(x = month_name, y = total, fill = fiscal_quarter)) +
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
  
  return(month_plot)
}

# Create recent terminations visualization (13 months)
create_recent_terminations_plot <- function(all_grants) {
  # Calculate monthly data
  temporal_monthly_data <- all_grants |>
    filter(!is.na(termination_date)) |>
    mutate(
      date = floor_date(termination_date, "month"),
      is_quarter_end = month(termination_date) %in% c(3, 6, 9, 12)
    ) |>
    group_by(date, agency) |>
    summarize(terminations = n(), .groups = "drop") |>
    arrange(date)
  
  # Get most recent 13 months
  latest_months <- temporal_monthly_data |>
    filter(date > max(date, na.rm = TRUE) - months(13)) |>
    arrange(date, agency)
  
  # Debug
  cat("Recent 13 months data summary:\n")
  print(summary(latest_months))
  
  # Create plot
  temporal_monthly_plot <- ggplot(latest_months, 
                                  aes(x = date, y = terminations, color = agency, group = agency)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = c("NIH" = "#5EECC2", "NSF" = "#FF7400")) +
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
      subtitle = "Monthly terminations with trend lines",
      x = "Month",
      y = "Number of Terminations",
      color = "Agency"
    )
  
  return(temporal_monthly_plot)
}

# Create fiscal quarter analysis
create_quarterly_plot <- function(all_grants) {
  # Calculate quarterly data
  quarter_term_counts <- all_grants |>
    filter(!is.na(fiscal_quarter), !is.na(has_trigger_term)) |>
    group_by(fiscal_quarter) |>
    summarize(
      total_grants = n(),
      grants_with_terms = sum(has_trigger_term),
      percent_with_terms = round(100 * mean(has_trigger_term)),
      .groups = "drop"
    )
  
  # Debug
  cat("Fiscal quarter data:\n")
  print(quarter_term_counts)
  
  # Create plot
  quarter_plot <- ggplot(quarter_term_counts, 
                         aes(x = fiscal_quarter, y = total_grants, fill = percent_with_terms)) +
    geom_col() +
    scale_fill_gradient(low = "#5EECC2", high = "#FF7400", 
                        name = "% with\ntrigger terms") +
    geom_text(aes(label = total_grants),
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
      title = "Grant Terminations by Fiscal Quarter",
      subtitle = "Color indicates percentage with trigger terms",
      x = "Fiscal Quarter",
      y = "Number of Terminations"
    )
  
  return(quarter_plot)
}

# Create monthly term prevalence plots
create_monthly_term_prevalence_plot <- function(all_grants) {
  # Calculate monthly term prevalence
  monthly_term_prevalence <- all_grants |>
    filter(!is.na(termination_date), !is.na(has_trigger_term)) |>
    mutate(month_year = floor_date(termination_date, "month")) |>
    group_by(month_year) |>
    summarize(
      total_grants = n(),
      grants_with_terms = sum(has_trigger_term),
      percent_with_terms = 100 * grants_with_terms / total_grants,
      .groups = "drop"
    ) |>
    arrange(month_year) |>
    # Get last 24 months for visualization
    filter(month_year > max(month_year, na.rm = TRUE) - months(24))
  
  # Debug
  cat("Monthly term prevalence data summary:\n")
  print(summary(monthly_term_prevalence))
  
  # Create plot
  monthly_term_plot <- ggplot(monthly_term_prevalence, 
                              aes(x = month_year, y = percent_with_terms)) +
    geom_line(size = 1.2, color = "#5EECC2") +
    geom_point(size = 3, color = "#5EECC2") +
    geom_smooth(method = "loess", se = TRUE, color = "#FF7400", fill = "#FF7400", alpha = 0.2) +
    scale_x_date(
      date_breaks = "3 months",
      date_labels = "%b %Y",
      expand = c(0.02, 0.02)
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
      title = "Monthly Percentage of Grants with Trigger Terms",
      subtitle = "Trend over past 24 months with smoothed trend line",
      x = "Month",
      y = "Percentage of Grants with Trigger Terms"
    )
  
  return(monthly_term_plot)
}

# Create yearly term prevalence plot
create_yearly_term_prevalence_plot <- function(all_grants) {
  # Calculate yearly term prevalence
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
  
  # Debug
  cat("Yearly term prevalence data:\n")
  print(yearly_term_prevalence)
  
  # Create plot
  yearly_term_plot <- ggplot(yearly_term_prevalence, 
                             aes(x = year, y = percent_with_terms)) +
    geom_line(size = 1.2, color = "#5EECC2") +
    geom_point(size = 3, color = "#5EECC2") +
    geom_text(aes(label = sprintf("%.1f%%", percent_with_terms)),
              vjust = -1,
              color = "#13151F",
              fontface = "bold") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.title = element_text(size = 12, face = "bold")
    ) +
    labs(
      title = "Yearly Percentage of Grants with Trigger Terms",
      subtitle = "Trend over years",
      x = "Year",
      y = "Percentage of Grants with Trigger Terms"
    )
  
  return(yearly_term_plot)
}