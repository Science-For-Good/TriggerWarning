# Create monthly terminations visualization
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

# Create daily terminations visualization (NEW)
create_daily_plot <- function(all_grants) {
  # Calculate day of week data
  day_data <- all_grants |>
    filter(!is.na(termination_date)) |>
    mutate(
      day_of_week = weekdays(termination_date),
      # Ensure proper ordering of days
      day_of_week = factor(day_of_week, 
                           levels = c("Monday", "Tuesday", "Wednesday", 
                                      "Thursday", "Friday", "Saturday", "Sunday"))
    ) |>
    group_by(day_of_week) |>
    summarize(
      total_grants = n(),
      grants_with_terms = sum(has_trigger_term, na.rm = TRUE),
      percent_with_terms = round(100 * mean(has_trigger_term, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # Debug
  cat("Day of week data:\n")
  print(day_data)
  
  # Create plot
  day_plot <- ggplot(day_data, 
                     aes(x = day_of_week, y = total_grants, fill = percent_with_terms)) +
    geom_col() +
    scale_fill_gradient(low = "#5EECC2", high = "#FF7400", 
                        name = "% with\ntrigger terms") +
    geom_text(aes(label = total_grants),
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
      title = "Grant Terminations by Day of Week",
      subtitle = "Color indicates percentage with trigger terms",
      x = "Day of Week",
      y = "Number of Terminations"
    )
  
  return(day_plot)
}

# Create monthly aggregation visualization (NEW)
create_monthly_aggregation_plot <- function(all_grants) {
  # Calculate monthly aggregated data across all years
  monthly_aggregate <- all_grants |>
    filter(!is.na(month_name)) |>
    mutate(
      # Ensure proper month ordering
      month_name = factor(month_name, 
                          levels = c("January", "February", "March", "April", 
                                     "May", "June", "July", "August", 
                                     "September", "October", "November", "December"))
    ) |>
    group_by(month_name) |>
    summarize(
      total_grants = n(),
      grants_with_terms = sum(has_trigger_term, na.rm = TRUE),
      percent_with_terms = round(100 * mean(has_trigger_term, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # Debug
  cat("Monthly aggregate data:\n")
  print(monthly_aggregate)
  
  # Create plot
  monthly_agg_plot <- ggplot(monthly_aggregate, 
                             aes(x = month_name, y = total_grants, fill = percent_with_terms)) +
    geom_col() +
    scale_fill_gradient(low = "#5EECC2", high = "#FF7400", 
                        name = "% with\ntrigger terms") +
    geom_text(aes(label = total_grants),
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
      title = "Grant Terminations by Month (All Years)",
      subtitle = "Color indicates percentage with trigger terms",
      x = "Month",
      y = "Number of Terminations"
    )
  
  return(monthly_agg_plot)
}

# Create monthly time series visualization (NEW)
create_monthly_timeseries_plot <- function(all_grants, months_to_show = 24) {
  # Calculate monthly time series
  monthly_timeseries <- all_grants |>
    filter(!is.na(termination_date)) |>
    mutate(month_year = floor_date(termination_date, "month")) |>
    group_by(month_year) |>
    summarize(
      total_grants = n(),
      grants_with_terms = sum(has_trigger_term, na.rm = TRUE),
      percent_with_terms = round(100 * mean(has_trigger_term, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    arrange(desc(month_year)) |>
    head(months_to_show) |>
    arrange(month_year)
  
  # Debug
  cat("Monthly time series data (first few rows):\n")
  print(head(monthly_timeseries))
  
  # Create plot
  monthly_ts_plot <- ggplot(monthly_timeseries, 
                            aes(x = month_year, y = total_grants)) +
    geom_line(size = 1.2, color = "#28D3A0") +
    geom_point(aes(color = percent_with_terms), size = 4) +
    scale_color_gradient(low = "#5EECC2", high = "#FF7400", 
                         name = "% with\ntrigger terms") +
    scale_x_date(
      date_breaks = "2 months",
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
      title = paste0("Grant Terminations by Month (Last ", months_to_show, " Months)"),
      subtitle = "Point color indicates percentage with trigger terms",
      x = "Month",
      y = "Number of Terminations"
    )
  
  return(monthly_ts_plot)
}

# Create daily time series visualization
create_daily_timeseries_plot <- function(all_grants, days_to_show = 30) {
  # Calculate daily time series
  daily_timeseries <- all_grants |>
    filter(!is.na(termination_date)) |>
    # Group by exact date
    group_by(date = as.Date(termination_date)) |>
    summarize(
      total_grants = n(),
      grants_with_terms = sum(has_trigger_term, na.rm = TRUE),
      percent_with_terms = round(100 * mean(has_trigger_term, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    arrange(desc(date)) |>
    head(days_to_show) |>
    arrange(date)
  
  # Debug
  cat("Daily time series data (first few rows):\n")
  print(head(daily_timeseries))
  
  # Create plot
  daily_ts_plot <- ggplot(daily_timeseries, 
                          aes(x = date, y = total_grants)) +
    geom_line(size = 1, color = "#28D3A0", alpha = 0.7) +
    geom_point(aes(color = percent_with_terms), size = 3) +
    scale_color_gradient(low = "#5EECC2", high = "#FF7400", 
                         name = "% with\ntrigger terms") +
    scale_x_date(
      date_breaks = "5 days",
      date_labels = "%b %d",
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
      title = paste0("Grant Terminations by Day (Last ", days_to_show, " Days)"),
      subtitle = "Point color indicates percentage with trigger terms",
      x = "Date",
      y = "Number of Terminations"
    )
  
  return(daily_ts_plot)
}