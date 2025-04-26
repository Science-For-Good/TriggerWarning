# Load common libraries
load_required_libs <- function() {
  library(tidyverse)
  library(ggplot2)
  library(scales)
  library(lubridate)
  library(RColorBrewer)
}

# Define color palette
get_color_palette <- function() {
  list(
    mint_green = "#5EECC2",
    orange = "#FF7400",
    white = "#FFFFFF",
    navy = "#13151F",
    light_gray = "#F5F5F5"
  )
}

# Save plot function
save_plot <- function(filename, plot, width = 10, height = 6, output_dir) {
  ggsave(file.path(output_dir, paste0(filename, ".png")), plot, width = width, height = height)
  ggsave(file.path(output_dir, paste0(filename, ".pdf")), plot, width = width, height = height)
  print(plot)
}

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

# Create donut chart
create_donut_chart <- function(data, title, colors = c("#5EECC2", "#FF7400", "#8AEFD6")) {
  # Calculate percentages and positions
  data <- data |>
    mutate(fraction = count / sum(count),
           percentage = paste0(round(fraction * 100, 1), "%"),
           ymax = cumsum(fraction),
           ymin = c(0, head(ymax, n = -1)),
           labelPosition = (ymax + ymin) / 2,
           label = paste0(category, "\n", percentage))
  
  # Create the donut chart
  ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
    geom_rect() +
    geom_text(aes(x = 2, y = labelPosition, label = label), size = 3) +
    scale_fill_manual(values = colors) +
    coord_polar(theta = "y") +
    xlim(c(0, 4)) +
    theme_void() +
    theme(legend.position = "none") +
    labs(title = title)
}