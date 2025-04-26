# Science for Good Visualization Style
# This script creates custom themes and color palettes for the Science for Good brand

library(ggplot2)
library(scales)
library(extrafont)
library(systemfonts)

# Import Inter font if available (you may need to install it on your system first)
# font_import(pattern = "Inter")
# loadfonts(device = "win")  # For Windows
# loadfonts(device = "pdf")  # For saving to PDF

# Science for Good Brand Colors
sfg_colors <- list(
  mint_green = "#5EECC2",
  orange = "#FF7400",
  white = "#FFFFFF",
  navy = "#13151F",
  light_gray = "#F5F5F5"
)

# Create color palettes for different visualization types
sfg_palette_main <- c(sfg_colors$mint_green, sfg_colors$orange)
sfg_palette_sequential <- colorRampPalette(c(sfg_colors$mint_green, sfg_colors$white))(5)
sfg_palette_diverging <- colorRampPalette(c(sfg_colors$orange, sfg_colors$white, sfg_colors$mint_green))(7)
sfg_palette_categorical <- c(
  sfg_colors$mint_green,
  sfg_colors$orange,
  "#8AEFD6",  # Lighter mint
  "#FFB366",  # Lighter orange
  "#28D3A0",  # Darker mint 
  "#CC5D00"   # Darker orange
)

# Function to get color palette based on type needed
get_sfg_palette <- function(palette_type = "main", n = NULL) {
  switch(palette_type,
         "main" = sfg_palette_main,
         "sequential" = sfg_palette_sequential,
         "diverging" = sfg_palette_diverging,
         "categorical" = sfg_palette_categorical
  )
}

# Create Science for Good theme for ggplot2
theme_sfg <- function(base_size = 14, base_family = "Inter", dark_mode = FALSE) {
  # Determine colors based on mode
  bg_color <- ifelse(dark_mode, sfg_colors$navy, sfg_colors$white)
  text_color <- ifelse(dark_mode, sfg_colors$white, sfg_colors$navy)
  grid_color <- ifelse(dark_mode, "#252833", sfg_colors$light_gray)
  
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Basic elements
      plot.background = element_rect(fill = bg_color, color = NA),
      panel.background = element_rect(fill = bg_color, color = NA),
      panel.grid.major = element_line(color = grid_color, size = 0.2),
      panel.grid.minor = element_line(color = grid_color, size = 0.1),
      
      # Text elements
      text = element_text(color = text_color, family = base_family),
      plot.title = element_text(
        family = base_family, 
        face = "bold",
        size = base_size * 1.5,
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        family = base_family,
        size = base_size * 1.1,
        margin = margin(b = 15)
      ),
      axis.title = element_text(
        family = base_family,
        face = "bold",
        size = base_size * 1.1
      ),
      axis.text = element_text(
        family = base_family,
        size = base_size * 0.9
      ),
      legend.title = element_text(
        family = base_family,
        face = "bold",
        size = base_size * 1.0
      ),
      legend.text = element_text(
        family = base_family,
        size = base_size * 0.9
      ),
      
      # Other elements
      legend.position = "bottom",
      legend.background = element_rect(fill = bg_color, color = NA),
      plot.margin = margin(15, 15, 15, 15)
    )
}

# Function to create a bar chart with Science for Good styling
sfg_bar_chart <- function(data, x, y, fill = NULL, title = NULL, subtitle = NULL, 
                          x_label = NULL, y_label = NULL, dark_mode = FALSE) {
  
  # Create the plot
  p <- ggplot(data, aes_string(x = x, y = y)) +
    theme_sfg(dark_mode = dark_mode)
  
  # Add bars with appropriate fill
  if (!is.null(fill)) {
    p <- p + geom_col(aes_string(fill = fill))
    
    # Add appropriate color scale
    if (length(unique(data[[fill]])) <= 2) {
      p <- p + scale_fill_manual(values = sfg_palette_main)
    } else {
      p <- p + scale_fill_manual(values = sfg_palette_categorical)
    }
  } else {
    p <- p + geom_col(fill = sfg_colors$mint_green)
  }
  
  # Add labels
  p <- p + 
    labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label
    )
  
  return(p)
}

# Function to create a line chart with Science for Good styling
sfg_line_chart <- function(data, x, y, color = NULL, title = NULL, subtitle = NULL, 
                           x_label = NULL, y_label = NULL, dark_mode = FALSE) {
  
  # Create the plot
  p <- ggplot(data, aes_string(x = x, y = y)) +
    theme_sfg(dark_mode = dark_mode)
  
  # Add lines with appropriate color
  if (!is.null(color)) {
    p <- p + 
      geom_line(aes_string(color = color), size = 1.2) +
      geom_point(aes_string(color = color), size = 3)
    
    # Add appropriate color scale
    if (length(unique(data[[color]])) <= 2) {
      p <- p + scale_color_manual(values = sfg_palette_main)
    } else {
      p <- p + scale_color_manual(values = sfg_palette_categorical)
    }
  } else {
    p <- p + 
      geom_line(color = sfg_colors$mint_green, size = 1.2) +
      geom_point(color = sfg_colors$mint_green, size = 3)
  }
  
  # Add labels
  p <- p + 
    labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label
    )
  
  return(p)
}

# Function to create a pie chart with Science for Good styling
sfg_pie_chart <- function(data, values, categories, title = NULL, subtitle = NULL, 
                          dark_mode = FALSE) {
  
  # Create the plot
  p <- ggplot(data, aes_string(x = "", y = values, fill = categories)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_sfg(dark_mode = dark_mode) +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()
    )
  
  # Add appropriate color scale
  if (length(unique(data[[categories]])) <= 2) {
    p <- p + scale_fill_manual(values = sfg_palette_main)
  } else {
    p <- p + scale_fill_manual(values = sfg_palette_categorical)
  }
  
  # Add labels
  p <- p + 
    labs(
      title = title,
      subtitle = subtitle,
      fill = NULL
    )
  
  return(p)
}

# Function to apply SFG styling to any ggplot
apply_sfg_style <- function(plot, dark_mode = FALSE) {
  plot + theme_sfg(dark_mode = dark_mode)
}

# Example usage
if (FALSE) {
  # Sample data
  df <- data.frame(
    category = c("Scientific Context", "Political Context", "Ambiguous"),
    percentage = c(60, 32, 8)
  )
  
  # Create bar chart
  bar_example <- sfg_bar_chart(
    data = df,
    x = "category",
    y = "percentage",
    title = "Classification of Trigger Terms",
    subtitle = "Percentage of contexts by classification",
    x_label = "Classification",
    y_label = "Percentage",
    dark_mode = TRUE
  )
  
  # Create pie chart
  pie_example <- sfg_pie_chart(
    data = df,
    values = "percentage",
    categories = "category",
    title = "Distribution of Term Classifications",
    subtitle = "Analysis of scientific vs. political contexts",
    dark_mode = TRUE
  )
  
  # Display the plots
  print(bar_example)
  print(pie_example)
  
  # Save the plots with appropriate styling
  ggsave("bar_example.png", bar_example, width = 10, height = 6)
  ggsave("pie_example.png", pie_example, width = 8, height = 8)
}