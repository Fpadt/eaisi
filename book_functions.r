library(gt)
library(scales)    # for color scales if you want them

# 1) Pythia Color palette
pythia_colors <- c(
  primary    = "#fff200",  # bright yellow
  accent     = "#E2A348",  # orange
  success    = "#38e56d",  # light green
  success2   = "#089b35",  # darker green
  success3   = "#0f5e3c",  # darkest green
  gray_text  = "#333333",  # dark gray
  black_text = "#000000"   # black
)

# 2) The pythia theme function
gt_theme_pythia <- function(gt_object) {
  gt_object %>%
    # Global options
    tab_options(
      table.background.color        = pythia_colors["primary"],
      table.border.top.color        = pythia_colors["black_text"],
      table.border.bottom.color     = pythia_colors["black_text"],
      table.font.size               = px(12),
      table.font.color              = pythia_colors["gray_text"],
      heading.title.font.size       = px(20),
      heading.subtitle.font.size    = px(14),
      heading.title.font.weight     = "bold",
      heading.subtitle.font.weight  = "normal",
      row.striping.background_color = pythia_colors["accent"]
    ) %>%
    
    # Style column labels
    tab_style(
      style = cell_text(weight = "bold",
                        color  = pythia_colors["black_text"]),
      locations = cells_column_labels()
    ) %>%
    
    # Optional: stripe every other row
    opt_row_striping(
      row_striping = TRUE,
      row_striping_background = pythia_colors["accent"]
    ) %>%
    
    # Optional: color‐scale one numeric column
    # (repeat or generalize for as many columns as you like)
    data_color(
      columns   = where(is.numeric),
      colors    = col_numeric(
        palette = c(pythia_colors["success3"], pythia_colors["success"]),
        domain  = NULL
      )
    )
}

