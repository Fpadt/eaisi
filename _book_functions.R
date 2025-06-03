# Functions for the Pythia book
# load required packages
library(padt)             # custom made library for Pythia's Advice - Data Tooling
library(scales)
library(collapsibleTree)  # for collapsible trees
library(treemapify)       # for treemaps
library(skimr)             # for skimr)
library(quarto)
library(gt)
library(DT)               # for datatables 
library(kableExtra)
# library(ggrepel)         # for ggrepel
library(openxlsx2)
library(magrittr)
library(ggplot2)
library(data.table)

# for color scales if you want them

# Get OneDrive paths from environment variables
odc   <- fs::path_abs(Sys.getenv("OneDriveConsumer"  , "")) #onedrive_consumer
# onedrive_commercial <- fs::path_abs(Sys.getenv("OneDriveCommercial", ""))

PDATA <- file.path(odc  , "ET", "pythia", "data")
ET2BB <- file.path(PDATA, "ET2BB.xlsx")
SCNGF <- file.path(PDATA, "scn_pythia.csv")

assign("FUNCTIONS_LOADED", TRUE, envir = .GlobalEnv)
# cat("Functions loaded at:", Sys.time(), "\n")

# 1) Pythia Color palette
pythia_colors <- c(
  primary    = "#ffffff",  # white
  accent1    = "#fff200",  # bright yellow
  accent2    = "#E2A348",  # orange
  light_grn  = "#38e56d",  # light green
  forest_grn = "#089b35",  # Forest Green
  castle_grn = "#0f5e3c",  # Castleton Green
  gray_text  = "#EEEEEE",  # light gray
  jetb_text  = "#333333",  # jet black
  black_text = "#000000"   # black
)

# 2) The pythia theme function
gt_theme_pythia <- function(gt_object) {
  gt_object %>%
    # Global options
    tab_options(
      table.background.color          = pythia_colors["primary"],
      table.border.top.color          = pythia_colors["gray_text"],
      table.border.bottom.color       = pythia_colors["gray_text"],
      table.font.size                 = px(12),
      table.font.color                = pythia_colors["jetb_text"],
      heading.title.font.size         = px(24),
      heading.subtitle.font.size      = px(18),
      heading.title.font.weight       = "normal",
      heading.subtitle.font.weight    = "normal",
      row.striping.background_color   = pythia_colors["gray_text"],
      row.striping.include_stub       = FALSE,
      row.striping.include_table_body = TRUE  
    ) %>%
    
    # Style column labels
    tab_style(
      style = cell_text(weight = "bold",
                        color  = pythia_colors["black_text"]),
      locations = cells_column_labels()
    ) %>%
    
    # stripe every other row
    opt_row_striping(
      row_striping = TRUE
    ) #%>%

  
    # Optional: color‐scale one numeric column
    # (repeat or generalize for as many columns as you like)
    # data_color(
    #   columns   = where(is.numeric),
    #   colors    = col_numeric(
    #     palette = c(pythia_colors["success3"], pythia_colors["success"]),
    #     domain  = NULL
    #   )
    # ) 
  # %>%
  #   
  #   tab_options(
  #     table.width = "auto", #pct(100),  # Use full allocated width
  #     table.layout = "auto", #"fixed"
  #   ) 
  # %>%
    # cols_width(everything() ~ px(100))
}


build_summary_matrix_pretty <- function(dt, metrics) {
  stopifnot(is.list(metrics), all(sapply(metrics, is.function)))
  
  # Fixed axes
  P_vars <- c("P1", "P2", "P3", "P4")
  C_vars <- c("C1", "C2", "C3", "C4")
  
  # Inner summary function
  build_summary_dt <- function(dt, P_fields, C_fields, metrics) {
    rbindlist(lapply(P_fields, function(p) {
      rbindlist(lapply(C_fields, function(c) {
        tmp <- dt[, .(agg = sum(Q)), by = .(get(p), get(c))]
        values <- lapply(metrics, function(f) f(tmp$agg))
        names(values) <- names(metrics)
        dt_out <- data.table(P_var = p, C_var = c)
        for (nm in names(values)) {
          dt_out[[nm]] <- values[[nm]]
        }
        dt_out
      }))
    }), use.names = TRUE, fill = TRUE)
  }
  
  # Compute long format and format combined cell
  res_long <- build_summary_dt(dt, P_vars, C_vars, metrics)
  
  metric_names <- names(metrics)
  # Combine values into formatted strings
  # res_long[, value := sprintf("(%.1f, %.1f)", mean, sd)]
  res_long[, value := sprintf("(μ=%.1f, σ=%.1f)", mean, sd)]  
  
  # Reshape to wide format
  res_wide <- dcast(res_long, C_var ~ P_var, value.var = "value")
  
  return(res_wide)
}

