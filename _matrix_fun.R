library(data.table)

# Example data
set.seed(123)
dt <- data.table(
  P1 = sample(letters[sample(1:26,5,TRUE)], 100, TRUE),
  P2 = sample(letters[sample(1:26,5,TRUE)], 100, TRUE),
  P3 = sample(letters[5:6], 100, TRUE),
  P4 = sample(letters[7:8], 100, TRUE),
  C1 = sample(LETTERS[sample(1:26,5,TRUE)], 100, TRUE),
  C2 = sample(LETTERS[sample(1:26,5,TRUE)], 100, TRUE),
  C3 = sample(LETTERS[5:6], 100, TRUE),
  C4 = sample(LETTERS[7:8], 100, TRUE),
  Q = sample(size = 100, x=1:100, replace = FALSE)
)

# Function to get summary table for a P and C pair
summarize_matrix <- function(dt, P, C, summary = c("unique", "mean", "sd")) {
  stopifnot(P %in% names(dt), C %in% names(dt))
  summary <- match.arg(summary)
  
  if (summary == "unique") {
    result <- dt[, .(n_unique = uniqueN(paste0(P, C))), by = c(P, C)]
  } else {
    result <- dt[, .(mean_Q = mean(Q, na.rm = TRUE)), by = c(P, C)]
}
  
  dcast(
    result, 
    formula   = as.formula(paste(C, "~", P)), 
    value.var = ifelse(summary == "unique", "n_unique", "mean_Q"))
}

# Example usage:
mat_P1_C1 <- summarize_matrix(dt, "P1", "C1", summary = "unique")
mat_P2_C2 <- summarize_matrix(dt, "P2", "C2", summary = "mean")




