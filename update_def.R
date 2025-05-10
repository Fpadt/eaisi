# update-defs.R
library(data.table)
library(magrittr)
library(fs)
library(stringr)
 
# notebooks <- data.table(
#   notebooks = lst <- list.files(
#     path = "notebooks",
#     pattern = "^0.*\\.qmd$",
#     full.names = FALSE
#   ) 
# ) %>%
#   .[, id:= substr(notebooks, 1, 2)] %>%
#   .[, target:= fs::path_ext_remove(notebooks) %>% 
#       fs::path_file() %>%
#       paste("def.qmd", sep = '_' )]
# 
# 
# 
# f_create_locl_def <- 
#   function(dt){
#     PN <- "notebooks"
#     src_defs <- "09_definitions.qmd"
# 
#     # read the master definitions
#     lines <- readLines(file.path(PN , src_defs), warn = FALSE)
#     
#     new_lines <- gsub(
#       pattern     = "#def-",
#       replacement = paste0("#def-", dt$id, "-"),
#       x           = lines,
#       perl        = TRUE
#     )    
#     
#     # write the per-notebook copy
#     writeLines(new_lines, file.path(PN, dt$target))
#     message("Wrote: ", dt$target)
#   }
# 
# for (n in 1:nrow(notebooks) ) {
#   f_create_locl_def(dt[n])
# }

# 1) Define a function that does the work for one notebook
rev_notebook <- function(
    nb_path, 
    defs_src = "notebooks/09_definitions.qmd",
    tg_path  = NULL) {

  f_fn <- function(file){
      file %>% path_ext_remove() %>% path_file()
    }
  
  # derive prefix from the first two chars of the filename (no extension)
  prefix    <- f_fn(nb_path) %>% substr(1, 2)
  if (missing(tg_path)) {
    dest_nb   <- file.path("nb", paste0(f_fn(nb_path), "_rev.qmd"))
    dest_defs <- file.path("nb", paste0(f_fn(nb_path), "_def.qmd"))
  } else {
    dest_nb   <- tg_path
    dest_defs <- tg_path
  }
  
  
  # 2) Read notebook and extract all @def-IDs
  lines <- readLines(nb_path, warn = FALSE)
  refs  <- str_extract_all(lines, "@def-[A-Za-z0-9_-]+") %>% unlist()
  refs  <- unique(refs)
  if (length(refs)==0) {
    fs::file_copy(nb_path , dest_nb)
    fs::file_copy(defs_src, dest_defs)
  }
  
  # 3) Build a data.table mapping
  dt <- data.table(
    old_id = str_remove(refs, "^@"),
    suffix = sub("^@def-", "", refs)
  )
  dt[, new_id    := paste0("def-", prefix, "-", suffix)]
  dt[, old_ref   := paste0("@", old_id)]
  dt[, new_ref   := paste0("@", new_id)]
  dt[, old_label := paste0("#", old_id)]
  dt[, new_label := paste0("#", new_id)]
  
  # 4) Rewrite the notebook
  new_lines <- lines
  for (i in seq_len(nrow(dt))) {
    new_lines <- str_replace_all(
      new_lines,
      fixed(dt$old_ref[i]), dt$new_ref[i]
    )
    new_lines <- str_replace_all(
      new_lines,
      fixed(dt$old_label[i]), dt$new_label[i]
    )
  }
  writeLines(new_lines, dest_nb)
  message("Wrote revised notebook to ", dest_nb)
  
  # 5) Extract matching definition blocks and rewrite IDs
  defs <- readLines(defs_src, warn = FALSE)
  out  <- character(0)
  for (i in seq_len(nrow(dt))) {
    pat  <- paste0("#", dt$old_id[i], "\\b")
    # find start of block
    start <- which(str_detect(defs, paste0("^:::.*", pat)))[1]
    if (is.na(start)) next
    # find end of block
    end   <- which(defs[(start+1):length(defs)] == ":::")[1] + start
    block <- defs[start:end]
    # rewrite the ID inside the block
    block <- str_replace_all(block,
                             fixed(paste0("#", dt$old_id[i])),
                             paste0("#", dt$new_id[i])
    )
    out <- c(out, block, "")
  }
  writeLines(out, dest_defs)
  message("Wrote local definitions to ", dest_defs)
}

# 6) Call the function on each notebook you care about:
notebooks <- list.files(
  path       = "notebooks",
  pattern    = "^[01].*\\.qmd$",
  full.names = TRUE
)                               %>% 
  .[ !grepl("_rev\\.qmd$", .) ] %>%
  .[ !grepl("_def\\.qmd$", .) ] %>%
  .[ !grepl("09_definiti", .) ] 

lapply(list.files("nb", full.names=TRUE), file_delete)
invisible(lapply(notebooks, rev_notebook))
# rev_notebook(
#   nb_path = "index_src.qmd",
#   tg_path = "index.qmd",
#   defs_src = "notebooks/09_definitions.qmd"
# )