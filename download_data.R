# Load required libraries
library(dplyr)
library(stringr)
library(readr)
library(tidyr)

# Set locale for date parsing
Sys.setlocale(locale = "en_US.UTF-8")

# Define constants
gsheet_id <- "1QZj6YgHAJ9NmFXFPCtu-i-1NDuDmAdMF2Wogts7S2_4" #'1iQpZmykHOFpVaNGzI3-1ycVIbWLKkx5KFWbmiejkT98' test sheet
# Using the direct export link with sheet-specific 'gid's to avoid Google's gviz API bug.
sheet_gids <- c(
  "Collection_data" = "900206579",
  "Photo_links"     = "439406691",
  "CRISPR"          = "952436162",
  "Insectary_data"  = "402580526",
  "Insectary_stocks" = "1546273864",
  "Lists"            = "731131660"
)
sheets <- names(sheet_gids)
rds_paths <- setNames(paste0(sheets, ".rds"), sheets)

# Function to download and save data
download_and_save_data <- function(show_toasts = FALSE) {
  # Download data from Google Sheets using the more reliable 'export' URL
  data_list <- lapply(sheets, function(sheet_name) {
    if (show_toasts) {
      showNotification(paste("Downloading", sheet_name, "sheet..."), 
                       type = "message", 
                       duration = NULL,
                       id = "downloading")
    }
    
    # Construct the more reliable export URL
    gid <- sheet_gids[sheet_name]
    url <- paste0("https://docs.google.com/spreadsheets/d/", gsheet_id, "/export?format=csv&gid=", gid)
    
    df <- read_csv(url, col_types = cols(.default = "c"), na = c(""))
    
    return(df)
  })
  
  # Name the data frames
  names(data_list) <- sheets
  
  # Normalize explicit "NA" strings so they remain as text and not converted to real NA
  normalize_na_strings <- function(df) {
    for (cn in names(df)) {
      # Operar solo sobre columnas de texto
      if (is.character(df[[cn]])) {
        # Reemplazar NAs reales por "NA" si en el origen eran "NA" textual
        # y asegurar que cadenas vacías sigan vacías
        vals <- df[[cn]]
        vals[!is.na(vals) & trimws(toupper(vals)) == "NA"] <- "NA"
        df[[cn]] <- vals
      }
    }
    df
  }

  # Save as RDS files (tras normalizar "NA" textuales)
  data_list <- lapply(data_list, normalize_na_strings)
  mapply(saveRDS, data_list, rds_paths)
  
  if (show_toasts) {
    showNotification("All sheets downloaded and saved successfully!", 
                     type = "message", 
                     duration = 5,
                     id = "downloading")
  } else {
    print("Data downloaded and saved successfully!")
  }
  
  return(data_list)
}

# Execute the download only if run directly (not sourced)
if (!interactive()) {
  download_and_save_data(show_toasts = FALSE)
}