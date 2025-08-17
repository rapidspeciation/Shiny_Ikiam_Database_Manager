# Ikiam_Insectary_DB_app.R — Merged Code

## Libraries -------------------------------------------------------------------
suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinyWidgets)
  library(DT)
  library(rhandsontable)
  library(dplyr)
  library(stringr)
  library(rlang)
  library(googlesheets4)
  library(cellranger)
  library(jsonlite)
  library(digest)
})

options(stringsAsFactors = FALSE)

## Auth (service account) ------------------------------------------------------
googlesheets4::gs4_auth(
  path   = "shiny-ikiam-db.json",
  scopes = "https://www.googleapis.com/auth/spreadsheets"
)

## External data loader --------------------------------------------------------
source("download_data.R")

## Constants & Schema ----------------------------------------------------------
APP_STATE_VERSION <- 2L

# Single, canonical UI date format (display + local storage):
# e.g., "14-Aug-25"
UI_DATE_FMT_R <- "%d-%b-%y"   # R's strftime format for "d-MMM-yy"
UI_DATE_REGEX <- "^\\d{1,2}-[A-Za-z]{3}-\\d{2}$"

# Centralized list of *data* date columns (single source of truth)
DATE_COLS <- c("Intro2Insectary_date", "Preservation_date", "Death_date")

# Tube columns registry (single source of truth)
TUBE_SLOTS <- 1:4
tube_col <- function(slot, kind = c("id","tissue","medium")){
  kind <- match.arg(kind)
  if (kind == "id")     return(paste0("Tube_", slot, "_id"))
  if (kind == "tissue") return(paste0("Tube_", slot, "_tissue"))
  # 'medium' exists for slots 1 and 2 in source data
  paste0("T", slot, "_Preservation_medium")
}
TUBE_ID_COLS     <- vapply(TUBE_SLOTS, tube_col, character(1), kind = "id")
TUBE_TISSUE_COLS <- vapply(TUBE_SLOTS, tube_col, character(1), kind = "tissue")

# Normalized list of acceptable inbound date formats (single source of truth)
DATE_FORMAT_CANDIDATES <- c(
  UI_DATE_FMT_R, "%Y-%m-%d", "%d-%b-%Y", "%Y/%m/%d",
  "%d/%m/%Y", "%m/%d/%Y", "%d-%m-%Y", "%m-%d-%Y"
)

# Common checks/utilities used across modules
is_date_col <- function(col) col %in% DATE_COLS
fmt_if_date <- function(v, col) if (is_date_col(col)) fmt_ui_date(v) else v

TISSUE_WHOLE <- "WHOLE_ORGANISM"

# Prefer a specific value if present, else fallback to the first element
pick_default_value <- function(vec, preferred) {
  if (length(vec) == 0) return(NULL)
  if (preferred %in% vec) preferred else vec[1]
}

# Small helper to render "no data" messages in DT tables
render_simple_table_message <- function(output, output_id, msg) {
  output[[output_id]] <- DT::renderDT(
    DT::datatable(
      data.frame(Mensaje = msg, check.names = FALSE),
      options = list(dom = 't'),
      rownames = FALSE
    )
  )
}

# Reusable "confirm on blur" handler for Selectize (Android-friendly)
# When the field loses focus, it selects the highlighted option;
# if there's exactly one match, it selects that one automatically.
SELECTIZE_CONFIRM_ONBLUR <- htmlwidgets::JS("
  function(){
    if(!this.items.length){
      var $a = this.$dropdown.find('.active');
      if ($a.length){
        this.addItem($a.attr('data-value'), true);
      } else {
        var q = this.getTextboxValue();
        if (q && this.search){
          var r = this.search(q);
          if (r && r.items && r.items.length === 1){
            this.addItem(r.items[0].id, true);
          }
        }
      }
    }
  }
")

## ---- Reusable Selectize option presets --------------------------------------
# Base multi/single ID pickers share the same UX knobs across tabs
SEL_OPTS_MULTI_IDS <- list(
  placeholder = "Seleccione uno o más IDs",
  maxOptions  = 5000, openOnFocus = TRUE,
  onBlur      = SELECTIZE_CONFIRM_ONBLUR,
  sortField   = NULL
)
SEL_OPTS_IDS <- list(
  placeholder = "Seleccione un ID",
  maxOptions  = 5000, openOnFocus = TRUE,
  onBlur      = SELECTIZE_CONFIRM_ONBLUR,
  sortField   = NULL
)

## ---- Helper: safe filenames (Windows-friendly) -----------------------------
safe_filename <- function(x) {
  # Replace characters illegal or awkward in filenames (\, /, :, *, ?, ", <, >, | and spaces)
  gsub("[^A-Za-z0-9._-]+", "_", as.character(x))
}

# ---- Commit snapshot helpers (single source of truth) -----------------------
DATA_DIR <- "data"
commit_file_path <- function(commit_id) file.path(DATA_DIR, paste0("commit_", safe_filename(commit_id), ".rds"))
ensure_data_dir <- function() dir.create(DATA_DIR, showWarnings = FALSE)

write_commit_snapshot <- function(df, commit_id) {
  ensure_data_dir()
  df$commit_id <- commit_id
  if (!inherits(df$ts, "POSIXct")) suppressWarnings(df$ts <- as.POSIXct(df$ts))
  saveRDS(df, commit_file_path(commit_id))
  invisible(TRUE)
}

read_commit_snapshots <- function() {
  files <- list.files(DATA_DIR, pattern = "^commit_.*\\.rds$", full.names = TRUE)
  if (!length(files)) return(NULL)
  parts <- lapply(files, function(f){
    x <- tryCatch(readRDS(f), error=function(e) NULL)
    if (is.null(x) || !nrow(x)) return(NULL)
    if (!"commit_id" %in% names(x) || all(!nzchar(as.character(x$commit_id)))) {
      x$commit_id <- sub("^commit_(.*)\\.rds$", "\\1", basename(f))
    }
    if (!inherits(x$ts, "POSIXct")) suppressWarnings(x$ts <- as.POSIXct(x$ts))
    x
  })
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) return(NULL)
  dplyr::bind_rows(parts)
}

# Local-day extractor (timezone-safe)
local_day <- function(ts) as.Date(as.POSIXlt(ts, tz = ""))

# Persisted state file (app-wide, not per-sheet RDS which are in `rds_paths`)
LOCAL_STATE_FILE <- "local_state.rds"

# Allowed Stock_of_origin choices (open set in UI, but we default to these)
ALLOWED_STOCK_ORIGIN <- c("NA", "deceptus", "messenoides", "intermedia")

# Canonical change-log columns (unified across tabs)
CHANGE_LOG_COLS <- c(
  "ts",          # POSIXct()
  "batch_id",    # batch identifier for grouping changes from same save operation
  "user_key",    # short key: AA, FCH, JM, KG, MJS, MS, PAS, INV
  "user_label",  # display label, e.g., "AA - Alex Arias" or "Invitado - <name>"
  "tab",         # where the change originated: "Registrar Muertes", ...
  "Insectary_ID",
  "Column",
  "Previous",
  "New"
)

## NA-like helper --------------------------------------------------------------
is_na_like_val <- function(x) {
  if (is.null(x)) return(TRUE)
  xs <- as.character(x)
  is_na     <- is.na(xs)
  is_blank  <- !nzchar(xs)
  is_lit_na <- toupper(xs) == "NA"
  (is_na | is_blank | is_lit_na)
}

## Date helpers (single source of truth) ---------------------------------------
# Simple date -> "d-MMM-yy" with NO left-zero and NO leading apostrophe
# - Keeps "", NA, and literal "NA" as-is.
# - Tries a few common input formats, otherwise returns the original string.
fmt_ui_date <- function(x) {
  xc <- as.character(x)
  n  <- length(xc)
  out <- character(n)
  bad <- is.na(xc) | xc == "" | toupper(xc) == "NA"
  out[bad] <- xc[bad]
  if (any(!bad)) {
    fmts <- DATE_FORMAT_CANDIDATES
    for (i in which(!bad)) {
      xi <- sub("^'+", "", xc[i])  # drop any leading apostrophes in input
      di <- NA
      for (f in fmts) {
        di <- suppressWarnings(as.Date(xi, format = f))
        if (!is.na(di)) break
      }
      if (is.na(di)) {
        out[i] <- xi
      } else {
        day <- as.integer(format(di, "%d"))           # no left zero
        mon <- format(di, "%b")
        yy  <- format(di, "%y")
        out[i] <- paste0(day, "-", mon, "-", yy)      # e.g., 9-Aug-25
      }
    }
  }
  out
}

# Parse many strings to Date (vectorized)
parse_ui_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  xc <- as.character(x)
  n  <- length(xc)
  out <- as.Date(rep(NA_character_, n))

  # filter candidates
  keep <- !(is.na(xc) | xc == "" | toupper(xc) == "NA")
  if (!any(keep)) return(out)

  fmts <- DATE_FORMAT_CANDIDATES
  left_idx <- which(keep)
  parsed_any <- rep(FALSE, length(left_idx))
  for (f in fmts) {
    suppressWarnings({
      d <- as.Date(xc[left_idx], format = f)
    })
    ok <- !is.na(d) & !parsed_any
    if (any(ok)) {
      out[left_idx[ok]] <- d[ok]
      parsed_any[ok] <- TRUE
    }
    if (all(parsed_any)) break
  }
  out
}

# Coerce UI strings ("d-MMM-yy") to Date for Sheets writes
to_sheet_date <- function(x) {
  xs <- as.character(x)
  blank <- is.na(xs) | xs == "" | toupper(xs) == "NA"
  out <- as.Date(rep(NA_character_, length(xs)))
  suppressWarnings({ out[!blank] <- as.Date(fmt_ui_date(xs[!blank]), format = "%d-%b-%y") })
  out
}

## Stock_of_origin rule helpers ------------------------------------------------
# Extract stock-of-origin from species:
#   If species starts with "Mechanitis messenoides", take the 3rd token
#   and normalize to one of ALLOWED_STOCK_ORIGIN; else "NA".
stock_origin_from_species <- function(species_chr) {
  if (is_na_like_val(species_chr)) return("NA")
  s <- trimws(as.character(species_chr))
  if (!nzchar(s)) return("NA")
  toks <- unlist(strsplit(s, "\\s+"))
  if (length(toks) >= 3L &&
      tolower(paste(toks[1:2], collapse = " ")) == "mechanitis messenoides") {
    cand <- tolower(toks[3])
    if (cand %in% tolower(ALLOWED_STOCK_ORIGIN)) {
      # return canonical case used in ALLOWED_STOCK_ORIGIN
      ALLOWED_STOCK_ORIGIN[match(cand, tolower(ALLOWED_STOCK_ORIGIN))]
    } else {
      "NA"
    }
  } else {
    "NA"
  }
}

## Local state persistence (versioned) -----------------------------------------
# Build an empty, typed change-log tibble
empty_change_log <- function() {
  tibble::tibble(
    ts          = as.POSIXct(character()),
    batch_id    = character(),
    user_key    = character(),
    user_label  = character(),
    tab         = character(),
    Insectary_ID= character(),
    Column      = character(),
    Previous    = character(),
    New         = character()
  )
}

# Save a named list `state` to LOCAL_STATE_FILE
save_local_state <- function(state) {
  state$version <- APP_STATE_VERSION
  tryCatch({
    saveRDS(state, LOCAL_STATE_FILE)
    TRUE
  }, error = function(e) FALSE)
}

# Load state from local file (no migration).
load_local_state <- function() {
  if (!file.exists(LOCAL_STATE_FILE)) {
    return(NULL)
  }
  # Simply read the file; if it's corrupted or invalid, return NULL.
  tryCatch(readRDS(LOCAL_STATE_FILE), error = function(e) NULL)
}

# Create initial state, combining on-disk .rds sheets and local_state, if any.
initialize_state <- function() {
  # Ensure per-sheet RDS exist; download if not
  raw_list <- if (!all(file.exists(unlist(rds_paths)))) {
    download_and_save_data(show_toasts = FALSE)
  } else {
    out <- lapply(rds_paths, readRDS)
    names(out) <- names(rds_paths)
    out
  }

  st_disk <- load_local_state()

  # Pick best sources for current data
  Insectary_data   <- st_disk$data$Insectary_data   %||% raw_list$Insectary_data
  Photo_links      <- raw_list$Photo_links %||% tibble::tibble()
  Insectary_stocks <- raw_list$Insectary_stocks %||% Insectary_data
  Lists            <- raw_list$Lists %||% tibble::tibble()

  # Reactive app state blueprint
  list(
    version = APP_STATE_VERSION,
    data = list(
      Photo_links      = Photo_links,
      Insectary_data   = Insectary_data,
      Insectary_stocks = Insectary_stocks,
      Lists            = Lists,
      raw              = raw_list
    ),
    # Unified change log (typed)
    changes = {
      ch <- st_disk$changes %||% empty_change_log()
      # ensure required columns exist
      miss <- setdiff(CHANGE_LOG_COLS, names(ch))
      if (length(miss)) {
        for (m in miss) ch[[m]] <- NA_character_
        # coerce ts if needed
        if (!inherits(ch$ts, "POSIXct")) {
          suppressWarnings({
            ch$ts <- as.POSIXct(ch$ts, origin = "1970-01-01", tz = "UTC")
          })
        }
      }
      ch[, CHANGE_LOG_COLS]
    },
    history = st_disk$history %||% list(),
    auth = list(
      is_logged_in = FALSE,
      is_guest     = FALSE,
      user_key     = NA_character_,
      user_label   = NA_character_,
      guest_name   = NA_character_
    )
  )
}

## Change-log writer -----------------------------------------------------------
user_label_from_auth <- function(auth) {
  # Prefer explicit label if present
  if (!isTRUE(auth$is_guest)) return(auth$user_label %||% NA_character_)
  # For guests, label must be "INV - <Usuario>"
  guest <- auth$guest_name %||% NA_character_
  paste0("INV - ", guest %||% "")
}

# Append a change entry to the in-memory log (no de-dup; tabs will decide).
add_change_entry <- function(rv, tab, insectary_id, column, previous, new_value, batch_id = NA_character_) {
  auth <- rv$auth %||% list(is_logged_in=FALSE, is_guest=FALSE, user_key=NA_character_, user_label=NA_character_)
  # For guests: key & label both carry the personal guest identity
  if (isTRUE(auth$is_guest)) {
    glabel <- user_label_from_auth(auth)
    ukey   <- glabel
    ulabel <- glabel
  } else {
    ukey   <- auth$user_key
    ulabel <- auth$user_label
  }
  entry <- tibble::tibble(
    ts           = Sys.time(),
    batch_id     = as.character(batch_id %||% NA_character_),
    user_key     = ukey %||% NA_character_,
    user_label   = ulabel %||% NA_character_,
    tab          = as.character(tab %||% ""),
    Insectary_ID = as.character(insectary_id %||% ""),
    Column       = as.character(column %||% ""),
    Previous     = if (is.null(previous)) NA_character_ else as.character(previous),
    New          = if (is.null(new_value)) NA_character_ else as.character(new_value)
  )
  rv$changes <- dplyr::bind_rows(rv$changes, entry)
  invisible(TRUE)
}

## Reusable constants ----------------------------------------------------------
sex_choices <- c("", "male", "female", "NA")

## Lists helpers --------------------------------------------------------------
get_lists_col <- function(rv, col) {
  L <- tryCatch(rv$data$Lists, error = function(e) NULL)
  if (is.null(L) || !nrow(L) || !(col %in% names(L))) return(character(0))
  vals <- na.omit(as.character(L[[col]]))
  vals[nzchar(vals)]
}

## Tissue list ordered by in-DB usage (Lists-driven) --------------------------
get_ordered_tissues <- function(rv) {
  # Source-of-truth from Lists
  tissues <- unique(c("NA", get_lists_col(rv, "ORGANISM_PART")))
  tissues <- tissues[nzchar(tissues)]
  if (!length(tissues)) return("NA")

  # Frequency from Tube_*_tissue columns
  df <- rv$data$Insectary_data %||% data.frame()
  cols <- intersect(names(df), paste0("Tube_", 1:4, "_tissue"))
  used <- character(0)
  if (length(cols)) {
    used <- as.character(unlist(df[cols], use.names = FALSE))
    used <- used[!is.na(used) & nzchar(used)]
  }
  if (!length(used)) return(tissues)

  freq <- sort(table(used), decreasing = TRUE)
  order_ref <- names(freq)
  idx <- match(tissues, order_ref)
  tissues[order(ifelse(is.na(idx), Inf, idx))]
}

## A1 range helper (for future Sheets writes) ----------------------------------
a1_range <- function(row_start, row_end, col_start, col_end) {
  paste0(
    cellranger::num_to_letter(col_start), row_start, ":",
    cellranger::num_to_letter(col_end),   row_end
  )
}

## URL helpers -----------------------------------------------------------------
# Turn Google Drive "file/d/.../view?..." links into high-res thumbnails
drive_view_to_thumb <- function(url_chr, width_px = 2000) {
  gsub("https://drive.google.com/file/d/(.*)/view\\?usp=drivesdk",
       paste0("https://drive.google.com/thumbnail?id=\\1&sz=w", width_px),
       url_chr)
}

# --- UI helpers: restore free-typed Selectize draft without selecting a value ---
restore_selectize_draft <- function(session, input_id, draft_value, only_if_empty = TRUE) {
  js <- sprintf("
    (function(){
      var el = document.getElementById(%s);
      if (!el || !el.selectize) return;
      var s = el.selectize;
      if (%s && s.items.length) return;
      s.setTextboxValue(%s);
    })();",
    jsonlite::toJSON(input_id),
    if (isTRUE(only_if_empty)) "true" else "false",
    jsonlite::toJSON(draft_value %||% "")
  )
  session$sendCustomMessage("jsCode", list(code = js))
}

force_hot_commit <- function(session, output_id) {
  session$sendCustomMessage("forceHotCommit", output_id)
}

# Mini helper: obtain photo URLs for a given CAM_ID (vectorized over CAM_ID list)
get_photo_urls <- function(photo_links_df, cam_id) {
  if (is.null(photo_links_df) || !nrow(photo_links_df)) {
    return(tibble::tibble(Name = character(0), URL_to_view = character(0)))
  }
  photo_links_df %>%
    mutate(URL_to_view = drive_view_to_thumb(URL)) %>%
    filter(str_detect(Name, fixed(cam_id)) &
             !str_detect(Name, regex("(ORF|CR2)$", ignore_case = TRUE))) %>%
    select(Name, URL_to_view)
}

## Stock-of-origin application rule -------------------------------------------
# Applies CLUTCH -> SPECIES (from stocks if needed) -> Stock_of_origin
# Returns a one-row data.frame with updated columns present in `row_df`.
apply_stock_origin_rule <- function(row_df, stocks_df) {
  stopifnot(nrow(row_df) == 1)
  out <- row_df

  # 1) If SPECIES is empty and CLUTCH NUMBER present, pull species from stocks
  clutch_col <- "CLUTCH NUMBER"
  has_clutch <- clutch_col %in% names(out) && !is_na_like_val(out[[clutch_col]][1])
  if (has_clutch && "SPECIES" %in% names(out) && is_na_like_val(out$SPECIES[1])) {
    sp <- tryCatch({
      rows <- dplyr::filter(stocks_df, .data[[clutch_col]] == out[[clutch_col]][1])
      if (nrow(rows)) rows$SPECIES[1] else NA_character_
    }, error = function(e) NA_character_)
    if (!is_na_like_val(sp)) out$SPECIES <- sp
  }

      # 2) Stock_of_origin (from SPECIES) — only fill if empty/"NA"
    if ("Stock_of_origin" %in% names(out) && is_na_like_val(out$Stock_of_origin[1])) {
      so <- stock_origin_from_species(out$SPECIES[1] %||% NA_character_)
      if (!(so %in% ALLOWED_STOCK_ORIGIN)) so <- "NA"
      out$Stock_of_origin <- so
    }

    # --- NEW: clutch/species-driven autofills ---
    # (a) If clutch is set and not literal "NA", default CAM_ID_CollData to "NA"
    if ("CAM_ID_CollData" %in% names(out) && clutch_col %in% names(out)) {
      cl <- out[[clutch_col]][1]
      if (!is_na_like_val(cl) && toupper(as.character(cl)) != "NA" &&
          is_na_like_val(out$CAM_ID_CollData[1])) {
        out$CAM_ID_CollData <- "NA"
      }
    }
    # (b) If SPECIES contains the letter "x" (case-insensitive), set research fields
    sp_now <- out$SPECIES[1] %||% NA_character_
    if (!is_na_like_val(sp_now) && grepl("x", sp_now, ignore.case = TRUE)) {
      if ("Research_purpose" %in% names(out) && is_na_like_val(out$Research_purpose[1])) {
        out$Research_purpose <- "F1/F2 mutation rate"
      }
      if ("Pedigree" %in% names(out) && is_na_like_val(out$Pedigree[1])) {
        out$Pedigree <- "YES"
      }
    }
    # --- end NEW ---

    out
}

## Autofill-by-clutch (centralized) -------------------------------------------
# Single place to fill SPECIES, Wild_Reared, Collection_location, Stock_of_origin
autofill_by_clutch <- function(df, stocks_df) {
  if (is.null(df) || !nrow(df)) return(df)
  if (!("CLUTCH NUMBER" %in% names(df))) return(df)

  # Row-wise, apply rules; keep vectorized where reasonable
  out <- df
  for (i in seq_len(nrow(out))) {
    row <- out[i, , drop = FALSE]
    # SPECIES/Stock_of_origin + clutch-side defaults in one call
    row <- apply_row_core_defaults(row, stocks_df)

    out[i, names(row)] <- row
  }
  out
}

apply_clutch_defaults_on_change <- function(prev_tbl, new_tbl, stocks_df) {
  if (is.null(new_tbl) || !nrow(new_tbl)) return(new_tbl)
  if (!("CLUTCH NUMBER" %in% names(new_tbl))) return(new_tbl)

  # rows to process
  changed_idx <- if (is.null(prev_tbl) || !("CLUTCH NUMBER" %in% names(prev_tbl)) || nrow(prev_tbl) != nrow(new_tbl)) {
    which(!is_na_like_val(new_tbl$`CLUTCH NUMBER`))
  } else {
    which(as.character(prev_tbl$`CLUTCH NUMBER`) != as.character(new_tbl$`CLUTCH NUMBER`))
  }
  if (!length(changed_idx)) return(new_tbl)

  for (i in changed_idx) {
    row <- new_tbl[i, , drop = FALSE]
    clutch_val <- row$`CLUTCH NUMBER`[1]

    # SPECIES from stocks for this clutch — only if empty/"NA"
    sp_from_stock <- tryCatch({
      ss <- dplyr::filter(stocks_df, .data$`CLUTCH NUMBER` == clutch_val)
      if (nrow(ss)) ss$SPECIES[1] else NA_character_
    }, error = function(e) NA_character_)
    if (!is_na_like_val(sp_from_stock) && ("SPECIES" %in% names(row)) && is_na_like_val(row$SPECIES[1])) {
      row$SPECIES <- sp_from_stock
    }

    # Apply all downstream defaults in one place (keeps behavior, reduces code)
    row <- apply_row_core_defaults(row, stocks_df)

    new_tbl[i, names(row)] <- row
  }
  new_tbl
}

## Suggestions builders (moved & reused) --------------------------------------
# Build Tube ID suggestions by prefix, medium, and consecutive runs
build_tube_id_suggestions <- function(insectary_df,
                                      max_suggestions_per_medium = 4,
                                      max_groups_per_key = max_suggestions_per_medium,
                                      max_total = 100,
                                      extra_tubes = NULL) {
  collect <- function(slot) {
    data.frame(
      Tube_id = insectary_df[[paste0("Tube_", slot, "_id")]],
      Tube_tissue = insectary_df[[paste0("Tube_", slot, "_tissue")]],
      Tube_medium = insectary_df[[paste0("T", slot, "_Preservation_medium")]],
      Preservation_date = insectary_df$Preservation_date,
      stringsAsFactors = FALSE
    )
  }
  tubes <- dplyr::bind_rows(collect(1), collect(2))
  # Include extra picks from the live table (if any)
  if (is.data.frame(extra_tubes) && nrow(extra_tubes)) {
    needed <- c("Tube_id", "Tube_tissue", "Tube_medium", "Preservation_date")
    miss <- setdiff(needed, names(extra_tubes))
    if (length(miss)) for (m in miss) extra_tubes[[m]] <- NA_character_
    tubes <- bind_rows(tubes, extra_tubes[, needed])
  }
  tubes <- tubes %>% dplyr::filter(!is.na(Tube_id), Tube_id != "", Tube_id != "NA")
  if (!nrow(tubes)) return(list(value = character(0), label = character(0)))

  tubes$pres_date_parsed <- as.Date(NA)
  idx_whole <- which(!is.na(tubes$Tube_tissue) &
                       grepl("^\\s*WHOLE_ORGANISM\\s*$", tubes$Tube_tissue, ignore.case = TRUE))
  if (length(idx_whole)) {
    tubes$pres_date_parsed[idx_whole] <- parse_ui_date(tubes$Preservation_date[idx_whole])
  }

  norm_medium <- function(x) {
    x <- as.character(x)
    x[is.na(x) | !nzchar(x) | toupper(x) == "NA"] <- "Unknown"
    x
  }
  tubes <- tubes %>%
    dplyr::mutate(
      Tube_medium = norm_medium(Tube_medium),
      prefix = stringr::str_extract(Tube_id, "^[A-Za-z]+"),
      number = suppressWarnings(as.integer(stringr::str_extract(Tube_id, "[0-9]+$")))
    ) %>%
    dplyr::filter(!is.na(prefix), !is.na(number))
  if (!nrow(tubes)) return(list(value = character(0), label = character(0)))

  summarize_runs <- function(df_key) {
    df_key <- df_key[order(df_key$number), , drop = FALSE]
    grp <- c(0, cumsum(diff(df_key$number) != 1))
    df_key$run_id <- grp
    runs <- df_key %>%
      dplyr::group_by(run_id) %>%
      dplyr::summarise(
        max_num = max(number, na.rm = TRUE),
        run_date = suppressWarnings(max(pres_date_parsed, na.rm = TRUE)),
        .groups = "drop"
      )
    runs$run_date[!is.finite(runs$run_date)] <- as.Date(NA)
    runs$next_id <- paste0(df_key$prefix[1], sprintf("%08d", runs$max_num + 1))
    runs
  }

  parts <- split(tubes, list(tubes$prefix, tubes$Tube_medium), drop = TRUE)
  if (!length(parts)) return(list(value = character(0), label = character(0)))

  summaries <- lapply(names(parts), function(key) {
    df_key <- parts[[key]]
    if (!nrow(df_key)) return(NULL)
    runs <- summarize_runs(df_key)
    if (!nrow(runs)) return(NULL)
    runs$prefix <- df_key$prefix[1]
    runs$medium <- df_key$Tube_medium[1]
    runs
  })
  summaries <- summaries[!vapply(summaries, is.null, logical(1))]
  if (!length(summaries)) return(list(value = character(0), label = character(0)))

  S <- dplyr::bind_rows(summaries)
  S <- S %>%
    dplyr::arrange(dplyr::desc(!is.na(run_date)), dplyr::desc(run_date)) %>%
    dplyr::group_by(prefix, medium) %>%
    dplyr::slice_head(n = max_groups_per_key) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(!is.na(run_date)), dplyr::desc(run_date)) %>%
    dplyr::slice_head(n = max_total)

  if (!nrow(S)) return(list(value = character(0), label = character(0)))

  # NEW: human-readable labels like "FS00001234 Flash frozen 14-Aug-25"
  fmt_med_disp <- function(x) {
    x <- as.character(x)
    x[is.na(x) | !nzchar(x) | toupper(x) == "NA"] <- "Unknown"
    x
  }
  lbl <- ifelse(
    is.na(S$run_date),
    sprintf("%s %s", S$next_id, fmt_med_disp(S$medium)),
    sprintf("%s %s %s", S$next_id, fmt_med_disp(S$medium), fmt_ui_date(S$run_date))
  )

  list(value = S$next_id, label = lbl)
}

# CAM-ID suggestions (DRY core + two thin wrappers)
build_camid_suggestions_core <- function(df, max_groups = 2, used_cam_ids = character(0)) {
  cam <- c(df$CAM_ID, used_cam_ids)
  cam <- cam[!is.na(cam) & nzchar(cam) & cam != "NA"]
  if (!length(cam)) return(tibble::tibble(value=character(0), prefix=character(0), date=as.Date(character(0))))
  pref <- stringr::str_extract(cam, "^[A-Za-z]+")
  num  <- suppressWarnings(as.integer(stringr::str_extract(cam, "[0-9]+$")))
  ok   <- !is.na(pref) & !is.na(num)
  cam  <- cam[ok]; pref <- pref[ok]; num <- num[ok]
  pd    <- parse_ui_date(df$Preservation_date); names(pd) <- df$CAM_ID
  intro <- parse_ui_date(df$Intro2Insectary_date); names(intro) <- df$CAM_ID
  get_date <- function(idvec) { dd <- pd[idvec]; dd[is.na(dd)] <- intro[idvec][is.na(dd)]; suppressWarnings(max(dd, na.rm = TRUE)) }
  out <- list()
  for (p in unique(pref)) {
    sub_idx <- which(pref == p)
    ord  <- order(num[sub_idx]); nums <- num[sub_idx][ord]; ids <- cam[sub_idx][ord]
    if (!length(nums)) next
    grp <- c(0, cumsum(diff(nums) != 1))
    for (g in unique(grp)) {
      g_ids <- ids[grp==g]; g_nums <- nums[grp==g]
      if (!length(g_ids)) next
      next_num <- max(g_nums, na.rm=TRUE) + 1L
      next_id  <- paste0(p, sprintf("%06d", next_num))
      g_date   <- get_date(g_ids); if (!is.finite(g_date)) g_date <- as.Date(NA)
      out[[length(out)+1]] <- data.frame(value=next_id, date=g_date, prefix=p, stringsAsFactors=FALSE)
    }
  }
  if (!length(out)) return(tibble::tibble(value=character(0), prefix=character(0), date=as.Date(character(0))))
  dplyr::bind_rows(out) %>%
    dplyr::arrange(dplyr::desc(!is.na(date)), dplyr::desc(date)) %>%
    dplyr::group_by(prefix) %>% dplyr::slice_head(n = max_groups) %>% dplyr::ungroup()
}
build_camid_suggestions <- function(df, max_groups = 2, used_cam_ids = character(0)) {
  unique(build_camid_suggestions_core(df, max_groups, used_cam_ids)$value)
}
build_camid_suggestions_labeled <- function(df, max_groups = 2, used_cam_ids = character(0)) {
  core <- build_camid_suggestions_core(df, max_groups, used_cam_ids)
  lbl  <- ifelse(is.na(core$date), core$value, paste0(core$value, " ", fmt_ui_date(core$date)))
  list(value = core$value, label = lbl)
}

## rHandsontable helpers -------------------------------------------------------
safe_hot_col <- function(rh, col_name, ...) {
  tmp <- try(hot_col(rh, col_name, ...), silent = TRUE)
  if (inherits(tmp, "try-error")) rh else tmp
}

# Centralized per-table editors that are common across tabs.
configure_common_editors <- function(rh, df, rv) {
  # Sex
  if ("Sex" %in% names(df)) {
    rh <- safe_hot_col(rh, "Sex", type = "dropdown", source = sex_choices, strict = FALSE, allowInvalid = TRUE)
  }

  # SPECIES (merge from both data sources)
  if ("SPECIES" %in% names(df)) {
    species_choices <- unique(c("NA", get_lists_col(rv, "Insectary_species")))
    rh <- safe_hot_col(rh, "SPECIES",
                       type = "dropdown",
                       source = sort(unique(species_choices)),
                       strict = FALSE, allowInvalid = TRUE)
  }

  # Stock_of_origin (restricted but not strict)
  if ("Stock_of_origin" %in% names(df)) {
    rh <- safe_hot_col(rh, "Stock_of_origin",
                       type = "dropdown",
                       source = ALLOWED_STOCK_ORIGIN,
                       strict = FALSE, allowInvalid = TRUE)
  }

  # CLUTCH NUMBER (from stocks if available)
  if ("CLUTCH NUMBER" %in% names(df)) {
    clutch_choices <- rv$data$Insectary_stocks$`CLUTCH NUMBER`
    clutch_choices <- clutch_choices[!is.na(clutch_choices) & nzchar(clutch_choices)]
    clutch_choices <- clutch_choices[!duplicated(clutch_choices)]
    rh <- safe_hot_col(rh, "CLUTCH NUMBER",
                       type = "dropdown",
                       source = c("", "NA", rev(clutch_choices)),
                       strict = FALSE, allowInvalid = TRUE)
  }

  # Various dropdowns if present
  add_dropdown_if <- function(rh, nm, values) {
    if (nm %in% names(df)) safe_hot_col(rh, nm, type = "dropdown",
                                        source = sort(unique(na.omit(values))),
                                        strict = FALSE, allowInvalid = TRUE) else rh
  }

  rh <- add_dropdown_if(rh, "Wild_Reared", rv$data$Insectary_data$Wild_Reared)
  rh <- add_dropdown_if(rh, "Collection_location", rv$data$Insectary_data$Collection_location)
  # Research_purpose & Location_body desde Lists
  if ("Research_purpose" %in% names(df)) {
    rp <- unique(c("NA", get_lists_col(rv, "Research_purpose")))
    rh <- safe_hot_col(rh, "Research_purpose", type = "dropdown",
                       source = sort(rp), strict = FALSE, allowInvalid = TRUE)
  }
  rh <- add_dropdown_if(rh, "Pedigree", rv$data$Insectary_data$Pedigree)
  if ("Location_body" %in% names(df)) {
    locs <- unique(c("NA", get_lists_col(rv, "Tissue locations")))
    rh <- safe_hot_col(rh, "Location_body", type = "dropdown",
                       source = sort(locs), strict = FALSE, allowInvalid = TRUE)
  }

  # Death_cause if present
  if ("Death_cause" %in% names(df)) {
    rh <- safe_hot_col(rh, "Death_cause",
                       type = "dropdown",
                       source = sort(unique(c("NA", na.omit(rv$data$Insectary_data$Death_cause)))),
                       strict = FALSE, allowInvalid = TRUE)
  }

  # Preserved_Dead_Alive dropdown
  if ("Preserved_Dead_Alive" %in% names(df)) {
    rh <- safe_hot_col(
      rh, "Preserved_Dead_Alive",
      type = "dropdown",
      source = c("Dead", "Alive", "NA"),
      strict = FALSE, allowInvalid = TRUE
    )
  }

  # Date columns in the single visible format "D-MMM-YY"
  date_cols <- intersect(names(df), c(DATE_COLS, "New_Death_Date"))
  for (dc in date_cols) {
    rh <- safe_hot_col(rh, dc, type = "date", dateFormat = "D-MMM-YY", correctFormat = TRUE)
  }



  # Tube IDs (aggregate suggestions)
  if (any(grepl("^Tube_\\d+_id$", names(df)))) {
    tube_sug <- build_tube_id_suggestions(rv$data$Insectary_data,
                                          max_suggestions_per_medium = 4, max_total = 100)
    tube_choices <- unique(tube_sug$value)
    for (k in TUBE_SLOTS) {
      id_col <- paste0("Tube_", k, "_id")
      if (id_col %in% names(df)) {
        rh <- safe_hot_col(rh, id_col, type = "dropdown",
                           source = tube_choices, strict = FALSE, allowInvalid = TRUE)
      }
      tissue_col <- paste0("Tube_", k, "_tissue")
      if (tissue_col %in% names(df)) {
        ord_tissues <- get_ordered_tissues(rv)
        rh <- safe_hot_col(rh, tissue_col, type = "dropdown",
                           source = ord_tissues, strict = FALSE, allowInvalid = TRUE)
      }
    }
    # Mediums 1 & 2
    if ("T1_Preservation_medium" %in% names(df)) {
      pres_choices <- sort(unique(na.omit(rv$data$Insectary_data$T1_Preservation_medium)))
      rh <- safe_hot_col(rh, "T1_Preservation_medium", type = "dropdown",
                         source = pres_choices, strict = FALSE, allowInvalid = TRUE)
    }
    if ("T2_Preservation_medium" %in% names(df)) {
      pres_choices <- sort(unique(na.omit(rv$data$Insectary_data$T2_Preservation_medium)))
      rh <- safe_hot_col(rh, "T2_Preservation_medium", type = "dropdown",
                         source = pres_choices, strict = FALSE, allowInvalid = TRUE)
    }
  }

  rh
}

## Generic table UI + renderer (kills duplicated spacers) ----------------------
# UI builder: standard table + spacer right after table + optional extra UI
make_table_ui <- function(output_id, after_table_ui = NULL,
                          height_spacer_px = 320, after_extras_spacer_px = 0) {
  tagList(
    rHandsontableOutput(output_id),
    # spacer must be right after the table so editors/dropdowns have room
    div(style = sprintf("height:%dpx;", as.integer(height_spacer_px))),
    # then (optionally) any extra UI (e.g., "Mostrar fotos")
    if (!is.null(after_table_ui)) tagList(
      after_table_ui,
      if (after_extras_spacer_px > 0)
        div(style = sprintf("height:%dpx;", as.integer(after_extras_spacer_px)))
    )
  )
}

camid_renderer_js <- function(valid_ids) {
  arr <- jsonlite::toJSON(as.character(na.omit(valid_ids)), auto_unbox = TRUE)
  htmlwidgets::JS(sprintf("
    function (instance, td, row, col, prop, value, cellProperties) {
      // Use AutocompleteRenderer so the dropdown arrow stays
      Handsontable.renderers.AutocompleteRenderer.apply(this, arguments);
      try {
        var valid = %s || [];
        var v = (value==null) ? '' : String(value).trim();
        var ok = (v === '' || v.toUpperCase() === 'NA' || valid.indexOf(v) !== -1);
        td.style.background = ok ? '' : '#FDE2E1';  // light red if invalid
      } catch (e) {}
    }", arr))
}

# Renderer: standard table render with common editors and optional custom editors
render_table <- function(output, output_id, df, rv, custom_editors = NULL, stretchH = "all") {
  output[[output_id]] <- renderRHandsontable({
    rh <- rhandsontable(df, rowHeaders = NULL, stretchH = stretchH)

    # Common editors (Sex, Species, Stock_of_origin, dates, etc.)
    rh <- configure_common_editors(rh, df, rv)

    # Apply any custom per-call editors (list of named lists)
    if (!is.null(custom_editors) && length(custom_editors)) {
      for (nm in names(custom_editors)) {
        ed <- custom_editors[[nm]]
        # ed is e.g., list(type="dropdown", source=c(...), strict=FALSE, allowInvalid=TRUE)
        if (nm %in% names(df)) {
          rh <- do.call(safe_hot_col, c(list(rh = rh, col_name = nm), ed))
        }
      }
    }

    # Make previous-value columns read-only to prevent accidental edits
    if ("Previous_Death_Date"   %in% names(df)) rh <- safe_hot_col(rh, "Previous_Death_Date",   readOnly = TRUE)
    if ("Previous_Death_Cause"  %in% names(df)) rh <- safe_hot_col(rh, "Previous_Death_Cause",  readOnly = TRUE)

    # CAM_ID: limit source to PREDICTIONS (from custom_editors if provided),
    # and color invalids against the full Lists whitelist.
    if ("CAM_ID" %in% names(df)) {
      # 1) Whitelist to validate/paint against (full Lists column):
      valid_cam <- tryCatch(unique(get_lists_col(rv, "InsectaryWild&Reared_CAMid")),
                            error = function(e) character(0))
      # 2) Source to show in dropdown = predicted short list
      #    If caller (apply_predictive_editors) passed a source, use it; else build a small default.
      cam_source <-
        if (!is.null(custom_editors) &&
            !is.null(custom_editors$CAM_ID) &&
            !is.null(custom_editors$CAM_ID$source)) {
          custom_editors$CAM_ID$source
        } else {
          unique(build_camid_suggestions(rv$data$Insectary_data, max_groups = 4))
        }
      rh <- safe_hot_col(
        rh, "CAM_ID",
        type = "dropdown",
        source = cam_source,
        strict = FALSE, allowInvalid = TRUE,
        renderer = camid_renderer_js(valid_cam)
      )
    }
    rh
  })
  # After render, bind edit hooks so we can detect active editing on the client
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session)) {
    session$sendCustomMessage('hotBindHooks', list(id = output_id))
  }
}

## Photo grid (for CAM_IDs in the table) --------------------------------------
photo_grid_for_cam_ids <- function(photo_links_df, cam_ids) {
  if (is.null(cam_ids) || !length(cam_ids)) return(div())
  cards <- lapply(cam_ids, function(cam) {
    urls <- get_photo_urls(photo_links_df, cam)
    if (!nrow(urls)) return(NULL)
    imgs <- lapply(seq_len(nrow(urls)), function(j) {
      img(src = urls$URL_to_view[j],
          style = "max-width: 48%; height: auto; margin: 2px; border-radius: 4px;")
    })
    tagList(
      h4(style = "font-weight:bold; margin-top:16px;", paste0("CAM_ID: ", cam)),
      div(style = "display:flex; flex-wrap:wrap; gap:6px;", imgs)
    )
  })
  tagList(Filter(Negate(is.null), cards))
}

## Consecutive tube ID generator (utility) ------------------------------------
generate_consecutive_tube_ids <- function(start_id, n) {
  prefix <- substr(start_id, 1, 2)
  num <- suppressWarnings(as.numeric(substr(start_id, 3, nchar(start_id))))
  if (is.na(num)) stop("Tube ID inicial inválido")
  paste0(prefix, sprintf("%08d", seq(num, length.out = n)))
}

## ---------- Common predictive editors (CAM_ID & Tube IDs) ----------
# Last-ID + 1
increment_alphanumeric_id <- function(id) {
  id <- as.character(id %||% "")
  pref <- sub("(\\D*).*", "\\1", id)
  digits <- sub("\\D*(\\d+).*", "\\1", id)
  if (!nzchar(digits)) return(id)
  width <- nchar(digits)
  num   <- suppressWarnings(as.integer(digits))
  if (is.na(num)) return(id)
  paste0(pref, sprintf(paste0("%0", width, "d"), num + 1L))
}

# Build & apply predictive dropdown editors for CAM_ID and Tube_*_id
# - output/output_id: where to render
# - cur_df: current table data
# - prev_df: previous table data (to detect last changed cell)
# - update_inputs: "both"/"add_only"/"none" - which start selects to refresh (Tubos tab)
# - start_values: list(start = input$tube_start_id, add = input$tube_add_start_id)
apply_predictive_editors <- function(output, session, output_id, cur_df, rv,
                                     prev_df = NULL,
                                     update_inputs = c("both","add_only","none"),
                                     start_values = list(start = NULL, add = NULL),
                                     defer_render = FALSE,
                                     render_mode = c("auto","force","none")) {
  update_inputs <- match.arg(update_inputs)
  render_mode   <- match.arg(render_mode)
  # ---- CAM_ID source (include live picks + last-change hint) ----
  next_cam_hint <- NULL
  if (!is.null(prev_df) && all(c("CAM_ID" %in% names(cur_df), "CAM_ID" %in% names(prev_df))) &&
      nrow(cur_df) == nrow(prev_df)) {
    changed <- which(as.character(cur_df$CAM_ID) != as.character(prev_df$CAM_ID))
    if (length(changed)) {
      last_val <- tail(na.omit(as.character(cur_df$CAM_ID[changed])), 1)
      if (length(last_val)) next_cam_hint <- increment_alphanumeric_id(last_val)
    }
  }
  used_cam <- if ("CAM_ID" %in% names(cur_df)) unique(na.omit(as.character(cur_df$CAM_ID))) else character(0)
  cam_source <- unique(build_camid_suggestions(rv$data$Insectary_data, max_groups = 4, used_cam_ids = used_cam))
  if (!is.null(next_cam_hint) && nzchar(next_cam_hint)) cam_source <- unique(c(next_cam_hint, cam_source))

  # ---- Tube sources (use live picks as extra_tubes + last-change hint) ----
  # harvest live tubes from cur_df
  extra <- list()
  for (j in TUBE_SLOTS) {
    idc <- paste0("Tube_", j, "_id"); tic <- paste0("Tube_", j, "_tissue"); mic <- paste0("T", j, "_Preservation_medium")
    if (idc %in% names(cur_df)) {
      ids_now <- as.character(cur_df[[idc]])
      keep <- !is.na(ids_now) & ids_now != "" & ids_now != "NA"
      if (any(keep)) {
        extra[[length(extra)+1]] <- data.frame(
          Tube_id = ids_now[keep],
          Tube_tissue = if (tic %in% names(cur_df)) cur_df[[tic]][keep] else NA,
          Tube_medium = if (mic %in% names(cur_df)) cur_df[[mic]][keep] else NA,
          Preservation_date = fmt_ui_date(Sys.Date()),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  extra_df <- if (length(extra)) dplyr::bind_rows(extra) else NULL

  sug <- build_tube_id_suggestions(rv$data$Insectary_data, extra_tubes = extra_df)
  tube_source <- unique(sug$value)
  lab_map <- setNames(sug$label, sug$value)  # value -> human label (with medium/date)

  last_tube_hint <- NULL
  if (!is.null(prev_df)) {
    for (j in TUBE_SLOTS) {
      col <- paste0("Tube_", j, "_id")
      if (col %in% names(cur_df) && col %in% names(prev_df) && nrow(cur_df) == nrow(prev_df)) {
        changed_rows <- which(as.character(cur_df[[col]]) != as.character(prev_df[[col]]))
        if (length(changed_rows)) {
          last_val <- tail(na.omit(as.character(cur_df[[col]][changed_rows])), 1)
          if (length(last_val)) last_tube_hint <- increment_alphanumeric_id(last_val)
        }
      }
    }
  }
  if (!is.null(last_tube_hint) && nzchar(last_tube_hint)) tube_source <- unique(c(last_tube_hint, tube_source))

  # ---- Compose per-column editors and render ----
  custom <- list()
  if ("CAM_ID" %in% names(cur_df)) {
    custom$CAM_ID <- list(type="dropdown", source = cam_source, strict = FALSE, allowInvalid = TRUE)
  }
  for (j in TUBE_SLOTS) {
    idc <- paste0("Tube_", j, "_id")
    if (idc %in% names(cur_df)) {
      custom[[idc]] <- list(type="dropdown", source = tube_source, strict = FALSE, allowInvalid = TRUE)
    }
  }
  # ---- Decide whether to re-render the table ----
  do_render <- switch(render_mode,
    force = TRUE,
    none  = FALSE,
    auto  = {
      if (is.null(prev_df)) TRUE else {
        nrow(prev_df) != nrow(cur_df) || !identical(names(prev_df), names(cur_df))
      }
    }
  )
  if (isTRUE(do_render)) {
    # Defer full Handsontable re-render until after current flush if requested.
    # This prevents repainting the widget while an edit is still being committed.
    if (isTRUE(defer_render)) {
      session$onFlushed(function(){
        render_table(output, output_id, cur_df, rv, custom_editors = custom)
      }, once = TRUE)
    } else {
      render_table(output, output_id, cur_df, rv, custom_editors = custom)
    }
  }

      # ---- (Tubos tab) refresh Tube start selects to reflect new prediction ----
    if (update_inputs != "none") {
      # Helpers
      first_choice <- function(vec){ if (length(vec) == 0) return(NULL); vec[1] }
      labelize <- function(values){
        if (length(values) == 0) return(list())
        lbl <- unname(lab_map[values])
        na_idx <- is.na(lbl)
        if (any(na_idx)) lbl[na_idx] <- values[na_idx]  # elementwise fallback (fixes warning)
        out <- as.list(values); names(out) <- lbl; out
      }
      # tube_start_id
      if (update_inputs %in% c("both")) {
        sel_start <- if (!is.null(start_values$start) && nzchar(start_values$start)) start_values$start else first_choice(tube_source)
        choices_start <- unique(c(sel_start, tube_source))
        updateSelectizeInput(session, "tube_start_id", choices = labelize(choices_start), selected = sel_start, server = TRUE)
      }
      # tube_add_start_id — prefer the CURRENT selection passed in start_values$add
      if (update_inputs %in% c("both","add_only")) {
        cur_sel <- start_values$add
        sel_add <- if (!is.null(cur_sel) && nzchar(cur_sel)) cur_sel else
                   if (!is.null(last_tube_hint) && nzchar(last_tube_hint)) last_tube_hint else
                   first_choice(tube_source)
        choices_add <- unique(c(sel_add, tube_source))
        updateSelectizeInput(session, "tube_add_start_id", choices = labelize(choices_add), selected = sel_add, server = TRUE)
      }
    }
}

## ---------- Reusable HOT helpers (debounce/guard/render) ----------
# Force-or-auto predictive render with deferred paint (to avoid flicker)
hot_render_predictive <- function(output, session, output_id, cur_df, rv,
                                  prev_df = NULL,
                                  update_inputs = "none",
                                  start_values = list(),
                                  force = FALSE) {
  apply_predictive_editors(
    output, session, output_id, cur_df, rv,
    prev_df       = prev_df,
    update_inputs = update_inputs,
    start_values  = start_values,
    defer_render  = TRUE,
    render_mode   = if (isTRUE(force)) "force" else "auto"
  )
}

# Generic, reusable debounced observer for a Handsontable:
# - input_id: the rHandsontableOutput id (e.g., "tube_table")
# - rv_slot : name of the rv field to store the data.frame (e.g., "tube_tbl")
# - process_fun(prev_df, new_df): returns processed new_df
# - update_inputs/start_values: forwarded to predictive editors
# - debounce_ms: quiet time window before processing
hot_debounced_observer <- function(input, output, session, rv,
                                   input_id, rv_slot,
                                   process_fun,
                                   update_inputs = "none",
                                   start_values = list(),
                                   debounce_ms = 200,
                                   key_columns = character(0)) {
  # Busy flag per table
  busy_name <- paste0("busy_", rv_slot)
  rv[[busy_name]] <- FALSE
  # Debounced stream of the HOT input
  deb <- debounce(reactive(input[[input_id]]), debounce_ms)
  observeEvent(deb(), {
    req(input[[input_id]])
    # Skip while client-side editor is open
    edit_flag_name <- paste0(input_id, "_editing")
    if (isTRUE(input[[edit_flag_name]])) return()

    # Re-entrancy guard
    if (isTRUE(rv[[busy_name]])) return()
    rv[[busy_name]] <- TRUE
    on.exit({ rv[[busy_name]] <- FALSE }, add = TRUE)

    prev_df <- rv[[rv_slot]]
    new_df  <- hot_to_r(input[[input_id]])
    if (is.null(new_df) || !is.data.frame(new_df) || !nrow(new_df)) return()

    # Detect whether any key column has changed vs previous df
    changed_key <- FALSE
    if (!is.null(prev_df)) {
      cols <- intersect(key_columns, intersect(names(prev_df), names(new_df)))
      if (length(cols)) {
        for (cn in cols) {
          pv <- as.character(prev_df[[cn]])
          cv <- as.character(new_df[[cn]])
          if (length(pv) != length(cv) ||
              any(is.na(pv) != is.na(cv)) ||
              any((pv != cv)[!(is.na(pv) | is.na(cv))])) {
            changed_key <- TRUE; break
          }
        }
      }
    }

    # Let caller transform / enforce row-wise rules
    new_df <- process_fun(prev_df, new_df)
    rv[[rv_slot]] <- new_df

    # Keep predictions fresh but do NOT repaint mid-edit
    apply_predictive_editors(
      output, session, input_id, rv[[rv_slot]], rv,
      prev_df       = prev_df,
      update_inputs = update_inputs,
      start_values  = start_values,
      defer_render  = TRUE,
      render_mode   = "none"
    )

    # If key columns changed, run a forced (deferred) repaint so UI reflects rules
    if (isTRUE(changed_key)) {
      apply_predictive_editors(
        output, session, input_id, rv[[rv_slot]], rv,
        prev_df       = prev_df,
        update_inputs = update_inputs,
        start_values  = start_values,
        defer_render  = TRUE,
        render_mode   = "force"
      )
    }
  }, ignoreInit = TRUE)
}

## User directory & passwords --------------------------------------------------
USERS_CFG <- tryCatch({
  fromJSON("users.json", simplifyVector = TRUE)
}, error = function(e) {
  message("Error loading users.json: ", e$message)
  stop("Failed to load users.json. Please check the file exists and has valid JSON format.")
})

# Validate the structure
if (is.null(USERS_CFG$users) || !is.data.frame(USERS_CFG$users)) {
  stop("Invalid users.json structure. Expected 'users' array with key, label, password fields.")
}

users_df <- USERS_CFG$users
USER_CHOICES <- setNames(users_df$key, users_df$label)  # names=labels, values=keys
USER_LABELS  <- setNames(users_df$label, users_df$key)  # "AA" -> "AA - Alex Arias"
USER_KEYS    <- setNames(users_df$key, users_df$label)  # "AA - Alex Arias" -> "AA"
KNOWN_KEYS   <- users_df$key

expected_password_for_key <- function(key) {
  row <- USERS_CFG$users[USERS_CFG$users$key == key, ]
  if (nrow(row)) row$password[1] else ""
}

## Reusable helpers around auth -----------------------------------------------
auth_is_logged_in <- function(rv) isTRUE(rv$auth$is_logged_in)
auth_is_guest     <- function(rv) isTRUE(rv$auth$is_guest)
auth_user_key     <- function(rv) rv$auth$user_key %||% NA_character_
auth_user_label   <- function(rv) user_label_from_auth(rv$auth)
auth_can_upload   <- function(rv) auth_is_logged_in(rv) && !auth_is_guest(rv)

## Reusable Save button (UI + renderer) ---------------------------------------
save_button_ui <- function(output_id) {
  uiOutput(output_id)
}

render_save_button <- function(output, output_id, auth_reactive) {
  output[[output_id]] <- renderUI({
    a <- auth_reactive()
    if (!isTRUE(a$is_logged_in)) {
      # Disabled, red button (does nothing when clicked)
      tags$button(
        id = paste0(output_id, "_placeholder"), type = "button", # Use a different id to avoid event conflicts
        class = "btn btn-danger",
        disabled = NA, paste0("No tienes permiso para ", "Guardar en local")
      )
    } else {
      # Enabled, green button
      actionButton(output_id, "Guardar en local", class = "btn btn-success")
    }
  })
}

## Reusable auth-aware action button (same UX as Guardar en local)
auth_button_ui <- function(output_id) uiOutput(output_id)
render_auth_button <- function(output, output_id, auth_reactive,
                               label, enabled_class = "btn btn-primary",
                               allow_guest = FALSE,
                               disabled_prefix = "No tienes permiso para ") {
  output[[output_id]] <- renderUI({
    a <- auth_reactive() %||% list(is_logged_in = FALSE, is_guest = FALSE)
    if (!isTRUE(a$is_logged_in) || (isTRUE(a$is_guest) && !allow_guest)) {
      tags$button(
        id = paste0(output_id, "_placeholder"), type = "button",
        class = "btn btn-danger", disabled = NA, paste0(disabled_prefix, label)
      )
    } else {
      actionButton(output_id, label, class = enabled_class)
    }
  })
}

## Tab-Specific Helpers --------------------------------------------------------

# -- Commit helper (for Subir Cambios tab) --
# Commit a data.frame of changes to Google Sheets
commit_changes_to_sheet <- function(changes_df, sheet_name = "Insectary_data", report_cb = NULL) {
  if (nrow(changes_df) == 0) return(invisible(TRUE))
  req(gsheet_id)
  failures <- data.frame(Insectary_ID=character(), Column=character(), Reason=character(), A1=character(), stringsAsFactors = FALSE)

  # Use the .rds snapshot for row/col mapping (header row is in the sheet)
  current <- readRDS(rds_paths[["Insectary_data"]])

  # --- Inject Preservation_medium "edited by App" note(s) when applicable ----
  # Only do this when the incoming payload carries `tab` (comes from normal "Subir" flow).
  if ("tab" %in% names(changes_df) && "Preservation_medium" %in% names(current)) {
    # Map tab -> message (English phrases per spec)
    map_tab_msg <- function(tb) {
      if (grepl("Muert", tb, ignore.case = TRUE))      return("Deaths set by App")
      if (grepl("Tubos", tb, ignore.case = TRUE))      return("Tubes set by App")
      if (grepl("Emergid", tb, ignore.case = TRUE))    return("Emergidos set by App")
      return(NA_character_)
    }
    today_note <- function(tb) {
      msg <- map_tab_msg(tb)
      if (is.na(msg)) return(NA_character_)
      paste0(fmt_ui_date(Sys.Date()), ": ", msg)
    }
    # Build one Preservation_medium append per Insectary_ID present in this commit,
    # combining multiple tabs (rare) into a single string separated by " | "
    add_rows <- list()
    for (id in unique(changes_df$Insectary_ID)) {
      if (is.na(id) || !nzchar(id)) next
      # Which tabs touched this ID?
      tabs <- unique(na.omit(as.character(changes_df$tab[changes_df$Insectary_ID == id])))
      notes <- unique(na.omit(vapply(tabs, today_note, character(1))))
      if (!length(notes)) next
      # Current PM value
      idx <- which(current$Insectary_ID == id)
      if (!length(idx)) next
      prev_pm <- as.character(current$Preservation_medium[idx][1])
      prev_pm[is.na(prev_pm)] <- ""
      # Compose new value, but avoid adding duplicates of the same note(s)
      pm_now <- prev_pm
      for (nt in notes) {
        if (!nzchar(pm_now)) {
          pm_now <- nt
        } else if (!grepl(paste0("\\Q", nt, "\\E"), pm_now, perl = TRUE)) {
          pm_now <- paste0(pm_now, " | ", nt)
        }
      }
      # If changed, enqueue a synthetic change row (Column=Preservation_medium)
      if (!identical(pm_now, prev_pm)) {
        add_rows[[length(add_rows) + 1]] <- data.frame(
          Insectary_ID = id,
          Column       = "Preservation_medium",
          New          = pm_now,
          stringsAsFactors = FALSE
        )
      }
    }
    if (length(add_rows)) {
      changes_df <- dplyr::bind_rows(changes_df[, intersect(c("Insectary_ID","Column","New","tab"), names(changes_df)), drop = FALSE],
                                     dplyr::bind_rows(add_rows))
    }
  }

  # Date columns used for coercion before writing to Sheets
  date_cols_all <- DATE_COLS

  # Map rows by Insectary_ID and columns by name
  changes_df <- changes_df %>%
    mutate(
      row_idx = vapply(Insectary_ID, function(id) {
        idx <- which(current$Insectary_ID == id)
        if (length(idx)) idx else NA_integer_
      }, integer(1)),
      col_idx = match(Column, names(current))
    ) %>%
    filter(!is.na(row_idx), !is.na(col_idx))

  if (!nrow(changes_df)) return(invisible(TRUE))

  # Group by row; derive contiguous column blocks
  by_row <- split(changes_df, changes_df$row_idx)
  row_blocks <- lapply(by_row, function(df_row) {
    df_row <- df_row[order(df_row$col_idx), , drop = FALSE]
    blocks <- list()
    if (!nrow(df_row)) return(blocks)
    start <- df_row$col_idx[1]; end <- start
    for (i in seq_len(nrow(df_row))) {
      if (i == 1) next
      col <- df_row$col_idx[i]
      if (col == end + 1) {
        end <- col
      } else {
        blocks[[length(blocks) + 1]] <- c(start, end)
        start <- col; end <- col
      }
    }
    blocks[[length(blocks) + 1]] <- c(start, end)
    blocks
  })

  # Build tasks (row, col range), merged across consecutive rows with same col range
  tasks <- do.call(rbind, unlist(mapply(function(r, bl) {
    if (length(bl) == 0) return(list())
    lapply(bl, function(b) data.frame(row_idx = as.integer(r), col_start = b[1], col_end = b[2]))
  }, names(row_blocks), row_blocks, SIMPLIFY = FALSE), recursive = FALSE))

  if (is.null(tasks) || !nrow(tasks)) return(invisible(TRUE))
  tasks <- tasks[order(tasks$col_start, tasks$col_end, tasks$row_idx), , drop = FALSE]
  split_by_range <- split(tasks, paste(tasks$col_start, tasks$col_end))

  # Pre-compute contiguous row segments per (col_start,col_end) to know total steps
  segments <- data.frame(row_start=integer(0), row_end=integer(0),
                         col_start=integer(0), col_end=integer(0))
  for (key in names(split_by_range)) {
    tk <- split_by_range[[key]]
    tk <- tk[order(tk$row_idx), , drop = FALSE]
    if (!nrow(tk)) next
    start_i <- tk$row_idx[1]; prev_i <- start_i
    for (i in seq_len(nrow(tk))) {
      if (i > 1 && tk$row_idx[i] != prev_i + 1) {
        segments <- rbind(segments, data.frame(row_start=start_i, row_end=prev_i,
                                               col_start=tk$col_start[1], col_end=tk$col_end[1]))
        start_i <- tk$row_idx[i]
      }
      prev_i <- tk$row_idx[i]
    }
    segments <- rbind(segments, data.frame(row_start=start_i, row_end=prev_i,
                                           col_start=tk$col_start[1], col_end=tk$col_end[1]))
  }
  total_steps <- nrow(segments)

  write_block <- function(row_start, row_end, col_start, col_end) {
    sub <- current[seq(row_start, row_end), seq(col_start, col_end), drop = FALSE]
    in_block <- changes_df$row_idx >= row_start & changes_df$row_idx <= row_end &
                changes_df$col_idx >= col_start & changes_df$col_idx <= col_end
    chb <- changes_df[in_block, , drop = FALSE]

    # CLUTCH NUMBER handling: detect changes in this block
    clutch_col_name <- "CLUTCH NUMBER"
    clutch_idx_abs  <- match(clutch_col_name, names(current))
    has_clutch_in_block <- !is.na(clutch_idx_abs) && clutch_idx_abs >= col_start && clutch_idx_abs <= col_end
    ch_clutch <- if (isTRUE(has_clutch_in_block)) chb[chb$col_idx == clutch_idx_abs, , drop = FALSE] else chb[0, , drop = FALSE]
    # Neutralize CLUTCH NUMBER inside block write so we can re-write it per-cell with proper typing
    if (nrow(ch_clutch)) {
      for (k in seq_len(nrow(ch_clutch))) {
        rr <- ch_clutch$row_idx[k] - row_start + 1
        cc <- clutch_idx_abs - col_start + 1
        if (rr >= 1 && rr <= nrow(sub) && cc >= 1 && cc <= ncol(sub)) {
          sub[rr, cc] <- current[ch_clutch$row_idx[k], clutch_idx_abs]
        }
      }
    }
    if (nrow(chb)) {
      for (k in seq_len(nrow(chb))) {
        rr <- chb$row_idx[k] - row_start + 1
        cc <- chb$col_idx[k] - col_start + 1
        sub[rr, cc] <- chb$New[k]
      }
    }

    # Convert any date-like columns to Date before write
    col_names    <- names(current)[seq(col_start, col_end)]
    dc_in_block  <- intersect(col_names, date_cols_all)
    if (length(dc_in_block)) {
      for (nm in dc_in_block) {
        sub[[nm]] <- to_sheet_date(sub[[nm]])
      }
    }

    range_a1 <- paste0(
      cellranger::num_to_letter(col_start), row_start + 1, ":",
      cellranger::num_to_letter(col_end),   row_end   + 1
    )
    ids_in_block <- current$Insectary_ID[seq(row_start, row_end)]
    cols_str <- paste(col_names, collapse = ", ")
    ids_str  <- paste(ids_in_block, collapse = ", ")

    # Helper to write CLUTCH NUMBER per-cell with numeric typing when purely numeric
    write_clutch_cells <- function() {
      if (!nrow(ch_clutch)) return(invisible(NULL))
      for (k in seq_len(nrow(ch_clutch))) {
        r_abs <- ch_clutch$row_idx[k]
        a1 <- paste0(cellranger::num_to_letter(clutch_idx_abs), r_abs + 1, ":", cellranger::num_to_letter(clutch_idx_abs), r_abs + 1)
        id_here <- current$Insectary_ID[r_abs]
        new_val <- ch_clutch$New[k]
        is_num_like <- grepl("^\\s*[0-9]+\\s*$", as.character(new_val))
        payload <- if (is_num_like) data.frame(x = as.numeric(new_val)) else data.frame(x = as.character(new_val), stringsAsFactors = FALSE)
        ok_cell <- tryCatch({
          googlesheets4::range_write(
            ss = gsheet_id, data = payload, sheet = sheet_name,
            range = a1, col_names = FALSE, reformat = FALSE
          )
          message(sprintf("[GS WRITE OK] cell=%s | col=%s | Insectary_ID=%s", a1, clutch_col_name, id_here))
          TRUE
        }, error = function(e) {
          emsg <- conditionMessage(e)
          message(sprintf("[GS WRITE ERROR] cell=%s | col=%s | Insectary_ID=%s | reason=%s", a1, clutch_col_name, id_here, emsg))
          failures <<- rbind(
            failures,
            data.frame(Insectary_ID = id_here, Column = clutch_col_name,
                       Reason = paste0("Write error: ", emsg),
                       A1 = a1, stringsAsFactors = FALSE)
          )
          FALSE
        })
        if (!ok_cell) next
      }
      invisible(NULL)
    }

    if (exists(".inc", inherits = FALSE)) .inc(sprintf("range=%s | cols=[%s] | Insectary_ID=[%s]", range_a1, cols_str, ids_str))

    try_rect <- function() {
      googlesheets4::range_write(
        ss = gsheet_id, data = sub, sheet = sheet_name,
        range = range_a1, col_names = FALSE, reformat = FALSE
      )
      message(sprintf("[GS WRITE OK] range=%s | cols=[%s] | Insectary_ID=[%s]", range_a1, cols_str, ids_str))
      TRUE
    }

    ok <- tryCatch(try_rect(), error = function(e) {
      emsg <- conditionMessage(e)
      message(sprintf("[GS WRITE ERROR] range=%s | cols=[%s] | Insectary_ID=[%s] | reason=%s",
                      range_a1, cols_str, ids_str, emsg))
      FALSE
    })
    if (ok) {
      if (nrow(ch_clutch)) write_clutch_cells()
      return(invisible(NULL))
    }

    # Fallback 1: write each column independently
    for (cc in seq(col_start, col_end)) {
      c_name <- names(current)[cc]
      # Skip CLUTCH NUMBER here; we'll write it per-cell with proper typing
      if (isTRUE(has_clutch_in_block) && identical(c_name, clutch_col_name)) next
      sub_col <- current[seq(row_start, row_end), cc, drop = FALSE]
      # inject only the changed values for this column
      ch_col <- chb[chb$col_idx == cc, , drop = FALSE]
      if (nrow(ch_col)) {
        for (k in seq_len(nrow(ch_col))) {
          rr <- ch_col$row_idx[k] - row_start + 1
          sub_col[rr, 1] <- ch_col$New[k]
        }
      }
      # date coercion if needed
      if (c_name %in% date_cols_all) {
        sub_col[[1]] <- to_sheet_date(sub_col[[1]])
      }
      range_col <- paste0(
        cellranger::num_to_letter(cc), row_start + 1, ":",
        cellranger::num_to_letter(cc), row_end   + 1
      )

      if (exists(".inc", inherits = FALSE)) .inc(sprintf("col=%s | range=%s | Insectary_ID=[%s]", c_name, range_col, ids_str))

      ok_col <- tryCatch({
        googlesheets4::range_write(ss = gsheet_id, data = sub_col, sheet = sheet_name,
                                   range = range_col, col_names = FALSE, reformat = FALSE)
        message(sprintf("[GS WRITE OK] range=%s | col=%s | Insectary_ID=[%s]",
                        range_col, c_name, ids_str))
        TRUE
      }, error = function(e) {
        emsg <- conditionMessage(e)
        message(sprintf("[GS WRITE ERROR] range=%s | col=%s | Insectary_ID=[%s] | reason=%s",
                        range_col, c_name, ids_str, emsg))
        FALSE
      })
      if (ok_col) next

      # Fallback 2: per-cell
      for (r in seq(row_start, row_end)) {
        new_val <- current[r, cc]
        ch_cell <- chb[chb$row_idx == r & chb$col_idx == cc, , drop = FALSE]
        if (nrow(ch_cell)) new_val <- ch_cell$New[1]
        # date coercion if needed
        if (c_name %in% date_cols_all) {
          new_val <- to_sheet_date(new_val)[1]
        }
        # Skip CLUTCH NUMBER here; we'll write it per-cell below with numeric typing
        if (isTRUE(has_clutch_in_block) && identical(c_name, clutch_col_name)) next

        a1 <- paste0(cellranger::num_to_letter(cc), r + 1, ":", cellranger::num_to_letter(cc), r + 1)
        id_here <- current$Insectary_ID[r]

        if (exists(".inc", inherits = FALSE)) .inc(sprintf("cell=%s | col=%s | Insectary_ID=%s", a1, c_name, id_here))

        ok_cell <- tryCatch({
          googlesheets4::range_write(ss = gsheet_id,
                                     data = data.frame(x = new_val, stringsAsFactors = FALSE),
                                     sheet = sheet_name, range = a1, col_names = FALSE, reformat = FALSE)
          message(sprintf("[GS WRITE OK] cell=%s | col=%s | Insectary_ID=%s", a1, c_name, id_here))
          TRUE
        }, error = function(e) {
          emsg <- conditionMessage(e)
          message(sprintf("[GS WRITE ERROR] cell=%s | col=%s | Insectary_ID=%s | reason=%s", a1, c_name, id_here, emsg))
          failures <<- rbind(
            failures,
            data.frame(Insectary_ID = id_here, Column = c_name,
                       Reason = paste0("Write error: ", emsg),
                       A1 = a1, stringsAsFactors = FALSE)
          )
          showNotification(sprintf("No se pudo escribir %s (%s) en %s", id_here, c_name, a1),
                           type = "warning", duration = NULL)
          FALSE
        })
        if (!ok_cell) { next }
      }
    }
    # Finally, write CLUTCH NUMBER cells with numeric typing where applicable
    if (nrow(ch_clutch)) write_clutch_cells()
    invisible(NULL)
  }

  # Execute each segment and report progress/counter + range/IDs
  if (total_steps) {
    for (i in seq_len(total_steps)) {
      rs <- segments$row_start[i]; re <- segments$row_end[i]
      cs <- segments$col_start[i]; ce <- segments$col_end[i]
      a1 <- paste0(cellranger::num_to_letter(cs), rs + 1, ":",
                   cellranger::num_to_letter(ce), re + 1)
      ids_block <- paste(current$Insectary_ID[seq(rs, re)], collapse = ", ")
      if (is.function(report_cb)) {
        msg <- sprintf("Escribiendo %s  |  IDs: %s", a1, ids_block)
        try(report_cb(i, total_steps, msg), silent = TRUE)
      }
      write_block(rs, re, cs, ce)
    }
  }
  return(failures)
}

# -- Helper for Registrar Muertes tab --
build_dead_table <- function(insectary_df, ids, review_only, default_cause, date_input_str) {
  if (is.null(ids) || !length(ids)) return(insectary_df[0, , drop = FALSE])

  df <- insectary_df %>% dplyr::filter(.data$Insectary_ID %in% ids)
  if (!nrow(df)) return(df)

  # Ensure consistent UI date strings in view for Death_date
  if ("Death_date" %in% names(df)) {
    df$Death_date <- vapply(df$Death_date, fmt_ui_date, character(1))
  }

  # Prepare helper columns ALWAYS (same structure for review and edit)
  target_date_ui <- fmt_ui_date(date_input_str %||% Sys.Date())

  prev_date <- df$Death_date
  prev_cause <- df$Death_cause

  if (isTRUE(review_only)) {
    # Review mode: mirror previous values; user sees same structure but cannot
    # change "Previous_*" columns (made readOnly in render_table).
    new_date  <- prev_date
    new_cause <- prev_cause
  } else {
    # Editing mode: prefill new values only when empty
    new_date <- ifelse(
      is.na(prev_date) | prev_date == "" | toupper(prev_date) == "NA",
      target_date_ui,
      prev_date
    )
    new_cause <- ifelse(
      is.na(prev_cause) | prev_cause == "" | toupper(prev_cause) == "NA",
      default_cause %||% prev_cause,
      prev_cause
    )
  }

  # Assemble view with helper columns
  tbl <- df %>%
    mutate(
      Previous_Death_Date = prev_date,
      New_Death_Date      = new_date,
      Previous_Death_Cause= prev_cause,
      Death_cause         = new_cause
    ) %>%
    select(
      Insectary_ID,
      Previous_Death_Date, New_Death_Date, Previous_Death_Cause, Death_cause,
      `CLUTCH NUMBER`, SPECIES, Sex, everything()
    ) %>%
    select(-Death_date)

  if ("Notes_Insectary_data" %in% names(tbl) && "Death_cause" %in% names(tbl)) {
    tbl <- dplyr::relocate(tbl, Notes_Insectary_data, .after = Death_cause)
  }
  tbl
}

# -- Helper for Registrar Tubos tab --
build_tube_table <- function(insectary_df, ids) {
  insectary_df %>%
    filter(.data$Insectary_ID %in% ids) %>%
    select(Insectary_ID, CAM_ID, Death_date, Death_cause, Preservation_date, Preserved_Dead_Alive,
           Tube_1_id, Tube_1_tissue, T1_Preservation_medium,
           Tube_2_id, Tube_2_tissue, T2_Preservation_medium,
           Tube_3_id, Tube_3_tissue,
           Tube_4_id, Tube_4_tissue, everything())
}

# -- Helper for Registrar Emergidos tab --
auto_start_insectary_id <- function(df) {
  last_filled <- suppressWarnings(max(which(!is.na(df$SPECIES) & df$SPECIES != ""), na.rm = TRUE))
  if (!is.finite(last_filled)) return(df$Insectary_ID[1])
  if (last_filled < nrow(df)) df$Insectary_ID[last_filled + 1] else NA_character_
}

# --- DRY helpers added ---
apply_clutch_side_defaults <- function(row_df) {
  if ("Wild_Reared" %in% names(row_df) && is_na_like_val(row_df$Wild_Reared[1])) {
    row_df$Wild_Reared <- "Reared"
  }
  if ("Collection_location" %in% names(row_df) && is_na_like_val(row_df$Collection_location[1])) {
    row_df$Collection_location <- "Mariposario Ikiam"
  }
  row_df
}

## Unified row-level defaults (wrapper) ----------------------------------------
# Applies stock/species rules AND simple clutch-side defaults in one place.
apply_row_core_defaults <- function(row_df, stocks_df) {
  # 1) SPECIES/Stock_of_origin + research extras
  row_df <- apply_stock_origin_rule(row_df, stocks_df)
  # 2) Side defaults driven by clutch presence (Wild_Reared, Collection_location)
  row_df <- apply_clutch_side_defaults(row_df)
  row_df
}

collect_live_tubes <- function(cur_df) {
  if (is.null(cur_df) || !nrow(cur_df)) return(NULL)
  parts <- list()
  for (j in TUBE_SLOTS) {
    idc <- paste0("Tube_", j, "_id")
    tic <- paste0("Tube_", j, "_tissue")
    mic <- paste0("T", j, "_Preservation_medium")
    if (!(idc %in% names(cur_df))) next
    ids_now <- as.character(cur_df[[idc]])
    keep <- !is.na(ids_now) & ids_now != "" & ids_now != "NA"
    if (!any(keep)) next
    parts[[length(parts)+1]] <- data.frame(
      Tube_id = ids_now[keep],
      Tube_tissue = if (tic %in% names(cur_df)) cur_df[[tic]][keep] else NA,
      Tube_medium = if (mic %in% names(cur_df)) cur_df[[mic]][keep] else NA,
      Preservation_date = fmt_ui_date(Sys.Date()),
      stringsAsFactors = FALSE
    )
  }
  if (!length(parts)) NULL else dplyr::bind_rows(parts)
}

apply_whole_org_defaults_for_row <- function(tbl, i, j, tissue_def, medium_def, today_ui,
                                             autofill_na = FALSE) {
  id_col  <- paste0("Tube_", j, "_id")
  tis_col <- paste0("Tube_", j, "_tissue")
  med_col <- paste0("T", j, "_Preservation_medium")

  if (tis_col %in% names(tbl)) {
    tv <- tbl[[tis_col]][i]
    if (is_na_like_val(tv)) tbl[[tis_col]][i] <- tissue_def
  }
  if (j <= 2 && med_col %in% names(tbl)) {
    mv <- tbl[[med_col]][i]
    # If caller didn't provide a medium default, use NOT_COLLECTED for current slot when blank
    md <- medium_def %||% "NOT_COLLECTED"
    if (is_na_like_val(mv)) tbl[[med_col]][i] <- md
  }
  is_whole <- (tis_col %in% names(tbl)) && identical(tbl[[tis_col]][i], TISSUE_WHOLE)
  if (is_whole) {
    if ("Preservation_date" %in% names(tbl)) {
      pd <- fmt_ui_date(tbl$Preservation_date[i])
      if (is_na_like_val(pd)) tbl$Preservation_date[i] <- today_ui
    }
    if ("Death_date" %in% names(tbl)) {
      dd <- fmt_ui_date(tbl$Death_date[i])
      if (is_na_like_val(dd)) tbl$Death_date[i] <- today_ui
    }
    if ("Location_body" %in% names(tbl)) {
      lb <- tbl$Location_body[i]
      if (is_na_like_val(lb)) tbl$Location_body[i] <- "Ikiam"
    }
    if (isTRUE(autofill_na)) {
      for (jj in (j+1):max(TUBE_SLOTS)) {
        ii <- paste0("Tube_", jj, "_id"); tt <- paste0("Tube_", jj, "_tissue")
        if (ii %in% names(tbl)) tbl[[ii]][i] <- "NA"
        if (tt %in% names(tbl)) tbl[[tt]][i] <- "NA"
        if (jj <= 2) {
          mm <- paste0("T", jj, "_Preservation_medium")
          if (mm %in% names(tbl)) tbl[[mm]][i] <- "NOT_COLLECTED"
        }
      }
    }
  }
  tbl
}

update_ids_picker <- function(session, input_id, df, selected = NULL,
                              start_from_id = NULL, offset_before = 200) {
  # Preserve original DF order (first occurrence)
  ids_all <- as.character(df$Insectary_ID)
  ids <- ids_all[!duplicated(ids_all)]

  # If we have a pivot ID, rotate the vector so rendering starts ~offset_before before it
  if (!is.null(start_from_id) && nzchar(start_from_id) && length(ids)) {
    idx <- match(start_from_id, ids)
    if (!is.na(idx)) {
      start_idx <- max(1L, idx - as.integer(offset_before))
      if (start_idx > 1L) {
        ids <- c(ids[start_idx:length(ids)], ids[1:(start_idx-1L)])
      }
    }
  }
  updateSelectizeInput(
    session, input_id,
    choices  = ids,
    selected = selected,
    server   = TRUE,
    options  = list(sortField = NULL)   # don't let Selectize re-sort
  )
}

suggest_next_id_after_block <- function(df, start_id, n) {
  pos <- which(df$Insectary_ID == start_id)
  if (!length(pos)) return("")
  next_pos <- pos + n
  if (next_pos <= nrow(df)) df$Insectary_ID[next_pos] else ""
}

normalize_batch_ids <- function(df) {
  if (!nrow(df)) return(df)
  df$batch_id <- ifelse(is.na(df$batch_id) | df$batch_id=="",
                        format(df$ts, "%Y-%m-%d %H:%M:%S"),
                        df$batch_id)
  df
}

toggle_btn_html <- function(key, status) {
  lab <- if (status == "Undone") "Redo" else "Undo"
  cls <- if (status == "Undone") "btn-default btn-sm" else "btn-warning btn-sm"
  as.character(actionButton(paste0("chg_toggle_", digest::digest(key)), lab, class = cls))
}


## UI Definitions for Each Tab -------------------------------------------------

# -- Tab 1: Ingreso (Login) --
IngresoTab <- tabPanel(
  "Ingreso",
  fluidPage(
    tags$head(tags$style(HTML(
      ".login-card{max-width:680px;margin:18px auto;padding:18px 20px;border:1px solid #e5e7eb;border-radius:12px;box-shadow:0 1px 2px rgba(0,0,0,0.04);} " ,
      ".muted{color:#6b7280;} .ok{color:#065f46;} .err{color:#7f1d1d;} "
    ))),
    div(class="login-card",
      h3("Inicio de sesión"),
      fluidRow(
        column(6,
          # Quick pick shows labels; values are keys
          selectizeInput(
            "user_quickpick", "Seleccionar usuario",
            choices = setNames(users_df$key, users_df$label),  # labels shown, keys stored
            selected = NULL,
            options = list(placeholder = "Elige un usuario…", allowEmptyOption = TRUE)
          )
        ),
        column(6,
          textInput("user_login", "Usuario", value = "", placeholder = "Iniciales (p. ej., AA)")
        )
      ),
      fluidRow(
        column(6,
          passwordInput("user_pwd", "Contraseña"),
          checkboxInput("show_pwd", "Mostrar contraseña", value = FALSE)
        ),
        column(6,
          actionButton("login_btn", "Iniciar sesión", class = "btn-primary"),
          actionButton("logout_btn", "Cerrar sesión", class = "btn-default", style="margin-left:8px;")
        )
      ),
      hr(),
      htmlOutput("login_status")
    )
  )
)

# -- Tab 2: Registrar Muertes --
RegistrarMuertesTab <- tabPanel(
  "Registrar Muertes",
  fluidPage(
    tags$head(tags$style(HTML(
      ".dimmed{opacity:0.5; pointer-events:none;} .inline-controls{display:flex; gap:10px; align-items:flex-end;}"
    ))),
    fluidRow(
      column(
        12,
        div(class = "inline-controls",
          selectizeInput(
            "dead_ids", "IDs",
            choices = NULL, multiple = TRUE,
            options = SEL_OPTS_MULTI_IDS
          ),
          checkboxInput("dead_review_only", "Solo revisar", value = FALSE)
        )
      )
    ),
    fluidRow(
      column(
        12,
        uiOutput("dead_controls_row")
      )
    ),
    fluidRow(
      column(
        12,
        div(class = "inline-controls",
          actionButton("dead_load", "Cargar/Revertir", class = "btn-primary"),
          actionButton("dead_add", "Añadir a la tabla", class = "btn-default"),
          save_button_ui("dead_save_btn")
        )
      )
    ),
    br(),
    fluidRow(
      column(
        12,
        make_table_ui("dead_table", after_table_ui = tagList(
          checkboxInput("dead_show_photos", "Mostrar fotos", value = FALSE),
          uiOutput("dead_photos")
        ), height_spacer_px = 320)
      )
    )
  )
)

# -- Tab 3: Registrar Tubos --
RegistrarTubosTab <- tabPanel(
  "Registrar Tubos",
  fluidPage(
    # Row 1: IDs + CAM ID inicial (left-aligned)
    fluidRow(
      column(6,
        selectizeInput(
          "tube_ids", "IDs",
          choices = NULL, multiple = TRUE,
          options = SEL_OPTS_MULTI_IDS
        )
      ),
      column(6,
        selectizeInput(
          "cam_start_id", "CAM ID inicial",
          choices = NULL, multiple = FALSE,
          options = list(
            create = TRUE,
            createOnBlur = TRUE,
            persist = FALSE,
            selectOnTab = FALSE,
            openOnFocus = TRUE,
            placeholder = "Escriba o elija sugerencia",
            searchField = c("value","label"),
            plugins = list("restore_on_backspace"),
            onDelete = I("function(values){ if(this.items.length){ var val=this.items[0]; this.setTextboxValue(val); this.clear(true); return false; } }"),
            onType = I("function (str) { Shiny.setInputValue('cam_start_id_draft', str, {priority:'event'}); }"),
            render = I("{option: function(item, escape){var lbl=item.label||item.text||item.value;return '<div>'+escape(lbl)+'</div>';}, item: function(item, escape){return '<div>'+escape(item.value)+'</div>';}}"),
            onBlur = SELECTIZE_CONFIRM_ONBLUR
          )
        )
      )
    ),

    # Row 2: Tissue / Medium / Date / Autofill NA
    fluidRow(
      column(3,
        selectInput(
          "tube_tissue_default", "Tejido por defecto",
          choices = c("WHOLE_ORGANISM", "HEAD | ABDOMEN", "HEAD",
                      "**OTHER_SOMATIC_ANIMAL_TISSUE** | WING CLIP", "THORAX | LEG"),
          selected = "**OTHER_SOMATIC_ANIMAL_TISSUE** | WING CLIP"
        )
      ),
      column(3,
        selectInput(
          "tube_medium_default", "Medio por defecto",
          choices = c("Ethanol", "DMSO", "Flash frozen", "NA"),
          selected = "Flash frozen"
        )
      ),
      column(3,
        dateInput("tube_pres_date", "Preservation_date",
                  value = Sys.Date(), format = "d-M-yy", language = "en")
      ),
      column(3,
        br(),  # align checkbox vertically with inputs without custom CSS
        checkboxInput("tube_autofill_na", "Autocompletar con NAs columnas con tubos vacíos si Tejido es WHOLE_ORGANISM", value = TRUE)
      )
    ),

    # Row 3: Tube ID inicial + Tube ID (para añadir) (left-aligned)
    fluidRow(
      column(6,
        selectizeInput(
          "tube_start_id", "Tube ID inicial (p.ej., FS00000001)",
          choices = NULL,
          options = list(
            create = TRUE, createOnBlur = TRUE, persist = FALSE, selectOnTab = FALSE,
            placeholder = "Escriba o elija sugerencia",
            render = I("{option: function(item, escape){var lbl=item.label||item.text||item.value;return '<div>'+escape(lbl)+'</div>';}, item: function(item, escape){return '<div>'+escape(item.value)+'</div>';}}"),
            searchField = c("value","label"),
            plugins = list("restore_on_backspace"),
            onDelete = I("function(values){ if(this.items.length){ var val=this.items[0]; this.setTextboxValue(val); this.clear(true); return false; } }"),
            onType = I("function (str) { Shiny.setInputValue('tube_start_id_draft', str, {priority:'event'}); }"),
            onBlur = SELECTIZE_CONFIRM_ONBLUR
          )
        )
      ),
      column(6,
        selectizeInput(
          "tube_add_start_id", "Tube ID (para añadir)",
          choices = NULL,
          options = list(
            create = TRUE, createOnBlur = TRUE, persist = FALSE, selectOnTab = FALSE, openOnFocus = TRUE,
            placeholder = "Escriba o elija sugerencia",
            searchField = c("value","label"),
            plugins = list("restore_on_backspace"),
            onDelete = I("function(values){ if(this.items.length){ var val=this.items[0]; this.setTextboxValue(val); this.clear(true); return false; } }"),
            onType = I("function (str) { Shiny.setInputValue('tube_add_start_id_draft', str, {priority:'event'}); }"),
            render = I("{option: function(item, escape){var lbl=item.label||item.text||item.value; return '<div>'+escape(lbl)+'</div>';}, item: function(item, escape){return '<div>'+escape(item.value)+'</div>';}}"),
            onBlur = SELECTIZE_CONFIRM_ONBLUR
          )
        )
      )
    ),

    # Row 4: buttons on their own row (left-aligned)
    fluidRow(
      column(12,
        actionButton("tube_apply", "Cargar/Revertir", class = "btn-primary"),
        actionButton("tube_add",   "Añadir a la tabla", class = "btn-default"),
        save_button_ui("tube_save_btn")
      )
    ),

    # Table + spacer directly after table
    fluidRow(
      column(12, make_table_ui("tube_table", height_spacer_px = 320))
    )
  )
)

# -- Tab 4: Registrar Emergidos --
RegistrarEmergidosTab <- tabPanel(
  "Registrar Emergidos",
  fluidPage(
    tags$head(tags$style(HTML("
      .inline{display:flex;gap:12px;align-items:flex-end;flex-wrap:wrap;margin-bottom:10px;}
      .grow{flex:1 1 180px;}
    "))),

          # Row 1: Block setup
      div(class="inline",
        div(class="grow", selectInput("em_clutch", "CLUTCH NUMBER", choices = NULL)),
        div(class="grow", numericInput("em_n", "Número de individuos", value = 1, min = 1)),
        div(class="grow",
            selectizeInput("em_start_id", "Insectary ID inicial",
              choices = NULL, multiple = FALSE, options = SEL_OPTS_IDS)
        ),
        div(class="grow", dateInput("em_intro_date", "Intro a Insectario", value = Sys.Date(), format = "d-M-yy", language = "en"))
      ),

      # Row 2: Add-one start (only the input)
      div(class="inline",
        div(class="grow",
            selectizeInput("em_add_id", "Insectary ID para añadir",
              choices = NULL, multiple = FALSE, options = SEL_OPTS_IDS)
        )
      ),

    # Row 3: WHOLE_ORGANISM autofill toggle (same behavior as Tubos)
    div(class="inline",
      checkboxInput(
        "em_autofill_na",
        "Autocompletar con NAs columnas con tubos vacíos si Tejido es WHOLE_ORGANISM", value = TRUE)
    ),

    # Row 4: Buttons alone in one row
    div(class="inline",
      actionButton("em_load", "Preparar/Revertir filas", class = "btn-primary"),
      actionButton("em_add",  "Añadir a la tabla", class = "btn-default"),
      save_button_ui("em_save_btn")
    ),

    # Table + spacer
    fluidRow(
      column(12, make_table_ui("em_table", height_spacer_px = 320))
    )
  )
)

# -- Tab 5: Subir Cambios --
SubirCambiosTab <- tabPanel(
  "Subir Cambios",
  fluidPage(
    # Row 1: filters
    fluidRow(
      column(6, selectizeInput("chg_user_filter", "Cambios del usuario", choices = NULL, multiple = TRUE,
                               options = list(placeholder = "Todos (ninguno seleccionado)"))),
      column(6, selectizeInput("chg_commit_filter", "Commit(s)", choices = NULL, multiple = TRUE,
                               options = list(placeholder = "Todos (ninguno seleccionado)")))
    ),
    # Row 2: buttons
    fluidRow(
      column(12,
        div(class = "inline-controls",
          actionButton("chg_show", "Mostrar cambios", class = "btn-primary"),
          auth_button_ui("chg_commit"),
          auth_button_ui("chg_update_db"),
          auth_button_ui("chg_clear")
        )
      )
    ),
    fluidRow(
      column(12, DTOutput("chg_table"))
    )
  )
)

# -- Tab 6: Historial de Cambios --
HistorialCambiosTab <- tabPanel(
  "Historial de Cambios",
  fluidPage(
    tags$head(tags$style(HTML("
      .inline{display:flex;gap:12px;align-items:flex-end;flex-wrap:wrap;margin-bottom:10px;}
      .grow{flex:1 1 180px;}
    "))),

    # Row 1: Date controls + 'usar rango'
    div(class="inline",
      div(class="grow",
        dateRangeInput("hist_range", "Rango de fechas",
          start = Sys.Date(), end = Sys.Date(), format = "d-M-yy")
      ),
      checkboxInput("hist_use_range", "Usar rango de fechas", value = FALSE)
    ),

    # Row 2: User / Commit / ID
    div(class="inline",
      div(class="grow",
        selectizeInput("hist_users", "Usuarios",
          choices = c("ALL"), multiple = TRUE,
          options = list(placeholder = "ALL por defecto"))
      ),
      div(class="grow",
        selectInput("hist_commits", "Commits", choices = c("ALL"), selected = "ALL")
      ),
      div(class="grow",
        selectizeInput("hist_ids", "Insectary_IDs",
          choices = NULL, multiple = TRUE,
          options = list(placeholder = "Todos"))
      ),
      actionButton("hist_load", "Buscar", class = "btn-primary"),
      auth_button_ui("hist_retry"),
      auth_button_ui("hist_undo")
    ),

    fluidRow(column(12, DTOutput("hist_table")))
  )
)

## -- Tab 7: Buscador --
BuscadorTab <- tabPanel(
  "Buscador",
  fluidPage(
    tags$head(tags$style(HTML("
      .inline{display:flex;gap:12px;align-items:flex-end;flex-wrap:wrap;margin-bottom:10px;}
      .grow{flex:1 1 220px;}
    "))),
    div(class="inline",
      div(class="grow", textInput("srch_query", "Buscar", value = "", placeholder = "Escribe texto a buscar…")),
      actionButton("srch_go", "Buscar", class = "btn-primary"),
      actionButton("srch_load", "Cargar/Revertir", class = "btn-default"),
      uiOutput("srch_save_btn_ui")
    ),
    fluidRow(
      column(12, make_table_ui("srch_table", height_spacer_px = 320))
    )
  )
)


## Final Assembled UI ----------------------------------------------------------
ui <- tagList(
  tags$head(
    tags$title("Ikiam Insectary DB"),
    tags$style(HTML("
    .selectize-dropdown{z-index:2000 !important;}
    .dropdown-menu{z-index:2000 !important;}
    .handsontable .htAutocomplete,
    .handsontable .handsontableEditor,
    .handsontable .htMenu,
    .handsontable .handsontableInput{z-index:2000 !important;}
    .rhandsontable{position:relative !important; overflow:visible !important;}
    .tab-pane, .container-fluid{overflow:visible !important;}
    /* Ensure Handsontable autocomplete/dropdown arrow is visible */
    .handsontable .htAutocompleteArrow { display: inline-block !important; opacity: 1 !important; }
    .handsontable td.htAutocomplete { background-clip: padding-box; }
    /* CAM_ID validation styling */
    .handsontable td.htInvalid { background-color: #FDE2E1 !important; }
  ")),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('forceHotCommit', function(id){
      try {
        document.activeElement && document.activeElement.blur();
        var el = document.getElementById(id);
        if(!el) return;
        var hot = $(el).find('.handsontable').data('handsontable');
        if(hot && hot.getActiveEditor && hot.getActiveEditor().isOpened()){
          hot.getActiveEditor().finishEditing();
        }
      } catch(e) {}
    });
    
    document.addEventListener('keydown', function(e){
      if (e.key === 'Enter') {
        var pwd = document.getElementById('user_pwd');
        var usr = document.getElementById('user_login');
        var inLogin = (pwd && document.activeElement === pwd) || (usr && document.activeElement === usr);
        if (inLogin) {
          e.preventDefault(); // stop default submit-like behavior
          // Push the latest values to Shiny immediately
          if (usr) Shiny.setInputValue('user_login', usr.value, {priority: 'event'});
          if (pwd) Shiny.setInputValue('user_pwd',   pwd.value, {priority: 'event'});
          // Click after a short tick so the server sees the updated values
          setTimeout(function(){
            var btn = document.getElementById('login_btn');
            if (btn) btn.click();
          }, 30);
        }
      }
    });
    
    Shiny.addCustomMessageHandler('jsCode', function(m){ try{ eval(m.code); }catch(e){} });

    // Minimal: ask browser to store credentials (Chrome/Edge/Safari; no reload)
    Shiny.addCustomMessageHandler('saveCreds', function(m){
      try{
        if ('credentials' in navigator && 'PasswordCredential' in window) {
          var c = new PasswordCredential({id: (m.user||''), password: (m.pwd||'')});
          navigator.credentials.store(c);
        }
      }catch(e){ /* ignore */ }
    });

    // Bind Handsontable edit hooks to signal editing state to Shiny
    Shiny.addCustomMessageHandler('hotBindHooks', function(m){
      try{
        var id = m && m.id; if(!id) return;
        var root = document.getElementById(id);
        if(!root) return;
        var hot = $(root).find('.handsontable').data('handsontable');
        if(!hot || hot.__hooksBound) return;
        hot.__hooksBound = true;
        hot.addHook('afterBeginEditing', function(){
          Shiny.setInputValue(id + '_editing', true, {priority:'event'});
        });
        hot.addHook('afterFinishEditing', function(){
          Shiny.setInputValue(id + '_editing', false, {priority:'event'});
        });
      }catch(e){ /* noop */ }
    });
  ")),
),
  navbarPage(
    title = "Ikiam Insectary DB",
    id = "nav_main",
    
    IngresoTab,
    RegistrarMuertesTab,
    RegistrarTubosTab,
    RegistrarEmergidosTab,
    BuscadorTab,
    SubirCambiosTab,
    HistorialCambiosTab
  )
)
# ==============================================================================
# SECTION 4: MAIN SERVER IMPLEMENTATION
# Purpose: Contains the complete server function, including state initialization,
#          login/logout logic, and the server-side wiring for all tabs
#          with the final patches applied.
# ------------------------------------------------------------------------------

## Per-Tab Server Logic Modules ------------------------------------------------

# -- Server logic for: Registrar Tubos --
register_tubos_server <- function(input, output, session, rv) {
  # Track rows that already received a Tube_*_id via "Añadir a la tabla"
  rv$tube_added_ids <- character(0)
  # Remember the last CAM_ID this session wrote (for +1 on next add/apply)
  rv$last_cam_written <- NULL
  
  observe({
    req(rv$data$Insectary_data)
    df <- rv$data$Insectary_data
    pivot_id <- auto_start_insectary_id(df)  # reuse as anchor
    update_ids_picker(session, "tube_ids", df, start_from_id = pivot_id, offset_before = 200)
    # Suggestions for Tube start id
    sug <- build_tube_id_suggestions(rv$data$Insectary_data)
    # Convert to named list for selectize rendering
    vals <- as.list(sug$value)
    names(vals) <- sug$label
    # keep previous selections
    updateSelectizeInput(session, "tube_start_id",
      choices = vals, selected = isolate(input$tube_start_id), server = TRUE)
    updateSelectizeInput(session, "tube_add_start_id",
      choices = vals, selected = isolate(input$tube_add_start_id), server = TRUE)
    
    # CAM-ID inicial suggestions WITH date labels
    cam_sug <- build_camid_suggestions_labeled(rv$data$Insectary_data, max_groups = 4)
    cam_vals <- as.list(cam_sug$value); names(cam_vals) <- cam_sug$label
    updateSelectizeInput(session, "cam_start_id",
      choices  = cam_vals,
      selected = isolate(input$cam_start_id),
      server   = TRUE
    )

    restore_selectize_draft(session, "cam_start_id",       isolate(input$cam_start_id_draft))
    restore_selectize_draft(session, "tube_start_id",      isolate(input$tube_start_id_draft))
    restore_selectize_draft(session, "tube_add_start_id",  isolate(input$tube_add_start_id_draft))

    # NEW: Tejido por defecto desde Lists, ordenado por frecuencia
    ord_tissues <- get_ordered_tissues(rv)
    cur_sel <- isolate(input$tube_tissue_default)
    if (is.null(cur_sel) || !(cur_sel %in% ord_tissues)) {
      cur_sel <- ord_tissues[1]
    }
    updateSelectInput(session, "tube_tissue_default",
                      choices = ord_tissues,
                      selected = cur_sel)
  })

  # PATCH 1A: Auto-check NA autofill AND blank date input when tissue != WHOLE_ORGANISM
  observeEvent(input$tube_tissue_default, {
    is_whole <- identical(input$tube_tissue_default, "WHOLE_ORGANISM")
    updateCheckboxInput(session, "tube_autofill_na", value = is_whole)
    if (is_whole) {
      # Keep or set date
      if (is.null(input$tube_pres_date) || is.na(input$tube_pres_date)) {
        updateDateInput(session, "tube_pres_date", value = Sys.Date())
      }
    } else {
      # Blank the user entry date input
      updateDateInput(session, "tube_pres_date", value = NULL)
    }
  }, ignoreInit = TRUE)

  # Restore CAM draft text on tab navigation
  observeEvent(input$nav_main, {
    if (identical(input$nav_main, "Registrar Tubos")) {
      restore_selectize_draft(session, "cam_start_id",      isolate(input$cam_start_id_draft))
      restore_selectize_draft(session, "tube_start_id",     isolate(input$tube_start_id_draft))
      restore_selectize_draft(session, "tube_add_start_id", isolate(input$tube_add_start_id_draft))
    }
  }, ignoreInit = TRUE)

  rv$tube_tbl <- NULL

  observeEvent(input$tube_apply, {
    req(input$tube_ids)
    ids <- input$tube_ids
    start_id <- input$tube_start_id %||% ""
    tissue_def <- input$tube_tissue_default
    medium_def <- input$tube_medium_default
    date_ui <- fmt_ui_date(input$tube_pres_date %||% Sys.Date())
    cam_cur <- input$cam_start_id %||% ""

    rv$tube_added_ids <- character(0)  # clear per-apply session memory

    tube_seq <- if (nzchar(start_id)) generate_consecutive_tube_ids(start_id, length(ids)) else rep("", length(ids))

    df <- rv$data$Insectary_data

    last_cam_assigned <- NULL  # track the last CAM_ID we actually wrote
    for (k in seq_along(ids)) {
      id <- ids[[k]]
      idx <- which(df$Insectary_ID == id)
      if (!length(idx)) next

      # Find first available tube slot and fill
      for (j in TUBE_SLOTS) {
        tube_field   <- paste0("Tube_", j, "_id")
        tissue_field <- paste0("Tube_", j, "_tissue")
        medium_field <- paste0("T", j, "_Preservation_medium")
        val <- df[[tube_field]][idx]
        if (is_na_like_val(val)) {
          df[[tube_field]][idx] <- tube_seq[[k]]
          df[[tissue_field]][idx] <- tissue_def
          if (j <= 2 && "T1_Preservation_medium" %in% names(df)) df[[medium_field]][idx] <- medium_def
          df <- apply_whole_org_defaults_for_row(
            df, i = idx, j = j, tissue_def = tissue_def, medium_def = medium_def,
            today_ui = date_ui, autofill_na = input$tube_autofill_na
          )
          break
        }
      }
      
      # --- CAM_ID autofill in ID order, skipping non-empty/non-NA without consuming ---
              if (nzchar(cam_cur) && "CAM_ID" %in% names(df)) {
          cur_cam <- df$CAM_ID[idx]
          if (is_na_like_val(cur_cam) || toupper(cur_cam)=="NA" || cur_cam=="") {
            # write and advance to next CAM
            df$CAM_ID[idx] <- cam_cur
            last_cam_assigned <- cam_cur
            cam_cur <- increment_alphanumeric_id(cam_cur)
          } else {
            # skip: do NOT advance; same cam_cur will be tried on next row
          }
        }
    }
    # Remember last CAM written during this apply click (for future adds)
    if (!is.null(last_cam_assigned) && nzchar(last_cam_assigned)) {
      rv$last_cam_written <- last_cam_assigned
    }

    rv$tube_tbl <- build_tube_table(df, ids)
    hot_render_predictive(output, session, "tube_table",
                          rv$tube_tbl, rv,
                          prev_df = NULL, update_inputs = "none",
                          force = TRUE)

    # --- NEW: prime "Tube ID (para añadir)" with the next after those just used ---
    if (nzchar(start_id) && length(ids) > 0) {
      # next after N generated: start + length(ids)
      next_after_apply <- generate_consecutive_tube_ids(start_id, length(ids) + 1L)
      next_after_apply <- tail(next_after_apply, 1)
      # label includes medium + date (like the initial suggestions)
      lbl <- paste(next_after_apply, input$tube_medium_default, fmt_ui_date(input$tube_pres_date %||% Sys.Date()))
      choices <- list(); choices[[lbl]] <- next_after_apply
      # prepend our next value while preserving any existing choices
      old <- isolate(input$tube_add_start_id)
      if (!is.null(old) && nzchar(old) && old != next_after_apply) {
        choices[[old]] <- old
      }
      updateSelectizeInput(session, "tube_add_start_id", choices = choices, selected = next_after_apply, server = TRUE)
    }
  })

  # Reuse a generic debounced observer for tube_table
  hot_debounced_observer(
    input, output, session, rv,
    input_id  = "tube_table",
    rv_slot   = "tube_tbl",
    process_fun = function(prev_tbl, new_tbl) {
      # 1) clutch-driven defaults only when CLUTCH NUMBER changed
      new_tbl <- apply_clutch_defaults_on_change(prev_tbl, new_tbl, rv$data$Insectary_stocks)
      # 2) detect newly-filled Tube_*_id and backfill dependent fields
      today_ui   <- fmt_ui_date(input$tube_pres_date %||% Sys.Date())
      tissue_def <- input$tube_tissue_default
      medium_def <- input$tube_medium_default
      for (j in TUBE_SLOTS) {
        idc <- paste0("Tube_", j, "_id")
        if (!(idc %in% names(new_tbl))) next
        prev_vec <- if (!is.null(prev_tbl) && idc %in% names(prev_tbl)) prev_tbl[[idc]] else rep(NA_character_, nrow(new_tbl))
        cur_vec  <- new_tbl[[idc]]
        changed_rows <- which(is_na_like_val(prev_vec) & !is_na_like_val(cur_vec))
        if (!length(changed_rows)) next
        for (i in changed_rows) {
          new_tbl <- apply_whole_org_defaults_for_row(
            new_tbl, i = i, j = j,
            tissue_def = tissue_def, medium_def = medium_def,
            today_ui = today_ui, autofill_na = input$tube_autofill_na
          )
        }
      }
      # 3) detect Tube_*_tissue changes to WHOLE_ORGANISM and cascade NA/NOT_COLLECTED
      for (j in TUBE_SLOTS) {
        tis_col <- paste0("Tube_", j, "_tissue")
        if (!(tis_col %in% names(new_tbl))) next
        prev_t  <- if (!is.null(prev_tbl) && tis_col %in% names(prev_tbl)) prev_tbl[[tis_col]] else rep(NA_character_, nrow(new_tbl))
        cur_t   <- new_tbl[[tis_col]]
        to_whole <- which(!is_na_like_val(cur_t) &
                          grepl("^\\s*WHOLE_ORGANISM\\s*$", cur_t, ignore.case = TRUE) &
                          (is_na_like_val(prev_t) | !grepl("^\\s*WHOLE_ORGANISM\\s*$", prev_t, ignore.case = TRUE)))
        if (!length(to_whole)) next
        for (i in to_whole) {
          new_tbl <- apply_whole_org_defaults_for_row(
            new_tbl, i = i, j = j,
            tissue_def = TISSUE_WHOLE, medium_def = medium_def,
            today_ui = today_ui, autofill_na = input$tube_autofill_na
          )
        }
      }
      new_tbl
    },
    update_inputs = "none",
    start_values  = list(start = input$tube_start_id, add = input$tube_add_start_id),
    debounce_ms   = 200,
    key_columns   = c("CLUTCH NUMBER","CAM_ID",
                      paste0("Tube_", 1:4, "_tissue"))
  )

  observeEvent(input$tube_add, {
    req(rv$data$Insectary_data)
    df <- rv$data$Insectary_data

    # 1) current table (before merge)
    cur_tbl <- tryCatch(hot_to_r(input$tube_table), error = function(e) NULL) %||%
               rv$tube_tbl %||% df[0, , drop = FALSE]
    cur_ids_before <- if ("Insectary_ID" %in% names(cur_tbl)) cur_tbl$Insectary_ID else character(0)

    # 2) rows the user is trying to add this click
    add_df <- if (!is.null(input$tube_ids) && length(input$tube_ids))
      build_tube_table(df, input$tube_ids) else df[0, , drop = FALSE]
    add_ids <- if (nrow(add_df)) add_df$Insectary_ID else character(0)

    # 3) only keep truly-new rows (not in table yet) AND not yet assigned this session
    new_ids_this_click <- setdiff(setdiff(add_ids, cur_ids_before), rv$tube_added_ids)

    # 4) merge once (dedup by Insectary_ID)
    merged <- suppressWarnings(dplyr::bind_rows(cur_tbl, add_df))
    if ("Insectary_ID" %in% names(merged)) {
      merged <- merged[!duplicated(merged$Insectary_ID), , drop = FALSE]
    }

    # 5) write Tube ID (para añadir) ONLY to those new rows we kept above
    tbl <- merged
    cur_id <- input$tube_add_start_id %||% ""

    # Seed for CAM_ID autofill from memory of last written; else CAM ID inicial
    cam_cur_add <- rv$last_cam_written %||% ""
    cam_cur_add <- if (nzchar(cam_cur_add)) increment_alphanumeric_id(cam_cur_add) else (input$cam_start_id %||% "")
    if (nzchar(cur_id) && nrow(tbl) && length(new_ids_this_click)) {
      tissue_def <- input$tube_tissue_default
      medium_def <- input$tube_medium_default
      today_ui   <- fmt_ui_date(input$tube_pres_date %||% Sys.Date())

      rows_to_fill <- which(tbl$Insectary_ID %in% new_ids_this_click)

      for (i in rows_to_fill) {
        wrote <- FALSE
        for (j in TUBE_SLOTS) {
          id_col  <- paste0("Tube_", j, "_id")
          tis_col <- paste0("Tube_", j, "_tissue")
          med_col <- paste0("T", j, "_Preservation_medium")
          if (!(id_col %in% names(tbl))) next

          val <- tbl[[id_col]][i]
          if (is_na_like_val(val)) {
            # write here
            tbl[[id_col]][i] <- cur_id
            tbl <- apply_whole_org_defaults_for_row(
              tbl, i = i, j = j,
              tissue_def = tissue_def, medium_def = medium_def,
              today_ui = today_ui, autofill_na = input$tube_autofill_na
            )

            wrote <- TRUE
            break
          }
        }

        if (wrote) {
          # mark this row as done for this session and only then increment
          rv$tube_added_ids <- unique(c(rv$tube_added_ids, tbl$Insectary_ID[i]))
          cur_id <- increment_alphanumeric_id(cur_id)
        }

        # --- CAM_ID autofill in add-flow (only if empty/"NA") ---
        if (nzchar(cam_cur_add) && "CAM_ID" %in% names(tbl)) {
          cur_cam <- tbl$CAM_ID[i]
          if (is_na_like_val(cur_cam) || toupper(cur_cam) == "NA" || cur_cam == "") {
            tbl$CAM_ID[i] <- cam_cur_add
            rv$last_cam_written <- cam_cur_add
            cam_cur_add <- increment_alphanumeric_id(cam_cur_add)
          }
        }
      }
    }

    rv$tube_tbl <- tbl

    # re-render & refresh predictions; keep 'Tube ID inicial' selection as-is
    hot_render_predictive(output, session, "tube_table",
                          rv$tube_tbl, rv,
                          prev_df = NULL, update_inputs = "add_only",
                          start_values = list(start = input$tube_start_id, add = cur_id),
                          force = TRUE)

    # After adding, set "Tube ID (para añadir)" to the next after those just written.
    # 'cur_id' has already been incremented by the loop; use it and include label.
    if (nzchar(cur_id)) {
      next_after_add <- cur_id
      lbl <- paste(next_after_add, input$tube_medium_default, fmt_ui_date(input$tube_pres_date %||% Sys.Date()))
      ch <- list(); ch[[lbl]] <- next_after_add
      updateSelectizeInput(session, "tube_add_start_id", choices = ch, selected = next_after_add, server = TRUE)
    }
  })

  render_save_button(output, "tube_save_btn", reactive(rv$auth))
  observeEvent(input$tube_save_btn, {
    if (!auth_is_logged_in(rv)) { showNotification("Inicia sesión para guardar.", type = "error"); return() }

    force_hot_commit(session, "tube_table")

    tbl <- rv$tube_tbl %||% hot_to_r(input$tube_table)
    req(nrow(tbl) > 0)
    apply_table_changes(rv, tab_label = "Registrar Tubos", tbl = tbl,
                        skip_if_blank_cols = c("Preservation_date"))

    # --- REFRESH VISIBLE ROWS FROM UPDATED BASE, PRESERVING SELECTION ---
    # 1) persist already happened inside apply_table_changes; take current ids
    sel_ids <- unique(na.omit(tbl$Insectary_ID))
    # 2) rebuild the same rows from the updated base and re-render
    rv$tube_tbl <- build_tube_table(rv$data$Insectary_data, sel_ids)
    hot_render_predictive(output, session, "tube_table", rv$tube_tbl, rv, force = TRUE)
    # 3) keep the IDs picker (choices + selection) stable
    df <- rv$data$Insectary_data
    pivot_id <- auto_start_insectary_id(df)
    isolate(update_ids_picker(session, "tube_ids", df,
            selected = input$tube_ids, start_from_id = pivot_id, offset_before = 200))

    showNotification("Tubos guardados en local.", type = "message")
  })
}

# -- Server logic for: Registrar Emergidos --
register_emergidos_server <- function(input, output, session, rv) {
  # Patched helper function, defined inside server to access rv
  build_em_table <- function(df, clutch, n, start_id, intro_date_ui) {
    pos <- which(df$Insectary_ID == start_id)
    if (!length(pos)) return(df[0, , drop = FALSE])
    ids <- df$Insectary_ID[seq(pos, length.out = n)]
    new_records <- df %>% filter(.data$Insectary_ID %in% ids)

    # PATCH 2A: Row-wise guarded update for clutch and clutch-driven defaults
    for (i in seq_len(nrow(new_records))) {
      cur_clutch <- new_records$`CLUTCH NUMBER`[i]
      if (is_na_like_val(cur_clutch)) {
        new_records$`CLUTCH NUMBER`[i] <- clutch
        row_i <- new_records[i, , drop = FALSE]
        row_i <- apply_row_core_defaults(row_i, rv$data$Insectary_stocks)
        new_records[i, names(row_i)] <- row_i
      }
    }

    if ("Sex" %in% names(new_records)) {
      is_empty_sex <- is.na(new_records$Sex) | new_records$Sex == ""
      new_records$Sex[is_empty_sex] <- "NA"
    }
    if ("Intro2Insectary_date" %in% names(new_records)) {
      prev <- vapply(new_records$Intro2Insectary_date, fmt_ui_date, character(1))
      is_empty_date <- is.na(prev) | prev == "" | toupper(prev) == "NA"
      new_records$Intro2Insectary_date[is_empty_date] <- intro_date_ui
    }

    if ("Sex" %in% names(new_records)) new_records <- dplyr::relocate(new_records, Sex, .after = Insectary_ID)
    if ("Notes_Insectary_data" %in% names(new_records)) new_records <- dplyr::relocate(new_records, Notes_Insectary_data, .after = Intro2Insectary_date)
    new_records
  }

  # NEW: after-save rebuild helper by explicit IDs (keeps same column arrangement)
  build_em_table_by_ids <- function(df, ids) {
    out <- df %>% dplyr::filter(.data$Insectary_ID %in% ids)
    # match the usual visible columns order from build_em_table
    if ("Sex" %in% names(out)) out <- dplyr::relocate(out, Sex, .after = Insectary_ID)
    if ("Notes_Insectary_data" %in% names(out))
      out <- dplyr::relocate(out, Notes_Insectary_data, .after = Intro2Insectary_date)
    out
  }

  observe({
    req(rv$data$Insectary_stocks)
    df <- rv$data$Insectary_stocks
    clutch_vec <- df$`CLUTCH NUMBER`
    clutch_vec <- clutch_vec[!is.na(clutch_vec) & nzchar(clutch_vec)]
    clutch_vec <- clutch_vec[!duplicated(clutch_vec)]
    updateSelectInput(session, "em_clutch", choices = rev(clutch_vec))
  })

  # Populate start/add ID dropdowns and keep sensible defaults
  observe({
    req(rv$data$Insectary_data)
    df <- rv$data$Insectary_data
    start_default <- auto_start_insectary_id(df)  # your existing predictor

    # Start picker: rotate so it renders near (predicted − 100)
    update_ids_picker(
      session, "em_start_id", df,
      selected = isolate({
        cur <- input$em_start_id %||% ""
        if (nzchar(cur)) cur else start_default
      }),
      start_from_id = start_default, offset_before = 200
    )

    # Add picker: same rotation so it opens near the same zone (you can change to next suggested if you prefer)
    update_ids_picker(
      session, "em_add_id", df,
      selected = isolate(input$em_add_id),
      start_from_id = start_default, offset_before = 200
    )
  })

  rv$em_tbl <- NULL

  observeEvent(input$em_load, {
    req(input$em_clutch, input$em_n > 0, nzchar(input$em_start_id) || !is.na(auto_start_insectary_id(rv$data$Insectary_data)))
    df <- rv$data$Insectary_data
    start_id <- if (nzchar(input$em_start_id)) input$em_start_id else auto_start_insectary_id(df)
    intro_ui <- fmt_ui_date(input$em_intro_date %||% Sys.Date())
    tbl <- build_em_table(df, input$em_clutch, input$em_n, start_id, intro_ui)
    rv$em_tbl <- tbl
    hot_render_predictive(output, session, "em_table",
                          rv$em_tbl, rv,
                          prev_df = NULL,
                          force = TRUE)
    
    # Suggest the next ID *after* the loaded block
    df  <- rv$data$Insectary_data
    sid <- input$em_start_id %||% auto_start_insectary_id(df)
    next_id <- suggest_next_id_after_block(df, sid, input$em_n)
    # Keep original order for choices
    ord_ids <- df$Insectary_ID[!duplicated(df$Insectary_ID)]
    updateSelectizeInput(session, "em_add_id",
      choices = ord_ids,
      selected = next_id, server = TRUE,
      options  = list(sortField = NULL)
    )
  })

  observeEvent(input$em_add, {
    req(input$em_clutch, input$em_n > 0)
    df <- rv$data$Insectary_data
    start_id <- if (nzchar(input$em_add_id)) input$em_add_id else auto_start_insectary_id(df)
    intro_ui <- fmt_ui_date(input$em_intro_date %||% Sys.Date())

    add_records <- build_em_table(df, input$em_clutch, input$em_n, start_id, intro_ui)

    cur <- tryCatch(hot_to_r(input$em_table), error = function(e) NULL) %||% rv$em_tbl %||% df[0, , drop = FALSE]
    combined <- suppressWarnings(dplyr::bind_rows(cur, add_records))
    if ("Insectary_ID" %in% names(combined)) {
      combined <- combined[!duplicated(combined$Insectary_ID), , drop = FALSE]
    }
    rv$em_tbl <- combined
    hot_render_predictive(output, session, "em_table", rv$em_tbl, rv, force = TRUE)

    # Keep same behavior but push the next suggestion into the dropdown
    df  <- rv$data$Insectary_data
    sid <- if (nzchar(input$em_add_id)) input$em_add_id else auto_start_insectary_id(df)
    next_id <- suggest_next_id_after_block(df, sid, input$em_n)
    ord_ids <- df$Insectary_ID[!duplicated(df$Insectary_ID)]
    updateSelectizeInput(session, "em_add_id",
      choices = ord_ids,
      selected = next_id, server = TRUE,
      options  = list(sortField = NULL)
    )
  })

  hot_debounced_observer(
    input, output, session, rv,
    input_id  = "em_table",
    rv_slot   = "em_tbl",
    process_fun = function(prev_tbl, new_tbl) {
      # 1) clutch-driven defaults
      if ("CLUTCH NUMBER" %in% names(new_tbl)) {
        new_tbl <- apply_clutch_defaults_on_change(prev_tbl, new_tbl, rv$data$Insectary_stocks)
      }

      # 2) cascade on WHOLE_ORGANISM for tube columns (same behavior as Tubos)
      today_ui <- fmt_ui_date(Sys.Date())
      for (j in TUBE_SLOTS) {
        tis_col <- paste0("Tube_", j, "_tissue")
        if (!(tis_col %in% names(new_tbl))) next

        prev_t <- if (!is.null(prev_tbl) && tis_col %in% names(prev_tbl))
                    prev_tbl[[tis_col]] else rep(NA_character_, nrow(new_tbl))
        cur_t  <- new_tbl[[tis_col]]

        to_whole <- which(!is_na_like_val(cur_t) &
                          grepl("^\\s*WHOLE_ORGANISM\\s*$", cur_t, ignore.case = TRUE) &
                          (is_na_like_val(prev_t) | !grepl("^\\s*WHOLE_ORGANISM\\s*$", prev_t, ignore.case = TRUE)))
        if (!length(to_whole)) next

        for (i in to_whole) {
          new_tbl <- apply_whole_org_defaults_for_row(
            new_tbl, i = i, j = j,
            tissue_def = TISSUE_WHOLE, medium_def = NULL,
            today_ui = today_ui, autofill_na = isTRUE(input$em_autofill_na)
          )
        }
      }
      new_tbl
    },
    update_inputs = "none",
    debounce_ms   = 200,
    key_columns   = c("CLUTCH NUMBER","CAM_ID", paste0("Tube_", 1:4, "_tissue"))
  )

  render_save_button(output, "em_save_btn", reactive(rv$auth))
  observeEvent(input$em_save_btn, {
    if (!auth_is_logged_in(rv)) { showNotification("Inicia sesión para guardar.", type = "error"); return() }

    force_hot_commit(session, "em_table")

    tbl <- rv$em_tbl %||% hot_to_r(input$em_table)
    req(nrow(tbl) > 0)
    apply_table_changes(rv, tab_label = "Registrar Emergidos", tbl = tbl)

    # --- REFRESH VISIBLE ROWS FROM UPDATED BASE, PRESERVING SELECTION ---
    sel_ids <- unique(na.omit(tbl$Insectary_ID))
    rv$em_tbl <- build_em_table_by_ids(rv$data$Insectary_data, sel_ids)
    hot_render_predictive(output, session, "em_table", rv$em_tbl, rv, force = TRUE)
    # keep pickers stable
    df <- rv$data$Insectary_data
    start_default <- auto_start_insectary_id(df)
    isolate({
      update_ids_picker(session, "em_start_id", df,
                        selected = input$em_start_id, start_from_id = start_default, offset_before = 200)
      update_ids_picker(session, "em_add_id",   df,
                        selected = input$em_add_id, start_from_id = start_default, offset_before = 200)
    })

    showNotification("Emergidos guardados en local.", type = "message")
  })
}

register_subir_server <- function(input, output, session, rv) {
  # --- helpers for live UI updates during long ops ---
  flush_now <- function() {
    # Push pending UI changes immediately
    try(shiny::flushReact(), silent = TRUE)
  }
  # keep: we won't need a remover for the unified, in-place toast

  # Cambios activos (rv$changes) ya existen.
  # Guardaremos los revertidos aquí para poder hacer Redo:
  rv$chg_undone <- empty_change_log()  # evita leer rv$changes fuera de reactivo

  change_key <- function(df) paste(df$ts, df$Insectary_ID, df$Column, sep = " | ")

  # Shared filter helper for Active/Undone changes
  filter_changes <- function(act, und, sel_users, sel_commits) {
    act <- normalize_batch_ids(act); und <- normalize_batch_ids(und)
    if (length(sel_users)) {
      act <- act %>% dplyr::filter(.data$user_label %in% sel_users)
      und <- und %>% dplyr::filter(.data$user_label %in% sel_users)
    }
    if (length(sel_commits)) {
      act <- act %>% dplyr::filter(.data$batch_id %in% sel_commits)
      und <- und %>% dplyr::filter(.data$batch_id %in% sel_commits)
    } else {
      # No commit selection -> hide *_uploaded by default
      act <- act %>% dplyr::filter(!grepl("_uploaded$", .data$batch_id %||% ""))
      und <- und %>% dplyr::filter(!grepl("_uploaded$", .data$batch_id %||% ""))
    }
    commits <- sort(unique(na.omit(act$batch_id)))
    list(act = act, und = und, commits = commits)
  }

  refresh_user_filter <- function() {
    ch <- normalize_batch_ids(rv$changes)
    users <- sort(unique(na.omit(ch$user_label)))
    updateSelectizeInput(session, "chg_user_filter",
                         choices = users, selected = isolate(input$chg_user_filter), server = TRUE)
    # Commit options depend on (possibly multiple) user selections
    if (!length(users)) {
      updateSelectizeInput(session, "chg_commit_filter", choices = character(0), selected = character(0), server = TRUE)
      return(invisible(NULL))
    }
    sel_users   <- isolate(input$chg_user_filter)
    sel_commits <- character(0)  # while populating, treat as 'none selected'
    res <- filter_changes(rv$changes, rv$chg_undone, sel_users, sel_commits)
    updateSelectizeInput(session, "chg_commit_filter",
                         choices = res$commits,
                         selected = isolate(input$chg_commit_filter), server = TRUE)
  }

  observe(refresh_user_filter())

  # Auth-aware buttons (disable for not logged in and for invitados)
  render_auth_button(output, "chg_commit",    reactive(rv$auth), "Subir a Google Sheets",      enabled_class = "btn btn-success", allow_guest = FALSE)
  render_auth_button(output, "chg_update_db", reactive(rv$auth), "Actualizar Base",            enabled_class = "btn btn-default", allow_guest = FALSE)
  render_auth_button(output, "chg_clear",     reactive(rv$auth), "Limpiar cambios filtrados",  enabled_class = "btn btn-warning", allow_guest = FALSE)

  add_toggle_cols <- function(x, status) {
    if (!nrow(x)) return(x)
    x$key    <- change_key(x)
    x$Status <- status
    x$Toggle <- vapply(x$key, function(k) toggle_btn_html(k, status), character(1))
    x
  }

  build_changes_view <- function() {
    if (nrow(rv$changes) == 0 && nrow(rv$chg_undone) == 0) return(NULL)
    res <- filter_changes(rv$changes, rv$chg_undone, input$chg_user_filter, input$chg_commit_filter)
    if (nrow(res$act) == 0 && nrow(res$und) == 0) return(NULL)
    act <- add_toggle_cols(res$act, "Active")
    und <- add_toggle_cols(res$und, "Undone")
    dplyr::bind_rows(act, und) %>% dplyr::arrange(dplyr::desc(Status), dplyr::desc(ts))
  }

  observeEvent(input$chg_show, {
    ch2 <- build_changes_view()
    if (is.null(ch2) || !nrow(ch2)) { render_simple_table_message(output, "chg_table", "No hay cambios."); return() }

    # Tabla
    output$chg_table <- renderDT({
      view <- ch2[, c("ts","batch_id","user_label","tab","Insectary_ID","Column","Previous","New","Status","Toggle")]
      datatable(
        view,
        colnames = c("ts","commit","Usuario","Tab","Insectary_ID","Columna","Previo","Nuevo","Estado",""),
        escape = FALSE, rownames = FALSE,
        options = list(pageLength = 12, autoWidth = TRUE)
      )
    }, server = FALSE)

    # Pintar filas Undone en amarillo tras dibujar
    proxy <- dataTableProxy("chg_table")
    if (nrow(ch2)) {
      undone_idx <- which(ch2$Status == "Undone")
      js <- sprintf(
        "setTimeout(function(){
           var rows=[%s];
           $('#chg_table table tbody tr').each(function(i){
             if(rows.indexOf(i+1)>=0){ $(this).css('background','#fff3cd'); }
             else { $(this).css('background',''); }
           });
         },10);",
        paste(undone_idx, collapse = ",")
      )
      session$sendCustomMessage("jsCode", list(code = js))
    }

    lapply(seq_len(nrow(ch2)), function(i) {
      btn_id <- paste0("chg_toggle_", digest::digest(ch2$key[i]))
      observeEvent(input[[btn_id]], {
        row <- ch2[i, , drop = FALSE]
        id  <- row$Insectary_ID
        col <- row$Column

        # Localiza índice en la base
        idx <- which(rv$data$Insectary_data$Insectary_ID == id)
        if (!length(idx) || !(col %in% names(rv$data$Insectary_data))) return()

        # Formateo especial para columnas de fecha
        fmt_if_date <- function(v) {
          if (col %in% c("Death_date","Intro2Insectary_date","Preservation_date")) fmt_ui_date(v) else v
        }

        if (row$Status[1] == "Active") {
          # --- UNDO: revertir base y mover de 'changes' a 'undone' ---
          prev_val <- fmt_if_date(row$Previous[1])
          rv$data$Insectary_data[[col]][idx] <- prev_val

          # quitar de activos
          rv$changes <- dplyr::anti_join(rv$changes, row[, intersect(names(row), names(rv$changes))], 
                                         by = c("ts","user_key","user_label","tab","Insectary_ID","Column","Previous","New","batch_id"))
          # añadir a revertidos (si no está)
          same_in_undone <- nrow(dplyr::semi_join(rv$chg_undone, row, 
                                   by=c("ts","user_key","user_label","tab","Insectary_ID","Column","Previous","New","batch_id")))
          if (same_in_undone == 0) rv$chg_undone <- dplyr::bind_rows(rv$chg_undone, row[, names(rv$changes), drop=FALSE])

        } else {
          # --- REDO: re-aplicar base y mover de 'undone' a 'changes' ---
          new_val <- fmt_if_date(row$New[1])
          rv$data$Insectary_data[[col]][idx] <- new_val

          # quitar de revertidos
          rv$chg_undone <- dplyr::anti_join(rv$chg_undone, row[, intersect(names(row), names(rv$chg_undone))],
                                            by = c("ts","user_key","user_label","tab","Insectary_ID","Column","Previous","New","batch_id"))
          # devolver a activos (si no está)
          same_in_active <- nrow(dplyr::semi_join(rv$changes, row, 
                                   by=c("ts","user_key","user_label","tab","Insectary_ID","Column","Previous","New","batch_id")))
          if (same_in_active == 0) rv$changes <- dplyr::bind_rows(rv$changes, row[, names(rv$changes), drop=FALSE])
        }

        # Persistir y refrescar
        saveRDS(rv$data$Insectary_data, rds_paths[["Insectary_data"]])
        save_local_state(list(
          version = rv$version, data = rv$data, changes = rv$changes, history = rv$history, auth = rv$auth
        ))
        shinyjs::click("chg_show")
      }, ignoreInit = TRUE)
    })
  })

  # Commit a Sheets: enviar solo activos
  observeEvent(input$chg_commit, {
    if (auth_is_guest(rv)) { showNotification("Invitado no puede subir cambios.", type = "error"); return() }
    ch2 <- build_changes_view()
    if (is.null(ch2) || !nrow(ch2)) { showNotification("No hay cambios para subir.", type = "message"); return() }

    ch_send <- ch2 %>% dplyr::filter(.data$Status == "Active")
    if (!nrow(ch_send)) { showNotification("Todas las filas están revertidas (Undo).", type = "warning"); return() }

    send_core <- ch_send[, intersect(names(ch_send), CHANGE_LOG_COLS), drop = FALSE]

    ensure_data_dir()

    # Colapsa a último cambio por (ID, Columna)
    ch_write <- send_core %>% group_by(Insectary_ID, Column) %>%
      arrange(desc(ts)) %>% slice(1) %>% ungroup()

    # Unified, in-place toast that updates as we progress
    notif_id <- "uploading_changes"
    showNotification("Preparando escritura…", type = "message", duration = NULL, id = notif_id)

    # Reporter callback invoked by commit_changes_to_sheet() — update same toast
    commit_status <- reactiveVal("Preparando escritura…")
    reporter <- function(i, n, label) {
      commit_status(sprintf("%d/%d  •  %s", i, n, label))
      flush_now()
      showNotification(commit_status(), type = "message", duration = NULL, id = notif_id)
    }

    fail_rep <- commit_changes_to_sheet(ch_write, sheet_name = "Insectary_data", report_cb = reporter)

    if (nrow(fail_rep)) {
      key <- paste(fail_rep$Insectary_ID, fail_rep$Column)
      ch_write$Result <- ifelse(paste(ch_write$Insectary_ID, ch_write$Column) %in% key,
                                "Skipped (protected)", "Committed")
    } else ch_write$Result <- "Committed"

    # --- Save ONE snapshot per selected batch and append "_uploaded" ---
    batches_committed <- sort(unique(na.omit(ch_send$batch_id)))
    for (b in batches_committed) {
      part <- ch_write %>% dplyr::filter(.data$batch_id == b)
      if (!nrow(part)) next
      # Keep a human commit label inside the file (unsanitized, for UI)
      part$commit_id <- paste0(b, "_uploaded")
      # Use centralized helper to persist snapshot (Windows-safe path)
      write_commit_snapshot(part, part$commit_id[1])
    }

    # --- Mark the committed rows inside rv$changes as uploaded (rename batch_id) ---
    if (length(batches_committed)) {
      mark <- rv$changes$batch_id %in% batches_committed
      rv$changes$batch_id[mark] <- ifelse(
        grepl("_uploaded$", rv$changes$batch_id[mark]),
        rv$changes$batch_id[mark],
        paste0(rv$changes$batch_id[mark], "_uploaded")
      )
      # Persist local state so filters immediately reflect new names
      save_local_state(list(
        version = rv$version, data = rv$data, changes = rv$changes, history = rv$history
      ))
    }
    # Finalize the single toast: replace text and set a short duration
    if (any(ch_write$Result == "Skipped (protected)")) {
      nskip <- sum(ch_write$Result == "Skipped (protected)")
      showNotification(sprintf("Cambios subidos. Omitidos %d por celdas protegidas.", nskip),
                       type = "warning", duration = 5, id = notif_id)
    } else {
      showNotification("Cambios subidos a Google Sheets.",
                       type = "message", duration = 5, id = notif_id)
    }

    # Refresh filters/UI to hide uploaded commits from the Commit(s) list
    shinyjs::click("chg_show")
    # And also rebuild the Commit(s) dropdown using shared helper
    isolate(refresh_user_filter())
  })

  observeEvent(input$chg_update_db, {
    showModal(modalDialog("Actualizando base desde Google Sheets...", footer=NULL))
    rv$data <- download_and_save_data(show_toasts = TRUE)
    removeModal()
    showNotification("Base actualizada desde Google Sheets.", type = "message")
  })

  # Limpiar sólo cambios filtrados: elimina tanto activos como revertidos visibles
  observeEvent(input$chg_clear, {
    if (auth_is_guest(rv)) { showNotification("Invitado no puede limpiar cambios.", type = "error"); return() }
    ch <- rv$changes
    if (nrow(ch) == 0 && nrow(rv$chg_undone) == 0) { showNotification("No hay cambios.", type = "message"); return() }
    sel_users   <- input$chg_user_filter
    sel_commits <- input$chg_commit_filter
    ch <- normalize_batch_ids(ch)
    keep <- rep(TRUE, nrow(ch))
    if (length(sel_users))   keep <- keep & !(ch$user_label %in% sel_users)
    if (length(sel_commits)) keep <- keep | !(ch$batch_id %in% sel_commits)

    rv$changes <- ch[keep, , drop = FALSE]

    # Limpiar revertidos filtrados
    und <- rv$chg_undone
    if (nrow(und)) {
      und <- normalize_batch_ids(und)
      keep_und <- rep(TRUE, nrow(und))
      if (length(sel_users))   keep_und <- keep_und & !(und$user_label %in% sel_users)
      if (length(sel_commits)) keep_und <- keep_und | !(und$batch_id %in% sel_commits)
      rv$chg_undone <- und[keep_und, , drop = FALSE]
    }

    save_local_state(list(
      version = rv$version, data = rv$data, changes = rv$changes, history = rv$history
    ))
    shinyjs::click("chg_show")
    showNotification("Cambios filtrados limpiados.", type = "message")
  })
}

register_hist_server <- function(input, output, session, rv) {

  # ---- helpers ----
  # Commit snapshots are read via read_commit_snapshots() (centralized).

  # returns filtered df AND updates commit/ID pickers
  apply_hist_filters <- function(update_inputs = TRUE) {
    all_hist <- read_commit_snapshots()
    if (is.null(all_hist)) return(NULL)

    # Fill 'Result' for old snapshots if missing
    if (!"Result" %in% names(all_hist)) {
      # infer using current DB (soft)
      get_current_val <- function(id, col)
        as.character(rv$data$Insectary_data[[col]][rv$data$Insectary_data$Insectary_ID == id][1])
      all_hist$Current <- mapply(get_current_val, all_hist$Insectary_ID, all_hist$Column)
      all_hist$Result  <- ifelse(all_hist$New == all_hist$Current, "Possibly not applied", "Committed")
    }

    # ----- date filter (robust if ts came in as character) -----
    rng <- as.Date(input$hist_range)
    if (!isTRUE(input$hist_use_range)) {
      # single day (end only); keep start==end and keep start disabled in UI
      rng[1] <- rng[2]
    }
    ts_dates <- suppressWarnings(local_day(all_hist$ts))
    all_hist <- dplyr::filter(all_hist, !is.na(ts_dates), ts_dates >= rng[1], ts_dates <= rng[2])

    # ----- users filter -----
    sel_users <- input$hist_users
    if (length(sel_users) && !"ALL" %in% sel_users) {
      all_hist <- dplyr::filter(all_hist, user_label %in% sel_users)
    }

    # choices for commits after date+user filter
    commit_choices <- sort(unique(all_hist$commit_id))
    if (update_inputs) {
      updateSelectInput(session, "hist_commits",
        choices = c("ALL", commit_choices),
        selected = isolate(if (input$hist_commits %in% commit_choices) input$hist_commits else "ALL"))
    }

    # ----- commit filter -----
    if (!is.null(input$hist_commits) && input$hist_commits != "ALL") {
      all_hist <- dplyr::filter(all_hist, commit_id == input$hist_commits)
    }

    # choices for IDs after all above filters
    id_choices <- sort(unique(all_hist$Insectary_ID))
    if (update_inputs) {
      selected_ids <- isolate(intersect(input$hist_ids, id_choices))
      updateSelectizeInput(session, "hist_ids", choices = id_choices, selected = selected_ids, server = TRUE)
    }

    # ----- ID filter (optional; empty = all) -----
    if (!is.null(input$hist_ids) && length(input$hist_ids)) {
      all_hist <- dplyr::filter(all_hist, Insectary_ID %in% input$hist_ids)
    }

    all_hist
  }

  # ---- initialize pickers ----
  observe({
    # build initial Users (include ALL) from centralized loader
    snap <- read_commit_snapshots()
    users <- if (!is.null(snap) && nrow(snap)) unique(snap$user_label) else character(0)
    if (nrow(rv$changes)) users <- c(users, unique(rv$changes$user_label))
    users <- sort(unique(na.omit(users)))
    updateSelectizeInput(session, "hist_users",
      choices = c("ALL", users),
      selected = "ALL", server = TRUE)
  })

  # Auth-aware buttons (disable for not logged in and for invitados)
  render_auth_button(output, "hist_retry", reactive(rv$auth), "Reintentar subir seleccionados",    enabled_class = "btn btn-success", allow_guest = FALSE)
  render_auth_button(output, "hist_undo",  reactive(rv$auth), "Deshacer cambios en Google Sheets", enabled_class = "btn btn-warning", allow_guest = FALSE)

  # ---- enable/disable start date based on 'usar rango' and keep start=end ----
  observe({
    start_id <- "hist_range_start"
    if (!isTRUE(input$hist_use_range)) {
      # lock start to end
      if (!is.null(input$hist_range)) {
        updateDateRangeInput(session, "hist_range",
          start = input$hist_range[2], end = input$hist_range[2])
      }
      shinyjs::disable(start_id)
    } else {
      shinyjs::enable(start_id)
    }
  })

  # Refresh Historial pickers when entering the tab
  observeEvent(input$nav_main, {
    if (identical(input$nav_main, "Historial de Cambios")) {
      # Rebuild Users from disk + live changes using centralized loader
      snap <- read_commit_snapshots()
      users <- if (!is.null(snap) && nrow(snap)) unique(snap$user_label) else character(0)
      if (nrow(rv$changes)) users <- c(users, unique(rv$changes$user_label))
      users <- sort(unique(na.omit(users)))
      updateSelectizeInput(session, "hist_users",
        choices = c("ALL", users), selected = "ALL", server = TRUE
      )
      # Also refresh Commits/IDs now
      apply_hist_filters(update_inputs = TRUE)
    }
  }, ignoreInit = TRUE)

  # When date/user/commit change, refresh dependent pickers
  observeEvent(list(input$hist_use_range, input$hist_range, input$hist_users), {
    apply_hist_filters(update_inputs = TRUE)
  }, ignoreInit = FALSE)

  # ---- build table on 'Buscar' ----
  observeEvent(input$hist_load, {
    hist_df <- apply_hist_filters(update_inputs = TRUE)
    if (is.null(hist_df) || !nrow(hist_df)) { render_simple_table_message(output, "hist_table", "No hay historial que coincida."); return() }

    # always show current value for context
    get_current_val <- function(id, col)
      as.character(rv$data$Insectary_data[[col]][rv$data$Insectary_data$Insectary_ID == id][1])
    hist_df$Current <- mapply(get_current_val, hist_df$Insectary_ID, hist_df$Column)

    view <- hist_df %>%
      transmute(
        Commit       = commit_id,
        Timestamp    = format(ts, "%Y-%m-%d %H:%M"),
        Usuario      = user_label,
        Tab          = tab,
        Insectary_ID, Column,
        Before = Previous, After = New, Current,
        Result
      )

    output$hist_table <- renderDT(
      datatable(
        view, escape = FALSE, rownames = FALSE,
        options = list(pageLength = 12), selection = 'multiple'
      ) %>% DT::formatStyle(
        'Result',
        target = 'row',
        backgroundColor = DT::styleEqual('Skipped (protected)', '#fff3cd')  # soft yellow
      )
    )
  })

  # ---- retry functionality (force overwrite with 'New') ----
  observeEvent(input$hist_retry, {
    # Get the filtered data from the current search
    hist_df <- apply_hist_filters(update_inputs = FALSE)
    sel <- input$hist_table_rows_selected
    if (!length(sel) || is.null(hist_df)) {
      showNotification("Selecciona una o más filas en Historial.", type = "warning"); return()
    }
    
    # Map selection to the actual data (always overwrite with 'New')
    df_sel <- hist_df[sel, , drop = FALSE]
    to_commit <- df_sel %>% dplyr::select(Insectary_ID, Column, New)
    showModal(modalDialog("Subiendo (forzando) valores 'Nuevo' de filas seleccionadas…", footer=NULL))
    on.exit(removeModal())

    failures <- commit_changes_to_sheet(to_commit, sheet_name = "Insectary_data")
    ts <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    df_sel$Result <- if (nrow(failures)) {
      key <- paste(failures$Insectary_ID, failures$Column)
      ifelse(paste(df_sel$Insectary_ID, df_sel$Column) %in% key, "Skipped (protected)", "Committed")
    } else "Committed"
    
    df_sel$commit_id <- paste0("retry_", ts)
    ensure_data_dir()
    saveRDS(df_sel, file = file.path(DATA_DIR, paste0("commit_retry_", ts, ".rds")))
    showNotification("Sobrescritura completada (usando 'Nuevo').", type = "message")
    shinyjs::click("hist_load")  # refresh table
  })

  # ---- undo-to-previous functionality (overwrite with Previous) ----
  observeEvent(input$hist_undo, {
    if (!auth_is_logged_in(rv) || auth_is_guest(rv)) {
      showNotification("Inicia sesión con usuario no invitado para deshacer.", type="error"); return()
    }
    hist_df <- apply_hist_filters(update_inputs = FALSE)
    sel <- input$hist_table_rows_selected
    if (is.null(hist_df) || !length(sel)) {
      showNotification("Selecciona una o más filas en Historial.", type="warning"); return()
    }
    df_sel <- hist_df[sel, , drop = FALSE]
    to_commit <- df_sel %>% dplyr::select(Insectary_ID, Column, New = Previous)
    showNotification("Deshaciendo cambios seleccionados en Google Sheets…", type="message", duration=NULL, id="hist_undo_toast")
    fails <- commit_changes_to_sheet(to_commit, sheet_name = "Insectary_data")
    if (nrow(fails)) {
      showNotification(sprintf("Deshacer completado con omisiones (%d).", nrow(fails)), type="warning", duration=6, id="hist_undo_toast")
    } else {
      showNotification("Deshacer completado.", type="message", duration=5, id="hist_undo_toast")
    }
    shinyjs::click("hist_load")
  })
}

## -- Server logic for: Buscador --
register_search_server <- function(input, output, session, rv) {
  rv$srch_tbl <- NULL

  # Save button (auth-aware)
  output$srch_save_btn_ui <- renderUI({
    a <- rv$auth %||% list(is_logged_in=FALSE)
    if (!isTRUE(a$is_logged_in)) {
      tags$button(id="srch_save_btn_placeholder", type="button",
                  class="btn btn-danger", disabled=NA, "Inicia sesion para guardar cambios")
    } else {
      actionButton("srch_save_btn", "Guardar Cambios", class = "btn btn-success")
    }
  })

  # Run search
  observeEvent(input$srch_go, {
    q <- trimws(input$srch_query %||% "")
    if (!nzchar(q)) { render_simple_table_message(output, "srch_table", "Escribe un texto para buscar."); return() }
    df <- rv$data$Insectary_data %||% data.frame()
    if (!nrow(df)) { render_simple_table_message(output, "srch_table", "No hay datos."); return() }
    # robust string match across all columns
    mat <- apply(df, 1, function(row) any(grepl(q, as.character(row), ignore.case = TRUE, perl = TRUE)))
    hit <- df[which(mat), , drop = FALSE]
    if (!nrow(hit)) { render_simple_table_message(output, "srch_table", "Sin coincidencias."); return() }
    rv$srch_tbl <- hit
    render_table(output, "srch_table", rv$srch_tbl, rv)
  }, ignoreInit = TRUE)

  # Cargar/Revertir = reload current results from base
  observeEvent(input$srch_load, {
    q <- trimws(input$srch_query %||% "")
    if (!nzchar(q)) return()
    df <- rv$data$Insectary_data %||% data.frame()
    if (!nrow(df)) return()
    mat <- apply(df, 1, function(row) any(grepl(q, as.character(row), ignore.case = TRUE, perl = TRUE)))
    hit <- df[which(mat), , drop = FALSE]
    rv$srch_tbl <- hit
    render_table(output, "srch_table", rv$srch_tbl, rv)
  }, ignoreInit = TRUE)

  # Debounced edit handling for the search table
  hot_debounced_observer(
    input, output, session, rv,
    input_id  = "srch_table",
    rv_slot   = "srch_tbl",
    process_fun = function(prev_tbl, new_tbl) {
      # Apply clutch-driven defaults on change (consistent with other tabs)
      apply_clutch_defaults_on_change(prev_tbl, new_tbl, rv$data$Insectary_stocks)
    },
    update_inputs = "none",
    debounce_ms   = 200,
    key_columns   = c("CLUTCH NUMBER","CAM_ID", paste0("Tube_", 1:4, "_tissue"))
  )

  # Save edits
  observeEvent(input$srch_save_btn, {
    if (!auth_is_logged_in(rv)) { showNotification("Inicia sesión para guardar.", type = "error"); return() }
    force_hot_commit(session, "srch_table")
    tbl <- rv$srch_tbl %||% hot_to_r(input$srch_table)
    req(nrow(tbl) > 0)
    apply_table_changes(rv, tab_label = "Buscador", tbl = tbl)
    
    # --- REFRESH SEARCH RESULTS FROM UPDATED BASE ---
    # Re-run the search with current query to show updated values
    q <- trimws(input$srch_query %||% "")
    if (nzchar(q)) {
      df <- rv$data$Insectary_data %||% data.frame()
      if (nrow(df)) {
        mat <- apply(df, 1, function(row) any(grepl(q, as.character(row), ignore.case = TRUE, perl = TRUE)))
        hit <- df[which(mat), , drop = FALSE]
        if (nrow(hit)) {
          rv$srch_tbl <- hit
          render_table(output, "srch_table", rv$srch_tbl, rv)
        }
      }
    }
    
    showNotification("Cambios guardados (Buscador).", type="message")
  }, ignoreInit = TRUE)
}


## Main Server Function --------------------------------------------------------
server <- function(input, output, session) {
  # Initialize reactive values holder
  rv <- reactiveValues()
  

  observe({
    st <- initialize_state()
    rv$version <- st$version
    rv$data    <- st$data
    rv$changes <- st$changes
    rv$history <- st$history
    rv$auth    <- st$auth
  })
  
  # Quick-pick -> put initials into Usuario
  observeEvent(input$user_quickpick, ignoreInit = TRUE, {
    # Do not overwrite 'Usuario' when quick-pick is Invitado (INV)
    key <- input$user_quickpick %||% ""
    if (nzchar(key) && toupper(key) != "INV" &&
        !identical(isolate(toupper(input$user_login %||% "")), toupper(key))) {
      updateTextInput(session, "user_login", value = key)
    }
  })

  # Usuario typed/autofilled -> highlight matching label in quick-pick (or clear)
  observeEvent(input$user_login, ignoreInit = TRUE, {
    key <- toupper(trimws(input$user_login %||% ""))
    sel <- if (nzchar(key) && key %in% KNOWN_KEYS) key else "INV"
    updateSelectizeInput(session, "user_quickpick",
                         choices  = setNames(users_df$key, users_df$label),
                         selected = sel, server = TRUE)
  })
  
  # Use shinyjs
  shinyjs::useShinyjs()

  # Mostrar/ocultar contraseña
  observeEvent(input$show_pwd, {
    js <- sprintf("
      (function(){
        var el = document.getElementById(%s);
        if (!el) return;
        el.type = %s ? 'text' : 'password';
      })();",
      jsonlite::toJSON("user_pwd"),
      if (isTRUE(input$show_pwd)) "true" else "false"
    )
    session$sendCustomMessage("jsCode", list(code = js))
  }, ignoreInit = FALSE)

  # ---- Login wiring (Step 3) ----
  output$login_status <- renderUI({
    if (!auth_is_logged_in(rv)) {
      HTML("<p class='muted'>Estado: No has iniciado sesión.</p>")
    } else {
      HTML(sprintf("<p class='ok'>Estado: Sesión iniciada como <b>%s</b>.</p>", htmltools::htmlEscape(auth_user_label(rv))))
    }
  })

  observeEvent(input$login_btn, {
    typed <- trimws(input$user_login %||% "")
    key   <- toupper(typed)
    pwd   <- input$user_pwd %||% ""

    row <- users_df[match(key, users_df$key), ]

    # If unknown user -> treat as Invitado (key 'INV'), but keep the typed name
    if (is.na(row$key[1])) {
      key <- "INV"
      row <- users_df[match(key, users_df$key), ]
      if (is.na(row$key[1])) { showNotification("Config de usuarios inválida.", type="error"); return() }
    }

    if (!identical(trimws(pwd), row$password[1])) {
      showNotification("Contraseña incorrecta.", type = "error"); return()
    }

    is_guest <- identical(key, "INV")
    guest    <- if (is_guest) typed else NA_character_

    rv$auth <- list(
      is_logged_in = TRUE, is_guest = is_guest,
      # For guests, key/label are personalized "INV - <Usuario>"
      user_key   = if (is_guest) paste0("INV - ", guest) else key,
      user_label = if (is_guest) paste0("INV - ", guest) else row$label[1],
      guest_name = guest
    )
    showNotification(sprintf("Sesión iniciada: %s", rv$auth$user_label), type = "message")

    # SIMPLE TRIGGER: hide/clear the password field and nudge the browser to save
    session$sendCustomMessage('saveCreds', list(user = input$user_login, pwd = pwd))
    shinyjs::hide("user_pwd")
    updateTextInput(session, "user_pwd", value = "")
  })

  observeEvent(input$logout_btn, {
    rv$auth <- list(is_logged_in = FALSE, is_guest = FALSE, user_key = NA_character_, user_label = NA_character_, guest_name = NA_character_)
    showNotification("Sesión cerrada.", type = "message")
  })
  
  # Persist state whenever it changes
  observe({
    req(rv$auth, rv$data, rv$changes, rv$history)
    save_local_state(list(
      version = rv$version, data = rv$data, changes = rv$changes, history = rv$history
    ))
  })

  # ---- Registrar Muertes (Step 4) ----
  observe({
    req(rv$data$Insectary_data)
    df <- rv$data$Insectary_data
    pivot_id <- auto_start_insectary_id(df)
    update_ids_picker(session, "dead_ids", df, start_from_id = pivot_id, offset_before = 200)
    # Use unique values from Insectary_data$Death_cause, ensure "Unknown" is present and selected
    dc <- rv$data$Insectary_data$Death_cause
    cause_choices <- unique(c("Unknown", sort(unique(na.omit(as.character(dc))))))
    updateSelectizeInput(session, "dead_cause_default",
                         choices = cause_choices, selected = "Unknown", server = TRUE)
  })

  output$dead_controls_row <- renderUI({
    req(rv$data$Insectary_data)
    # Mirror the same source & default as in the observer above
    dc <- rv$data$Insectary_data$Death_cause
    cause_choices <- unique(c("Unknown", sort(unique(na.omit(as.character(dc))))))

    div(class = if (isTRUE(input$dead_review_only)) "dimmed" else "",
        div(class = "inline-controls",
            selectizeInput("dead_cause_default", "Causa por defecto",
                           choices = cause_choices, selected = "Unknown",
                           options = list(openOnFocus = TRUE)),
            dateInput("dead_date", "Fecha de muerte",
                      value = Sys.Date(), format = "d-M-yy", language = "en"),
            checkboxInput(
              "dead_autofill_na",
              "Autocompletar con NAs columnas con tubos vacíos si Tejido es WHOLE_ORGANISM",
              value = TRUE
            )
        )
    )
  })

  rv$dead_tbl <- NULL

  observeEvent(input$dead_load, {
    req(input$dead_ids)
    df <- build_dead_table(rv$data$Insectary_data, input$dead_ids, isTRUE(input$dead_review_only),
                           input$dead_cause_default, input$dead_date)
    rv$dead_tbl <- df
    hot_render_predictive(output, session, "dead_table", rv$dead_tbl, rv, force = TRUE)
  })

  observeEvent(input$dead_add, {
    req(input$dead_ids)
    current <- hot_to_r(input$dead_table) %||% rv$dead_tbl %||% rv$data$Insectary_data[0, ]
    add_df <- build_dead_table(rv$data$Insectary_data, input$dead_ids, isTRUE(input$dead_review_only),
                             input$dead_cause_default, input$dead_date)
    combined <- distinct(bind_rows(current, add_df), Insectary_ID, .keep_all = TRUE)
    rv$dead_tbl <- combined
    hot_render_predictive(output, session, "dead_table", rv$dead_tbl, rv, force = TRUE)
  })

  hot_debounced_observer(
    input, output, session, rv,
    input_id  = "dead_table",
    rv_slot   = "dead_tbl",
    process_fun = function(prev_tbl, new_tbl) {
      # 1) clutch-driven defaults
      new_tbl <- apply_clutch_defaults_on_change(prev_tbl, new_tbl, rv$data$Insectary_stocks)

      # 2) cascade on WHOLE_ORGANISM like in Tubos tab
      #    (also prime New_Death_Date if blank)
      today_ui <- fmt_ui_date(input$dead_date %||% Sys.Date())
      for (j in TUBE_SLOTS) {
        tis_col <- paste0("Tube_", j, "_tissue")
        if (!(tis_col %in% names(new_tbl))) next

        prev_t <- if (!is.null(prev_tbl) && tis_col %in% names(prev_tbl))
                    prev_tbl[[tis_col]] else rep(NA_character_, nrow(new_tbl))
        cur_t  <- new_tbl[[tis_col]]

        to_whole <- which(!is_na_like_val(cur_t) &
                          grepl("^\\s*WHOLE_ORGANISM\\s*$", cur_t, ignore.case = TRUE) &
                          (is_na_like_val(prev_t) | !grepl("^\\s*WHOLE_ORGANISM\\s*$", prev_t, ignore.case = TRUE)))
        if (!length(to_whole)) next

        for (i in to_whole) {
          new_tbl <- apply_whole_org_defaults_for_row(
            new_tbl, i = i, j = j,
            tissue_def = TISSUE_WHOLE, medium_def = NULL,
            today_ui = today_ui, autofill_na = isTRUE(input$dead_autofill_na)
          )
          # The Muertes table has New_Death_Date instead of Death_date in view.
          if ("New_Death_Date" %in% names(new_tbl)) {
            nd <- new_tbl$New_Death_Date[i]
            if (is_na_like_val(nd)) new_tbl$New_Death_Date[i] <- today_ui
          }
        }
      }
      new_tbl
    },
    update_inputs = "none",
    debounce_ms   = 200,
    key_columns   = c("CLUTCH NUMBER","CAM_ID", paste0("Tube_", 1:4, "_tissue"))
  )

  observeEvent(input$dead_show_photos, {
    if (!isTRUE(input$dead_show_photos)) {
      output$dead_photos <- renderUI(div())
      return()
    }
    tbl <- hot_to_r(input$dead_table) %||% rv$dead_tbl
    req(tbl, "CAM_ID" %in% names(tbl))
    output$dead_photos <- renderUI(photo_grid_for_cam_ids(rv$data$Photo_links, unique(na.omit(tbl$CAM_ID))))
  }, ignoreInit = TRUE)

  render_save_button(output, "dead_save_btn", reactive(rv$auth))
  observeEvent(input$dead_save_btn, {
    if (!auth_is_logged_in(rv)) { showNotification("Inicia sesión para guardar.", type = "error"); return() }

    session$sendCustomMessage("forceHotCommit", "dead_table")  # commit active edit

    tbl <- rv$dead_tbl %||% hot_to_r(input$dead_table)
    req(tbl, nrow(tbl) > 0)
    base <- rv$data$Insectary_data
    changed_count <- 0
    batch_id <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    
    for (i in 1:nrow(tbl)) {
      id <- tbl$Insectary_ID[i]
      idx <- which(base$Insectary_ID == id)
      if (length(idx) == 0) next
      
      # Always consider Death_cause and New_Death_Date changes (map to Death_date),
      # but never write the Previous_* helper columns.
      cols_to_check <- setdiff(names(tbl), c("Previous_Death_Cause", "Previous_Death_Date"))
      
      for(col in cols_to_check) {
        target_col <- if(col == "New_Death_Date") "Death_date" else col
        if(!target_col %in% names(base) || target_col %in% c("Previous_Death_Date", "Previous_Death_Cause")) next
        
        prev <- base[[target_col]][idx]
        newv <- tbl[[col]][i]
        
        if (target_col %in% c("Death_date", "Intro2Insectary_date", "Preservation_date")) {
          prev <- fmt_ui_date(prev); newv <- fmt_ui_date(newv)
        }
        
        if(!identical(as.character(prev), as.character(newv))) {
          base[[target_col]][idx] <- newv
          add_change_entry(rv, "Registrar Muertes", id, target_col, prev, newv, batch_id = batch_id)
          changed_count <- changed_count + 1
        }
      }
      
      prev_clutch <- base$`CLUTCH NUMBER`[idx]
      new_clutch  <- if ("CLUTCH NUMBER" %in% names(tbl)) tbl$`CLUTCH NUMBER`[i] else prev_clutch
      clutch_changed <- !identical(as.character(prev_clutch), as.character(new_clutch))

      if (isTRUE(clutch_changed)) {
        row_now  <- base[idx, , drop = FALSE]
        enforced <- apply_stock_origin_rule(row_now, rv$data$Insectary_stocks)

        if (!identical(as.character(row_now$Stock_of_origin[1]), as.character(enforced$Stock_of_origin[1]))) {
          add_change_entry(rv, "Registrar Muertes", id, "Stock_of_origin", row_now$Stock_of_origin[1], enforced$Stock_of_origin[1], batch_id = batch_id)
          base$Stock_of_origin[idx] <- enforced$Stock_of_origin[1]
          changed_count <- changed_count + 1
        }
        if ("Wild_Reared" %in% names(base) && is_na_like_val(base$Wild_Reared[idx])) {
          prev_wr <- base$Wild_Reared[idx]
          base$Wild_Reared[idx] <- "Reared"
          add_change_entry(rv, "Registrar Muertes", id, "Wild_Reared", prev_wr, "Reared", batch_id = batch_id)
          changed_count <- changed_count + 1
        }
        if ("Collection_location" %in% names(base) && is_na_like_val(base$Collection_location[idx])) {
          prev_cl <- base$Collection_location[idx]
          base$Collection_location[idx] <- "Mariposario Ikiam"
          add_change_entry(rv, "Registrar Muertes", id, "Collection_location", prev_cl, "Mariposario Ikiam", batch_id = batch_id)
          changed_count <- changed_count + 1
        }
      }
    }
    
    if (changed_count > 0) {
      # 1) persist the updated base
      rv$data$Insectary_data <- base

      # 2) refresh the on-screen table FROM the updated base, preserving mode and the same rows
      sel_ids <- unique(na.omit(tbl$Insectary_ID))
      rv$dead_tbl <- build_dead_table(
        rv$data$Insectary_data,
        ids          = sel_ids,
        review_only  = isTRUE(input$dead_review_only),
        default_cause= input$dead_cause_default,
        date_input_str = input$dead_date
      )
      # Ensure UI shows the newly-committed values (Death_cause + Death_date)
      hot_render_predictive(output, session, "dead_table", rv$dead_tbl, rv, force = TRUE)

      # 3) keep ID picker stable (choices + selection)
      df <- rv$data$Insectary_data
      pivot_id <- auto_start_insectary_id(df)
      isolate(update_ids_picker(session, "dead_ids", df,
              selected = input$dead_ids, start_from_id = pivot_id, offset_before = 200))

      showNotification("Actualizado y guardado en local.", type = "message")
    } else {
      showNotification("No hubo cambios para guardar.", type = "message")
    }
  })

  # ---- Register server logic for other tabs ----
  register_tubos_server(input, output, session, rv)
  register_emergidos_server(input, output, session, rv)
  register_subir_server(input, output, session, rv)
  register_hist_server(input, output, session, rv)
  register_search_server(input, output, session, rv)
}

# --- DRY: unified table change applier (used by Tubos & Emergidos) ---
apply_table_changes <- function(rv, tab_label, tbl,
                                skip_if_blank_cols = character(0)) {
  stopifnot(is.data.frame(tbl))
  base <- rv$data$Insectary_data
  batch_id <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  apply_change <- function(id, col, newv) {
    idx <- which(base$Insectary_ID == id)
    if (!length(idx) || !(col %in% names(base))) return()
    prev <- base[[col]][idx]
    if (col %in% skip_if_blank_cols && is_na_like_val(newv)) return()
    if (is_date_col(col)) { newv <- fmt_ui_date(newv); prev <- fmt_ui_date(prev) }
    if (!identical(as.character(prev), as.character(newv))) {
      base[[col]][idx] <<- newv
      add_change_entry(rv, tab = tab_label, insectary_id = id,
                       column = col, previous = prev, new_value = newv,
                       batch_id = batch_id)
    }
  }
  for (i in seq_len(nrow(tbl))) {
    id  <- tbl$Insectary_ID[i]
    cols <- intersect(names(tbl), names(base))
    for (col in cols) {
      if (!identical(col, "Insectary_ID")) apply_change(id, col, tbl[[col]][i])
    }
    idx <- which(base$Insectary_ID == id)
    if (!length(idx)) next
    prev_clutch <- base$`CLUTCH NUMBER`[idx]
    new_clutch  <- if ("CLUTCH NUMBER" %in% names(tbl)) tbl$`CLUTCH NUMBER`[i] else prev_clutch
    clutch_changed <- !identical(as.character(prev_clutch), as.character(new_clutch))
    if (isTRUE(clutch_changed)) {
      row_now  <- base[idx, , drop = FALSE]
      enforced <- apply_stock_origin_rule(row_now, rv$data$Insectary_stocks)
      if (!identical(as.character(row_now$Stock_of_origin[1]), as.character(enforced$Stock_of_origin[1]))) {
        apply_change(id, "Stock_of_origin", enforced$Stock_of_origin[1])
      }
      if ("Wild_Reared" %in% names(base) && is_na_like_val(base$Wild_Reared[idx])) {
        apply_change(id, "Wild_Reared", "Reared")
      }
      if ("Collection_location" %in% names(base) && is_na_like_val(base$Collection_location[idx])) {
        apply_change(id, "Collection_location", "Mariposario Ikiam")
      }
    }
  }
  rv$data$Insectary_data <- base
  saveRDS(rv$data$Insectary_data, rds_paths[["Insectary_data"]])
  save_local_state(list(
    version = rv$version, data = rv$data, changes = rv$changes, history = rv$history
  ))
}

## Run the Application ---------------------------------------------------------
shinyApp(ui = ui, server = server)