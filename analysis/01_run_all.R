# Reproducibility script for cervical cancer prevention and education gaps in the Republic of the Congo
# This script was extracted from the repository R Markdown file.
# It contains no author names, emails, degrees, or affiliations.
# Run from the root of the repository.


# ---- Rmd chunk 01 ----
# ---------------------------------------------------------------------------
# SETUP CHUNK
# ---------------------------------------------------------------------------
# All packages used in this manuscript. Install once if missing.
# pkgs <- c("tidyverse", "knitr", "kableExtra", "scales", "sf", "rnaturalearth", "rnaturalearthdata", "patchwork", "ggrepel", "officer", "flextable", "gridExtra")
# to_install <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
# if (length(to_install)) install.packages(to_install)

library(tidyverse)   # data handling, ggplot2, readr, dplyr, tidyr, tibble, forcats
library(knitr)       # tables and knitting
library(kableExtra)  # table styling
library(scales)      # percent and number formatting
library(sf)          # spatial data handling for maps
library(rnaturalearth)      # country boundaries for map figures
library(rnaturalearthdata)  # Natural Earth data used by rnaturalearth
library(patchwork)          # panel figures
library(ggrepel)            # non-overlapping map labels
library(officer)            # saving table outputs as Word documents
library(flextable)          # publication-ready Word tables
library(gridExtra)          # supplementary PDF tables
# Note: flextable and kableExtra both have a footnote() function.
# In table chunks below, kableExtra::footnote() is used explicitly.

# ---------------------------------------------------------------------------
# RESULTS FOLDER
# ---------------------------------------------------------------------------
# When this Rmd is knitted from RStudio, getwd() is normally the folder that
# contains the Rmd file. All generated outputs are saved in the folder below.
results_dir_name <- "cervical_cancer_congo_results"
results_dir        <- file.path(getwd(), results_dir_name)

# Clear old outputs first so outdated files do not remain after a new knit.
if (dir.exists(results_dir)) {
  unlink(results_dir, recursive = TRUE, force = TRUE)
}

main_dir             <- file.path(results_dir, "01_main_manuscript")
main_figures_dir     <- file.path(main_dir, "figures")
main_tables_csv_dir  <- file.path(main_dir, "tables_csv")
main_tables_word_dir <- file.path(main_dir, "tables_word")

online_dir             <- file.path(results_dir, "02_online_resources")
online_figures_dir     <- file.path(online_dir, "figures")
online_tables_csv_dir  <- file.path(online_dir, "tables_csv")
online_tables_word_dir <- file.path(online_dir, "tables_word")

manifest_dir       <- file.path(results_dir, "03_manifests")
data_dir           <- file.path(results_dir, "04_data")

# Backward-compatible aliases used by helper functions.
figures_dir        <- main_figures_dir
tables_dir         <- main_tables_csv_dir
tables_word_dir    <- main_tables_word_dir
supp_dir           <- online_dir
supp_figures_dir   <- online_figures_dir
supp_tables_csv_dir  <- online_tables_csv_dir
supp_tables_word_dir <- online_tables_word_dir
supplement_dir     <- online_figures_dir

dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(main_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(main_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(main_tables_csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(main_tables_word_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(online_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(online_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(online_tables_csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(online_tables_word_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(manifest_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

writeLines(
  c(
    "Cervical cancer Congo manuscript results",
    paste0("Generated on: ", Sys.time()),
    "",
    "01_main_manuscript/figures contains Fig. 1 and Fig. 2 only.",
    "01_main_manuscript/tables_csv contains Table 1 only.",
    "01_main_manuscript/tables_word contains Table 1 in Word format.",
    "02_online_resources/figures contains Online Resource 1 to Online Resource 3 as separate PNG and PDF files.",
    "02_online_resources/tables_csv contains Online Resource 4 to Online Resource 7 as CSV files.",
    "02_online_resources/tables_word contains Online Resource 4 to Online Resource 7 in Word format.",
    "02_online_resources/online_resources_supplementary_material.pdf is a single PDF compendium.",
    "03_manifests contains clear file lists for the main manuscript and online resources.",
    "04_data contains source and analysis datasets."
  ),
  file.path(results_dir, "README.txt")
)

# Helper to save data frames without breaking the knit if an object is missing.
save_csv_result <- function(x, filename, subfolder = c("tables", "data")) {
  subfolder <- match.arg(subfolder)
  target_dir <- if (subfolder == "tables") tables_dir else data_dir
  if (!is.null(x) && is.data.frame(x)) {
    readr::write_csv(x, file.path(target_dir, filename), na = "")
  }
  invisible(x)
}

save_csv_to <- function(x, target_dir, filename) {
  if (!is.null(x) && is.data.frame(x)) {
    if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(x, file.path(target_dir, filename), na = "")
  }
  invisible(x)
}

# Helper to format a data frame as a beautiful Word table.
make_word_table <- function(x, title = NULL, font_size = 9) {
  ft <- flextable::flextable(x)
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::font(ft, fontname = "Aptos", part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  ft <- flextable::fontsize(ft, size = font_size + 1, part = "header")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::color(ft, color = "#1F2A33", part = "all")
  ft <- flextable::bg(ft, bg = "#F7EAEA", part = "header")
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline_top(ft, part = "header",
                             border = officer::fp_border(color = "#B23A48", width = 1.2))
  ft <- flextable::hline_bottom(ft, part = "header",
                                border = officer::fp_border(color = "#B23A48", width = 0.8))
  ft <- flextable::hline_bottom(ft, part = "body",
                                border = officer::fp_border(color = "#D6DEE3", width = 0.6))
  ft <- flextable::align(ft, align = "left", part = "all")
  ft <- flextable::valign(ft, valign = "top", part = "all")
  ft <- flextable::padding(ft, padding.top = 4, padding.bottom = 4,
                           padding.left = 4, padding.right = 4, part = "all")
  ft <- flextable::set_table_properties(ft, layout = "autofit", width = 1)
  ft <- flextable::autofit(ft)
  ft
}

# Helper to add a flextable to a Word document.
# body_add_flextable belongs to the flextable package, not officer.
add_flextable_to_doc <- function(doc, ft) {
  flextable::body_add_flextable(doc, value = ft)
}

# Helper to save each table as a standalone Word document.
save_docx_table_result <- function(x, title, filename, note = NULL, font_size = 9, target_dir = tables_word_dir) {
  if (!is.null(x) && is.data.frame(x)) {
    if (!dir.exists(target_dir)) {
      dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    }

    doc <- officer::read_docx()
    doc <- officer::body_add_par(doc, title, style = "heading 1")
    doc <- officer::body_add_par(doc, "", style = "Normal")
    doc <- add_flextable_to_doc(doc, make_word_table(x, title = title, font_size = font_size))

    if (!is.null(note) && nzchar(note)) {
      doc <- officer::body_add_par(doc, "", style = "Normal")
      doc <- officer::body_add_par(doc, paste0("Note: ", note), style = "Normal")
    }

    print(doc, target = file.path(target_dir, filename))
  }

  invisible(x)
}

# Helper to save all manuscript tables into one Word document.
save_all_docx_tables <- function(tables, filename = "online_resources_tables_compendium.docx", target_dir = supp_tables_word_dir) {
  if (length(tables) == 0) return(invisible(NULL))

  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  }

  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "Cervical cancer Congo manuscript tables", style = "heading 1")
  doc <- officer::body_add_par(doc, paste0("Generated on: ", Sys.time()), style = "Normal")

  for (i in seq_along(tables)) {
    item <- tables[[i]]
    if (i > 1) doc <- officer::body_add_break(doc)
    doc <- officer::body_add_par(doc, item$title, style = "heading 1")
    doc <- add_flextable_to_doc(doc, make_word_table(item$data, title = item$title, font_size = item$font_size))

    if (!is.null(item$note) && nzchar(item$note)) {
      doc <- officer::body_add_par(doc, "", style = "Normal")
      doc <- officer::body_add_par(doc, paste0("Note: ", item$note), style = "Normal")
    }
  }

  print(doc, target = file.path(target_dir, filename))
  invisible(NULL)
}

# Helper to explicitly save each plot as PNG and PDF.
# This works both when knitting and when running chunks manually in RStudio.
save_figure_result <- function(plot, filename, width, height, dpi = 300) {
  if (!dir.exists(figures_dir)) {
    dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
  }

  png_file <- file.path(figures_dir, paste0(filename, ".png"))
  pdf_file <- file.path(figures_dir, paste0(filename, ".pdf"))

  ggplot2::ggsave(
    filename = png_file,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    bg = "white"
  )

  try(
    ggplot2::ggsave(
      filename = pdf_file,
      plot = plot,
      width = width,
      height = height,
      units = "in",
      device = "pdf",
      bg = "white"
    ),
    silent = TRUE
  )

  invisible(plot)
}

# Helper to explicitly save supplementary figures.
save_supp_figure_result <- function(plot, filename, width, height, dpi = 300) {
  if (!dir.exists(supplement_dir)) {
    dir.create(supplement_dir, recursive = TRUE, showWarnings = FALSE)
  }

  png_file <- file.path(supplement_dir, paste0(filename, ".png"))
  pdf_file <- file.path(supplement_dir, paste0(filename, ".pdf"))

  ggplot2::ggsave(
    filename = png_file,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    bg = "white"
  )

  try(
    ggplot2::ggsave(
      filename = pdf_file,
      plot = plot,
      width = width,
      height = height,
      units = "in",
      device = "pdf",
      bg = "white"
    ),
    silent = TRUE
  )

  invisible(plot)
}


# Helper to draw a data frame as a table page in a PDF device.
draw_pdf_table_page <- function(df, title, footnote = NULL, max_rows = 18, start_row = 1) {
  df <- as.data.frame(df)
  end_row <- min(nrow(df), start_row + max_rows - 1)
  page_df <- df[start_row:end_row, , drop = FALSE]

  grid::grid.newpage()
  grid::grid.text(
    title,
    x = grid::unit(0.04, "npc"),
    y = grid::unit(0.96, "npc"),
    just = c("left", "top"),
    gp = grid::gpar(fontface = "bold", fontsize = 13, col = "#1F2A33")
  )

  table_theme <- gridExtra::ttheme_minimal(
    base_size = 7,
    core = list(
      fg_params = list(hjust = 0, x = 0.02, col = "#1F2A33"),
      bg_params = list(fill = "white", col = "#D6DEE3")
    ),
    colhead = list(
      fg_params = list(fontface = "bold", hjust = 0, x = 0.02, col = "#1F2A33"),
      bg_params = list(fill = "#F7EAEA", col = "#B23A48")
    )
  )

  tg <- gridExtra::tableGrob(page_df, rows = NULL, theme = table_theme)
  grid::grid.draw(gridExtra::arrangeGrob(tg, top = NULL))

  if (!is.null(footnote) && nzchar(footnote)) {
    grid::grid.text(
      paste0("Note: ", footnote),
      x = grid::unit(0.04, "npc"),
      y = grid::unit(0.04, "npc"),
      just = c("left", "bottom"),
      gp = grid::gpar(fontsize = 7, col = "#6E7B87")
    )
  }

  invisible(end_row)
}

# Helper to save a single PDF compendium for online resources.
save_supplementary_pdf <- function() {
  if (!dir.exists(online_dir)) {
    dir.create(online_dir, recursive = TRUE, showWarnings = FALSE)
  }

  pdf_file <- file.path(online_dir, "online_resources_supplementary_material.pdf")

  grDevices::pdf(pdf_file, width = 8.5, height = 11, onefile = TRUE, paper = "special")
  on.exit(grDevices::dev.off(), add = TRUE)

  grid::grid.newpage()
  grid::grid.text(
    "Online Resources",
    x = grid::unit(0.5, "npc"),
    y = grid::unit(0.72, "npc"),
    gp = grid::gpar(fontface = "bold", fontsize = 22, col = "#1F2A33")
  )
  grid::grid.text(
    "Cervical cancer prevention and education gaps in the Republic of the Congo",
    x = grid::unit(0.5, "npc"),
    y = grid::unit(0.65, "npc"),
    gp = grid::gpar(fontsize = 12, col = "#6E7B87")
  )
  grid::grid.text(
    paste0("Generated on ", Sys.Date()),
    x = grid::unit(0.5, "npc"),
    y = grid::unit(0.58, "npc"),
    gp = grid::gpar(fontsize = 10, col = "#6E7B87")
  )

  if (exists("p4")) print(p4 + labs(title = "Online Resource 1. HPV 16/18 pathway"))
  if (exists("p5")) print(p5 + labs(title = "Online Resource 2. WHO 90-70-90 readiness gap"))
  if (exists("p8")) print(p8 + labs(title = "Online Resource 3. Illustrative 2030 coverage endpoints"))

  if (exists("burden_tab")) {
    draw_pdf_table_page(
      burden_tab,
      "Online Resource 4. Cervical cancer burden indicators",
      "Sources: GLOBOCAN 2022 and ICO/IARC HPV Information Centre 2023.",
      max_rows = 18
    )
  }

  if (exists("gap_tab")) {
    draw_pdf_table_page(
      gap_tab,
      "Online Resource 5. WHO 90-70-90 readiness gap",
      "Vaccination is zero because no national programme is in place. Screening and treatment show no located national figure, so the gap is shown as an upper bound.",
      max_rows = 18
    )
  }

  if (exists("scen_tab")) {
    draw_pdf_table_page(
      scen_tab,
      "Online Resource 6. Illustrative scale-up assumptions",
      "These are stated assumptions for discussion, not measured values or predictions.",
      max_rows = 18
    )
  }

  if (exists("supp_tab")) {
    start <- 1
    page <- 1
    while (start <= nrow(supp_tab)) {
      end <- draw_pdf_table_page(
        supp_tab,
        paste0("Online Resource 7. All extracted indicators, page ", page),
        "All values are from public aggregated sources. Missing values indicate that no national figure was located in the public sources used.",
        max_rows = 18,
        start_row = start
      )
      start <- end + 1
      page <- page + 1
    }
  }

  invisible(pdf_file)
}

knitr::opts_chunk$set(
  echo = FALSE,        # hide code in the knitted manuscript; set TRUE to show
  warning = FALSE,
  message = FALSE,
  fig.align = "center",
  fig.width = 7,
  fig.height = 4.2,
  dpi = 300,
  dev = "png",
  fig.path = file.path(results_dir_name, "figures", "fig-")
)

# Detect output format so tables degrade gracefully across Word and PDF.
out_fmt <- knitr::opts_knit$get("rmarkdown.pandoc.to")
out_fmt <- ifelse(is.null(out_fmt), "html", out_fmt)

# ---- Rmd chunk 02 ----
# ---------------------------------------------------------------------------
# DATA ENTRY CHUNK  (single source of truth for Congo indicators)
# ---------------------------------------------------------------------------
# Every value below was read from an official public source and is stored here
# so the whole manuscript is reproducible. No individual-level data are used.
# Data objects in this manuscript, all defined in the next few chunks:
#   indicators          - Congo indicators (this chunk)
#   targets             - WHO 90-70-90 targets and Congo status (calculations chunk)
#   female_cancers      - top cancers in women, Congo (calculations chunk)
#   africa_lookup       - UN M49 sub-region of each African country (africa chunk)
#   africa_inc_df       - live GLOBOCAN/OWID incidence by African country (africa chunk)
#   africa_map          - Africa country boundaries joined to incidence data (map chunk)
#   neighbour_map       - Congo and bordering African neighbours used for Figure 7
#   scenario_assumptions, proj - illustrative scale-up scenarios (scenario chunk)
#
# HOW TO UPDATE A VALUE:
#   1. Find the row in the tibble. 2. Replace the number in the `value` column.
#   3. Keep `source`, `source_year`, and `note` truthful. 4. Re-knit.
# Items still marked "Not reported" in `note` need manual confirmation; the note
# says exactly where to read the correct number before submission.

indicators <- tibble::tribble(
  ~indicator,                          ~value,    ~unit,                 ~source,                 ~source_year, ~note,
  # --- Population (GLOBOCAN 2022 factsheet, Congo, Republic of) -------------
  "Total population",                   5797801,  "persons",             "GLOBOCAN 2022",          2022,        "Factsheet, Congo (Rep.)",
  "Female population",                  2900297,  "women",               "GLOBOCAN 2022",          2022,        "Factsheet, Congo (Rep.)",
  "Women at risk (female aged 15+)",    1750000,  "women",               "HPV Information Centre", 2023,        "Reported as 1.75 million",
  # --- Cervical cancer burden (GLOBOCAN 2022) ------------------------------
  "Cervical cancer new cases",          397,      "cases per year",      "GLOBOCAN 2022",          2022,        "Rank 3 all cancers; rank 2 in women",
  "Cervical cancer deaths",             248,      "deaths per year",     "GLOBOCAN 2022",          2022,        "Rank 3 all cancers",
  "Cervical cancer 5-year prevalence",  944,      "cases",               "GLOBOCAN 2022",          2022,        "5-year prevalent cases",
  "Cumulative incidence risk to age 75",2.6,      "percent",             "GLOBOCAN 2022",          2022,        "Cervix uteri, women",
  "Cumulative mortality risk to age 75",1.7,      "percent",             "GLOBOCAN 2022",          2022,        "Cervix uteri, women",
  # --- Age-standardized incidence rate (now filled, two-source agreement) --
  "Age-standardized incidence rate",    22.3,     "per 100000 women",    "GLOBOCAN 2022",          2022,        "World ASR, women. IARC factsheet ASR chart shows 22.3; OWID/GLOBOCAN lists 22.32. Used for the like-for-like comparison with the elimination threshold.",
  # --- Earlier modelled estimate, kept for comparison (HPV Info Centre) ----
  "Cervical cancer cases (earlier est.)",350,     "cases per year",      "HPV Information Centre", 2023,        "Older estimate; for comparison only",
  "Cervical cancer deaths (earlier est.)",214,    "deaths per year",     "HPV Information Centre", 2023,        "Older estimate; for comparison only",
  "Crude cervical cancer incidence rate",12.7,    "per 100000 women",    "HPV Information Centre", 2023,        "Crude rate, not age-standardized",
  # --- HPV 16/18 prevalence by cervical finding (HPV Info Centre) -----------
  "HPV 16/18 in normal cytology",       3.8,      "percent",             "HPV Information Centre", 2023,        "Women with normal cytology",
  "HPV 16/18 in low-grade lesions",     24.9,     "percent",             "HPV Information Centre", 2023,        "LSIL / CIN-1",
  "HPV 16/18 in high-grade lesions",    38.6,     "percent",             "HPV Information Centre", 2023,        "HSIL / CIN-2 / CIN-3 / CIS",
  "HPV 16/18 in cervical cancer",       67.2,     "percent",             "HPV Information Centre", 2023,        "Invasive cervical cancer",
  # --- Co-factors (HPV Info Centre) ----------------------------------------
  "HIV prevalence, women 15-49",        3.7,      "percent",             "HPV Information Centre", 2023,        "Women living with HIV face higher risk",
  "Total fertility rate",               4.6,      "births per woman",    "HPV Information Centre", 2023,        "",
  "Smoking prevalence, women",          1.7,      "percent",             "HPV Information Centre", 2023,        "",
  # --- Prevention system status (HPV Info Centre) --------------------------
  "National screening recommendation",  0,        "0=No, 1=Yes",         "HPV Information Centre", 2023,        "Reported as: none",
  "HPV vaccination programme",          0,        "0=No, 1=Yes",         "HPV Information Centre", 2023,        "Reported as: none in place",
  "HPV vaccine coverage, first dose",   NA,       "percent",             "HPV Information Centre", 2023,        "No programme, so no coverage reported",
  "Screening coverage (women screened)",NA,       "percent",             "WHO / national data",    NA,          "No national figure located in the public sources used",
  "Treatment coverage",                 NA,       "percent",             "WHO / national data",    NA,          "No national figure located in the public sources used"
)

# Pull single values out for use in the text and figures.
get_val <- function(name) indicators$value[indicators$indicator == name]

# Format a value for tables: integers with thousands separators, decimals as-is.
fmt_val <- function(v) {
  ifelse(is.na(v), "Not reported",
         ifelse(v == round(v),
                scales::comma(v, accuracy = 1),
                formatC(v, format = "f", digits = 1, big.mark = ",")))
}

cc_cases   <- get_val("Cervical cancer new cases")
cc_deaths  <- get_val("Cervical cancer deaths")
cc_prev    <- get_val("Cervical cancer 5-year prevalence")
crude_rate <- get_val("Crude cervical cancer incidence rate")
asr_inc    <- get_val("Age-standardized incidence rate")
fem_pop    <- get_val("Female population")
tot_pop    <- get_val("Total population")
hpv_cancer <- get_val("HPV 16/18 in cervical cancer")
hiv_women  <- get_val("HIV prevalence, women 15-49")

# ---- Rmd chunk 03 ----
# ---------------------------------------------------------------------------
# CALCULATION CHUNK
# ---------------------------------------------------------------------------

# 1. Mortality-to-incidence ratio (MIR): a population-level proxy for how late
#    disease is found and how well it is treated. Higher means worse.
mir         <- cc_deaths / cc_cases
mir_earlier <- get_val("Cervical cancer deaths (earlier est.)") /
               get_val("Cervical cancer cases (earlier est.)")

# 2. WHO 90-70-90 targets and current Congo status.
#    Source: WHO global strategy 2020. Vaccination current is 0 (no programme).
#    Screening and treatment have no located national figure.
targets <- tibble::tibble(
  pillar  = c("Vaccination", "Screening", "Treatment"),
  target  = c(90, 70, 90),
  current = c(0, NA, NA)
) %>%
  mutate(
    current_display = ifelse(is.na(current), 0, current),
    gap_pp          = target - current_display,
    status          = ifelse(is.na(current), "No national figure", "Reported")
  )

# 3. Elimination threshold (WHO): age-standardized incidence below 4 per 100,000
#    women-years. Now compared like-for-like against Congo's ASR (22.3).
elim_threshold   <- 4
asr_vs_threshold <- asr_inc / elim_threshold   # how many times above the line

# 4. Top cancers in women, for the leading-cancer figure.
#    GLOBOCAN 2022 factsheet, Congo (Rep.), females, all ages.
female_cancers <- tibble::tibble(
  cancer = c("Breast", "Cervix uteri", "Colorectum", "Liver", "Ovary"),
  cases  = c(530, 397, 84, 81, 68)
)

# ---- Rmd chunk 04 ----
# ---------------------------------------------------------------------------
# SHARED LOOK FOR ALL FIGURES AND TABLES
# ---------------------------------------------------------------------------
pal <- list(
  ink    = "#1F2A33", muted = "#6B7884", teal = "#2C7A7B", blue = "#2F5C8A",
  red    = "#B23A48", amber = "#D08C34", faint = "#E7ECEF", cervix = "#B23A48",
  paper  = "white", soft = "#F7F9FA", border = "#D9E1E7"
)

axis_pub <- theme(
  axis.line = element_line(colour = pal$ink, linewidth = 0.35),
  axis.ticks = element_line(colour = pal$ink, linewidth = 0.35),
  axis.ticks.length = grid::unit(2.5, "pt")
)

theme_jce <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      text             = element_text(colour = pal$ink),
      plot.title       = element_text(face = "bold", size = base_size + 3, margin = margin(b = 2)),
      plot.subtitle    = element_text(colour = pal$muted, size = base_size - 1, margin = margin(b = 10)),
      plot.caption     = element_text(colour = pal$muted, size = base_size - 3, hjust = 0, margin = margin(t = 10)),
      axis.title       = element_text(colour = pal$muted, size = base_size - 2),
      axis.text        = element_text(colour = pal$ink, size = base_size - 2),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = pal$faint, linewidth = 0.4),
      legend.position  = "top",
      legend.title     = element_text(colour = pal$muted, size = base_size - 2),
      legend.text      = element_text(size = base_size - 2),
      legend.key       = element_rect(fill = pal$paper, colour = NA),
      legend.background = element_rect(fill = pal$paper, colour = NA),
      panel.background = element_rect(fill = pal$paper, colour = NA),
      plot.background  = element_rect(fill = pal$paper, colour = NA),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.margin = margin(12, 14, 10, 12)
    )
}

theme_map_pub <- function(base_size = 11) {
  theme_jce(base_size = base_size) +
    theme(
      # Remove latitude / longitude grid lines behind the maps.
      panel.grid = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),

      # Keep axes invisible for map figures.
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),

      # White, publication-ready background.
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = "#CBD5DC", linewidth = 0.6),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key = element_rect(fill = "white", colour = NA),

      # Add a clean border around the map panel.
      panel.border = element_rect(fill = NA, colour = "#CBD5DC", linewidth = 0.6),
      legend.position = "bottom"
    )
}

# A single helper so all tables have a similar publication-ready style.
pretty_kable <- function(data, caption, longtable = FALSE, font_size = 9) {
  latex_opts <- if (longtable) c("repeat_header", "striped") else c("hold_position", "striped")
  kbl(data, booktabs = TRUE, longtable = longtable, caption = caption, align = "l") %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      latex_options = latex_opts,
      full_width = FALSE,
      position = "center",
      font_size = font_size
    ) %>%
    row_spec(0, bold = TRUE, color = "white", background = pal$ink) %>%
    column_spec(1, bold = TRUE, color = pal$ink)
}

# A placeholder used only if the live download for Figures 6 and 7 is unavailable.
placeholder_plot <- function(msg) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = msg, size = 4, colour = pal$muted, lineheight = 1.15) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA))
}

# ---- Rmd chunk 05 ----
# ---------------------------------------------------------------------------
# AFRICAN COMPARISON DATA  (official, downloaded live, no hand-typed values)
# ---------------------------------------------------------------------------
# Source: IARC GLOBOCAN 2022, republished by Our World in Data (OWID).
#   - Age-standardized incidence per 100,000 women, 2022 (one value per country)
#   - Annual new cases and deaths counts, 2022 (used to derive a country MIR)
# Sub-region membership is the UN M49 geographic classification (a fixed fact).

africa_lookup <- tibble::tribble(
  ~code, ~subregion,
  "DZA","Northern","EGY","Northern","LBY","Northern","MAR","Northern","SDN","Northern","TUN","Northern","ESH","Northern",
  "BEN","Western","BFA","Western","CPV","Western","CIV","Western","GMB","Western","GHA","Western","GIN","Western",
  "GNB","Western","LBR","Western","MLI","Western","MRT","Western","NER","Western","NGA","Western","SEN","Western",
  "SLE","Western","TGO","Western",
  "AGO","Middle","CMR","Middle","CAF","Middle","TCD","Middle","COG","Middle","COD","Middle","GNQ","Middle",
  "GAB","Middle","STP","Middle",
  "BDI","Eastern","COM","Eastern","DJI","Eastern","ERI","Eastern","ETH","Eastern","KEN","Eastern","G","Eastern",
  "MWI","Eastern","MUS","Eastern","MOZ","Eastern","RWA","Eastern","SYC","Eastern","SOM","Eastern","SSD","Eastern",
  "TZA","Eastern","UGA","Eastern","ZMB","Eastern","ZWE","Eastern",
  "BWA","Southern","SWZ","Southern","LSO","Southern","NAM","Southern","ZAF","Southern"
)

owid_inc_url    <- "https://ourworldindata.org/grapher/rate-of-new-cervical-cancer-cases-gco.csv?v=1&csvType=full&useColumnShortNames=false"
owid_counts_url <- "https://ourworldindata.org/grapher/number-of-new-cases-and-deaths-from-cervical-cancer.csv?v=1&csvType=full&useColumnShortNames=false"

africa_ok_inc <- FALSE; africa_ok_counts <- FALSE
africa_inc_df <- NULL;  counts_df <- NULL; africa_profile <- NULL

value_cols <- function(df) setdiff(names(df), c("Entity", "Code", "Year"))

# Incidence (one indicator column)
try({
  raw <- readr::read_csv(owid_inc_url, show_col_types = FALSE, progress = FALSE)
  vc  <- value_cols(raw)[1]
  inc <- raw %>%
    filter(Year == max(Year, na.rm = TRUE)) %>%
    transmute(code = Code, country = Entity, asr_inc = .data[[vc]]) %>%
    inner_join(africa_lookup, by = "code") %>%
    filter(!is.na(asr_inc))
  if (nrow(inc) > 5) { africa_inc_df <- inc; africa_ok_inc <- TRUE }
}, silent = TRUE)

# Counts (two indicator columns: new cases and deaths) to derive a country MIR
try({
  rawc <- readr::read_csv(owid_counts_url, show_col_types = FALSE, progress = FALSE)
  ic   <- value_cols(rawc)
  dcol <- ic[grepl("death", ic, ignore.case = TRUE)][1]
  ccol <- ic[grepl("case|new", ic, ignore.case = TRUE) & !grepl("death", ic, ignore.case = TRUE)][1]
  if (!is.na(dcol) && !is.na(ccol)) {
    counts_df <- rawc %>%
      filter(Year == max(Year, na.rm = TRUE)) %>%
      transmute(code = Code, cases = .data[[ccol]], deaths = .data[[dcol]])
    africa_ok_counts <- TRUE
  }
}, silent = TRUE)

# Central African burden data retained for optional descriptive checks
if (africa_ok_inc && africa_ok_counts) {
  africa_profile <- africa_inc_df %>%
    inner_join(counts_df, by = "code") %>%
    filter(subregion == "Middle", cases > 0) %>%
    transmute(code, country, inc = asr_inc, deaths = deaths, mir = deaths / cases)
}

# Safe summary text for the prose (works whether or not the download succeeded)
if (africa_ok_inc) {
  n_africa  <- nrow(africa_inc_df)
  congo_row <- africa_inc_df %>% filter(code == "COG")
  congo_asr <- if (nrow(congo_row) > 0) congo_row$asr_inc[1] else asr_inc
  n_above   <- sum(africa_inc_df$asr_inc > elim_threshold, na.rm = TRUE)
  africa_text <- paste0(
    "Across the ", n_africa, " African countries with a 2022 estimate, ", n_above,
    " sit above the WHO elimination threshold of ", elim_threshold,
    " per 100,000 women. Congo, at about ", formatC(congo_asr, format = "f", digits = 1),
    " per 100,000, is high in absolute terms while sitting in the middle of the African range, ",
    "well below the highest-burden countries of Eastern and Southern Africa."
  )
} else {
  africa_text <- paste0(
    "Congo's age-standardized cervical cancer incidence is about ", formatC(asr_inc, format = "f", digits = 1),
    " per 100,000 women (GLOBOCAN 2022), more than ", round(asr_vs_threshold, 1),
    " times the WHO elimination threshold of ", elim_threshold,
    " per 100,000. The African comparison figures download official country data live from Our World in Data ",
    "and will render when the document is knitted with internet access."
  )
}

# ---- Rmd chunk 06 ----
# ---------------------------------------------------------------------------
# AFRICA MAP DATA
# ---------------------------------------------------------------------------
# Figures 6 and 7 use Natural Earth country boundaries joined to the live
# GLOBOCAN/OWID cervical cancer incidence data downloaded in the previous chunk.
# Figure 6 is an Africa-wide heatmap map. Figure 7 is also a heatmap map, but
# its fill scale is recalculated only among the Republic of the Congo and its
# bordering African neighbours. Label positions are hand-tuned for a clean,
# publication-ready appearance.

africa_map <- NULL
neighbour_map <- NULL
africa_callout_df <- NULL
neighbour_callout_df <- NULL
africa_fill_limits <- c(0, 1)
neighbour_fill_limits <- c(0, 1)

# Immediate African neighbours of the Republic of the Congo:
# Gabon, Cameroon, Central African Republic, Democratic Republic of the Congo,
# and Angola, through Cabinda.
neighbour_codes <- c("COG", "GAB", "CMR", "CAF", "COD", "AGO")

if (africa_ok_inc) {
  africa_map <- rnaturalearth::ne_countries(continent = "Africa", returnclass = "sf") %>%
    sf::st_as_sf() %>%
    select(name, iso_a3, geometry) %>%
    left_join(
      africa_inc_df %>% select(code, country, asr_inc, subregion),
      by = c("iso_a3" = "code")
    )

  africa_fill_limits <- range(africa_map$asr_inc, na.rm = TRUE)

  africa_callout_df <- africa_map %>%
    filter(iso_a3 == "COG") %>%
    sf::st_point_on_surface() %>%
    bind_cols(as_tibble(sf::st_coordinates(.))) %>%
    sf::st_drop_geometry() %>%
    transmute(
      iso_a3,
      cx = X, cy = Y,
      lx = 45.0, ly = 2.0,
      label = paste0("Republic of the Congo
", formatC(asr_inc, format = "f", digits = 1))
    )

  neighbour_map <- africa_map %>%
    filter(iso_a3 %in% neighbour_codes) %>%
    mutate(
      short_name = dplyr::case_when(
        iso_a3 == "COG" ~ "Republic of the Congo",
        iso_a3 == "COD" ~ "DR Congo",
        iso_a3 == "CAF" ~ "Central African Rep.",
        iso_a3 == "CMR" ~ "Cameroon",
        iso_a3 == "GAB" ~ "Gabon",
        iso_a3 == "AGO" ~ "Angola",
        TRUE ~ name
      )
    )

  neighbour_fill_limits <- range(neighbour_map$asr_inc, na.rm = TRUE)

  neighbour_callout_df <- neighbour_map %>%
    sf::st_point_on_surface() %>%
    bind_cols(as_tibble(sf::st_coordinates(.))) %>%
    sf::st_drop_geometry() %>%
    transmute(
      iso_a3, short_name, asr_inc,
      cx = X, cy = Y,
      lx = dplyr::case_when(
        iso_a3 == "COG" ~ 18.7,
        iso_a3 == "GAB" ~ 6.4,
        iso_a3 == "CMR" ~ 6.4,
        iso_a3 == "CAF" ~ 31.4,
        iso_a3 == "COD" ~ 31.4,
        iso_a3 == "AGO" ~ 6.4,
        TRUE ~ NA_real_
      ),
      ly = dplyr::case_when(
        iso_a3 == "COG" ~ 12.4,
        iso_a3 == "GAB" ~ -2.0,
        iso_a3 == "CMR" ~ 7.8,
        iso_a3 == "CAF" ~ 9.6,
        iso_a3 == "COD" ~ 0.3,
        iso_a3 == "AGO" ~ -11.7,
        TRUE ~ NA_real_
      ),
      curvature = dplyr::case_when(
        iso_a3 == "COG" ~ -0.10,
        iso_a3 == "GAB" ~  0.18,
        iso_a3 == "CMR" ~ -0.12,
        iso_a3 == "CAF" ~  0.10,
        iso_a3 == "COD" ~ -0.08,
        iso_a3 == "AGO" ~  0.15,
        TRUE ~ 0
      ),
      hjust = dplyr::case_when(
        iso_a3 %in% c("GAB", "CMR", "AGO") ~ 0,
        iso_a3 %in% c("CAF", "COD") ~ 1,
        iso_a3 == "COG" ~ 0.5,
        TRUE ~ 0.5
      ),
      label = paste0(short_name, "
", formatC(asr_inc, format = "f", digits = 1))
    )
}

# ---- Rmd chunk 07 ----
# ---------------------------------------------------------------------------
# ILLUSTRATIVE SCENARIO ASSUMPTIONS (transparent, not official data)
# ---------------------------------------------------------------------------
# These are stated assumptions for a simple coverage scale-up exercise, NOT
# measured values and NOT a prediction of individual outcomes. They describe how
# far Congo would move toward each WHO 90-70-90 target by 2030 under three paths.
scenario_assumptions <- tibble::tribble(
  ~scenario,               ~frac_of_target_by_2030, ~note,
  "Current status",        0.0,  "Illustrative assumption: no national scale-up, coverage stays at baseline",
  "Moderate scale-up",     0.5,  "Illustrative assumption: reach half of each WHO target by 2030",
  "Accelerated scale-up",  1.0,  "Illustrative assumption: reach each WHO target by 2030"
)

base_year <- as.integer(format(Sys.Date(), "%Y"))
end_year  <- 2030

# Linear coverage trajectory from baseline (0% today) to the scenario endpoint.
proj <- tidyr::crossing(
  pillar   = targets$pillar,
  scenario = scenario_assumptions$scenario,
  year     = base_year:end_year
) %>%
  left_join(targets %>% select(pillar, target), by = "pillar") %>%
  left_join(scenario_assumptions %>% select(scenario, frac_of_target_by_2030), by = "scenario") %>%
  mutate(
    endpoint = frac_of_target_by_2030 * target,
    coverage = (year - base_year) / (end_year - base_year) * endpoint,
    pillar   = factor(pillar, levels = c("Vaccination", "Screening", "Treatment")),
    scenario = factor(scenario, levels = scenario_assumptions$scenario)
  )

# ---- Rmd chunk 08 ----
# ---------------------------------------------------------------------------
# SAVE SOURCE AND ANALYSIS DATASETS
# ---------------------------------------------------------------------------
save_csv_result(indicators, "source_indicators.csv", subfolder = "data")
save_csv_result(targets, "who_targets_and_gaps.csv", subfolder = "data")
save_csv_result(female_cancers, "female_cancer_sites_congo.csv", subfolder = "data")
save_csv_result(scenario_assumptions, "scenario_assumptions.csv", subfolder = "data")
save_csv_result(proj, "scenario_projection_data.csv", subfolder = "data")

if (!is.null(africa_inc_df)) {
  save_csv_result(africa_inc_df, "africa_incidence_iarc_owid_2022.csv", subfolder = "data")
}

if (!is.null(africa_profile)) {
  save_csv_result(africa_profile, "central_africa_burden_profile.csv", subfolder = "data")
}

if (!is.null(neighbour_map)) {
  save_csv_result(
    neighbour_map %>% sf::st_drop_geometry(),
    "congo_and_neighbour_incidence.csv",
    subfolder = "data"
  )
}

# ---- Rmd chunk 09 ----
# ---------------------------------------------------------------------------
# MAIN TABLE 1 FOR JOURNAL OF CANCER EDUCATION
# Combined burden indicators and WHO target gaps to help meet the 3-item limit.
# ---------------------------------------------------------------------------
burden_main <- indicators %>%
  filter(indicator %in% c(
    "Cervical cancer new cases",
    "Cervical cancer deaths",
    "Cervical cancer 5-year prevalence",
    "Age-standardized incidence rate",
    "Crude cervical cancer incidence rate",
    "HPV 16/18 in cervical cancer"
  )) %>%
  transmute(
    Domain = "Burden and HPV profile",
    Indicator = indicator,
    Value = fmt_val(value),
    Unit = unit
  )

gap_main <- targets %>%
  transmute(
    Domain = "WHO 90-70-90 readiness gap",
    Indicator = pillar,
    Value = ifelse(
      status == "No national figure",
      paste0("No national figure located, gap up to ", gap_pp, " percentage points"),
      paste0(current_display, "%, gap ", gap_pp, " percentage points")
    ),
    Unit = paste0("WHO target: ", target, "%")
  )

main_table1 <- bind_rows(burden_main, gap_main)

save_csv_to(main_table1, main_tables_csv_dir, "table1_main_combined_burden_and_who_gap.csv")
save_docx_table_result(
  main_table1,
  title = "Table 1. Combined cervical cancer burden and WHO 90-70-90 readiness gap",
  filename = "table1_main_combined_burden_and_who_gap.docx",
  note = "Screening and treatment are reported as no national figure located, not as confirmed zero values.",
  font_size = 9,
  target_dir = main_tables_word_dir
)

pretty_kable(
  main_table1,
  caption = "Table 1. Cervical cancer burden and WHO 90-70-90 readiness gap in the Republic of the Congo.",
  font_size = 9
) %>%
  kableExtra::footnote(
    general = "Screening and treatment are reported as no national figure located, not as confirmed zero values.",
    general_title = "Note: ",
    footnote_as_chunk = TRUE
  )

# ---- Rmd chunk 10 ----
burden_tab <- indicators %>%
  filter(indicator %in% c(
    "Total population", "Female population",
    "Cervical cancer new cases", "Cervical cancer deaths",
    "Cervical cancer 5-year prevalence",
    "Age-standardized incidence rate", "Crude cervical cancer incidence rate"
  )) %>%
  transmute(
    Indicator = indicator,
    Value     = fmt_val(value),
    Unit      = unit,
    Source    = paste0(source, " (", source_year, ")")
  )

save_csv_to(burden_tab, supp_tables_csv_dir, "online_resource_4_burden_only.csv")
save_docx_table_result(
  burden_tab,
  title = "Online Resource 4. Cervical cancer burden indicators in the Republic of the Congo",
  filename = "online_resource_4_burden_only.docx",
  note = "Sources: GLOBOCAN 2022 and ICO/IARC HPV Information Centre 2023.",
  font_size = 9,
  target_dir = supp_tables_word_dir
)

pretty_kable(
  burden_tab,
  caption = "Online Resource 4. Cervical cancer burden indicators in the Republic of the Congo from public sources.",
  font_size = 9
)

# ---- Rmd chunk 11 ----
burden_snapshot <- tibble(
  measure = factor(
    c("New cases per year", "Deaths per year", "Living with diagnosis\n(5-year)"),
    levels = c("Living with diagnosis\n(5-year)", "Deaths per year", "New cases per year")
  ),
  value = c(cc_cases, cc_deaths, cc_prev),
  role  = c("cases", "deaths", "prevalence")
)

p1 <- ggplot(burden_snapshot, aes(x = value, y = measure, fill = role)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = scales::comma(value)), hjust = -0.18, fontface = "bold",
            size = 3.6, colour = pal$ink) +
  scale_fill_manual(values = c(cases = pal$blue, deaths = pal$red, prevalence = pal$teal),
                    guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(
    title = "Annual burden",
    subtitle = "New cases, deaths, and five-year prevalence",
    x = "Women", y = NULL
  ) +
  theme_jce(base_size = 10) +
  axis_pub +
  theme(panel.grid.major.y = element_blank(), plot.margin = margin(8, 14, 8, 8))

fc <- female_cancers %>%
  mutate(is_cervix = cancer == "Cervix uteri",
         cancer    = reorder(cancer, cases))

p2 <- ggplot(fc, aes(x = cancer, y = cases, fill = is_cervix)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = scales::comma(cases), colour = is_cervix),
            hjust = -0.18, fontface = "bold", size = 3.2) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c(`TRUE` = pal$cervix, `FALSE` = pal$faint), guide = "none") +
  scale_colour_manual(values = c(`TRUE` = pal$cervix, `FALSE` = pal$muted), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(
    title = "Rank among cancers in women",
    subtitle = "New cases per year by cancer site",
    x = NULL, y = "New cases"
  ) +
  theme_jce(base_size = 10) +
  axis_pub +
  theme(panel.grid.major.y = element_blank(), plot.margin = margin(8, 18, 8, 8))

mir_df <- tibble(
  metric = factor(c("New cases per year", "Deaths per year"),
                  levels = c("Deaths per year", "New cases per year")),
  value  = c(cc_cases, cc_deaths),
  hl     = c(FALSE, TRUE)
)

p3 <- ggplot(mir_df, aes(x = value, y = metric, fill = hl)) +
  geom_col(width = 0.58) +
  geom_text(aes(label = scales::comma(value), colour = hl),
            hjust = -0.2, fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c(`TRUE` = pal$red, `FALSE` = pal$blue), guide = "none") +
  scale_colour_manual(values = c(`TRUE` = pal$red, `FALSE` = pal$blue), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Many cases end in death",
    subtitle = paste0("Mortality-to-incidence ratio = ", round(mir, 2)),
    x = "Women per year", y = NULL
  ) +
  theme_jce(base_size = 10) +
  axis_pub +
  theme(panel.grid.major.y = element_blank(), plot.margin = margin(8, 14, 8, 8))

panel_123 <- (p1 + p2 + p3) +
  patchwork::plot_layout(design = "AA\nBC", heights = c(0.9, 1.1)) +
  patchwork::plot_annotation(
    tag_levels = "A",
    caption = "Source: GLOBOCAN 2022, Congo (Rep.) factsheet. Mortality-to-incidence ratio is a population-level proxy, not a survival rate.",
    theme = theme(
      plot.background = element_rect(fill = "white", colour = NA),
      plot.caption = element_text(colour = pal$muted, size = 8, hjust = 0)
    )
  )

save_figure_result(panel_123, "fig1_main_three_panel_burden", width = 7.8, height = 7.4)
panel_123

# ---- Rmd chunk 12 ----
hpv_fig <- tibble(
  step  = 1:4,
  label = c("Normal
cytology", "Low-grade
lesions", "High-grade
lesions", "Cervical
cancer"),
  value = c(get_val("HPV 16/18 in normal cytology"),
            get_val("HPV 16/18 in low-grade lesions"),
            get_val("HPV 16/18 in high-grade lesions"),
            get_val("HPV 16/18 in cervical cancer"))
)

p4 <- ggplot(hpv_fig, aes(x = step, y = value)) +
  geom_area(fill = pal$cervix, alpha = 0.10) +
  geom_line(colour = pal$cervix, linewidth = 1.15) +
  geom_point(colour = pal$cervix, fill = "white", shape = 21, stroke = 1.1, size = 3.6) +
  geom_text(aes(label = paste0(value, "%")), vjust = -1.1, fontface = "bold",
            colour = pal$cervix, size = 4) +
  scale_x_continuous(breaks = hpv_fig$step, labels = hpv_fig$label,
                     expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 20),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.03))) +
  labs(
    title    = "More severe lesions, more HPV 16/18",
    subtitle = "Share of cervical findings linked to HPV 16 and 18, Republic of Congo",
    x = "Cervical finding", y = "HPV 16/18 prevalence",
    caption  = "Source: ICO/IARC HPV Information Centre, Congo report, 2023."
  ) +
  theme_jce() +
  axis_pub +
  theme(panel.grid.major.x = element_blank())

save_supp_figure_result(p4, "online_resource_1_hpv_16_18_pathway", width = 7, height = 4.2)
p4

# ---- Rmd chunk 13 ----
gap_tab <- targets %>%
  transmute(
    Pillar = pillar,
    `WHO target (%)` = target,
    `Current status` = ifelse(status == "No national figure", "No national figure",
                              paste0(current_display, "%")),
    `Gap (percentage points)` = ifelse(status == "No national figure",
                                        paste0("up to ", gap_pp), as.character(gap_pp))
  )

save_csv_to(gap_tab, supp_tables_csv_dir, "online_resource_5_who_target_gap.csv")
save_docx_table_result(
  gap_tab,
  title = "Online Resource 5. Prevention gap against the WHO 90-70-90 targets",
  filename = "online_resource_5_who_target_gap.docx",
  note = "Vaccination is zero because no national programme is in place. Screening and treatment show no located national figure, so the gap is shown as an upper bound.",
  font_size = 9,
  target_dir = supp_tables_word_dir
)

pretty_kable(
  gap_tab,
  caption = "Online Resource 5. Prevention gap against the WHO 90-70-90 targets, Republic of the Congo.",
  font_size = 9
) %>%
  kableExtra::footnote(general = "Vaccination is zero because no national programme is in place. Screening and treatment show no located national figure, so the gap is shown as an upper bound, not a confirmed zero.",
           general_title = "Note: ", footnote_as_chunk = TRUE)

# ---- Rmd chunk 14 ----
gap_wide <- targets %>%
  mutate(
    pillar = factor(pillar, levels = c("Treatment", "Screening", "Vaccination")),
    lab_current = ifelse(status == "No national figure", "no national figure",
                         paste0(current_display, "%"))
  )

gap_points <- gap_wide %>%
  select(pillar, `WHO target` = target, `Current status` = current_display) %>%
  pivot_longer(c(`WHO target`, `Current status`), names_to = "type", values_to = "pct") %>%
  mutate(type = factor(type, levels = c("Current status", "WHO target")))

p5 <- ggplot() +
  geom_segment(data = gap_wide,
               aes(x = current_display, xend = target, y = pillar, yend = pillar),
               colour = pal$faint, linewidth = 3, lineend = "round") +
  geom_point(data = gap_points, aes(x = pct, y = pillar, colour = type), size = 5) +
  geom_text(data = gap_wide, aes(x = target, y = pillar, label = paste0(target, "%")),
            vjust = -1.25, colour = pal$blue, fontface = "bold", size = 3.7) +
  geom_text(data = gap_wide, aes(x = current_display, y = pillar, label = lab_current),
            hjust = 0, nudge_x = 2.2, vjust = 2.25, colour = pal$red, size = 3.2) +
  scale_colour_manual(values = c("Current status" = pal$red, "WHO target" = pal$blue)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.03))) +
  coord_cartesian(clip = "off") +
  labs(
    title    = "A wide gap to every WHO target",
    subtitle = "Current national status and the 90-70-90 targets for 2030",
    x = "Coverage", y = "Prevention pillar",
    caption  = "Sources: ICO/IARC HPV Information Centre, Congo, 2023 and WHO global strategy, 2020. Screening and treatment are plotted at zero as an upper bound on the gap."
  ) +
  theme_jce() +
  axis_pub +
  theme(panel.grid.major.y = element_blank(), legend.position = "top")

save_supp_figure_result(p5, "online_resource_2_who_90_70_90_gap", width = 7.2, height = 4.1)
p5

# ---- Rmd chunk 15 ----
if (africa_ok_inc && !is.null(africa_map)) {
  p6 <- ggplot(africa_map) +
    geom_sf(aes(fill = asr_inc), colour = "white", linewidth = 0.24) +
    geom_sf(
      data = africa_map %>% filter(iso_a3 == "COG"),
      fill = NA, colour = pal$ink, linewidth = 1.15
    ) +
    geom_curve(
      data = africa_callout_df,
      aes(x = lx - 0.9, y = ly - 0.05, xend = cx, yend = cy),
      inherit.aes = FALSE,
      curvature = 0.12,
      linewidth = 0.46,
      colour = pal$ink,
      arrow = grid::arrow(length = grid::unit(0.12, "inches"), type = "closed")
    ) +
    geom_label(
      data = africa_callout_df,
      aes(x = lx, y = ly, label = label),
      inherit.aes = FALSE,
      size = 2.7,
      lineheight = 0.95,
      label.size = 0.25,
      label.padding = grid::unit(0.16, "lines"),
      fill = "white",
      colour = pal$ink
    ) +
    scale_fill_gradientn(
      colours = c("#FFF7F3", "#F8C9BE", "#E37F7D", "#C64256", "#8A1538"),
      limits = africa_fill_limits,
      na.value = "#F4F4F4",
      name = "Age-standardized incidence
(per 100,000 women)",
      guide = guide_colourbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = grid::unit(4.2, "cm"),
        barheight = grid::unit(0.30, "cm"),
        frame.colour = "#D9D9D9",
        ticks.colour = "#8C8C8C"
      )
    ) +
    coord_sf(xlim = c(-19, 53), ylim = c(-36, 38), expand = FALSE, clip = "off", datum = NA) +
    labs(title = NULL, subtitle = NULL, caption = NULL) +
    theme_map_pub(base_size = 10.0) +
    theme(
      plot.margin = margin(2, 2, 2, 2),
      legend.position = "bottom",
      legend.title = element_text(hjust = 0.5, size = 8.5),
      legend.text = element_text(size = 8),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
      legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),
      panel.border = element_rect(fill = NA, colour = "#CBD5DC", linewidth = 0.55),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  invisible(p6)
} else {
  p6 <- placeholder_plot(paste0(
    "Africa-wide heatmap map needs the downloaded official African incidence data.
",
    "Re-knit when online and this panel will appear."
  ))
  invisible(p6)
}

# ---- Rmd chunk 16 ----
if (africa_ok_inc && !is.null(neighbour_map) && nrow(neighbour_map) > 0) {
  p7 <- ggplot(neighbour_map) +
    geom_sf(aes(fill = asr_inc), colour = "white", linewidth = 0.58) +
    geom_sf(
      data = neighbour_map %>% filter(iso_a3 == "COG"),
      fill = NA, colour = pal$ink, linewidth = 1.25
    ) +
    geom_curve(
      data = neighbour_callout_df,
      aes(x = lx, y = ly, xend = cx, yend = cy),
      inherit.aes = FALSE,
      linewidth = 0.32,
      lineend = "round",
      colour = pal$muted,
      curvature = 0.08,
      arrow = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed")
    ) +
    geom_label(
      data = neighbour_callout_df,
      aes(x = lx, y = ly, label = label, hjust = hjust),
      inherit.aes = FALSE,
      size = 2.35,
      lineheight = 0.92,
      label.size = 0.22,
      label.padding = grid::unit(0.13, "lines"),
      fill = "white",
      colour = pal$ink,
      fontface = "bold"
    ) +
    scale_fill_gradientn(
      colours = c("#FFF7F3", "#F8C9BE", "#E37F7D", "#C64256", "#8A1538"),
      limits = neighbour_fill_limits,
      na.value = "#F4F4F4",
      name = "Local incidence
(per 100,000 women)",
      guide = guide_colourbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = grid::unit(3.9, "cm"),
        barheight = grid::unit(0.30, "cm"),
        frame.colour = "#D9D9D9",
        ticks.colour = "#8C8C8C"
      )
    ) +
    coord_sf(
      xlim = c(5.6, 32.7),
      ylim = c(-14.3, 14.5),
      expand = FALSE,
      clip = "off",
      datum = NA
    ) +
    labs(title = NULL, subtitle = NULL, caption = NULL) +
    theme_map_pub(base_size = 10.0) +
    theme(
      plot.margin = margin(2, 2, 2, 2),
      legend.position = "bottom",
      legend.title = element_text(hjust = 0.5, size = 8.5),
      legend.text = element_text(size = 8),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
      legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),
      panel.border = element_rect(fill = NA, colour = "#CBD5DC", linewidth = 0.55),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  invisible(p7)
} else {
  p7 <- placeholder_plot(paste0(
    "Neighbour heatmap map needs the downloaded African incidence data.
",
    "Re-knit when online and this panel will appear."
  ))
  invisible(p7)
}

# ---- Rmd chunk 17 ----
# ---------------------------------------------------------------------------
# MAIN FIGURE 2 FOR JOURNAL OF CANCER EDUCATION
# Two-panel map figure to help meet the 3-item limit.
# This version removes internal titles and reduces white space.
# ---------------------------------------------------------------------------
if (exists("p6") && exists("p7")) {
  p6_main <- p6 + theme(plot.margin = margin(2, 2, 2, 2))
  p7_main <- p7 + theme(plot.margin = margin(2, 2, 2, 2))

  main_figure2_maps <- p6_main | p7_main
  main_figure2_maps <- main_figure2_maps +
    patchwork::plot_layout(widths = c(1, 1.02)) +
    patchwork::plot_annotation(
      tag_levels = "A",
      theme = theme(
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(2, 2, 2, 2)
      )
    )

  save_figure_result(
    main_figure2_maps,
    "fig2_main_two_panel_africa_and_neighbour_maps",
    width = 7.8,
    height = 5.3
  )
  main_figure2_maps
} else {
  placeholder_plot("Main Figure 2 needs the Africa map and neighbour map objects. Run the map chunks first.")
}

# ---- Rmd chunk 18 ----
scen_tab <- scenario_assumptions %>%
  transmute(
    Scenario = scenario,
    `Share of each WHO target reached by 2030` = paste0(frac_of_target_by_2030 * 100, "%"),
    Note = note
  )

save_csv_to(scen_tab, supp_tables_csv_dir, "online_resource_6_scaleup_scenarios.csv")
save_docx_table_result(
  scen_tab,
  title = "Online Resource 6. Illustrative scale-up assumptions",
  filename = "online_resource_6_scaleup_scenarios.docx",
  note = "These are stated assumptions for discussion, not measured values or predictions.",
  font_size = 9,
  target_dir = supp_tables_word_dir
)

pretty_kable(
  scen_tab,
  caption = "Online Resource 6. Illustrative scale-up assumptions used in the 2030 scenario figure. These are stated assumptions for discussion, not measured values or predictions.",
  font_size = 9
)

# ---- Rmd chunk 19 ----
proj_bar <- proj %>%
  filter(year == end_year) %>%
  mutate(
    pillar = factor(pillar, levels = c("Vaccination", "Screening", "Treatment")),
    scenario = factor(scenario, levels = c("Current status", "Moderate scale-up", "Accelerated scale-up")),
    label = paste0(round(coverage), "%"),
    label_y = ifelse(coverage == 0, 3.5, coverage + 4)
  )

p8 <- ggplot(proj_bar, aes(x = scenario, y = coverage, fill = scenario)) +
  geom_hline(aes(yintercept = target), linetype = "dashed", colour = pal$muted, linewidth = 0.45) +
  geom_col(width = 0.62, colour = "white", linewidth = 0.35) +
  geom_text(aes(y = label_y, label = label), fontface = "bold", size = 3.4, colour = pal$ink) +
  facet_wrap(~ pillar, nrow = 1) +
  scale_fill_manual(values = c("Current status"       = pal$muted,
                               "Moderate scale-up"     = pal$amber,
                               "Accelerated scale-up"  = pal$teal), guide = "none") +
  scale_x_discrete(labels = c("Current\nstatus", "Moderate\nscale-up", "Accelerated\nscale-up")) +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 25), labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Three illustrative 2030 coverage endpoints",
       subtitle = "Bars show the endpoint reached by each scenario. Dashed lines show the WHO target.",
       x = NULL, y = "Coverage",
       caption = "Illustrative scenario, not a prediction. Baseline coverage is zero for all three pillars.") +
  theme_jce() +
  axis_pub +
  theme(panel.grid.major.x = element_blank(),
        strip.text = element_text(face = "bold", colour = pal$ink),
        axis.text.x = element_text(size = 9))

save_supp_figure_result(p8, "online_resource_3_2030_coverage_endpoints", width = 7.6, height = 4.2)
p8

# ---- Rmd chunk 20 ----
# ---------------------------------------------------------------------------
# JCE SUBMISSION MANIFEST
# ---------------------------------------------------------------------------
jce_main_items <- tibble::tribble(
  ~Item, ~File, ~Purpose,
  "Table 1", "table1_main_combined_burden_and_who_gap.csv / .docx",
  "Combined burden indicators and WHO target gaps to meet the JCE 3-item limit",
  "Fig. 1", "fig1_main_three_panel_burden.png / .pdf",
  "Three-panel burden figure",
  "Fig. 2", "fig2_main_two_panel_africa_and_neighbour_maps.png / .pdf",
  "Two-panel Africa-wide and neighbour heatmap maps"
)

jce_online_items <- tibble::tribble(
  ~Item, ~File, ~Purpose,
  "Online Resource 1", "online_resource_1_hpv_16_18_pathway.png / .pdf",
  "HPV 16/18 pathway figure",
  "Online Resource 2", "online_resource_2_who_90_70_90_gap.png / .pdf",
  "WHO 90-70-90 gap figure, if not shown in main Table 1",
  "Online Resource 3", "online_resource_3_2030_coverage_endpoints.png / .pdf",
  "2030 scenario barplot",
  "Online Resource 4", "online_resource_4_burden_only.csv / .docx",
  "Cervical cancer burden table only",
  "Online Resource 5", "online_resource_5_who_target_gap.csv / .docx",
  "WHO 90-70-90 target gap table only",
  "Online Resource 6", "online_resource_6_scaleup_scenarios.csv / .docx",
  "Scenario assumptions",
  "Online Resource 7", "online_resource_7_all_indicators.csv / .docx",
  "All extracted indicators"
)

readr::write_csv(jce_main_items, file.path(manifest_dir, "manifest_main_items.csv"))
readr::write_csv(jce_online_items, file.path(manifest_dir, "manifest_online_resources_items.csv"))

save_docx_table_result(
  jce_main_items,
  title = "JCE main manuscript items",
  filename = "manifest_main_items.docx",
  note = "Use only these three items in the main manuscript to meet the 3 combined tables or figures limit.",
  font_size = 9,
  target_dir = manifest_dir
)

save_docx_table_result(
  jce_online_items,
  title = "JCE online resources",
  filename = "manifest_online_resources_items.docx",
  note = "Use these as Springer Online Resources.",
  font_size = 9,
  target_dir = manifest_dir
)

# ---- Rmd chunk 21 ----
supp_tab <- indicators %>%
  mutate(
    clean_value = dplyr::case_when(
      indicator == "HPV vaccine coverage, first dose" ~ "Not reported",
      indicator == "Screening coverage (women screened)" ~ "No national figure located",
      indicator == "Treatment coverage" ~ "No national figure located",
      is.na(value) ~ "Not reported",
      TRUE ~ as.character(value)
    ),
    clean_note = dplyr::case_when(
      indicator == "Screening coverage (women screened)" ~ "No national figure located in the public sources used",
      indicator == "Treatment coverage" ~ "No national figure located in the public sources used",
      TRUE ~ note
    )
  ) %>%
  transmute(
    Indicator = indicator,
    Value = clean_value,
    Unit = unit,
    Source = source,
    `Source year` = ifelse(is.na(source_year), "-", as.character(source_year)),
    Note = clean_note
  )

save_csv_to(supp_tab, supp_tables_csv_dir, "online_resource_7_all_indicators.csv")
save_docx_table_result(
  supp_tab,
  title = "Online Resource 7. All extracted indicators for the Republic of the Congo",
  filename = "online_resource_7_all_indicators.docx",
  note = "All values were checked against the public sources used.",
  font_size = 8,
  target_dir = supp_tables_word_dir
)

pretty_kable(
  supp_tab,
  caption = "Online Resource 7. All extracted indicators for the Republic of the Congo, with source and notes.",
  longtable = TRUE,
  font_size = 8
)

# ---- Rmd chunk 22 ----
# ---------------------------------------------------------------------------
# SAVE ONE COMBINED WORD DOCUMENT WITH ALL BEAUTIFUL TABLES
# ---------------------------------------------------------------------------
if (exists("burden_tab") && exists("gap_tab") && exists("scen_tab") && exists("supp_tab")) {
  save_all_docx_tables(
    list(
      list(
        title = "Online Resource 4. Cervical cancer burden indicators in the Republic of the Congo",
        data = burden_tab,
        note = "Sources: GLOBOCAN 2022 and ICO/IARC HPV Information Centre 2023.",
        font_size = 9
      ),
      list(
        title = "Online Resource 5. Prevention gap against the WHO 90-70-90 targets",
        data = gap_tab,
        note = "Vaccination is zero because no national programme is in place. Screening and treatment show no located national figure, so the gap is shown as an upper bound.",
        font_size = 9
      ),
      list(
        title = "Online Resource 6. Illustrative scale-up assumptions",
        data = scen_tab,
        note = "These are stated assumptions for discussion, not measured values or predictions.",
        font_size = 9
      ),
      list(
        title = "Online Resource 7. All extracted indicators for the Republic of the Congo",
        data = supp_tab,
        note = "All values were checked against the public sources used.",
        font_size = 8
      )
    ),
    filename = "online_resources_tables_compendium.docx",
    target_dir = supp_tables_word_dir
  )
}

# ---- Rmd chunk 23 ----
# ---------------------------------------------------------------------------
# SAVE A SINGLE PDF COMPENDIUM FOR SPRINGER ONLINE RESOURCES
# ---------------------------------------------------------------------------
if (exists("p4") && exists("p5") && exists("p8") &&
    exists("burden_tab") && exists("gap_tab") && exists("scen_tab") && exists("supp_tab")) {
  save_supplementary_pdf()
}
