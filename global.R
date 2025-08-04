##packages ==========
library(shiny)
library(readxl)
library(dplyr)
library(lubridate)
library(forecast)
library(tibble)
library(Metrics)
library(ggplot2)
library(shinycssloaders)
library(shinyjs)
library(janitor)
library(tidyr)
library(vars)
library(shinyWidgets)

##preprocess data =========

filepath <- "XXXX - Inflation\\Data Inputs.xlsx"

#define cpi per country
cpi_column_map <- list(
  SG = "SG CPI",
  IDN = "cpi",
  VN  = "CPI",
  JN  = "Japan CPI Headline",
  TH = "headline sa"
)

#define unemployment per country
unemployment_proxy_map <- list(
  SG  = "SG Unemployment",
  IDN = "pmi",
  VN = "pmi",
  JN  = "Unemployment",
  TH = "unemployment"
)

#shared columns that are globally relevant
shared_column_map <- list(
  oil_px = "WTI Oil Mthly Average",  
  stage2sea = "Stage2 SEA",
  stage2 = "Stage2"
)

load_shared_data <- function(filepath) {
  raw <- read_excel(filepath, sheet = "SG", .name_repair = "minimal") %>%
    janitor::clean_names()
  
  df <- raw %>%
    mutate(date = floor_date(as.Date(raw[[2]], origin = "1899-12-30"), unit = "month")) %>%
    rename(
      oil_px = janitor::make_clean_names(shared_column_map$oil_px),
      stage2 = janitor::make_clean_names(shared_column_map$stage2),
      stage2sea = janitor::make_clean_names(shared_column_map$stage2sea)
    ) %>%
    dplyr::select(date, oil_px, stage2, stage2sea) %>%
    mutate(across(c(oil_px, stage2, stage2sea), ~as.numeric(.))) %>%
    drop_na()
  
  return(df)
}

shared_data <- load_shared_data(filepath)

#load country CPI
load_country_data <- function(filepath, sheet, cpi_column_map, unemployment_proxy_map) {
  raw <- read_excel(filepath, sheet = sheet, .name_repair = "minimal") %>%
    janitor::clean_names()
  
  cpi_col <- janitor::make_clean_names(cpi_column_map[[sheet]])
  unemp_col <- janitor::make_clean_names(unemployment_proxy_map[[sheet]])
  
  
  #adjust date column based on country
  raw_date_col <- if (sheet == "JN") raw[[1]] else raw[[2]]
  parsed_date <- floor_date(as.Date(raw_date_col, origin = "1899-12-30"), unit = "month")
  
  df <- raw %>%
    mutate(date = parsed_date) %>%
    rename(
      cpi = !!cpi_col,
      unemp = !!unemp_col) %>%
    dplyr::select(date, cpi, unemp) %>%
    mutate(across(c(cpi, unemp), as.numeric)) %>%
    drop_na()
  
  return(df)
}

#run preprocessing
sheets_to_read <- c("SG", "IDN", "VN", "JN", "TH")
country_data <- lapply(sheets_to_read, function(sheet) {
  load_country_data(filepath, sheet, cpi_column_map, unemployment_proxy_map)
})
names(country_data) <- sheets_to_read

#align and join
shared_data_aligned <- shared_data

merged_country_data <- lapply(sheets_to_read, function(sheet) {
  df_data <- country_data[[sheet]]
  
  df_merged <- left_join(df_data, shared_data_aligned, by = "date")
  return(df_merged)
})
names(merged_country_data) <- sheets_to_read
all_country_data <- merged_country_data
