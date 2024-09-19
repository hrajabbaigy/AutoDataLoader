# R/load_functions.R

# Required Libraries
library(readr)
library(readxl)
library(httr)
library(jsonlite)
library(pdftools)
library(xml2)
library(roxygen2)

#' Load CSV File
#'
#' This function loads a CSV file from the specified file path and returns the data as a data frame.
#'
#' @param file_path The path to the CSV file.
#' @return A data frame containing the contents of the CSV file.
#' @examples
#' \dontrun{
#' load_csv("data/sample.csv")
#' }
#' @export
load_csv <- function(file_path) {
  tryCatch({
    data <- read_csv(file_path)
    return(data)
  }, error = function(e) {
    message("Error loading CSV: ", e)
    return(NULL)
  })
}

#' Load Excel File
#'
#' This function loads an Excel file from the specified file path and returns the data as a data frame.
#' It allows specifying a particular sheet and handles errors by returning \code{NULL}.
#'
#' @param file_path A string specifying the path to the Excel file.
#' @param sheet An optional integer specifying the sheet number to load. Default is 1.
#' @return A data frame containing the contents of the specified Excel sheet, or \code{NULL} in case of an error.
#' @examples
#' \dontrun{
#' load_excel("data/sample.xlsx", sheet = 1)
#' }
#' @export
load_excel <- function(file_path, sheet = 1) {
  tryCatch({
    data <- read_excel(file_path, sheet = sheet)
    return(data)
  }, error = function(e) {
    message("Error loading Excel: ", e)
    return(NULL)
  })
}

#' Load Data from API (JSON response)
#'
#' This function sends a GET request to a specified API endpoint and returns the response as a data frame.
#' It expects the response to be in JSON format.
#'
#' @param url A string specifying the URL of the API.
#' @return A data frame containing the parsed JSON response, or \code{NULL} in case of an error.
#' @examples
#' \dontrun{
#' load_api("https://api.example.com/data")
#' }
#' @export
load_api <- function(url) {
  tryCatch({
    response <- GET(url)
    content <- content(response, as = "text")
    data <- fromJSON(content)
    return(data)
  }, error = function(e) {
    message("Error loading API: ", e)
    return(NULL)
  })
}

#' Load Data from a PDF File
#'
#' This function extracts the text from a PDF file and returns it as a character vector.
#' Each element of the vector corresponds to a page in the PDF.
#'
#' @param file_path A string specifying the path to the PDF file.
#' @return A character vector containing the text from the PDF file, or \code{NULL} in case of an error.
#' @examples
#' \dontrun{
#' pdf_text <- load_pdf("data/sample.pdf")
#' }
#' @export
load_pdf <- function(file_path) {
  tryCatch({
    text <- pdf_text(file_path)
    return(text)
  }, error = function(e) {
    message("Error loading PDF: ", e)
    return(NULL)
  })
}

#' Load HTML Table from a Website
#'
#' This function loads an HTML page from the specified URL and extracts a table from it.
#' The function allows you to specify which table to return if the page contains multiple tables.
#'
#' @param url A string specifying the URL of the website.
#' @param table_num An optional integer specifying which table to extract. Default is 1.
#' @return A data frame containing the contents of the specified HTML table, or \code{NULL} in case of an error.
#' @examples
#' \dontrun{
#' load_html_table("https://example.com/page", table_num = 1)
#' }
#' @export
load_html_table <- function(url, table_num = 1) {
  tryCatch({
    page <- read_html(url)
    tables <- html_table(page, fill = TRUE)
    return(tables[[table_num]])
  }, error = function(e) {
    message("Error loading HTML table: ", e)
    return(NULL)
  })
}

#' Automate Data Loading from Various Sources
#'
#' @param source A string specifying the path or URL of the data source.
#' @param source_type A string specifying the type of the data source.
#' @param ... Additional arguments to pass to the underlying loader functions.
#' @return The loaded data or \code{NULL} in case of an error.
#' @export
load_data <- function(source, source_type, ...) {
  message("Loading data from source: ", source)
  message("Source type: ", source_type)

  result <- switch(source_type,
                   "csv" = load_csv(source),
                   "excel" = load_excel(source, ...),
                   "api" = load_api(source),
                   "pdf" = load_pdf(source),
                   "html" = load_html_table(source, ...),
                   {
                     message("Unsupported source type: ", source_type)
                     stop("Unsupported source type.")
                   }
  )

  message("Data loaded successfully.")
  return(result)
}


