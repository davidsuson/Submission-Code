read_pc_report_submissions <- function(submission_page_link){

  # SHOULD INTERNALLY HANDLE AND RETURN FAILURE

  submission_page <- rvest::read_html(submission_page_link)

  table_sections <- find_table(submission_page)

  read_tables <- lapply(table_sections, function(table_section) read_pc_report_submission_table(table_section))

  required_table <- read_tables %>%
    dplyr::bind_rows()

}


# HELPER -------------------------

read_pc_report_submission_table <- function(table_section){

  table_name <- table_section %>%
    html_elements("h3") %>%
    html_text2() %>%
    gsub("\\(\\d+\\)", "", .) %>%
    trimws()

  table <- table_section %>%
    html_element("table")

  if (!grepl("submissions", tolower(table_name))){
    return(data.frame())
  }

  rows_with_no_links <- table %>%
    rvest::html_elements(xpath = ".//tr[not(.//a) and not(.//th)]")

  xml2::xml_remove(rows_with_no_links)

  rename_vec <- c(
    Submission_Received = "Received",
    Submission_Name = "Name",
    Submission_Pages = "Pages"
  )

  required_table <- table %>%
    rvest::html_table() %>%
    dplyr::mutate(
      Pages = ifelse(!"Pages" %in% names(.), as.integer(NA), Pages),
      Submission_Period = table_name,
      Received = as.Date(Received, tryFormats = c("%d/%m/%Y")),
      Pages = as.integer(Pages)
    ) %>%
    dplyr::select(-`No.`) %>%
    dplyr::rename(any_of(rename_vec))

  links <- table %>% html_elements("tr a") %>% html_attr("href")

  if (length(links) >= nrow(required_table)){
    links <- table %>%
      html_elements("tr") %>%
      purrr::map(~ .x %>% html_element("a") %>% html_attr("href")) %>%
      unlist() %>%
      na.omit()

  }

  required_table <- required_table %>%
    dplyr::mutate(
      Submission_Link = links
    ) %>%
    dplyr::filter(stringr::str_detect(Submission_Link, "pdf"))

  browser()

  required_table

}
