find_table <- function(submission_page){
  UseMethod("find_table")
}

#' @export
find_table.xml_document <- function(submission_page){

  table_sections <- submission_page %>%
    html_elements(xpath = "//div[@data-state and h3]")



  table_sections
}
