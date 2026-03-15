read_pc_search_page <- function(page){

  Sys.sleep(0.5)

  links <- page %>%
    rvest::html_elements("a") %>%
    rvest::html_attr("href") %>%
    .[stringr::str_detect(., "/inquiries-and-research/") & !stringr::str_detect(., "/inquiries-and-research/$")]

  names <- page %>%
    rvest::html_elements("a h3") %>%
    rvest::html_text2()

  reports <- data.frame(
    Name = names,
    Link = links
  )

  reports

}
