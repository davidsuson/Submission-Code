read_pc_search_pages <- function(search_page_link, max_page){

  page <- rvest::read_html_live(search_page_link)

  Sys.sleep(0.5)

  all_reports_list <- vector("list", max_page)

  current_page = 1

  while(current_page <= max_page){

    page_reports <- read_pc_search_page(page)

    all_reports_list[[current_page]] <- page_reports

    cli::cli_alert("Read {current_page} out of {max_page} successfully read!")

    # This clicks the next page button
    page$click("div.flex.flex-row.gap-5.justify-center.my-5 button:nth-last-child(2)")
    current_page = current_page + 1

    Sys.sleep(0.5)

  }

  all_reports <- all_reports_list %>%
    dplyr::bind_rows()

  all_reports

}
