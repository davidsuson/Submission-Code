read_pc_reports <- function(reports, ...){
  UseMethod("read_pc_reports")
}

#' @export
read_pc_reports.data.frame <- function(reports){

  report_details <- apply(reports, 1, function(report){

    tryCatch({

      report_name <- report["Name"]
      report_link <- report["Link"]

      report_page <- paste0(report_link, "/report/")
      submission_page <- paste0(report_link, "/submissions/")

      report_released_date <- read_pc_report_details(report_page)
      submissions <- read_pc_report_submissions(submission_page)

      final_report_details <- submissions %>%
        dplyr::mutate(
          Report_Name = report_name,
          Report_Link = report_link,
          Report_Released = report_released_date
        )


    }, error = function(e) {

      print(report)
      return(data.frame())

    })



  })

  all_report_details <- report_details %>%
    dplyr::bind_rows()

}
