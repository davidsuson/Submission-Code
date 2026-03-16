read_pc_reports <- function(reports, ...){
  UseMethod("read_pc_reports")
}

#' @export
read_pc_reports.data.frame <- function(reports){

  report_details <- apply(reports, 1, function(report){


      report_name <- report["Name"]
      report_link <- report["Link"]

      valid_links <- valid_links(report_link)

      if (length(valid_links) == 2){

        final_report_details <- tryCatch(
          error = function(cnd){
            print(cnd)
            cli::cli_alert_danger("{report_name} at {report_link} failed to process.")

            final_report_details <- data.frame(
              Report_Name = report_name,
              Report_Link = report_link,
              Unknown_Error = TRUE
            )

          },
          {

            report_released_date <- read_pc_report_details(valid_links$report_page)
            submissions <- read_pc_report_submissions(valid_links$submission_page)

            final_report_details <- submissions %>%
              dplyr::mutate(
                Report_Name = report_name,
                Report_Link = report_link,
                Report_Released = report_released_date,
                Valid_Links = TRUE,
                Unknown_Error = FALSE
              )

            cli::cli_alert_success("{report_name} at {report_link} processed succesfully.")

            final_report_details

          }
        )


      } else {

        final_report_details <- data.frame(
          Report_Name = report_name,
          Report_Link = report_link,
          Valid_Links = FALSE,
          Unknown_Error = FALSE
        )

        cli::cli_alert_danger("{report_name} at {report_link} does not have a submission and/or report page equivalent.")

      }

      final_report_details

  })

  all_report_details <- report_details %>%
    dplyr::bind_rows()

  all_report_details
}
