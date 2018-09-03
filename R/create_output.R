#' Create Output
#'
#' @param target_directories A list of PDF file locations to be scraped and converted.
#'
#' @return A data frame representing structured data for the list of input files.
#' @export
#'
#' @examples
#' \dontrun{
#' course_ratings <- create_output('~/courses/')
#' }
create_output <- function(target_directories) {
  list_of_input_files <- get_file_paths(target_directories)

  result <- dplyr::bind_rows(lapply(list_of_input_files, create_row)) %>%
    purrr::set_names(c("Course Number", "Faculty Name(s)", "Semester",
                       "Respondents", "Q01", "Q02*", "Q03", "Q04", "Q05",
                       "Q06", "Q07", "Q08", "Q09", "Q10", "Q11", "Q12*",
                       "Q13*", "Q14*", "Q15*", "Q16*", "Q17*", "Q18*",
                       "Q19", "Evaluation Format (New = 2016-2017 onward)"))
  result
}
