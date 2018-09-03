#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' Import Lines
#'
#' @param input_file A PDF file location to be scraped and converted.
#'
#' @return List object with each element representing a line of the source PDF.
#'
#' @examples
#' \dontrun{
#' lines <- import_lines('~/courses/API-101B.pdf')
#' }
import_lines <- function(input_file) {
  read <- tm::readPDF(control = list(text = "-layout"))
  document <- tm::Corpus(URISource(input_file), readerControl = list(reader = read))
  doc <- NLP::content(document[[1]])
  lines <- doc %>%
    stringr::str_split('\n') %>%
    .[[1]]
  lines
}

#' Get Line Number
#'
#' @param lines Input file represented as list object (output of `import_lines`).
#' @param question_number The question number (1-19) to be identified.
#'
#' @return A number representing the list element in which the question is found.
#'
#' @examples
#' \dontrun{
#' line_number <- get_line_number(lines, 01)
#' }
get_line_number <- function(lines = lines, question_number) {
  question_number <- as.character(question_number)
  pattern <- paste0('^\\s+', question_number, '\\.')
  result <- grep(pattern, lines)
  result
}

#' Get Course Number
#'
#' @param lines Input file represented as list object (output of `import_lines`).
#'
#' @return A seven- or eight-character course identifier.
#'
#' @examples
#' \dontrun{
#' course_number <- get_course_number(lines)
#' }
get_course_number <- function(lines) {
  result <- lines[1] %>%
    stringr::str_extract('^[A-z]+-[0-9]+[A-z]{0,1}')
  result
}

#' Get Semester
#'
#' @param lines Input file represented as list object (output of `import_lines`).
#'
#' @return The semester and year in which the course was taught.
#'
#' @examples
#' \dontrun{
#' semester <- get_semester(lines)
#' }
get_semester <- function(lines) {
  result <- lines[1] %>%
    stringr::str_extract('(Fall|Spring|January) [0-9]{4}$') %>%
    stringr::str_replace('January', 'J-Term')
  result
}

#' Get Year
#'
#' @param lines Input file represented as list object (output of `import_lines`).
#'
#' @return The year in which the course was taught.
#'
#' @examples
#' \dontrun{
#' year <- get_year(lines)
#' }
get_year <- function(lines) {
  result <- lines[1] %>%
    stringr::str_extract('[0-9]{4}$') %>%
    as.integer()
  result
}

#' Get Faculty Name
#'
#' @param lines Input file represented as list object (output of `import_lines`).
#'
#' @return The name of the faculty member(s) teaching the course. If multiple
#'     faculty members teach the course, their names will be separated with a
#'     backslash.
#'
#' @examples
#' \dontrun{
#' faculty_name <- get_faculty_name(lines)
#' }
get_faculty_name <- function(lines) {
  row_number <- grep('Faculty Overall', lines)
  result <- lines[row_number] %>%
    stringr::str_match('(?<=Faculty Overall\\s{20}).*') %>%
    .[[1]] %>%
    stringr::str_trim() %>%
    stringr::str_replace_all('\\s{2,}', ' / ')
  result
}

#' Get Question
#'
#' @param lines Input file represented as list object (output of `import_lines`).
#' @param question_number The question number (1-19) for which a mean response is desired.
#'
#' @return The mean response from all respondents for the specified question.
#'
#' @examples
#' \dontrun{
#' question_01 <- get_question(lines, 01)
#' }
get_question <- function(lines = lines, question_number) {
  line_number <- get_line_number(lines, question_number)
  result <- lines[line_number] %>%
    stringr::str_extract('\\d\\.\\d{2}') %>%
    as.numeric()
  result
}

#' Get Respondents
#'
#' @param lines Input file represented as list object (output of `import_lines`).
#'
#' @return The number of respondents (HKS plus non-HKS) who submitted a course evaluation.
#'
#' @examples
#' \dontrun{
#' respondents <- get_respondents(lines)
#' }
get_respondents <- function(lines) {
  hks_line <- grep('\\s{2}[A-Z]{3}\\sSTUDENTS', lines)
  non_hks_line <- grep('\\sNON-[A-Z]{3}\\sSTUDENTS', lines)
  hks_line <- ifelse(grepl('HKS', lines[hks_line]), hks_line - 1, hks_line)
  non_hks_line <- ifelse(grepl('HKS', lines[non_hks_line]), non_hks_line - 1, non_hks_line)
  hks_students <- lines[hks_line] %>%
    stringr::str_match('\\(Total (\\d+)\\)$') %>%
    .[[2]]
  non_hks_students <- lines[non_hks_line] %>%
    stringr::str_match('\\(Total (\\d+)\\)$') %>%
    .[[2]]
  result <- as.integer(hks_students) + as.integer(non_hks_students)
  result
}

#' Get Evaluation Format
#'
#' @param lines Input file represented as list object (output of `import_lines`).
#'
#' @return Designation as "Old" or "New" depending on semester and year
#'
#' @examples
#' \dontrun{
#' eval_format <- get_eval_format(lines)
#' }
get_eval_format <- function(lines) {
  semester <- get_semester(lines)
  year <- get_year(lines)
  result <- ifelse(year < 2017 & semester != 'Fall 2016', 'Old', 'New')
  result
}

#' Create Row
#'
#' @param input_file A PDF file location to be scraped and converted.
#'
#' @return A data frame representing structured data for the input file.
#'
#' @examples
#' \dontrun{
#' row <- create_row('~/courses/API-101B.pdf')
#' }
create_row <- function(input_file) {
  lines <- import_lines(input_file)
  eval_format <- get_eval_format(lines)

  if (eval_format == 'New') {
    result <- data.frame(
      course_number = get_course_number(lines),
      faculty_name  = get_faculty_name(lines),
      semester      = get_semester(lines),
      respondents   = get_respondents(lines),
      question_01   = get_question(lines, 01),
      question_02   = get_question(lines, 02),
      question_03   = get_question(lines, 03),
      question_04   = get_question(lines, 04),
      question_05   = get_question(lines, 05),
      question_06   = get_question(lines, 06),
      question_07   = get_question(lines, 07),
      question_08   = get_question(lines, 08),
      question_09   = get_question(lines, 09),
      question_10   = get_question(lines, 10),
      question_11   = get_question(lines, 11),
      question_12   = get_question(lines, 12),
      question_13   = get_question(lines, 13),
      question_14   = get_question(lines, 14),
      question_15   = get_question(lines, 15),
      question_16   = get_question(lines, 16),
      question_17   = get_question(lines, 17),
      question_18   = get_question(lines, 18),
      question_19   = get_question(lines, 19),
      eval_format   = eval_format
    )
  } else if (eval_format == 'Old') {
    result <- data.frame(
      course_number = get_course_number(lines),
      faculty_name  = get_faculty_name(lines),
      semester      = get_semester(lines),
      respondents   = get_respondents(lines),
      question_01   = get_question(lines, 01),
      question_02   = get_question(lines, 02),
      question_03   = get_question(lines, 03),
      question_04   = get_question(lines, 04),
      question_05   = get_question(lines, 05),
      question_06   = get_question(lines, 06),
      question_07   = get_question(lines, 07),
      question_08   = get_question(lines, 08),
      question_09   = NA,
      question_10   = get_question(lines, 09),
      question_11   = get_question(lines, 10),
      question_12   = get_question(lines, 11),
      question_13   = get_question(lines, 12),
      question_14   = get_question(lines, 13),
      question_15   = get_question(lines, 14),
      question_16   = get_question(lines, 15),
      question_17   = get_question(lines, 16),
      question_18   = get_question(lines, 17),
      question_19   = get_question(lines, 18),
      eval_format   = eval_format
    )
  }

  result <- result %>%
    dplyr::mutate_at(dplyr::vars(c(course_number, faculty_name, semester, eval_format)),
                     dplyr::funs(as.character))
  result
}
