# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

# Import PDF with each line as element in a list object
import_lines <- function(input_file) {
  read <- readPDF(control = list(text = "-layout"))
  document <- Corpus(URISource(input_file), readerControl = list(reader = read))
  doc <- content(document[[1]])
  lines <- doc %>% stringr::str_split('\n') %>% .[[1]]
  lines
}

# Question Numbers
get_line_number <- function(lines = lines, question_number) {
  question_number <- as.character(question_number)
  pattern <- paste0('^\\s+', question_number, '\\.')
  result <- grep(pattern, lines)
  result
}

# Course number
get_course_number <- function(lines) {
  result <- stringr::str_extract(lines[1], '^[A-z]+-[0-9]+[A-z]{0,1}')
  result
}

# Semester
get_semester <- function(lines) {
  result <- stringr::str_extract(lines[1], '(Fall|Spring|January) [0-9]{4}$') %>%
    stringr::str_replace('January', 'J-Term')
  result
}

# Year
get_year <- function(lines) {
  result <- stringr::str_extract(lines[1], '[0-9]{4}$')
  result
}

# Faculty name
get_faculty_name <- function(lines) {
  row_number <- grep('Faculty Overall', lines)
  result <- lines[row_number] %>%
    stringr::str_match('(?<=Faculty Overall\\s{20}).*') %>%
    .[[1]] %>%
    stringr::str_trim() %>%
    stringr::str_replace_all('\\s{2,}', ' / ')
  result
}

# Questions
get_question <- function(lines = lines, question_number) {
  line_number <- get_line_number(lines, question_number)
  result <- lines[line_number] %>%
    stringr::str_extract('\\d\\.\\d{2}') %>%
    as.numeric()
  result
}

# Respondents
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

get_eval_format <- function(lines) {
  semester <- get_semester(lines)
  year <- get_year(lines)
  result <- ifelse(year < 2017 & semester != 'Fall 2016', 'Old', 'New')
  result
}

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
    dplyr::mutate_at(vars(c(course_number, faculty_name, semester, eval_format)),
                     funs(as.character))
  result
}

create_output <- function(list_of_input_files) {
  result <- dplyr::bind_rows(lapply(list_of_input_files, create_row)) %>%
    purrr::set_names(c("Course Number", "Faculty Name(s)", "Semester",
                       "Respondents", "Q01", "Q02*", "Q03", "Q04", "Q05",
                       "Q06", "Q07", "Q08", "Q09", "Q10", "Q11", "Q12*",
                       "Q13*", "Q14*", "Q15*", "Q16*", "Q17*", "Q18*",
                       "Q19", "Evaluation Format (New = 2016-2017 onward)"))
  result
}
