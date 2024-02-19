main <- function(){
  my_folder <- "unemployment"
  file_name <- "lt01-a10.xlsx"
  
  raw_data <- read_raw(my_folder, file_name)
  
  tidy_data <- raw_data %>%
    clean_data() %>%
    generate_date() %>%
    log_transformation()
  
  basics$save_interim(tidy_data, my_folder, extension = "tidy")
}


read_raw <- function(my_folder, file_name) {
  file_path <- here::here("02_raw", my_folder, "data", file_name)
  output <- readxl::read_excel(file_path, skip = 729)
  
  return(output)
}


clean_data <- function(input){
  
  output <- input %>%
    dplyr::select(c(21,22)) %>%
    dplyr::slice(1:(nrow(input)-4))
  colnames(output) <- c("man", "wom")
  
  return(output)
}


generate_date <- function(input) {
  
  year_month <- seq(as.Date("2013-01-01"),
                    as.Date("2022-12-01"),
                    by = "month")
  year_month <- year_month %>%
    as.data.frame()
  
  output <- input %>%
    dplyr::mutate(year_month = year_month,
                  .before = "man")
  
  return(output)
}


log_transformation <- function(data_input){
  data_output <- data_input %>%
    dplyr::mutate_at(dplyr::vars(colnames(data_input)[2:length(data_input)]), log)
  
  return(data_output)
}


box::use(`functions`/basics)
main()