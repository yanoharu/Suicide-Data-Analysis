main <- function(){
  my_folder <- "confirmed_case"
  file_name <- "newly_confirmed_cases_daily.csv"
  date_range <- c("2013-01-01", "2022-12-31")
  
  raw_data <- read_raw(my_folder, file_name)
  tidy_data <- raw_data %>%
    aggregate_data_by_month(date_range = date_range) %>%
    log_transformation
  
  basics$save_interim(tidy_data, my_folder, extension = "tidy")
}

read_raw <- function(my_folder, file_name){
  file_path <- here::here("02_raw", my_folder, "data", file_name)
  output <- read.csv(file_path)

  
  return(output)
}


aggregate_data_by_month <- function(input, date_range) {
  
  monthly_data <- aggregate(ALL ~ format(as.Date(Date),
                                   format = "%Y-%m-%d"),
                      data = input, sum)
  colnames(monthly_data) <- c("year_month", "confirmed_cases")
  
  year_month <- seq(as.Date(date_range[1]),
                    as.Date(date_range[2]), by="month") 
  year_month <- format(year_month, format = "%Y-%m-%d") %>%
    as.data.frame()%>%
    dplyr::rename(year_month = 1)
  
  output <- as.data.frame(year_month) %>%
    dplyr::left_join(monthly_data, by = "year_month") %>%
    replace(is.na(.), 0)
  
  
  return(output)
}


log_transformation <- function(data_input){
  data_input[ ,colnames(data_input)[2:length(data_input)]] <- data_input[ ,colnames(data_input)[2:length(data_input)]] + 1
  data_output <- data_input %>% 
    dplyr::mutate_at(dplyr::vars(colnames(data_input)[2:length(data_input)]), log)
  
  return(data_output)
}



box::use(`functions`/basics)
main()