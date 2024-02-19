main <- function(){
  suicide_data <- basics$read_interim("num_suicide", extension = "tidy")
  confirmed_case_data <- basics$read_interim("confirmed_case", extension = "tidy")
  unemployment_data <- basics$read_interim("unemployment", extension = "tidy")
  
  
  var_list <- c("man_U19", "man_20s", "man_30s", "man_40s", "man_50s",
               "man_60s", "man_70s", "man_80s", "wom_U19", "wom_20s",
               "wom_30s", "wom_40s", "wom_50s", "wom_60s", "wom_70s",
               "wom_80s", "man", "wom", "confirmed_cases")
  
  var_name <- c("dman_U19", "dman_20s", "dman_30s", "dman_40s", "dman_50s",
                "dman_60s", "dman_70s", "dman_80s", "dwom_U19", "dwom_20s",
                "dwom_30s", "dwom_40s", "dwom_50s", "dwom_60s", "dwom_70s",
                "dwom_80s", "dman", "dwom", "dconfirmed_cases")
  
  
  
  master_data <- merge_data(suicide_data,
                            confirmed_case_data,
                            unemployment_data,
                            ncol_suicide_data = length(suicide_data),
                            ncol_confirmed_cases_data = length(confirmed_case_data),
                            ncol_unemployment_data = length(unemployment_data)) %>%
    generate_coverage_dummy(var_list = var_list,
                            var_name = var_name) %>%
    generate_spring_dummy() %>%
    generate_trend()
  
  
  basics$save_interim(master_data, "master")
}


merge_data <- function(suicide_data,
                       confirmed_case_data,
                       unemployment_data,
                       ncol_suicide_data,
                       ncol_confirmed_cases_data,
                       ncol_unemployment_data) {
  
  initial_col_unem <- ncol_suicide_data+2
  end_col_unem <- ncol_suicide_data+ncol_unemployment_data
  inital_col_confirmed <- ncol_suicide_data+ncol_unemployment_data+ncol_confirmed_cases_data
  
  output <- suicide_data %>%
    dplyr::bind_cols(unemployment_data) %>%
    dplyr::bind_cols(confirmed_case_data) %>%
    dplyr::select(c(1:ncol_suicide_data,
                    initial_col_unem:end_col_unem,
                    inital_col_confirmed)) 
    
  return(output)
}


generate_coverage_dummy <- function(input, var_list, var_name) {
  
  print(colnames(input))
  i = 1
  for (pick_var in var_list) {
    var_picked <- input[pick_var]
    calculate_differenced_data <- input %>% 
      dplyr::transmute(input[pick_var] - dplyr::lag(input[pick_var]))
    input <- input %>% 
      dplyr::bind_cols(calculate_differenced_data)
    i <- i + 1
  }
  
  differenced_data <- input %>%
    dplyr::select((length(input)-(length(var_list)-1)):length(input))
  colnames(differenced_data) <- var_name
  input <- input %>%
    dplyr::select(1:(length(input)-length(differenced_data)))%>%
    dplyr::bind_cols(differenced_data) %>%
    dplyr::relocate((length(input)-(length(var_list)-1)):length(input), .before = confirmed_cases)
  
  output <- input %>%
    dplyr::rename(year_month = 1)
  output <- output %>%
    dplyr::mutate(suicide_report1 = dplyr::if_else(
      year_month == "2020-05-01", 1, 0)) %>%
    dplyr::mutate(suicide_report2 = dplyr::if_else(
      year_month == "2020-07-01", 1, 0)) %>%
    dplyr::mutate(suicide_report3 = dplyr::if_else(
      year_month == "2021-12-01", 1, 0)) %>%
    dplyr::mutate(suicide_report4 = dplyr::if_else(
      year_month == "2022-05-01", 1, 0))
    
  return(output)
}


generate_spring_dummy <- function(data_input){
  
  data_output <- data_input %>%
    dplyr::mutate(March = dplyr::if_else(
      lubridate::month(data_input$year_month) == 3, 1, 0)) %>%
    dplyr::mutate(April = dplyr::if_else(
      lubridate::month(data_input$year_month) == 4, 1, 0))

  return(data_output)
}


generate_trend <- function(data_input){
  
  data_output <- data_input %>%
    dplyr::mutate(trend = seq(1, nrow(data_input)))
  
  return(data_output)
}

box::use(`functions`/basics)
main()