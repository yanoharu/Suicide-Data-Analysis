main <- function(){
  my_folder <- "num_suicide"
  
  col_names <- c("man_U19", "man_20s", "man_30s", "man_40s", "man_50s",
                 "man_60s", "man_70s", "man_80s",
                 "wom_U19", "wom_20s", "wom_30s", "wom_40s", "wom_50s",
                 "wom_60s", "wom_70s", "wom_80s")
  
  year_list_2015_2017 <- c(2013,2014,2015,2016,2017)
  month_list_2015_2017 <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  year_list_2022 <- c("令和４")
  year_list_2018_2021 <- c("平成30","平成31","令和元","令和２","令和３")
  month_list_2018_2022 <- c("１","２","３","４","５","６",
                            "７","８","９","10","11","12")
  
  
  tidy_data <- clean_data_2015_2017(year = year_list_2015_2017, 
                                   month = month_list_2015_2017,
                                   col_names) %>%
    clean_data_2018_2021(year = year_list_2018_2021,
                         month = month_list_2018_2022,
                         col_names) %>%
    clean_data_2022(year = year_list_2022,
                    month_list = month_list_2018_2022,
                    col_names) %>%
    log_transformation()
  
  basics$save_interim(tidy_data, my_folder, extension = "tidy")
}


clean_data_2015_2017 <- function(year_list, month_list, col_names){
  
  pick_col_index <- seq(9,38,4)
  pick_row_index <- c(3,4)
  ymd <- seq(as.Date("2013-01-01"), length.out=60, by="month")
  output <- data.frame(matrix(ncol = 16, nrow = 0))
  colnames(output) <- col_names
  
  for (year in year_list) {
    year <- as.character(year)
    for (month in month_list){
      month <- as.character(month)
      file_name <- paste0("^",year,"-",month,"-")
      table <- readxl::read_excel(dir("02_raw/num_suicide/data", full.name = T, 
                              pattern=file_name), skip = 2)
      table <- table %>% 
        dplyr::select(c(pick_col_index)) %>%
        dplyr::slice(pick_row_index)
      table <- table %>%
        dplyr::bind_cols(table[2,]) %>%
        dplyr::slice(1)%>%
        dplyr::mutate_all(as.numeric)
      colnames(table) <- col_names
      output <- output %>%
        dplyr::bind_rows(table)
      print("Finished proccess for 2015 to 2017")
    }
  }
  
  output <- output %>%
    dplyr::mutate(year_month = format(
      ymd, format = "%Y-%m-%d"), .before =man_U19)
  
  
  return(output)
}


clean_data_2018_2021 <- function(pre_output, year_list, month_list, col_names){
  
  pick_col_index <- c(5,6)
  pick_row_index <- seq(1,16)
  ymd <- seq(as.Date("2018-01-01"), length = 48,
             by = "month")
  output <- data.frame(matrix(ncol = 16, nrow = 0))
  colnames(output) <- col_names
  for (year in year_list) {
    for (month in month_list) {
      if ((year == "平成31" & 
           any(stringr::str_detect(month, c("５","６","７","８","９","10","11","12")))) |
          (year == "令和元" &
           any(stringr::str_detect(month, c("１","２","３","４"))))){
        #implement nothing because file names corresponding to month of year
        #in conditions don't exist
      }else {
        file_name <- paste0("^",year,"年",month,"月")
        table <- readxl::read_excel(dir("02_raw/num_suicide/data", full.name = T, 
                                pattern=file_name), skip = 10)
        table <- table %>%
          dplyr::select(pick_col_index)
        colnames(table) <- c("sex", "num")
        table <- table %>%
          dplyr::filter(!is.na(sex))%>%
          dplyr::slice(pick_row_index)%>%
          dplyr::arrange(desc(sex))%>%
          dplyr::select(2) %>%
          dplyr::mutate(sex_age = col_names,
                        .before = num) %>%
          tibble::column_to_rownames(var = "sex_age") %>%
          t() %>%
          as.data.frame()
        output <- output %>%
          dplyr::bind_rows(table)
        print("Finished proccess for 2018 to 2021")
      }
    }
  }
  
  output <- output %>%
    dplyr::mutate(year_month = format(
      ymd, format = "%Y-%m-%d"), .before =man_U19)
  
  output <- pre_output %>%
    dplyr::bind_rows(output)
  
  
  return(output)
}


clean_data_2022 <- function(pre_output, year_list, month_list, col_names) {
  
  pick_col_index <- c(1,3,4)
  pick_row_index <- seq(1,16)
  ymd <- seq(as.Date("2022-01-01"), length = 12,
             by = "month")
  output <- data.frame(matrix(ncol = 16, nrow = 0))
  colnames(output) <- col_names
  for (year in year_list) {
    for (month in month_list) {
      file_name <- paste0("^",year,"年",month,"月")
      table <- readxl::read_excel(dir("02_raw/num_suicide/data", full.name = T, 
                              pattern=file_name), skip = 10)
      table <- table %>%
        dplyr::select(pick_col_index)
      colnames(table) <- c("age", "sex", "num")
      table <- table %>%
        dplyr::filter(is.na(age))%>%
        dplyr::slice(pick_row_index) %>%
        dplyr::arrange(desc(sex)) %>%
        dplyr::select(3) %>%
        dplyr::mutate(sex_age = col_names,
                      .before = num) %>%
        
        tibble::column_to_rownames(var = "sex_age") %>%
        t() %>%
        as.data.frame()
      output <- output %>%
        dplyr::bind_rows(table)
      print("Finished proccess for 2022")
    }
  }
  output <- output %>%
    dplyr::mutate(year_month = format(
      ymd, format = "%Y-%m-%d"), .before =man_U19)
  
  output <- pre_output %>%
    dplyr::bind_rows(output)
  
  
  return(output)
}

log_transformation <- function(data_input){
  data_output <- data_input %>%
    dplyr::mutate_at(dplyr::vars(colnames(data_input)[2:length(data_input)]), log)
  print("log_transformation finished")
  return(data_output)
}


box::use(`functions`/basics)
main()