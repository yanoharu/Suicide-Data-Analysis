main <- function(){
  data_master <- basics$read_interim("master")
  
  vars_manU19 <- c("man_U19", "man", "confirmed_cases",
                   "suicide_report1", "suicide_report2",
                   "suicide_report3", "suicide_report4",
                   "March", "April", "trend")
  vars_man20 <- c("man_20s", "man", "confirmed_cases",
                   "suicide_report1", "suicide_report2",
                   "suicide_report3", "suicide_report4",
                  "March", "April", "trend")
  vars_man30 <- c("man_30s", "man", "confirmed_cases",
                   "suicide_report1", "suicide_report2",
                   "suicide_report3", "suicide_report4",
                  "March", "April", "trend")
  vars_man40 <- c("man_40s", "man", "confirmed_cases",
                   "suicide_report1", "suicide_report2",
                   "suicide_report3", "suicide_report4",
                  "March", "April", "trend")
  vars_man50 <- c("man_50s", "man", "confirmed_cases",
                   "suicide_report1", "suicide_report2",
                   "suicide_report3", "suicide_report4",
                  "March", "April", "trend")
  vars_man60 <- c("man_60s", "man", "confirmed_cases",
                   "suicide_report1", "suicide_report2",
                   "suicide_report3", "suicide_report4",
                  "March", "April", "trend")
  vars_man70 <- c("man_70s", "man", "confirmed_cases",
                   "suicide_report1", "suicide_report2",
                   "suicide_report3", "suicide_report4",
                  "March", "April", "trend")
  vars_man80 <- c("man_80s", "man", "confirmed_cases",
                   "suicide_report1", "suicide_report2",
                   "suicide_report3", "suicide_report4",
                  "March", "April", "trend")
  vars_womU19 <- c("wom_U19", "wom", "confirmed_cases",
                   "suicide_report1", "suicide_report2",
                   "suicide_report3", "suicide_report4",
                   "March", "April", "trend")
  vars_wom20 <- c("wom_20s", "wom", "confirmed_cases",
                   "suicide_report1", "suicide_report2",
                   "suicide_report3", "suicide_report4",
                  "March", "April", "trend")
  vars_wom30 <- c("wom_30s", "wom", "confirmed_cases",
                   "suicide_report1", "suicide_report2",
                   "suicide_report3", "suicide_report4",
                  "March", "April", "trend")
  vars_wom40 <- c("wom_40s", "wom", "confirmed_cases",
                   "suicide_report1", "suicide_report2",
                   "suicide_report3", "suicide_report4",
                  "March", "April", "trend")
  vars_wom50 <- c("wom_50s", "wom", "confirmed_cases",
                   "suicide_report1", "suicide_report2",
                   "suicide_report3", "suicide_report4",
                  "March", "April", "trend")
  vars_wom60 <- c("wom_60s", "wom", "confirmed_cases",
                   "suicide_report1", "suicide_report2",
                   "suicide_report3", "suicide_report4",
                  "March", "April", "trend")
  vars_wom70 <- c("wom_70s", "wom", "confirmed_cases",
                   "suicide_report1", "suicide_report2",
                   "suicide_report3", "suicide_report4",
                  "March", "April", "trend")
  vars_wom80 <- c("wom_80s", "wom", "confirmed_cases",
                   "suicide_report1", "suicide_report2",
                   "suicide_report3", "suicide_report4",
                  "March", "April", "trend")
  reg_vars <- list(vars_manU19, vars_man20, vars_man30, vars_man40,
                   vars_man50, vars_man60, vars_man70, vars_man80,
                   vars_womU19, vars_wom20, vars_wom30, vars_wom40,
                   vars_wom50, vars_wom60, vars_wom70, vars_wom80)
  
  age_vec <- c("ManU19", "Man20s", "Man30s", "Man40s", "Man50s", "Man60s", "Man70s", "Man80s",
               "WomU19", "Wom20s", "Wom30s", "Wom40s", "Wom50s", "Wom60s", "Wom70s", "Wom80s")
  
  column_for_coefficient <- c("num_suicide",
                              "unemployment_rate",
                              "confirmed_cases",
                              "suicide_report1",
                              "suicide_report2",
                              "suicide_report3",
                              "suicide_report4",
                              "March",
                              "April",
                              "Trend")
  
  data_master %>% 
    dplyr::select(c(2:19, 39)) %>% # data for the num of suicide and unemployment rate and confirmed case
    make_descriptive_statistics_table(label = c(age_vec, "Unemp(male)", "Unemp(female)", "Confirmed case")) %>%
    gt::gtsave(my_file_html <- here::here("04_analyze", "suicide_study",
                                          "table", "descriptive_table.html"))
 
  data_master %>% run_regression(reg_vars = reg_vars,
                                 column_for_coefficient = column_for_coefficient)
}


make_descriptive_statistics_table <- function(data_input, label){
  
    gtsummary::theme_gtsummary_mean_sd()
    data_output <- data_input %>% 
      gtsummary::tbl_summary(
        label = list(man_U19 ~ "Male~19",
                     man_20s ~ "Male20s",
                     man_30s ~ "Male30s",
                     man_40s ~ "Male40s",
                     man_50s ~ "Male50s",
                     man_60s ~ "Male60s",
                     man_70s ~ "Male70s",
                     man_80s ~ "Male80s",
                     wom_U19 ~ "Female~19",
                     wom_20s ~ "Female20s",
                     wom_30s ~ "Female30s",
                     wom_40s ~ "Female40s",
                     wom_50s ~ "Female50s",
                     wom_60s ~ "Female60s",
                     wom_70s ~ "Female70s",
                     wom_80s ~ "Female80s",
                     man ~ "Unemp(male)",
                     wom ~ "Unemp(female)",
                     confirmed_cases ~ "Confirmed case")) %>%
      gtsummary::as_gt()
  
  return(data_output)
}


implement_diagnostic_test <- function(model,title){
  test_result <- dynamac::dynardl.auto.correlated(model,
                                         object.out = TRUE)
  
  normality_test <- as.vector(test_result$sw)
  correlation_test <- as.vector(test_result$bg)
  
  normality_stat <- normality_test[1] %>% as.character()
  normality_stat <- strsplit(normality_stat, split = " ")[[1]]
    
  normality_pvalue <- as.character(normality_test[2]) 
  
  correlation_stat <- correlation_test[1] %>% as.character()
  correlation_stat <- strsplit(correlation_stat, split = " ")[[1]]
  
  correlation_pvalue <- as.character(correlation_test[4]) 
  
  Shapiro_Wilk_normality_test <- c(normality_stat[3], normality_pvalue)
  Breusch_Godfrey_test_for_serial_correlation <- c(correlation_stat[4], correlation_pvalue)
  
  table_output <- data.frame(Shapiro_Wilk_normality_test, Breusch_Godfrey_test_for_serial_correlation)
  table_output <- table_output %>% 
    gt::gt() %>%
    gt::tab_header(title = title) %>%
    gt::tab_row_group(
      label = "Test Statistics",
      rows = 1) %>%
    gt::tab_row_group(
      label = "P value",
      rows = 2) %>%
    gt::tab_footnote(
      footnote = "Null hypothsis is 'population is normally distributed'",
      locations = gt::cells_column_labels(columns = Shapiro_Wilk_normality_test))%>%
    gt::tab_footnote(
      footnote = "Null hypothesis is 'no serial correlation'",
      locations = gt::cells_column_labels(columns = Breusch_Godfrey_test_for_serial_correlation))%>%
    gt::cols_label(
      Shapiro_Wilk_normality_test = gt::html("Shapiro-Wilk normality test"),
      Breusch_Godfrey_test_for_serial_correlation = gt::html("Breusch-Godfrey test for serial correlation")
    )
    
  return(table_output)
}


save_coefficient_matrix <- function(df_coefficients, age){
  
  #age <- substr(file_name, 4, 6)
  colnames(df_coefficients) <- c("Estimate", "S.E.", "t stat", "p value")
  row_names <- c(row.names(df_coefficients))
  df_coefficients <- df_coefficients %>%
      dplyr::mutate(coefficients = row_names) %>%
      dplyr::relocate(coefficients, .before = colnames(df_coefficients)[1])
    
  table_output <- df_coefficients %>%
      gt::gt() %>%
      gt::tab_header(title = "Coefficient Table") %>%
      gt::tab_spanner(label = paste0(age), columns = colnames(df_coefficients)[1:4])
    
    return(table_output)
 }



run_regression <- function(data_input,
                           reg_vars,
                           table_name,
                           age_vec,
                           column_for_coefficient
                           ){
  
  set.seed(123)
  df_coefficients_list <- list()
  for (column in reg_vars) {
    
    data <- data_input %>%
      dplyr::select(column)
    colnames(data) <- column_for_coefficient
    
    count_for_simulation_each_report <- 0
    
    model <- ARDL::auto_ardl(num_suicide ~ unemployment_rate + confirmed_cases | suicide_report1 + suicide_report2
                       + suicide_report3 + suicide_report4 + March
                       + April + Trend, data = data,
                       max_order = c(12,12,12),
                       grid = FALSE,
                       selection = "AIC")
    best_order <- c(model$best_order)
    
    # output image of impulse response shock on suicide report
    for (shock_var in c("suicide_report1",
                        "suicide_report2",
                        "suicide_report3",
                        "suicide_report4")) {
      
      if (best_order[2]==0 & best_order[3]!=0){
      # set unemployment rate as level, confirmed_cases as lagged dependent variables
        ardl.model <- dynamac::dynardl(num_suicide ~ unemployment_rate + confirmed_cases + 
                                         suicide_report1 + suicide_report2 +
                                         suicide_report3 + suicide_report4 +
                                         March + April,
                                       data = data,
                                       lags = list("num_suicide" = c(1:best_order[1]),
                                                   "confirmed_cases" = c(1:best_order[3])),
                                       levels = c("unemployment_rate",
                                                  "suicide_report1",
                                                  "suicide_report2",
                                                  "suicide_report3",
                                                  "suicide_report4",
                                                  "March",
                                                  "April"),
                                       ec = FALSE,
                                       trend = TRUE,
                                       noLDV = FALSE,
                                       simulate = TRUE,
                                       range = 20,
                                       shockvar = sprintf("%s", shock_var),
                                       shockval = 1,
                                       fullsims = TRUE)
      }
      else if(best_order[2]!=0 & best_order[3]==0){
        # set unemployment rate as lagged, confirmed_cases as level dependent variables
        ardl.model <- dynamac::dynardl(num_suicide ~ unemployment_rate + confirmed_cases + 
                                         suicide_report1 + suicide_report2 +
                                         suicide_report3 + suicide_report4 +
                                         March + April,
                                       data = data,
                                       lags = list("num_suicide" = c(1:best_order[1]),
                                                   "unemployment_rate" = c(1:best_order[2])),
                                       levels = c("confirmed_cases",
                                                  "suicide_report1",
                                                  "suicide_report2",
                                                  "suicide_report3",
                                                  "suicide_report4",
                                                  "March",
                                                  "April"),
                                       ec = FALSE,
                                       trend = TRUE,
                                       noLDV = FALSE,
                                       simulate = TRUE,
                                       range = 20,
                                       shockvar = sprintf("%s", shock_var),
                                       shockval = 1,
                                       fullsims = TRUE)
        
      }
      else if(best_order[2]==0 & best_order[3]==0){
        # set unemployment rate as level, confirmed_cases as level dependent variables
        ardl.model <- dynamac::dynardl(num_suicide ~ unemployment_rate + confirmed_cases + 
                                         suicide_report1 + suicide_report2 +
                                         suicide_report3 + suicide_report4 +
                                         March + April,
                                       data = data,
                                       lags = list("num_suicide" = c(1:best_order[1])),
                                       levels = c("confirmed_cases",
                                                  "unemployment_rate",
                                                  "suicide_report1",
                                                  "suicide_report2",
                                                  "suicide_report3",
                                                  "suicide_report4",
                                                  "March",
                                                  "April"),
                                       ec = FALSE,
                                       trend = TRUE,
                                       noLDV = FALSE,
                                       simulate = TRUE,
                                       range = 20,
                                       shockvar = sprintf("%s", shock_var),
                                       shockval = 1,
                                       fullsims = TRUE)
        
      }
      else if(column[1]=="wom_U19"){
      # as for wom_U19, I use error correction model
        ardl.model  <- dynamac::dynardl(num_suicide ~ unemployment_rate + confirmed_cases + suicide_report1 + suicide_report2
                       + suicide_report3 + suicide_report4 + March
                       + April,
                       data = data,
                       lags = list("num_suicide" = 1,
                                   "confirmed_cases" = c(1)
                       ),
                       diffs = c( "confirmed_cases"),
                       lagdiffs = list("num_suicide" = c(1:9)),
                       levels=c("unemployment_rate",
                                
                                "suicide_report1",
                                "suicide_report2",
                                "suicide_report3",
                                "suicide_report4",
                                "March",
                                "April"), 
                       ec = TRUE,
                       trend = TRUE,
                       simulate = TRUE,
                       range = 20,
                       shockvar = sprintf("%s", shock_var),
                       shockval = 1,
                       fullsims = TRUE)
      }
      else{
      
        ardl.model <- dynamac::dynardl(num_suicide ~ unemployment_rate + confirmed_cases + 
                                         suicide_report1 + suicide_report2 +
                                         suicide_report3 + suicide_report4 +
                                         March + April,
                                       data = data,
                                       lags = list("num_suicide" = c(1:best_order[1]),
                                                   "unemployment_rate" = c(1:best_order[2]),
                                                   "confirmed_cases" = 1:best_order[3]),
                                       levels = c("suicide_report1",
                                                  "suicide_report2",
                                                  "suicide_report3",
                                                  "suicide_report4",
                                                  "March",
                                                  "April"),
                                       ec = FALSE,
                                       trend = TRUE,
                                       noLDV = FALSE,
                                       simulate = TRUE,
                                       range = 20,
                                       shockvar = sprintf("%s", shock_var),
                                       shockval = 1,
                                       fullsims = TRUE)
        
      }
    
      
    
      pdf(here::here("04_analyze", "suicide_study", "figure",
                 column[1], paste0(shock_var,".pdf")))
      dynamac::dynardl.simulation.plot(ardl.model, type = "area", response = "levels")
      dev.off()
      png(here::here("04_analyze", "suicide_study", "figure",
                 column[1], paste0(shock_var,".png")))
      dynamac::dynardl.simulation.plot(ardl.model,  type = "area", response = "levels")
      dev.off()
      
      
      if (count_for_simulation_each_report == 0) {
        
        df_coefficients <- data.frame(summary(ardl.model)$coefficients)
        for (format in c(".html", ".tex")){
          save_coefficient_matrix(df_coefficients = df_coefficients, age = column[1]) %>%
            gt::gtsave(paste0("coefficient_table_", column[1], format),
                       path = here::here("04_analyze", "suicide_study", "table", paste0(column[1]), "coefficient_table"))}
        
        for (format in c(".html", ".tex")){
          implement_diagnostic_test(ardl.model,title = paste0("Diagnostic Test","(",column[1],")")) %>%
            gt::gtsave(paste0("diagnostic_test_", column[1], format),
                       path = here::here("04_analyze", "suicide_study", "table", paste0(column[1]), "diagnostic_test"))}}
      
      count_for_simulation_each_report <- count_for_simulation_each_report + 1
    }
  }
}


box::use(`functions`/basics)
main()