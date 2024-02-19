main <- function(){
  my_folder <- "unit_root_test"
  data_master <- basics$read_interim("master")
  file_path_table = here::here("04_analyze", my_folder,
                               "table")
  
  var_male <- c("year_month",
                "man_U19",
                "man_20s",
                "man_30s",
                "man_40s",
                "man_50s",
                "man_60s",
                "man_70s",
                "man_80s")
  
  var_female <- c("year_month",
                  "wom_U19",
                  "wom_20s",
                  "wom_30s",
                  "wom_40s",
                  "wom_50s",
                  "wom_60s",
                  "wom_70s",
                  "wom_80s")
  
  var_unemployment <- c("year_month",
                        "man",
                        "wom")
  
  var_confirmed_cases <- c("year_month",
                          "confirmed_cases")
  
  
  colors_male <- c(man_U19 = "#000066",
                   man_20s = "#CC6633",
                   man_30s = "#009999",
                   man_40s = "#FF0033",
                   man_50s = "#006600",
                   man_60s = "#CC33CC",
                   man_70s = "#999900",
                   man_80s = "#6600FF")
  
  colors_female <- c(wom_U19 = "#000066",
                     wom_20s = "#CC6633",
                     wom_30s = "#009999",
                     wom_40s = "#FF0033",
                     wom_50s = "#006600",
                     wom_60s = "#CC33CC",
                     wom_70s = "#999900",
                     wom_80s = "#6600FF")
  
  labels_male <- c(man_U19 = "~19",
                   man_20s = "20s",
                   man_30s = "30s",
                   man_40s = "40s",
                   man_50s = "50s",
                   man_60s = "60s",
                   man_70s = "70s",
                   man_80s = "80s")
  
  labels_female <- c(wom_U19 = "~19",
                     wom_20s = "20s",
                     wom_30s = "30s",
                     wom_40s = "40s",
                     wom_50s = "50s",
                     wom_60s = "60s",
                     wom_70s = "70s",
                     wom_80s = "80s")
  
  labels_unemp <- c(man = "unemployment rate of males",
                    wom = "unemployment rate of females")
  
  pick_var <- c("man_U19","dman_U19","man_20s","dman_20s",
                "man_30s","dman_30s","man_40s","dman_40s",
                "man_50s","dman_50s","man_60s","dman_60s",
                "man_70s","dman_70s","man_80s","dman_80s",
                "wom_U19","dwom_U19","wom_20s","dwom_20s",
                "wom_30s","dwom_30s","wom_40s","dwom_40s",
                "wom_50s","dwom_50s","wom_60s","dwom_60s",
                "wom_70s","dwom_70s","wom_80s","dwom_80s",
                "man","man_d1","wom","wom_d1",
                "confirmed_cases", "dconfirmed_cases")
  
  variable_names <-  c("U19(male)","dU19(male)", "20s(male)","d20s(male)",
                       "30s(male)", "d30s(male)","40s(male)","d40s(male)",
                       "50s(male)","d50s(male)", "60s(male)","d60s(male)",
                       "70s(male)", "d70s(male)","80s(male)","d80s(male)",
                       "U19(female)","dU19(female)","20s(female)", "d20s(female)",
                       "30s(female)", "d30s(female)","40s(female)","d40s(female)",
                       "50s(female)","d50s(female)", "60s(female)", "d60s(female)", 
                       "70s(female)", "d70s(female)","80s(female)","d80s(female)",
                       "Unemployment Rate(male)","dUnemployment Rate(male)", 
                       "Unemployment Rate(female)", "dUnemployment Rate(female)",
                       "confirmed cases", "dconfirmed_cases")
  
    data_master %>%
    lay_graph_suicide_male(pick_var = var_male[1:5],
                           colors = colors_male[1:4],
                           labels = labels_male[1:4]) %>%
    basics$save_my_plot(var_name = "suicide_male_1_study",
                        folder_name = my_folder)
    
    
    data_master %>%
      lay_graph_suicide_male(pick_var = var_male[c(1, 6:7)],
                             colors = colors_male[5:6],
                             labels = labels_male[5:6]) %>%
      basics$save_my_plot(var_name = "suicide_male_2_study",
                          folder_name = my_folder)
    
    data_master %>%
      lay_graph_suicide_male(pick_var = var_male[c(1, 8:9)],
                             colors = colors_male[7:8],
                             labels = labels_male[7:8]) %>%
      basics$save_my_plot(var_name = "suicide_male_3_study",
                          folder_name = my_folder)
   
    
    data_master %>%
      lay_graph_suicide_female(pick_var = var_female[1:5],
                               colors = colors_female[1:4],
                               labels = labels_female[1:4]) %>%
      basics$save_my_plot(var_name = "suicide_female_1_study",
                          folder_name = my_folder)
    
    data_master %>%
      lay_graph_suicide_female(pick_var = var_female[c(1, 6:7)],
                               colors = colors_female[5:6],
                               labels = labels_female[5:6]) %>%
      basics$save_my_plot(var_name = "suicide_female_2_study",
                          folder_name = my_folder)
    
    data_master %>%
      lay_graph_suicide_female(pick_var = var_female[c(1, 8:9)],
                               colors = colors_female[7:8],
                               labels = labels_female[7:8]) %>%
      basics$save_my_plot(var_name = "suicide_female_3_study",
                          folder_name = my_folder)
    
    
    data_master %>%
      lay_graph_unemployment_rate(pick_var = var_unemployment,
                                  labels = labels_unemp) %>%
      basics$save_my_plot(var_name = "unemployment_rate_study",
                          folder_name = my_folder)
    
    data_master %>%
      lay_graph_confirmed_cases(pick_var = var_confirmed_cases) %>%
      basics$save_my_plot(var_name = "confirmed_case_study",
                          folder_name = my_folder)
    
    num_exogenious_variable <- length(data_master) - length(variable_names) -1 
    
    p.value_table <- data_master %>%
      implement_unit_root_test(pick_var = seq(2,(length(data_master)-num_exogenious_variable),1),
                               variable = variable_names)
    p.value_table %>% gt::gtsave("unit_root_study.html", 
                                  path = file_path_table)
    p.value_table %>% gt::gtsave("unit_root_study.tex", 
                                  path = file_path_table)
    
}

require(ggplot2)
lay_graph_suicide_male <- function(data_input,
                                   pick_var,
                                   colors,
                                   labels){
  
  data_suicide <- data_input %>%
    dplyr::select(pick_var) %>%
    reshape2::melt(id = "year_month")
  
  plot_output <- ggplot(data = data_suicide,
                        aes(x = lubridate::ymd(year_month),
                            y = value,
                            color = variable)) +
    geom_line(lwd = 1.0) +
    scale_color_manual(values = colors,
                       name = "Age",
                       labels = labels) +
    labs(x = "time", y = "number(log transformed)",
         title = "number of suicide(male)") +
    theme(legend.position = "left") +
    geom_vline(aes(xintercept = lubridate::ymd(data_suicide[data_input["suicide_report1"]==1][1])),
               alpha = 0.7, col = 2, lwd = 1.5) +
    geom_vline(aes(xintercept = lubridate::ymd(data_suicide[data_input["suicide_report2"]==1][1])),
               alpha = 0.7, col = 2, lwd = 1.5) +
    geom_vline(aes(xintercept = lubridate::ymd(data_suicide[data_input["suicide_report3"]==1][1])),
               alpha = 0.7, col = 2, lwd = 1.5) +
    geom_vline(aes(xintercept = lubridate::ymd(data_suicide[data_input["suicide_report4"]==1][1])),
               alpha = 0.7, col = 2, lwd = 1.5) +
    annotate(geom = "text",
             x = lubridate::ymd(data_suicide[data_input["suicide_report1"]==1][1]),
             y = 5,
             label = "Suicide news 1",
             angle = 90) +
    annotate(geom = "text",
             x = lubridate::ymd(data_suicide[data_input["suicide_report2"]==1][1]),
             y = 4.5,
             label = "Suicide news 2",
             angle = 90)+
    annotate(geom = "text",
             x = lubridate::ymd(data_suicide[data_input["suicide_report3"]==1][1]),
             y = 5,
             label = "Suicide news 3",
             angle = 90)+
    annotate(geom = "text",
             x = lubridate::ymd(data_suicide[data_input["suicide_report4"]==1][1]),
             y = 5,
             label = "Suicide news 4",
             angle = 90)
  
  return(plot_output)
}


lay_graph_suicide_female <- function(data_input,
                                     pick_var,
                                     colors,
                                     labels){
  
  data_suicide <- data_input %>%
    dplyr::select(pick_var) %>%
    reshape2::melt(id = "year_month")
  
  plot_output <- ggplot(data = data_suicide,
                        aes(x = lubridate::ymd(year_month),
                            y = value,
                            color = variable)) +
    geom_line(lwd = 1.0) +
    scale_color_manual(values = colors,
                       name = "Age",
                       labels = labels) +
    labs(x = "time", y = "number(log transformed)",
         title = "number of suicide(female)") +
    theme(legend.position = "left") +
    geom_vline(aes(xintercept = lubridate::ymd(data_suicide[data_input["suicide_report1"]==1][1])),
               alpha = 0.7, col = 2, lwd = 1.5) +
    geom_vline(aes(xintercept = lubridate::ymd(data_suicide[data_input["suicide_report2"]==1][1])),
               alpha = 0.7, col = 2, lwd = 1.5) +
    geom_vline(aes(xintercept = lubridate::ymd(data_suicide[data_input["suicide_report3"]==1][1])),
               alpha = 0.7, col = 2, lwd = 1.5) +
    geom_vline(aes(xintercept = lubridate::ymd(data_suicide[data_input["suicide_report4"]==1][1])),
               alpha = 0.7, col = 2, lwd = 1.5) +
    annotate(geom = "text",
             x = lubridate::ymd(data_suicide[data_input["suicide_report1"]==1][1]),
             y = 4.5,
             label = "Suicide news 1",
             angle = 90) +
    annotate(geom = "text",
             x = lubridate::ymd(data_suicide[data_input["suicide_report2"]==1][1]),
             y = 4.5,
             label = "Suicide news 2",
             angle = 90)+
    annotate(geom = "text",
             x = lubridate::ymd(data_suicide[data_input["suicide_report3"]==1][1]),
             y = 4.5,
             label = "Suicide news 3",
             angle = 90)+
    annotate(geom = "text",
             x = lubridate::ymd(data_suicide[data_input["suicide_report4"]==1][1]),
             y = 4.5,
             label = "Suicide news 4",
             angle = 90)
  
  return(plot_output)
}


lay_graph_unemployment_rate <- function(data_input,
                                        pick_var,
                                        labels){
  
  data_unemployment <- data_input %>%
    dplyr::select(pick_var) %>%
    reshape2::melt(id = "year_month")
  
  plot_output <- ggplot(data = data_unemployment,
                              aes(x = lubridate::ymd(year_month),
                                  y = value,
                                  color = variable)) +
    geom_line(lwd = 1.0) + 
    scale_color_manual(values = c(man = "#000099",
                                  wom = "#CC6600"),
                       name = "gender",
                       labels = labels) +
    labs(x = "time", y = "rate(log transformed)",
         title = "Unemployment rate") +
    theme(legend.position = "left")
  
  return(plot_output)
}


lay_graph_confirmed_cases <- function(data_input,
                                      pick_var){
  
  data_confirmed_cases <- data_input %>%
    dplyr::select(pick_var) %>%
    reshape2::melt(id = "year_month")
  
  plot_output <- ggplot(data = data_confirmed_cases,
                                aes(x = lubridate::ymd(year_month),
                                    y = value,
                                    color = variable)) +
    geom_line(lwd = 1.0) +
    scale_color_manual(values = c("#000099"),
                       name = "",
                       labels = "confirmed cases") +
    labs(x = "time", y = "confirmed case(log transformed)",
         title = "Confirmed cases of Covid-19") +
    theme(legend.position = "left")
  
  return(plot_output)
}


implement_unit_root_test <- function(data_input,
                                     pick_var,
                                     variable){
  AR_lag <- c()
  stats_ADF <- c()
  stats_PP <- c()
  stats_KPSS <- c()
  stats_ZA <- c() 
  for(var_index in pick_var){
    
    series_data <- data_input %>% 
      dplyr::select(var_index) %>%
      dplyr::pull(-1)
    
    if (!is.na(any(series_data))){
      series_data <- series_data[2:length(series_data)]
      
      fit <- forecast::auto.arima(series_data,ic = "aic")
      result_ADF <- tseries::adf.test(series_data,
                                      k = fit$arma[1])
      result_PP <- tseries::pp.test(series_data)
      result_KPSS <- tseries::kpss.test(series_data,
                                        null = "Trend")
      result_ZA <- urca::ur.za(series_data,
                               model = c("trend"),
                               lag = fit$arma[1])
    } else {
      
      fit <- forecast::auto.arima(series_data, ic = "aic")
      result_ADF <- tseries::adf.test(series_data,
                                      k = fit$arma[1])
      result_PP <- tseries::pp.test(series_data)
      result_KPSS <- tseries::kpss.test(series_data,
                                        null = "Trend")
      result_ZA <- urca::ur.za(series_data,
                               model = c("trend"),
                               lag = fit$arma[1])
    }
    
    AR_lag <- c(AR_lag, fit$arma[1])
    stats_ADF <- c(stats_ADF, result_ADF$p.value)
    stats_PP <- c(stats_PP, result_PP$p.value)
    stats_KPSS <- c(stats_KPSS, result_KPSS$p.value)
    stats_ZA <- c(stats_ZA, result_ZA@teststat)
  }
  
  result_table <- data.frame(AR_lag,
                             stats_ADF,
                             stats_PP,
                             stats_KPSS,
                             stats_ZA)
  result_table <- result_table %>%
    round(3) %>%
    dplyr::mutate(Variable = variable,
                  .before = AR_lag)
  
  table_output <- result_table %>%
    gt::gt() %>%
    gt::tab_header(title = "Unit Root Test") %>%
    gt::tab_footnote(
      footnote = "Lag order was chosen by AIC.\nConstant and trend are included.",
      locations = gt::cells_column_labels(columns = stats_ADF)
    ) %>%
    gt::tab_footnote(
      footnote = "Constant and trend are included",
      locations = gt::cells_column_labels(columns = stats_PP)
    ) %>%
    gt::tab_footnote(
      footnote = "Null hypothesis is `time series is stationary`.\nConstant and trend are included",
      locations = gt::cells_column_labels(columns = stats_KPSS))%>%
    gt::tab_footnote(
      footnote = "Alternative hypothesis is `time series doesn`t have unit root with single structual break`",
      locations = gt::cells_column_labels(columns = stats_ZA)
    ) %>%
    gt::cols_label(
      Variable = gt::html(" "),
      stats_ADF = gt::html("p.value(ADF)"),
      stats_PP = gt::html("p.value(PP)"),
      stats_KPSS = gt::html("p.value(KPSS)"),
      stats_ZA = gt::html("Stats(ZA),<br>Critical Value<br>= -4.8     ")
    )
    
  
  return(table_output)
  
}

box::use(`functions`/basics)
main()
