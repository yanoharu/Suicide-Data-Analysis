main <- function(){
  my_folder <- "confirmed_case_tidy"
  my_data <- basics$read_interim(my_folder)
  
  var_quantitative <- c("confirmed_cases")
  
  checks$check_quantitative(my_data,
                            var_quantitative,
                            my_folder)
  
}

box::use(`functions`/basics)
box::use(`functions`/checks)
main()