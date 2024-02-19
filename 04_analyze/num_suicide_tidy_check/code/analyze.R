main <- function(){
  my_folder <- "num_suicide_tidy"
  my_data <- basics$read_interim(my_folder)
  
  var_quantitative <- c("man_U19","man_20s","man_30s","man_40s",
                        "man_50s","man_60s","man_70s","man_80s",
                        "wom_U19","wom_20s","wom_30s","wom_40s",
                        "wom_50s","wom_60s","wom_70s","wom_80s")
  
  checks$check_quantitative(my_data,
                            var_quantitative,
                            my_folder)
}

box::use(`functions`/basics)
box::use(`functions`/checks)
main()