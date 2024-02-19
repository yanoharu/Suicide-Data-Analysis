main <- function(){
  preamble()
  build()
  analyze()
  report()
  postamble()
}

preamble <- function(){
  options(box.path = here::here("01_admin"))
  lets('set', 'preamble')
  lets('set', 'packages')
  lets('set', 'initialize')
}


build <- function(){
  lets('build','num_suicide_tidy')
  lets('build','unemployment_tidy')
  lets('build','confirmed_case_tidy')
  lets('build','master')
}


analyze <- function(){
  lets('analyze','num_suicide_tidy_check')
  lets('analyze','unemployment_tidy_check')
  lets('analyze','confirmed_case_tidy_check')
  lets('analyze','unit_root_test')
  lets('analyze','suicide_study')
}


report <- function(){
  lets('report', 'checks')
  lets('report', 'report')
}


postamble <- function(){
  lets('set', 'postamble')
}

lets <- function(verb_name, object_name){
  if (verb_name == 'set' && object_name == 'preamble'){
    source(here::here('01_admin', '02_preamble', 'R', 'admin.R'))
  }
  
  else if (verb_name == 'set' && object_name == 'initialize'){
    source(here::here('01_admin', "initialize", 'admin.R'))
  }
  
  else if (verb_name == 'set' && object_name == 'packages'){
    source(here::here('01_admin', "packages", 'admin.R'))
  }
  
  else if (verb_name == 'build'){
    source(here::here('03_build', object_name, 'code', 'build.R'))
  }
  
  else if (verb_name == 'analyze'){
    source(here::here('04_analyze', object_name, 'code', 'analyze.R'))
  }
  
  else if (verb_name == 'report'){
    rmarkdown::render(here::here('05_report', 
                                 object_name, 'text', 'report.Rmd'),
                      output_dir = here::here('05_report', 
                                              object_name, 'output')) 
  }
  
  else if (verb_name == 'set' && object_name == 'postamble'){
    source(here::here('01_admin', '03_postamble', 'admin.R'))
  }
  
}

main()
