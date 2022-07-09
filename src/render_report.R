library(rmarkdown)
code_dir <- "src" 
report_filename <- "mid-season_planning.Rmd"
report_filename <- file.path(code_dir, report_filename)
output_dir <- "htmls"
output <- file.path("..",output_dir)
render(report_filename, output_dir = output_dir, params = list(output_dir = output))

