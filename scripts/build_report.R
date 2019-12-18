
unlink("./rmd/_main.Rmd")
setwd("C:/projects/TAMASA/rmd/")
bookdown::render_book(input = "index.Rmd", 
                      output_dir = here::here("./docs/"))
unlink("./_bookdown_files", recursive = TRUE)
setwd(here::here())

