
unlink("_main.Rmd")
bookdown::render_book(input = "index.Rmd", 
                      output_dir = here::here("./docs/"))
unlink("./_bookdown_files", recursive = TRUE)

