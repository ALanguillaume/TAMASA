
unlink("_main.Rmd")
unlink("docs/main_files", recursive = TRUE)
bookdown::render_book(input = "index.Rmd", 
                      output_dir = here::here("./docs/"))
unlink("./_bookdown_files", recursive = TRUE)
system("scripts/make_images_ref_compatible_github_pages.sh")


