library(renderthis)

slides_html <- list.files("_site/slides", 
                     recursive = TRUE, 
                     pattern = "*.html$",
                    full.names = TRUE)

slides_qmd <- list.files("slides", 
          recursive = TRUE, 
pattern = "*.qmd$",
full.names = TRUE)

slides_html <- slides_html[!grepl("^_", basename(slides_html))]
slides_qmd <- slides_qmd[!grepl("^_", basename(slides_qmd))]

slides_html <- slides_html[xfun::sans_ext(basename(slides_html)) %in% xfun::sans_ext(basename(slides_qmd))]



for(i in 1:length(slides)){
  folder <- stringr::str_extract(slides_html[i], "slides/[^/]+")
  file <- basename(xfun::sans_ext(slides_html[i]))
  renderthis::to_pdf(slides_html[i], to = sprintf("%s/%s.pdf", folder, file))
}


