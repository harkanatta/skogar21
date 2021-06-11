library(magrittr)
library(magick)
library(here)

library(here)
herepath <- here()
pathnew <- paste(herepath,"nytt",sep = "/")
dir.create(pathnew)
myndir <- list.files(paste(herepath,"myndir",sep = "/"),pattern = "png", recursive = T,full.names = T)
for (i in myndir) {
  mynd <- image_read(i) %>%
    image_convert("svg") %>% 
    image_write(path = paste(pathnew,paste(gsub(".*[/]([^.]+)[.].*", "\\1", i),"svg",sep = "."),sep = "/"))
}