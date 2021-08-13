library(tidyverse)
library(plyr)
library(ggtextures)
library(magick)

Sys.setlocale("LC_ALL", "Icelandic")
sysfonts::font_add_google("Catamaran", "Catamaran")
showtext::showtext_auto()
gogn <- readr::read_csv("https://github.com/harkanatta/skogar21/raw/main/fuglasnid/snid.csv", locale = locale("is",encoding = "latin1"))

data <- gogn %>% 
  ddply(.(tegund),summarise,N=table(tegund)) %>% 
  mutate(N=as.double(N))

  
#---
#Mynd 1:

  DF <- tibble(
    fjöldi = data$N,
    tegund = unique(data$tegund),
    image = list(image_read(list.files("./myndir",full.names = T)[1]),
                 image_read(list.files("./myndir",full.names = T)[2]),
                 image_read(list.files("./myndir",full.names = T)[3]),
                 image_read(list.files("./myndir",full.names = T)[4]),
                 image_read(list.files("./myndir",full.names = T)[5]),
                 image_read(list.files("./myndir",full.names = T)[6]),
                 image_read(list.files("./myndir",full.names = T)[7]),
                 image_read(list.files("./myndir",full.names = T)[8]),
                 image_read(list.files("./myndir",full.names = T)[9]),
                 image_read(list.files("./myndir",full.names = T)[10]))
    )

f1 = "Catamaran"
p <- ggplot(DF, aes(tegund, fjöldi, image = image)) +
  geom_isotype_col(
    img_height = grid::unit(1, "null"), img_width = NULL,
    ncol = 1, nrow = 1, hjust = 1, vjust = 0.5,
    colour = "#76653BFF"
  ) +
  coord_flip() +
  guides(fill = "none") +
  theme_classic(base_family = f1,
                base_size=80,
                base_line_size = .3,
                base_rect_size = 2)

png(here::here(paste0("talning", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), res = 320, height = 9, width = 12, units = "in")
p
dev.off()

#---
#Mynd 2

data <- gogn %>% mutate(fjarlaegd = fct_relevel(fjarlaegd,
                                                "0-25",
                                                "25-50",
                                                "50-100",
                                                "100+")) %>% 
  ddply(.(tegund,fjarlægð=fjarlaegd),summarise,fjöldi=table(tegund)) %>% 
  mutate(fjöldi=as.double(fjöldi))

p2 <- ggplot(data, aes(x=tegund, y=fjöldi, fill=fjarlægð)) +
  geom_bar(stat='identity', position='dodge') +
  theme_minimal(base_family = f1,
                base_size=80,
                base_line_size = .3,
                base_rect_size = 2) +
  coord_flip() +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  harrypotter::scale_fill_hp(discrete = TRUE, option = "ronweasley2", name = "fjarlægð") 

png(here::here(paste0("fjarlaegd", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), res = 320, height = 9, width = 12, units = "in")
p2
dev.off()


