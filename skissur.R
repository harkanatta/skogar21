library(tidyverse)
library(plyr)
library(ggtextures)
library(magick)

Sys.setlocale("LC_ALL", "Icelandic")
gogn <- readr::read_csv("https://github.com/harkanatta/skogar21/raw/main/fuglasnid/snid.csv", locale = locale("is",encoding = "latin1"))
head(gogn)
#gogn <- read.csv("https://github.com/harkanatta/skogar21/raw/main/fuglasnid/snid.csv")

fragments <- gogn %>% 
  ddply(.(tegund,fjarlaegd),summarise,N=table(tegund)) %>% 
  mutate(N=as.double(N))

fragments$img <- list(image_read(sort(list.files("./myndir",full.names = T))))
fragments <- as_tibble(fragments)
  
  
myndir <- sort(list.files("./myndir/nytt",full.names = T))
for (i in myndir) {
  image_scale(image, "200")
}
  
  
pal <- wesanderson::wes_palette("Zissou1",10, type = "continuous")

  data <- tibble(
    fjöldi = fragments$N,
    tegund = unique(fragments$tegund),
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
p <- ggplot(data, aes(tegund, fjöldi, image = image)) +
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


líkur = 1-kx
1/k = x #sést ekki
k=(1-sqrt(1-p))/w


Líkanið gerir ráð fyrir að líkurnar á að sjá tiltekinn fugl í x metra fjarlægð
séu 1 - kx, þar sem k er óþekktur stuðull. Ef fjarlægð í fuglinn er 1/k, þá sést fuglinn ekki.
Leiðréttingarstuðullinn k fæst með eftirfarandi jöfnu:
  k = (1-√(1-p))/w
þar sem p er hlutfall fugla sem sést á innra beltinu (t.d. 100 m á hvora hönd) af heildarfjölda á öllu
sniðinu og w er breidd innra beltisins frá miðlínu sniðs. Þéttleiki (D = pör á km²) fugla fæst þá með
eftirfarandi jöfnu:
  D = 1000*N*k/L

þar sem N eru allar athuganir á tiltekinni tegund á báðum athugunarbeltunum, k fyrrgreindur
stuðull og L er lengd mælisniðs í km.
Það ræðst af sýnileika tegunda hvaða breidd innra beltis hentar hverri tegund best. Þéttleiki var
reiknaður út frá þremur breiddum innra beltis, 25 m, 50 m og 100 m. Í flestum tilfellum gaf 50 m
breitt innra belti hæstan þéttleika tegunda eftir búsvæðum og því var það notað til
stofnstærðarútreikninga í öllum tilfellum. 




fragments <- gogn %>% 
  ddply(.(tegund,fjarlægð=fjarlaegd),summarise,fjöldi=table(tegund)) %>% 
  mutate(fjöldi=as.double(fjöldi))

p2 <- ggplot(fragments, aes(x=tegund, y=fjöldi, fill=fjarlægð)) +
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


