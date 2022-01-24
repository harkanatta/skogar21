
freadClip <- function(...) {
  X <- tempfile()
  writeLines(readLines("clipboard"), X)
  data.table::fread(X, header=F, ...)
}

Sys.setlocale("LC_ALL", "Icelandic")


talning <- structure(list(isl_heiti  = c("Dílaskarfur", "Himbrimi", "Lómur", "Æður", 
                              "Hávella", "Toppönd", "Stokkönd", "Teista", "Hvítmáfur", "Svartbakur", 
                              "Bjartmáfur", "Silfurmáfur", "Tildra", "Tjaldur", "Skógarþröstur", 
                              "Svartþröstur", "Stari", "Hrafn", "Snjótittlingur"), fjoldi  = c(8L, 
                                                                                          1L, 6L, 267L, 105L, 5L, 34L, 4L, 9L, 27L, 58L, 9L, 11L, 1L, 1L, 
                                                                                          2L, 40L, 18L, 27L)), row.names = c(NA, -19L), class = "data.frame")
talning <- as_tibble(talning) %>% arrange(isl_heiti)

fuglar <- structure(list(isl_heiti = c("Bjartmáfur", "Dvergmáfur", "Fjallkjói", 
                                "Fjöruspói", "Geirfugl", "Haftyrðill", "Heiðlóa", "Hettumáfur", 
                                "Hrossagaukur", "Hvítmáfur", "Jaðrakan", "Kjói", "Kría", "Langvía", 
                                "Lundi", "Lóuþræll", "Rauðbrystingur", "Rita", "Sanderla", "Sandlóa", 
                                "Sendlingur", "Silfurmáfur", "Skógarsnípa", "Skúmur", "Spói", 
                                "Stelkur", "Stormmáfur", "Stuttnefja", "Svartbakur", "Sílamáfur", 
                                "Teista", "Tildra", "Tjaldur", "Álka", "Óðinshani", "Þórshani", 
                                "Blesgæs", "Brandönd", "Duggönd", "Gargönd", "Grafönd", "Grágæs", 
                                "Gulönd", "Heiðagæs", "Helsingi", "Hrafnsönd", "Hávella", "Húsönd", 
                                "Margæs", "Rauðhöfðaönd", "Skeiðönd", "Skúfönd", "Stokkönd", 
                                "Straumönd", "Toppönd", "Urtönd", "Álft", "Æðarkóngur", "Æður", 
                                "Bjargdúfa", "Fálki", "Haförn", "Smyrill", "Rjúpa", "Auðnutittlingur", 
                                "Barrfinka", "Glókollur", "Gráspör", "Gráþröstur", "Hrafn", "Krossnefur", 
                                "Maríuerla", "Músarrindill", "Skógarþröstur", "Snjótittlingur", 
                                "Stari", "Steindepill", "Svartþröstur", "Þúfutittlingur", "Dílaskarfur", 
                                "Flórgoði", "Fýll", "Himbrimi", "Lómur", "Sjósvala", "Skrofa", 
                                "Stormsvala", "Súla", "Toppskarfur", "Keldusvín", "Brandugla", 
                                "Eyrugla", "Snæugla"), latn_heiti = c("Larus glaucoides", "Hydrocoloeus minutus", 
                                                              "Stercorarius longicaudus", "Numenius arquata", "Pinguinus impennis", 
                                                              "Alle alle", "Pluvialis apricaria", "Chroicocephalus ridibundus", 
                                                              "Gallinago gallinago", "Larus hyperboreus", "Limosa limosa", 
                                                              "Stercorarius parasiticus", "Sterna paradisaea", "Uria aalge", 
                                                              "Fratercula arctica", "Calidris alpina", "Calidris canutus", 
                                                              "Rissa tridactyla", "Calidris alba", "Charadrius hiaticula", 
                                                              "Calidris maritima", "Larus argentatus", "Scolopax rusticola", 
                                                              "Catharacta skua", "Numenius phaeopus", "Tringa totanus", "Larus canus", 
                                                              "Uria lomvia", "Larus marinus", "Larus fuscus", "Cepphus grylle", 
                                                              "Arenaria interpres", "Haematopus ostralegus", "Alca torda", 
                                                              "Phalaropus lobatus", "Phalaropus fulicarius", "Anser albifrons flavirostris", 
                                                              "Tadorna tadorna", "Aythya marila", "Mareca strepera", "Anas acuta", 
                                                              "Anser anser", "Mergus merganser", "Anser brachyrhynchus", "Branta leucopsis", 
                                                              "Melanitta nigra", "Clangula hyemalis", "Bucephala islandica", 
                                                              "Branta bernicla", "Mareca penelope", "Spatula clypeata", "Aythya fuligula", 
                                                              "Anas platyrhynchos", "Histrionicus histrionicus", "Mergus serrator", 
                                                              "Anas crecca", "Cygnus cygnus", "Somateria spectabilis", "Somateria mollissima", 
                                                              "Columba livia", "Falco rusticolus", "Haliaeetus albicilla", 
                                                              "Falco columbarius", "Lagopus muta", "Acanthis flammea", "Spinus spinus", 
                                                              "Regulus regulus", "Passer domesticus", "Turdus pilaris", "Corvus corax", 
                                                              "Loxia curvirostra", "Motacilla alba", "Troglodytes troglodytes", 
                                                              "Turdus iliacus", "Plectrophenax nivalis", "Sturnus vulgaris", 
                                                              "Oenanthe oenanthe", "Turdus merula", "Anthus pratensis", "Phalacrocorax carbo", 
                                                              "Podiceps auritus", "Fulmarus glacialis", "Gavia immer", "Gavia stellata", 
                                                              "Hydrobates leucorhous", "Puffinus puffinus", "Hydrobates pelagicus", 
                                                              "Morus bassanus", "Phalacrocorax aristotelis", "Rallus aquaticus", 
                                                              "Asio flammeus", "Asio otus", "Bubo scandiacus")), row.names = c(NA, 
                                                                                                                               -93L), class = "data.frame")

fuglar <- as_tibble(fuglar) %>% arrange(isl_heiti)

myndalisti <- fuglar[fuglar$isl_heiti %in% talning$isl_heiti,]

library(rphylopic)
person <- name_search(text = "Homo sapiens", options = "namebankID")[[1]]

