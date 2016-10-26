library(maptools)
library(rgdal)
library(RColorBrewer)

work_dir_save <- getwd()
setwd("C:/data/fraunhofer_bda/plz-karte")

pdf_datei <- "karten_deutschland_shp_plz.pdf"
cairo_pdf(bg = "grey98",
          pdf_datei,
          width = 16,
          height = 9)

par(
  mar = c(0, 0, 0, 0),
  oma = c(1, 1, 1, 0),
  mfcol = c(1, 2),
  family = "Lato Light",
  las = 1
)

# Daten einlesen und Grafik vorbereiten
y <-
  readShapeSpatial(file.path("C:/data/fraunhofer_bda/",
                             "plz-karte/plz/post_pl.shp"),
                   proj4string = CRS("+proj=longlat"))
x = spTransform(y, CRS = CRS("+proj=merc"))
farbe <- sample(1:7, length(x), replace = T)

# Grafik erstellen und weitere Elemente
plot(x, col = brewer.pal(7, "Oranges")[farbe], border = F)
mtext(
  paste("N=", format(length(x), big.mark = "."), sep = ""),
  side = 3,
  line = -6,
  adj = 0,
  cex = 1.7
)

# Betitelung
mtext(
  "PLZ Grenzen",
  side = 3,
  line = -4,
  adj = 0,
  cex = 1.7
)
mtext(
  "Quelle: http://arnulf.us/PLZ",
  side = 1,
  line = -1,
  adj = 0,
  cex = 1.3
)

dev.off()

setwd(work_dir_save)

# EOF .

