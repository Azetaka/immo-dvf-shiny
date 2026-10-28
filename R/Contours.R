# R/Contours.R
# install.packages("sf")

library(sf)
library(data.table)

url_base = "https://etalab-datasets.geo.data.gouv.fr/contours-administratifs/latest/geojson"
fichiers = c(departements = "departements-100m.geojson.gz",
             communes     = "communes-1000m.geojson.gz")

dir.create("data/raw/contours", recursive = TRUE, showWarnings = FALSE)
for (f in fichiers) {
  dest = file.path("data/raw/contours", f)
  if (!file.exists(dest)) download.file(paste0(url_base, "/", f), dest, mode = "wb")
}

lire = function(f) {
  x = read_sf(paste0("/vsigzip/", file.path("data/raw/contours", f)))
  x = st_transform(x, 4326)
  st_make_valid(x)
}

geo_dep = lire(fichiers["departements"])[, c("code", "nom")]
geo_com = lire(fichiers["communes"])[, c("code", "nom", "departement")]

saveRDS(geo_dep, "app/data/geo_dep.rds")
saveRDS(geo_com, "app/data/geo_com.rds")

# Contrôle des jointures avec les agrégats
dep_annee = readRDS("app/data/dep_annee.rds")
com_annee = readRDS("app/data/com_annee.rds")

setdiff(unique(dep_annee$code_departement), geo_dep$code)

codes_absents = setdiff(unique(com_annee$code_commune), geo_com$code)
length(codes_absents)
head(sort(codes_absents), 30)

round(file.size(c("app/data/geo_dep.rds", "app/data/geo_com.rds")) / 1e6, 1)
