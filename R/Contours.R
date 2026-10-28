# R/Contours.R
# install.packages("sf")

library(sf)
library(data.table)

url_base = "https://etalab-datasets.geo.data.gouv.fr/contours-administratifs/latest/geojson"
fichiers = c(departements = "departements-100m.geojson.gz",
             communes     = "communes-1000m.geojson.gz",
             deleguees    = "communes-associees-deleguees-1000m.geojson.gz")

dir.create("data/raw/contours", recursive = TRUE, showWarnings = FALSE)
dir.create("data/interim", showWarnings = FALSE)

for (f in fichiers) {
  dest = file.path("data/raw/contours", f)
  if (!file.exists(dest)) download.file(paste0(url_base, "/", f), dest, mode = "wb")
}

lire = function(f) {
  x = read_sf(paste0("/vsigzip/", file.path("data/raw/contours", f)))
  x = st_transform(x, 4326)
  st_make_valid(x)
}

# 1. Contours pour la carte
geo_dep = lire(fichiers["departements"])[, c("code", "nom")]
geo_com = lire(fichiers["communes"])[, c("code", "nom", "departement")]

saveRDS(geo_dep, "app/data/geo_dep.rds")
saveRDS(geo_com, "app/data/geo_com.rds")

# 2. Table de passage : ancien code commune -> commune actuelle (jointure spatiale)
deleg = lire(fichiers["deleguees"])[, "code"]

pts_deleg = st_point_on_surface(st_transform(deleg, 2154))
jointure  = st_join(pts_deleg, st_transform(geo_com[, "code"], 2154),
                    suffix = c("_ancien", "_actuel"))

passage_communes = as.data.table(st_drop_geometry(jointure))[
  !is.na(code_actuel) & code_ancien != code_actuel, .(code_ancien, code_actuel)]
passage_communes = passage_communes[, .SD[1], by = code_ancien]

saveRDS(passage_communes, "data/interim/passage_communes.rds")

# Contrôles
nrow(geo_dep)
nrow(geo_com)
nrow(passage_communes)
passage_communes[code_ancien %in% c("93059", "49069", "01039")]
round(file.size(c("app/data/geo_dep.rds", "app/data/geo_com.rds")) / 1e6, 1)
