library(data.table)
library(arrow)
library(sf)

ventes = as.data.table(read_parquet("data/interim/ventes.parquet"))
seuil = 10

# Communes fusionnées : rattachement à la commune actuelle
passage_communes = readRDS("data/interim/passage_communes.rds")
ventes[passage_communes, on = .(code_commune = code_ancien), code_commune := i.code_actuel]

resumer = function(dt, by) {
  dt[, .(
    n_ventes        = .N,
    prix_m2_median  = median(prix_m2),
    prix_m2_q1      = quantile(prix_m2, 0.25),
    prix_m2_q3      = quantile(prix_m2, 0.75),
    prix_median     = median(as.numeric(valeur_fonciere)),
    surface_mediane = median(as.numeric(surface_reelle_bati))
  ), by = by]
}

ajouter_evol = function(agg, cle) {
  setorderv(agg, c(cle, "type_local", "annee"))
  agg[, `:=`(prec_prix  = shift(prix_m2_median),
             prec_n     = shift(n_ventes),
             prec_annee = shift(annee)),
      by = c(cle, "type_local")]
  agg[, evol_1an := fifelse(prec_annee == annee - 1 & n_ventes >= seuil & prec_n >= seuil,
                            prix_m2_median / prec_prix - 1, NA_real_)]
  agg[, c("prec_prix", "prec_n", "prec_annee") := NULL]
  agg
}

cols_trim = c("annee", "trimestre", "type_local")

# 1. France et départements (avant l'ajout PLM)
france_annee = ajouter_evol(resumer(ventes, c("annee", "type_local")), character(0))
dep_annee    = ajouter_evol(resumer(ventes, c("code_departement", "annee", "type_local")), "code_departement")
france_trim  = ventes[, .(n_ventes = .N, prix_m2_median = median(prix_m2)), by = cols_trim]
dep_trim     = ventes[, .(n_ventes = .N, prix_m2_median = median(prix_m2)), by = c("code_departement", cols_trim)]

# 2. Paris, Lyon, Marseille : commune entière en plus des arrondissements
ventes_plm = ventes[grepl("^(751|6938|132[0-1])", code_commune)]
ventes_plm[, nom_commune := fcase(startsWith(code_commune, "751"),  "Paris",
                                  startsWith(code_commune, "6938"), "Lyon",
                                  startsWith(code_commune, "132"),  "Marseille")]
ventes_plm[, code_commune := fcase(startsWith(code_commune, "751"),  "75056",
                                   startsWith(code_commune, "6938"), "69123",
                                   startsWith(code_commune, "132"),  "13055")]
ventes = rbind(ventes, ventes_plm)

# 3. Communes
com_annee = ajouter_evol(resumer(ventes, c("code_commune", "code_departement", "annee", "type_local")), "code_commune")
com_trim  = ventes[, .(n_ventes = .N, prix_m2_median = median(prix_m2)), by = c("code_commune", cols_trim)]

ventes[, pieces := fifelse(nombre_pieces_principales >= 5, "T5 et +",
                           paste0("T", nombre_pieces_principales))]
com_pieces = ventes[nombre_pieces_principales >= 1,
                    .(n_ventes = .N, prix_m2_median = median(prix_m2)),
                    by = .(code_commune, annee, type_local, pieces)]

com_deciles = ventes[, if (.N >= seuil) as.list(quantile(prix_m2, seq(0, 1, 0.1))),
                     by = .(code_commune, annee, type_local)]
setnames(com_deciles, 4:14, paste0("d", seq(0, 100, 10)))

# 4. Référentiel des communes (nom officiel actuel quand il existe)
communes = ventes[order(-date_mutation),
                  .(nom_commune      = first(nom_commune),
                    code_postal      = first(code_postal),
                    code_departement = first(code_departement)),
                  by = code_commune]

geo_com  = readRDS("app/data/geo_com.rds")
geo_noms = as.data.table(st_drop_geometry(geo_com))[, .(code_commune = code, nom)]
communes[geo_noms, on = "code_commune", nom_commune := i.nom]

# Export
tables = list(france_annee = france_annee, dep_annee = dep_annee, com_annee = com_annee,
              france_trim = france_trim, dep_trim = dep_trim, com_trim = com_trim,
              com_pieces = com_pieces, com_deciles = com_deciles, communes = communes)

for (nom in names(tables)) {
  saveRDS(tables[[nom]], file.path("app/data", paste0(nom, ".rds")))
}

# Contrôles
geo_dep = readRDS("app/data/geo_dep.rds")
setdiff(unique(dep_annee$code_departement), geo_dep$code)

absents = setdiff(unique(com_annee$code_commune), geo_com$code)
absents[!grepl("^(751|6938|132[0-1])", absents)]

com_annee[code_commune %in% c("75056", "69123", "13055") & annee == 2025,
          .(code_commune, type_local, n_ventes, prix_m2_median = round(prix_m2_median))]

fichiers_rds = list.files("app/data", "\\.rds$", full.names = TRUE)
data.frame(fichier = basename(fichiers_rds), Mo = round(file.size(fichiers_rds) / 1e6, 2))
