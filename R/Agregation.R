# R/03_agregation.R

library(data.table)
library(arrow)

ventes = as.data.table(read_parquet("data/interim/ventes.parquet"))
seuil = 10

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

# 1. Annuel
france_annee = ajouter_evol(resumer(ventes, c("annee", "type_local")), character(0))
dep_annee    = ajouter_evol(resumer(ventes, c("code_departement", "annee", "type_local")), "code_departement")
com_annee    = ajouter_evol(resumer(ventes, c("code_commune", "code_departement", "annee", "type_local")), "code_commune")

# 2. Trimestriel
cols_trim = c("annee", "trimestre", "type_local")
france_trim = ventes[, .(n_ventes = .N, prix_m2_median = median(prix_m2)), by = cols_trim]
dep_trim    = ventes[, .(n_ventes = .N, prix_m2_median = median(prix_m2)), by = c("code_departement", cols_trim)]
com_trim    = ventes[, .(n_ventes = .N, prix_m2_median = median(prix_m2)), by = c("code_commune", cols_trim)]

# 3. Par nombre de pièces
ventes[, pieces := fifelse(nombre_pieces_principales >= 5, "T5 et +",
                           paste0("T", nombre_pieces_principales))]
com_pieces = ventes[nombre_pieces_principales >= 1,
                    .(n_ventes = .N, prix_m2_median = median(prix_m2)),
                    by = .(code_commune, annee, type_local, pieces)]

# 4. Déciles (communes avec au moins 10 ventes)
com_deciles = ventes[, if (.N >= seuil) as.list(quantile(prix_m2, seq(0, 1, 0.1))),
                     by = .(code_commune, annee, type_local)]
setnames(com_deciles, 4:14, paste0("d", seq(0, 100, 10)))

# 5. Référentiel des communes (libellé le plus récent)
communes = ventes[order(-date_mutation),
                  .(nom_commune      = first(nom_commune),
                    code_postal      = first(code_postal),
                    code_departement = first(code_departement)),
                  by = code_commune]

# Export
tables = list(france_annee = france_annee, dep_annee = dep_annee, com_annee = com_annee,
              france_trim = france_trim, dep_trim = dep_trim, com_trim = com_trim,
              com_pieces = com_pieces, com_deciles = com_deciles, communes = communes)

for (nom in names(tables)) {
  saveRDS(tables[[nom]], file.path("app/data", paste0(nom, ".rds")))
}

# Contrôles
fichiers_rds = list.files("app/data", "\\.rds$", full.names = TRUE)
data.frame(table = names(tables), lignes = sapply(tables, nrow))
data.frame(fichier = basename(fichiers_rds), Mo = round(file.size(fichiers_rds) / 1e6, 2))
com_annee[n_ventes >= seuil, .(communes_fiables = .N), by = .(annee, type_local)][order(type_local, annee)]
