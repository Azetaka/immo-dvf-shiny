#install.packages(c("data.table", "arrow"))

library(data.table)
library(arrow)

dir.create("data/interim", recursive = TRUE, showWarnings = FALSE)

cols = c("id_mutation", "date_mutation", "nature_mutation", "valeur_fonciere",
         "code_postal", "code_commune", "nom_commune", "code_departement",
         "type_local", "surface_reelle_bati", "nombre_pieces_principales")

fichiers = list.files("data/raw", pattern = "^dvf_\\d{4}\\.csv\\.gz$", full.names = TRUE)

preparer_annee = function(f) {
  dt = fread(f, select = cols, encoding = "UTF-8",
             colClasses = list(character = c("code_postal", "code_commune", "code_departement")))
  n_brut = nrow(dt)

  dt = dt[nature_mutation == "Vente"]

  # Composition de chaque mutation
  compo = dt[, .(
    n_logements  = sum(type_local %in% c("Appartement", "Maison")),
    n_locaux_pro = sum(grepl("^Local", type_local))
  ), by = id_mutation]
  ids_ok = compo[n_logements == 1 & n_locaux_pro == 0, id_mutation]

  ventes = dt[id_mutation %in% ids_ok & type_local %in% c("Appartement", "Maison")]
  ventes = ventes[valeur_fonciere > 0 & surface_reelle_bati >= 9]

  ventes[, date_mutation := as.IDate(date_mutation)]
  ventes[, `:=`(
    annee     = year(date_mutation),
    trimestre = quarter(date_mutation),
    prix_m2   = valeur_fonciere / surface_reelle_bati
  )]

  # Retrait des valeurs extrêmes (1 % de chaque côté)
  ventes[, `:=`(p01 = quantile(prix_m2, 0.01), p99 = quantile(prix_m2, 0.99)),
         by = .(code_departement, type_local)]
  ventes = ventes[prix_m2 >= p01 & prix_m2 <= p99]
  ventes[, c("p01", "p99", "nature_mutation") := NULL]

  message(basename(f), " : ", format(n_brut, big.mark = " "), " lignes brutes -> ",
          format(nrow(ventes), big.mark = " "), " ventes retenues")
  ventes
}

ventes = rbindlist(lapply(fichiers, preparer_annee))
write_parquet(ventes, "data/interim/ventes.parquet")

# Contrôles
ventes[, .(n_ventes = .N, prix_m2_median = round(median(prix_m2))),
       by = .(annee, type_local)][order(type_local, annee)]
