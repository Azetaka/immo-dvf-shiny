# app/app.R
# install.packages(c("shiny", "bslib"))

library(shiny)
library(bslib)
library(data.table)
library(sf)

# Données ----------------------------------------------------------------
lire = function(nom) readRDS(file.path("data", paste0(nom, ".rds")))

france_annee = lire("france_annee")
dep_annee    = lire("dep_annee")
com_annee    = lire("com_annee")
communes     = lire("communes")
geo_dep      = lire("geo_dep")

seuil  = 10
annees = sort(unique(france_annee$annee))

deps = as.data.table(st_drop_geometry(geo_dep))[code %in% dep_annee$code_departement][order(code)]
choix_dep = c("France entière" = "FR", setNames(deps$code, paste(deps$code, "—", deps$nom)))

code_arrondissement = "^(751|6938|132[0-1])"

# Mise en forme ----------------------------------------------------------
fmt_nombre = function(x) format(round(x), big.mark = " ", scientific = FALSE)
fmt_pct = function(x) {
  if (is.na(x)) return("—")
  paste0(ifelse(x > 0, "+", ""), format(round(100 * x, 1), decimal.mark = ","), " %")
}

# Thème ------------------------------------------------------------------
theme = bs_theme(
  version   = 5,
  primary   = "#0E6464",
  bg        = "#F5F5F2",
  fg        = "#1B1F24",
  base_font = font_google("IBM Plex Sans"),
  code_font = font_google("IBM Plex Mono")
)

# Interface --------------------------------------------------------------
ui = page_navbar(
  title = "Prix de l'immobilier en France",
  theme = theme,

  nav_panel("Vue d'ensemble",
            layout_sidebar(
              sidebar = sidebar(
                title = "Filtres", width = 280,
                radioButtons("type", "Type de bien", c("Appartement", "Maison"), inline = TRUE),
                selectInput("dep", "Département", choix_dep),
                selectInput("annee", "Année", rev(annees)),
                helpText("Une commune n'est affichée que si elle compte au moins 10 ventes sur l'année.")
              ),

              layout_columns(
                fill = FALSE,
                value_box("Prix médian au m²", textOutput("kpi_prix"), textOutput("kpi_prix_sous")),
                value_box("Évolution sur un an", textOutput("kpi_evol"), textOutput("kpi_evol_sous")),
                value_box("Nombre de ventes", textOutput("kpi_ventes"), "Ventes retenues"),
                value_box("Commune la plus chère", textOutput("kpi_top"), textOutput("kpi_top_sous"))
              ),

              layout_columns(
                card(card_header("Prix médian au m² par département"), "Carte — bloc suivant"),
                card(card_header("Évolution du prix médian au m²"), "Courbes — bloc suivant")
              ),

              card(card_header("Communes"), "Tableau — bloc suivant")
            )
  ),

  nav_panel("Carte", "À venir"),
  nav_panel("Communes", "À venir"),
  nav_panel("Méthodologie", "À venir")
)

# Serveur ----------------------------------------------------------------
server = function(input, output, session) {

  annee_sel = reactive(as.integer(input$annee))

  # Série annuelle de la zone sélectionnée (France ou département)
  zone = reactive({
    if (input$dep == "FR") {
      france_annee[type_local == input$type]
    } else {
      dep_annee[code_departement == input$dep & type_local == input$type]
    }
  })

  ligne = reactive(zone()[annee == annee_sel()])

  # Communes de la zone pour l'année (arrondissements PLM exclus)
  com_zone = reactive({
    x = com_annee[type_local == input$type & annee == annee_sel() &
                    n_ventes >= seuil & !grepl(code_arrondissement, code_commune)]
    if (input$dep != "FR") x = x[code_departement == input$dep]
    communes[, .(code_commune, nom_commune)][x, on = "code_commune"]
  })

  output$kpi_prix      = renderText(paste(fmt_nombre(ligne()$prix_m2_median), "€"))
  output$kpi_prix_sous = renderText(paste0(input$type, "s · ", input$annee))

  output$kpi_evol      = renderText(fmt_pct(ligne()$evol_1an))
  output$kpi_evol_sous = renderText(paste(input$annee, "vs", annee_sel() - 1))

  output$kpi_ventes    = renderText(fmt_nombre(ligne()$n_ventes))

  output$kpi_top = renderText({
    top = com_zone()[which.max(prix_m2_median)]
    if (nrow(top) == 0) "—" else top$nom_commune
  })
  output$kpi_top_sous = renderText({
    top = com_zone()[which.max(prix_m2_median)]
    if (nrow(top) == 0) "" else paste(fmt_nombre(top$prix_m2_median), "€/m² ·", top$n_ventes, "ventes")
  })
}

shinyApp(ui, server)
