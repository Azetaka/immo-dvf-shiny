# app/app.R
# install.packages(c("shiny", "bslib", "data.table", "sf", "leaflet", "plotly", "DT"))

library(shiny)
library(bslib)
library(data.table)
library(sf)
library(leaflet)
library(plotly)
library(DT)

# Données ----------------------------------------------------------------
lire = function(nom) readRDS(file.path("data", paste0(nom, ".rds")))

france_annee = lire("france_annee")
dep_annee    = lire("dep_annee")
com_annee    = lire("com_annee")
france_trim  = lire("france_trim")
dep_trim     = lire("dep_trim")
com_trim     = lire("com_trim")
com_pieces   = lire("com_pieces")
com_deciles  = lire("com_deciles")
communes     = lire("communes")
geo_dep      = lire("geo_dep")
geo_com      = lire("geo_com")

seuil       = 10   # ventes minimum par commune et par année
seuil_trim  = 5    # ventes minimum par trimestre
annees      = sort(unique(france_annee$annee))
annee_min   = min(annees)
annee_max   = max(annees)
code_arrondissement = "^(751|6938|132[0-1])"

deps = as.data.table(st_drop_geometry(geo_dep))[code %in% dep_annee$code_departement][order(code)]
choix_dep = c("France entière" = "FR", setNames(deps$code, paste(deps$code, "—", deps$nom)))
nom_dep   = setNames(deps$nom, deps$code)

communes[, libelle := paste0(nom_commune, " (", code_postal, ")")]
choix_communes = setNames(communes$code_commune, communes$libelle)

choix_indicateur = c("Prix médian au m²" = "prix_m2_median",
                     "Évolution sur un an" = "evol_1an",
                     "Nombre de ventes" = "n_ventes")

# Mise en forme ----------------------------------------------------------
fmt_nombre = function(x) ifelse(is.na(x), "—", formatC(round(x), format = "d", big.mark = " "))
fmt_pct = function(x) {
  ifelse(is.na(x), "—",
         paste0(ifelse(x > 0, "+", ""), formatC(100 * x, format = "f", digits = 1, decimal.mark = ","), " %"))
}
fmt_prix_m2 = function(x) ifelse(is.na(x), "—", paste(fmt_nombre(x), "€/m²"))

couleur_accent = "#1F4E8C"
couleurs_type  = c(Appartement = "#1F4E8C", Maison = "#D9822B")
pal_seq = c("#E5EDF7", "#B9CDE8", "#7FA3D1", "#3F6FAE", "#163A6B")
pal_div = c("#B35806", "#E08214", "#FDDBB0", "#B9CDE8", "#3F6FAE", "#163A6B")
gris    = "#D6D6D0"

# Classes de couleur d'un indicateur (quintiles, ou seuils fixes pour l'évolution)
classer = function(x, indicateur) {
  if (indicateur == "evol_1an") {
    bornes = c(-Inf, -0.05, -0.02, 0, 0.02, 0.05, Inf)
    etiquettes = c("Baisse de plus de 5 %", "Baisse de 2 à 5 %", "Baisse de moins de 2 %",
                   "Hausse de moins de 2 %", "Hausse de 2 à 5 %", "Hausse de plus de 5 %")
    couleurs = pal_div
  } else {
    bornes = unique(quantile(x, seq(0, 1, 0.2), na.rm = TRUE, names = FALSE))
    bornes = bornes[!is.na(bornes)]
    if (length(bornes) < 2) {
      return(list(classe = factor(rep(NA, length(x)), levels = "—"), couleurs = c("—" = gris)))
    }
    unite = if (indicateur == "prix_m2_median") " €" else ""
    b = fmt_nombre(bornes)
    etiquettes = paste0(b[-length(b)], " – ", b[-1], unite)
    couleurs = pal_seq[round(seq(1, 5, length.out = length(etiquettes)))]
  }
  list(classe   = cut(x, bornes, labels = etiquettes, include.lowest = TRUE),
       couleurs = setNames(couleurs, etiquettes))
}

# Fond de carte + polygones colorés
carte_choroplethe = function(d, indicateur, titre, selection = character(0)) {
  cl  = classer(d$valeur, indicateur)
  pal = colorFactor(unname(cl$couleurs), levels = names(cl$couleurs), na.color = gris)
  sel = d$code %in% selection

  leaflet(d, options = leafletOptions(minZoom = 4)) |>
    addProviderTiles(providers$Esri.WorldGrayCanvas) |>
    addPolygons(
      layerId     = ~code,
      fillColor   = pal(cl$classe),
      fillOpacity = 0.9,
      color       = ifelse(sel, "#1B1F24", "#FFFFFF"),
      weight      = ifelse(sel, 3, 0.7),
      label       = d$etiquette,
      highlightOptions = highlightOptions(weight = 2, color = "#1B1F24", bringToFront = TRUE)
    ) |>
    addLegend("bottomleft", colors = c(unname(cl$couleurs), gris),
              labels = c(names(cl$couleurs), "Moins de 10 ventes ou non couvert"),
              title = titre, opacity = 0.9)
}

etiquettes_carte = function(nom, prix, n, evol) {
  lapply(sprintf("<b>%s</b><br>%s · %s ventes<br>Sur un an : %s",
                 nom, fmt_prix_m2(prix), fmt_nombre(n), fmt_pct(evol)),
         htmltools::HTML)
}

graphe_vide = function(message) {
  plotly_empty(type = "scatter", mode = "markers") |>
    layout(annotations = list(text = message, showarrow = FALSE,
                              font = list(size = 14, color = "#5B616B")),
           xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)) |>
    config(displayModeBar = FALSE)
}

langue_dt = list(
  search = "Rechercher :", lengthMenu = "Afficher _MENU_ lignes",
  info = "_START_ à _END_ sur _TOTAL_", infoEmpty = "Aucune ligne",
  infoFiltered = "(filtré sur _MAX_)", zeroRecords = "Aucun résultat",
  paginate = list(previous = "Précédent", `next` = "Suivant")
)

# Thème ------------------------------------------------------------------
theme = bs_theme(
  version   = 5,
  primary   = couleur_accent,
  bg        = "#F6F7F9",
  fg        = "#1B1F24",
  base_font = font_google("IBM Plex Sans", local = FALSE),
  code_font = font_google("IBM Plex Mono", local = FALSE)
)

css = "
.bslib-value-box .value-box-value { font-size: 1.6rem; }
.bslib-value-box .value-box-title { font-size: 0.9rem; }
.barre-filtres { display: flex; flex-wrap: wrap; gap: 0 1.5rem; align-items: flex-end; }
.barre-filtres .form-group { margin-bottom: 0.5rem; }
.etape { font-size: 0.8rem; font-weight: 600; color: #5B616B; }
.badge-outil { background: #E5EDF7; color: #163A6B; border-radius: 999px; padding: 4px 10px;
               margin: 0 6px 6px 0; display: inline-block; font-size: 0.85rem; }
"

# Interface --------------------------------------------------------------
onglet_vue = nav_panel(
  "Vue d'ensemble",
  layout_sidebar(
    sidebar = sidebar(
      title = "Filtres", width = 280,
      radioButtons("type", "Type de bien", names(couleurs_type), inline = TRUE),
      selectInput("dep", "Département", choix_dep),
      selectInput("annee", "Année", rev(annees)),
      helpText("Une commune n'est affichée que si elle compte au moins 10 ventes sur l'année."),
      downloadLink("telecharger_vue", "Télécharger les communes filtrées (CSV)")
    ),

    layout_columns(
      fill = FALSE,
      value_box("Prix médian au m²", textOutput("kpi_prix"), textOutput("kpi_prix_sous")),
      value_box("Évolution sur un an", textOutput("kpi_evol"), textOutput("kpi_evol_sous")),
      value_box("Nombre de ventes", textOutput("kpi_ventes"), "Ventes retenues"),
      value_box("Commune la plus chère", textOutput("kpi_top"), textOutput("kpi_top_sous"))
    ),

    layout_columns(
      card(card_header("Prix médian au m² par département"),
           leafletOutput("carte_dep", height = 420)),
      card(card_header("Évolution du prix médian au m²"),
           plotlyOutput("courbes", height = 420))
    ),

    card(card_header(textOutput("titre_table", inline = TRUE)),
         DTOutput("table_communes"))
  )
)

onglet_carte = nav_panel(
  "Carte",
  card(
    fill = FALSE,
    div(class = "barre-filtres",
        radioButtons("c_type", "Type de bien", names(couleurs_type), inline = TRUE),
        selectInput("c_annee", "Année", rev(annees), width = "120px"),
        selectInput("c_ind", "Indicateur", choix_indicateur, width = "220px"),
        radioButtons("c_maille", "Maille", c("Départements", "Communes"), inline = TRUE),
        selectInput("c_dep", "Zoomer sur", choix_dep, width = "260px"))
  ),
  layout_columns(
    col_widths = c(8, 4),
    card(uiOutput("c_note"), leafletOutput("carte", height = 640)),
    div(
      card(card_header("Commune sélectionnée"), uiOutput("mini_fiche")),
      card(card_header(textOutput("titre_hausses", inline = TRUE)), uiOutput("hausses"))
    )
  )
)

onglet_communes = nav_panel(
  "Communes",
  card(
    fill = FALSE,
    div(class = "barre-filtres",
        selectizeInput("f_commune", "Commune", choices = NULL, width = "300px",
                       options = list(placeholder = "Nom ou code postal")),
        radioButtons("f_type", "Type de bien", names(couleurs_type), inline = TRUE),
        selectInput("f_annee", "Année", rev(annees), width = "120px"),
        selectInput("f_comp", "Comparer avec",
                    c("Son département" = "dep", "France entière" = "fr", "Une autre commune" = "com"),
                    width = "200px"),
        conditionalPanel("input.f_comp == 'com'",
                         selectizeInput("f_commune2", "Seconde commune", choices = NULL, width = "300px")))
  ),
  h3(textOutput("f_titre"), class = "mt-2 mb-0"),
  p(textOutput("f_sous_titre"), class = "text-muted"),

  layout_columns(
    fill = FALSE,
    value_box("Appartements", textOutput("f_kpi_apt"), textOutput("f_kpi_apt_sous")),
    value_box("Maisons", textOutput("f_kpi_mai"), textOutput("f_kpi_mai_sous")),
    value_box(textOutput("f_kpi_evol_titre"), textOutput("f_kpi_evol"), textOutput("f_kpi_evol_sous")),
    value_box("Prix médian d'un bien", textOutput("f_kpi_bien"), textOutput("f_kpi_bien_sous")),
    value_box("Rang", textOutput("f_kpi_rang"), textOutput("f_kpi_rang_sous"))
  ),

  layout_columns(
    col_widths = c(7, 5), fill = FALSE,
    card(card_header("Prix médian au m², par trimestre"), plotlyOutput("f_trim", height = 320)),
    card(card_header("Distribution des prix au m²"), plotlyOutput("f_distrib", height = 320))
  ),

  layout_columns(
    col_widths = c(5, 7), fill = FALSE,
    card(card_header("Prix médian au m² selon le nombre de pièces"), plotlyOutput("f_pieces", height = 300)),
    card(card_header(class = "d-flex justify-content-between",
                     "Historique annuel",
                     downloadLink("telecharger_commune", "Exporter (CSV)")),
         DTOutput("f_historique"))
  )
)

n_ventes_total = sum(france_annee$n_ventes)
n_communes     = uniqueN(com_annee[!grepl(code_arrondissement, code_commune), code_commune])

onglet_methodo = nav_panel(
  "Méthodologie",
  div(
    class = "container", style = "max-width: 1000px;",
    h2("Données et méthode", class = "mt-3"),

    layout_columns(
      fill = FALSE,
      card(span("1 · Source", class = "etape"), "Fichiers DVF géolocalisés annuels"),
      card(span("2 · Filtrage", class = "etape"), "Ventes d'un seul logement"),
      card(span("3 · Prix au m²", class = "etape"), "Valeur foncière / surface bâtie"),
      card(span("4 · Agrégation", class = "etape"), "Commune, département, France")
    ),

    card(
      card_header("Source"),
      p("Demandes de valeurs foncières (DVF) géolocalisées, publiées par la DGFiP et mises en forme par
         Etalab sur data.gouv.fr. Le fichier couvre les cinq dernières années et il est mis à jour en avril et
         en octobre. L'Alsace, la Moselle et Mayotte ne sont pas couvertes."),
      p(sprintf("Période retenue : %d à %d. Ventes retenues : %s, réparties sur %s communes.",
                annee_min, annee_max, fmt_nombre(n_ventes_total), fmt_nombre(n_communes)))
    ),

    card(
      card_header("Règles de préparation"),
      tags$ul(
        tags$li("Seules les mutations de nature « Vente » sont gardées."),
        tags$li("Une mutation peut regrouper plusieurs biens pour un prix global. On ne garde que celles qui
                 portent sur un seul appartement ou une seule maison, sans local professionnel. Les dépendances
                 et les terrains associés sont acceptés."),
        tags$li("Le prix au m² est la valeur foncière divisée par la surface réelle bâtie. Les biens de moins
                 de 9 m² sont écartés."),
        tags$li("Les 1 % de prix au m² les plus bas et les plus hauts sont retirés, par département, type de
                 bien et année."),
        tags$li("Les ventes des communes fusionnées sont rattachées à la commune actuelle, pour garder des
                 séries comparables d'une année à l'autre."),
        tags$li("Paris, Lyon et Marseille sont disponibles en commune entière et par arrondissement."),
        tags$li("Une commune n'est affichée pour une année que si elle compte au moins 10 ventes. Sur les
                 courbes trimestrielles, le seuil est de 5 ventes.")
      )
    ),

    card(
      card_header("Limites"),
      tags$ul(
        tags$li("Le prix déclaré peut inclure un terrain, des dépendances ou du mobilier, surtout pour les maisons."),
        tags$li("La médiane dépend des biens vendus chaque année. Ce n'est pas un indice à qualité constante
                 comme ceux des notaires ou de l'Insee."),
        tags$li("Dans les petites communes, le nombre de ventes reste faible et les prix varient beaucoup
                 d'une année à l'autre.")
      )
    ),

    layout_columns(
      fill = FALSE,
      card(
        card_header("Outils"),
        div(lapply(c("R", "data.table", "arrow", "sf", "Shiny", "bslib", "leaflet", "plotly", "DT", "shinyapps.io"),
                   function(x) span(x, class = "badge-outil")))
      ),
      card(
        card_header("Auteur"),
        p("Uriel FAGNINOU - Data analyst / Data scientist"),
        a("Code source sur GitHub", href = "https://github.com/Azetaka/immo-dvf-shiny", target = "_blank")
      )
    )
  )
)

ui = page_navbar(
  title  = "Prix de l'immobilier en France",
  id     = "onglets",
  theme  = theme,
  header = tags$head(tags$style(HTML(css))),
  onglet_vue,
  onglet_carte,
  onglet_communes,
  onglet_methodo,
  nav_spacer(),
  nav_item(span(sprintf("DVF %d–%d", annee_min, annee_max), class = "text-muted small"))
)

# Serveur ----------------------------------------------------------------
server = function(input, output, session) {

  # ===== Vue d'ensemble ================================================
  annee_sel = reactive(as.integer(input$annee))

  zone = reactive({
    if (input$dep == "FR") {
      france_annee[type_local == input$type]
    } else {
      dep_annee[code_departement == input$dep & type_local == input$type]
    }
  })

  ligne = reactive(zone()[annee == annee_sel()])

  com_zone = reactive({
    x = com_annee[type_local == input$type & annee == annee_sel() &
                    n_ventes >= seuil & !grepl(code_arrondissement, code_commune)]
    if (input$dep != "FR") x = x[code_departement == input$dep]
    communes[, .(code_commune, nom_commune)][x, on = "code_commune"]
  })

  output$kpi_prix      = renderText(paste(fmt_nombre(ligne()$prix_m2_median), "€"))
  output$kpi_prix_sous = renderText(paste0(input$type, "s · ", input$annee))
  output$kpi_evol      = renderText(fmt_pct(ligne()$evol_1an))
  output$kpi_evol_sous = renderText(paste(input$annee, "par rapport à", annee_sel() - 1))
  output$kpi_ventes    = renderText(fmt_nombre(ligne()$n_ventes))

  top = reactive(com_zone()[which.max(prix_m2_median)])
  output$kpi_top = renderText(if (nrow(top()) == 0) "—" else top()$nom_commune)
  output$kpi_top_sous = renderText({
    if (nrow(top()) == 0) "" else paste(fmt_prix_m2(top()$prix_m2_median), "·", top()$n_ventes, "ventes")
  })

  # Carte des départements
  output$carte_dep = renderLeaflet({
    x = dep_annee[type_local == input$type & annee == annee_sel()]
    d = merge(geo_dep, as.data.frame(x), by.x = "code", by.y = "code_departement", all.x = TRUE)
    d$valeur    = d$prix_m2_median
    d$etiquette = etiquettes_carte(paste(d$code, "—", d$nom), d$prix_m2_median, d$n_ventes, d$evol_1an)

    carte_choroplethe(d, "prix_m2_median", "€/m²", selection = input$dep) |>
      setView(lng = 2.4, lat = 46.6, zoom = 5)
  })

  observeEvent(input$carte_dep_shape_click, {
    updateSelectInput(session, "dep", selected = input$carte_dep_shape_click$id)
  })

  # Courbes appartements / maisons
  output$courbes = renderPlotly({
    s = if (input$dep == "FR") france_annee else dep_annee[code_departement == input$dep]

    plot_ly(s, x = ~annee, y = ~prix_m2_median, color = ~type_local, colors = couleurs_type,
            type = "scatter", mode = "lines+markers",
            line = list(width = 3), marker = list(size = 7),
            hovertemplate = "%{y:,.0f} €/m²<extra>%{fullData.name}</extra>") |>
      layout(separators = ", ",
             xaxis  = list(title = "", dtick = 1),
             yaxis  = list(title = "€/m²", tickformat = ",.0f"),
             legend = list(orientation = "h", y = -0.15),
             hovermode = "x unified") |>
      config(displayModeBar = FALSE)
  })

  # Tableau des communes
  output$titre_table = renderText({
    if (input$dep == "FR") "Communes de France" else paste("Communes du département", input$dep, "—", nom_dep[[input$dep]])
  })

  table_vue = reactive({
    com_zone()[order(-prix_m2_median),
               .(Commune = nom_commune, `Code INSEE` = code_commune,
                 `Prix médian €/m²` = round(prix_m2_median), Ventes = n_ventes, `Évol. 1 an` = evol_1an)]
  })

  output$table_communes = renderDT({
    datatable(table_vue(), rownames = FALSE, selection = "none",
              options = list(pageLength = 10, language = langue_dt)) |>
      formatRound("Prix médian €/m²", digits = 0, mark = " ") |>
      formatRound("Ventes", digits = 0, mark = " ") |>
      formatPercentage("Évol. 1 an", digits = 1, mark = " ", dec.mark = ",")
  })

  output$telecharger_vue = downloadHandler(
    filename = function() paste0("communes_", input$dep, "_", input$type, "_", input$annee, ".csv"),
    content  = function(file) fwrite(table_vue(), file, sep = ";", dec = ",", bom = TRUE)
  )

  # ===== Carte =========================================================
  commune_sel = reactiveVal(NULL)
  observeEvent(list(input$c_dep, input$c_maille), commune_sel(NULL))

  maille_communes = reactive(input$c_maille == "Communes" && input$c_dep != "FR")

  output$c_note = renderUI({
    if (input$c_maille == "Communes" && input$c_dep == "FR") {
      p("Choisissez un département (liste ou clic sur la carte) pour afficher ses communes.",
        class = "text-muted mb-1")
    }
  })

  carte_data = reactive({
    a = as.integer(input$c_annee)
    if (maille_communes()) {
      x = com_annee[type_local == input$c_type & annee == a & code_departement == input$c_dep]
      g = geo_com[geo_com$departement == input$c_dep, ]
      d = merge(g, as.data.frame(x), by.x = "code", by.y = "code_commune", all.x = TRUE)
      noms = d$nom
    } else {
      x = dep_annee[type_local == input$c_type & annee == a]
      d = merge(geo_dep, as.data.frame(x), by.x = "code", by.y = "code_departement", all.x = TRUE)
      noms = paste(d$code, "—", d$nom)
    }
    fiable      = !is.na(d$n_ventes) & d$n_ventes >= seuil
    d$valeur    = ifelse(fiable, d[[input$c_ind]], NA)
    d$etiquette = etiquettes_carte(noms, ifelse(fiable, d$prix_m2_median, NA), d$n_ventes,
                                   ifelse(fiable, d$evol_1an, NA))
    d
  })

  output$carte = renderLeaflet({
    d = carte_data()
    titre = names(choix_indicateur)[choix_indicateur == input$c_ind]
    selection = if (maille_communes()) commune_sel() else input$c_dep
    m = carte_choroplethe(d, input$c_ind, titre, selection = selection)

    if (input$c_dep == "FR") {
      m |> setView(lng = 2.4, lat = 46.6, zoom = 6)
    } else {
      bb = st_bbox(if (maille_communes()) d else d[d$code == input$c_dep, ])
      m |> fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    }
  })

  observeEvent(input$carte_shape_click, {
    id = input$carte_shape_click$id
    if (maille_communes()) commune_sel(id) else updateSelectInput(session, "c_dep", selected = id)
  })

  output$mini_fiche = renderUI({
    code = commune_sel()
    if (is.null(code)) {
      return(p("Passez en maille Communes, puis cliquez sur une commune pour afficher sa fiche.",
               class = "text-muted"))
    }
    info = communes[code_commune == code]
    x = com_annee[code_commune == code & type_local == input$c_type & annee == as.integer(input$c_annee)]
    if (nrow(x) == 0 || x$n_ventes < seuil) x = data.table(prix_m2_median = NA, n_ventes = x$n_ventes[1], evol_1an = NA)
    prix_dep = dep_annee[code_departement == input$c_dep & type_local == input$c_type &
                           annee == as.integer(input$c_annee), prix_m2_median]
    ecart = x$prix_m2_median / prix_dep - 1

    stat = function(titre, valeur) div(div(titre, class = "small text-muted"), div(valeur, class = "fs-5"))

    tagList(
      h4(if (nrow(info)) info$nom_commune else code, class = "mb-0"),
      p(paste0(nom_dep[[input$c_dep]], " · code INSEE ", code), class = "text-muted small"),
      layout_columns(
        col_widths = c(6, 6), fill = FALSE,
        stat("Prix médian", fmt_prix_m2(x$prix_m2_median)),
        stat("Ventes", fmt_nombre(x$n_ventes)),
        stat("Sur un an", fmt_pct(x$evol_1an)),
        stat("Écart au département", fmt_pct(ecart))
      ),
      plotlyOutput("mini_courbe", height = 130),
      actionButton("ouvrir_fiche", "Ouvrir la fiche complète", class = "btn-primary w-100 mt-2")
    )
  })

  output$mini_courbe = renderPlotly({
    req(commune_sel())
    s_com = com_annee[code_commune == commune_sel() & type_local == input$c_type & n_ventes >= seuil]
    s_dep = dep_annee[code_departement == input$c_dep & type_local == input$c_type]

    plot_ly() |>
      add_lines(data = s_dep, x = ~annee, y = ~prix_m2_median, name = "Département",
                line = list(color = "#6B7079", dash = "dash", width = 1.5),
                hovertemplate = "%{y:,.0f} €/m²<extra>Département</extra>") |>
      add_lines(data = s_com, x = ~annee, y = ~prix_m2_median, name = "Commune",
                line = list(color = couleur_accent, width = 2.5),
                hovertemplate = "%{y:,.0f} €/m²<extra>Commune</extra>") |>
      layout(separators = ", ", showlegend = FALSE, margin = list(l = 0, r = 0, t = 5, b = 0),
             xaxis = list(title = "", dtick = 1), yaxis = list(title = "", tickformat = ",.0f")) |>
      config(displayModeBar = FALSE)
  })

  observeEvent(input$ouvrir_fiche, {
    updateSelectizeInput(session, "f_commune", choices = choix_communes,
                         selected = commune_sel(), server = TRUE)
    updateRadioButtons(session, "f_type", selected = input$c_type)
    updateSelectInput(session, "f_annee", selected = input$c_annee)
    nav_select("onglets", "Communes")
  })

  output$titre_hausses = renderText({
    if (input$c_dep == "FR") "Plus fortes hausses par département" else "Plus fortes hausses du département"
  })

  output$hausses = renderUI({
    a = as.integer(input$c_annee)
    if (input$c_dep == "FR") {
      x = dep_annee[type_local == input$c_type & annee == a & !is.na(evol_1an)]
      x[, nom := paste(code_departement, "—", nom_dep[code_departement])]
    } else {
      x = com_annee[type_local == input$c_type & annee == a & code_departement == input$c_dep &
                      !is.na(evol_1an) & !grepl(code_arrondissement, code_commune)]
      x = communes[, .(code_commune, nom = nom_commune)][x, on = "code_commune"]
    }
    x = head(x[order(-evol_1an)], 5)
    if (nrow(x) == 0) return(p("Pas assez de ventes pour calculer une évolution.", class = "text-muted"))

    tagList(lapply(seq_len(nrow(x)), function(i) {
      div(class = "d-flex justify-content-between border-bottom py-2",
          span(x$nom[i]), span(fmt_pct(x$evol_1an[i]), class = "font-monospace"))
    }))
  })

  # ===== Communes ======================================================
  updateSelectizeInput(session, "f_commune", choices = choix_communes, selected = "75056", server = TRUE)
  updateSelectizeInput(session, "f_commune2", choices = choix_communes, selected = "69123", server = TRUE)

  code_f  = reactive({ req(input$f_commune, input$f_type, input$f_annee); input$f_commune })
  info_f  = reactive({ code = code_f(); communes[code_commune == code] })
  com_f   = reactive({ code = code_f(); com_annee[code_commune == code] })
  annee_f = reactive(as.integer(input$f_annee))

  output$f_titre = renderText(info_f()$nom_commune)
  output$f_sous_titre = renderText({
    i = info_f()
    paste0(i$code_departement, " — ", nom_dep[i$code_departement], " · code INSEE ", i$code_commune,
           " · code postal ", i$code_postal)
  })

  valeur_type = function(type) {
    x = com_f()[type_local == type & annee == annee_f()]
    if (nrow(x) == 0 || x$n_ventes < seuil) return(NULL)
    x
  }

  output$f_kpi_apt = renderText({ x = valeur_type("Appartement"); if (is.null(x)) "—" else fmt_prix_m2(x$prix_m2_median) })
  output$f_kpi_apt_sous = renderText({
    x = com_f()[type_local == "Appartement" & annee == annee_f()]
    paste(fmt_nombre(sum(x$n_ventes)), "ventes en", input$f_annee)
  })
  output$f_kpi_mai = renderText({ x = valeur_type("Maison"); if (is.null(x)) "—" else fmt_prix_m2(x$prix_m2_median) })
  output$f_kpi_mai_sous = renderText({
    x = com_f()[type_local == "Maison" & annee == annee_f()]
    paste(fmt_nombre(sum(x$n_ventes)), "ventes en", input$f_annee)
  })

  output$f_kpi_evol_titre = renderText(paste("Évolution depuis", annee_min))
  output$f_kpi_evol = renderText({
    x = com_f()[type_local == input$f_type & n_ventes >= seuil]
    debut = x[annee == annee_min, prix_m2_median]
    fin   = x[annee == annee_f(), prix_m2_median]
    if (length(debut) == 0 || length(fin) == 0 || annee_f() == annee_min) "—" else fmt_pct(fin / debut - 1)
  })
  output$f_kpi_evol_sous = renderText(paste0(input$f_type, "s, ", annee_min, " → ", input$f_annee))

  output$f_kpi_bien = renderText({
    x = valeur_type(input$f_type)
    if (is.null(x)) "—" else paste(fmt_nombre(x$prix_median), "€")
  })
  output$f_kpi_bien_sous = renderText({
    x = valeur_type(input$f_type)
    if (is.null(x)) paste0(input$f_type, "s : moins de 10 ventes") else paste0(input$f_type, "s · surface médiane ", fmt_nombre(x$surface_mediane), " m²")
  })

  rang = reactive({
    code = code_f()
    pool = com_annee[type_local == input$f_type & annee == annee_f() & n_ventes >= seuil]
    if (grepl(code_arrondissement, code)) {
      prefixe = if (startsWith(code, "6938")) "6938" else substr(code, 1, 3)
      pool = pool[startsWith(code_commune, prefixe)]
      cadre = "arrondissements"
    } else {
      pool = pool[code_departement == info_f()$code_departement & !grepl(code_arrondissement, code_commune)]
      cadre = "communes du département"
    }
    pool = pool[order(-prix_m2_median)]
    list(position = match(code, pool$code_commune), total = nrow(pool), cadre = cadre)
  })

  output$f_kpi_rang = renderText({
    r = rang()
    if (is.na(r$position)) "—" else paste(r$position, "/", r$total)
  })
  output$f_kpi_rang_sous = renderText(paste0("Parmi les ", rang()$cadre, " (", tolower(input$f_type), "s)"))

  # Courbe trimestrielle
  output$f_trim = renderPlotly({
    preparer = function(dt, nom) {
      x = dt[type_local == input$f_type]
      x[, `:=`(date  = as.Date(sprintf("%d-%02d-15", annee, (trimestre - 1) * 3 + 2)),
               texte = paste0("T", trimestre, " ", annee),
               prix  = fifelse(n_ventes >= seuil_trim, prix_m2_median, NA_real_),
               serie = nom)]
      x[order(date)]
    }

    code = code_f()
    nom_com = info_f()$nom_commune
    series = list(preparer(com_trim[code_commune == code], nom_com))
    if (input$f_comp == "dep") {
      series = c(series, list(preparer(dep_trim[code_departement == info_f()$code_departement], "Département")))
    }
    if (input$f_comp == "com" && isTruthy(input$f_commune2)) {
      nom2 = communes[code_commune == input$f_commune2, nom_commune]
      series = c(series, list(preparer(com_trim[code_commune == input$f_commune2], nom2)))
    }
    series = c(series, list(preparer(france_trim, "France")))
    s = rbindlist(series, fill = TRUE)
    if (all(is.na(s$prix[s$serie == nom_com]))) return(graphe_vide("Pas assez de ventes trimestrielles"))

    styles = list(list(color = couleur_accent, width = 3),
                  list(color = "#D9822B", width = 2, dash = "dash"),
                  list(color = "#D9822B", width = 2.5))
    noms_series = unique(s$serie)

    p = plot_ly()
    for (i in seq_along(noms_series)) {
      p = p |> add_trace(data = s[serie == noms_series[i]], x = ~date, y = ~prix, text = ~texte,
                         name = noms_series[i], type = "scatter", mode = "lines",
                         line = styles[[min(i, 3)]], connectgaps = FALSE,
                         hovertemplate = "%{text} : %{y:,.0f} €/m²<extra>%{fullData.name}</extra>")
    }
    p |>
      layout(separators = ", ",
             xaxis = list(title = "", tickformat = "%Y", dtick = "M12"),
             yaxis = list(title = "€/m²", tickformat = ",.0f"),
             legend = list(orientation = "h", y = -0.15)) |>
      config(displayModeBar = FALSE)
  })

  # Distribution à partir des déciles
  output$f_distrib = renderPlotly({
    code = code_f()
    x = com_deciles[code_commune == code & type_local == input$f_type & annee == annee_f()]
    if (nrow(x) == 0) return(graphe_vide("Moins de 10 ventes cette année"))

    d = unlist(x[, paste0("d", seq(0, 100, 10)), with = FALSE])
    largeur = diff(d)
    largeur[largeur <= 0] = 1
    barres = data.table(
      milieu  = d[-11] + largeur / 2,
      largeur = largeur,
      hauteur = 10 / (largeur / 100),
      texte   = paste0(fmt_nombre(d[-11]), " – ", fmt_nombre(d[-1]), " €/m² : 10 % des ventes")
    )

    plot_ly(barres) |>
      add_bars(x = ~milieu, y = ~hauteur, width = ~largeur,
               marker = list(color = "#7FA3D1", line = list(color = "#FFFFFF", width = 1)),
               text = ~texte, textposition = "none", hovertemplate = "%{text}<extra></extra>") |>
      layout(separators = ", ", bargap = 0,
             xaxis = list(title = "€/m²", tickformat = ",.0f"),
             yaxis = list(title = "% des ventes par 100 €/m²"),
             shapes = list(list(type = "line", x0 = d[6], x1 = d[6], y0 = 0, y1 = 1, yref = "paper",
                                line = list(color = "#1B1F24", dash = "dot"))),
             annotations = list(list(x = d[6], y = 1, yref = "paper", text = paste("Médiane", fmt_nombre(d[6]), "€"),
                                     showarrow = FALSE, xanchor = "left", yanchor = "bottom"))) |>
      config(displayModeBar = FALSE)
  })

  # Prix par nombre de pièces
  output$f_pieces = renderPlotly({
    code = code_f()
    x = com_pieces[code_commune == code & type_local == input$f_type &
                     annee == annee_f() & n_ventes >= seuil_trim]
    if (nrow(x) == 0) return(graphe_vide("Pas assez de ventes"))
    x[, pieces := factor(pieces, levels = rev(c("T1", "T2", "T3", "T4", "T5 et +")))]

    plot_ly(x, x = ~prix_m2_median, y = ~pieces, type = "bar", orientation = "h",
            marker = list(color = couleur_accent),
            text = ~paste(fmt_nombre(prix_m2_median), "€"), textposition = "inside",
            insidetextanchor = "end", textfont = list(color = "#FFFFFF"),
            customdata = ~n_ventes,
            hovertemplate = "%{y} : %{x:,.0f} €/m² (%{customdata} ventes)<extra></extra>") |>
      layout(separators = ", ", xaxis = list(title = "€/m²", tickformat = ",.0f"), yaxis = list(title = "")) |>
      config(displayModeBar = FALSE)
  })

  # Historique
  historique = reactive({
    com_f()[order(-annee, type_local),
            .(`Année` = annee, Type = type_local,
              `€/m² médian` = round(prix_m2_median),
              `Q1 – Q3` = paste(fmt_nombre(prix_m2_q1), "–", fmt_nombre(prix_m2_q3)),
              Ventes = n_ventes, `Évol. 1 an` = evol_1an)]
  })

  output$f_historique = renderDT({
    datatable(historique(), rownames = FALSE, selection = "none",
              options = list(dom = "t", pageLength = 20, language = langue_dt)) |>
      formatRound("€/m² médian", digits = 0, mark = " ") |>
      formatPercentage("Évol. 1 an", digits = 1, mark = " ", dec.mark = ",")
  })

  output$telecharger_commune = downloadHandler(
    filename = function() paste0("historique_", code_f(), ".csv"),
    content  = function(file) fwrite(historique(), file, sep = ";", dec = ",", bom = TRUE)
  )
}

shinyApp(ui, server)
