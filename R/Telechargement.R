options(timeout = 1800)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

url_base = "https://files.data.gouv.fr/geo-dvf/latest/csv"
annees = 2020:2026

for (a in annees) {
  dest = file.path("data/raw", paste0("dvf_", a, ".csv.gz"))
  if (file.exists(dest)) {
    message(a, " : déjà présent")
    next
  }
  url = paste0(url_base, "/", a, "/full.csv.gz")
  ok = tryCatch({
    download.file(url, dest, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok) {
    if (file.exists(dest)) file.remove(dest)
    message(a, " : non disponible")
  } else {
    message(a, " : OK (", round(file.size(dest) / 1e6), " Mo)")
  }
}

list.files("data/raw")
