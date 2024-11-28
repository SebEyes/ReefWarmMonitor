customHeaderPanel <- function(title, windowTitle = title) {
  tagList(tags$head(
    tags$title(windowTitle),
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
    tags$h1(a(href = "www.someURLlogoLinksto.com"))
  ))
}

custom_theme = bs_theme(
  # Controls the default grayscale palette
  bg = "#FFFFFF",
  fg = "#0A0A0A",
  # Controls the accent (e.g., hyperlink, button, etc) colors
  primary = "#049DBF",
  secondary = "#BF9E60",
  base_font = c("Grandstander", "sans-serif"),
  heading_font = "'Helvetica Neue', Helvetica, sans-serif",
  # Can also add lower-level customization
  "input-border-color" = "#0D0D0D",
  `enable-shadows` = TRUE
)

ACF_explanation = function() {
  renderText(
    HTML(
      "Ce graphique montre la <b>fonction d'autocorrélation (ACF)</b> d'une variable sur une série temporelle.
    <br>L'ACF mesure à quel point les valeurs d'une variable à différents moments dans le temps sont corrélées entre elles.
    <br>En d'autres termes, elle nous indique si les valeur d'un jour donné est influencée par la valeur des jours précédents.
    <br>
    <br>Les <b>barres verticales</b> représentent la corrélation entre une variable à différents retards (ou \"lags\").
    <br>Le lag 0 montre une corrélation parfaite (1,0) car une variable est toujours corrélée avec elle-même.
    <br>Plus on avance vers la droite (avec des lags croissants), plus la corrélation diminue progressivement, mais elle peut rester significative sur un certain nombre de lags."
    )
  )
}