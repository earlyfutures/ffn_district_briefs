library(ggplot2)
library(dplyr)
library(ggiraph)
library(patchwork)
library(sf)
library(htmltools)
library(jsonlite)

# Spatial Data -----------------------------------------------------------------

load("./data/legis_objects.rda")
source("./scripts/functions.R")

# Add LD URL Links 
senators_roster <- readRDS("./data/senators_roster.rds")

df_tooltip <- senators_roster[,c("District","Sen.Page", "Full.Name")]

ne_legis <- add_tooltip_cols(ne_legis)
legis_lincoln <- add_tooltip_cols(legis_lincoln)
legis_omaha <- add_tooltip_cols(legis_omaha)
greaterne_label_geom <- add_tooltip_cols(greaterne_label_geom)
lincoln_label_geom <- add_tooltip_cols(lincoln_label_geom)
omaha_label_geom <- add_tooltip_cols(omaha_label_geom)
label_ld35_geom <- add_tooltip_cols(label_ld35_geom)

# Individual Plots -------------------------------------------------------------

p1 <- build_nebraska_ggplot()

p2 <- build_metro_ggplot(base_data = legis_lincoln,
                         label_data = lincoln_label_geom,
                         title = "Lincoln")

p3 <- build_metro_ggplot(base_data = legis_omaha,
                         label_data = omaha_label_geom,
                         title = "Omaha")

# Combined Plot ----------------------------------------------------------------

combined_maps <- p1 / (p2 + p3 
                       & theme(plot.margin = margin(t = 3, r = 0, b = 0, l = 15, unit = "pt"),
                               plot.title = element_text(hjust = 0.5,
                                                         family = "Plus Jakarta Sans",
                                                         margin = margin(b = 5, unit = "pt")))
                       ) +
  plot_layout(heights = c(1, 1.1)) +
  plot_annotation(
    title = "Nebraska Legislative Districts",
    theme = theme(plot.title = element_text(hjust = 0.5,
                                            family = "Plus Jakarta Sans")))

# Custom CSS for ggiraph render ------------------------------------------------

hover_css <- girafe_css(
  css  = "cursor: pointer;",
  area = "fill:#58595B; fill-opacity:1; stroke:#58595B; stroke-width:0.5px;",
  text = "fill: white; font-weight: 700;"
)

# Matches most of the style for frozen_tooltip JS
tooltip_css <- "
  background-color: white;
  border: 1px solid #F5F5F5;
  box-shadow: 0 3px 3px rgba(0,0,0,0.3);
  font-size: 14px;
  font-family: 'Plus Jakarta Sans', sans-serif;
  max-width: 240px;
  padding: 10px;
  z-index: 9999;
"

ggirafe_plot <- girafe(
  ggobj = combined_maps,
  width_svg = 9,
  height_svg = 8,
  options = list(
    opts_hover(css = hover_css),
    opts_tooltip(css = tooltip_css,
                 opacity = 1),
    opts_sizing(rescale = FALSE)
  )
) |> 
  suppressWarnings()

# Final HTML -------------------------------------------------------------------

final_html <- tags$html(
  tags$head(
    tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
    tags$link(rel="preconnect", href="https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:ital,wght@0,200..800;1,200..800&display=swap"),
    tags$link(rel = "stylesheet", href = "css/style.css"),
    tags$link(rel = "stylesheet", href = "css/nebr-map.css"),
    tags$script(src = "js/nebr-map.js")
  ),
  tags$body(
    tags$div(ggirafe_plot)
  )
)

# Save HTML --------------------------------------------------------------------

htmltools::save_html(final_html, "./map.html") |> 
  suppressWarnings()

