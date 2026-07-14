library(tidyverse)
library(DT)
library(htmltools)

# Data Frame -------------------------------------------------------------------
senators_roster <- readRDS("data/senators_roster.rds")


df_datatable <- senators_roster |>
  mutate(Senator = paste0("<a href=\"", Sen.Page, 
                          "\" target = \"_blank\" rel=\"noopener noreferrer\">",
                          Abbr.Name, "</a>")) |> 
  # Change to correct URL column
  mutate(`District Profile` = paste0("<a href=\"", Sen.Page, 
                                     "\" target = \"_blank\" rel=\"noopener noreferrer\">",
                                     "Link to District ", District, " Profile</a>")) |> 
  select(District, Senator, `District Profile`) |> 
  mutate(District = as.character(District))

# Datatable --------------------------------------------------------------------

header_tags <- lapply(names(df_datatable), tags$th)
header_container <- tags$table(class = "display",
                               tags$thead(tags$tr(header_tags)))

dt <- datatable(df_datatable, 
                escape = FALSE, 
                rownames = FALSE,
                filter = "top",
                container = header_container,
                options = list(
                  dom = "ltp",
                  lengthMenu = c(10, 25, 49)
                )) |> 
  formatStyle(
    columns = "District", 
    `text-align` = "center")

# Final HTML -------------------------------------------------------------------

final_html <- tags$html(
  tags$head(
    tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
    tags$link(rel="preconnect", href="https://fonts.gstatic.com", 
              crossorigin = "anonymous"),
    tags$link(rel="stylesheet",
              href="https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:ital,wght@0,200..800;1,200..800&display=swap"),
    tags$link(rel = "stylesheet",
              type = "text/css",
              href = "css/style.css")
    ),
  tags$body(
    tags$div(dt)
  )
)

# Save HTML --------------------------------------------------------------------

htmltools::save_html(final_html, "table.html") |> 
  suppressWarnings()
