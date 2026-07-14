ffn_colors <- list(darkgray = "#58595B",
                   gold = "#EFA923",
                   offwhite = "#FAEED3",
                   lightgray = "#939598")

onclick_js_columns <- function(df) {
  mutate(df,
         tooltip_title = paste0("Legislative District ", District),
         onclick_js = paste0(
           "showFrozenTooltip(event, ",
           sapply(tooltip_title,  toJSON, auto_unbox = TRUE), ", ",
           sapply(Full.Name,      toJSON, auto_unbox = TRUE), ", ",
           sapply(Sen.Page,       toJSON, auto_unbox = TRUE), ", ",
           sapply(Sen.Page,       toJSON, auto_unbox = TRUE),  # second URL
           ")"
         )
  )
}

add_tooltip_cols <- function(df) {
  df |> 
    left_join(df_tooltip, by = "District") |>
    onclick_js_columns()
}

tooltip_html <- function(district, url1, url2, sen_name) {
  tooltip_title <- paste0("Legislative District ", district)
  paste0(
    "<h2>", tooltip_title, "</h2>",
    "<a href=\"", url1, "\" target=\"_blank\">", sen_name, "</a><br>",
    "<a href=\"", url2, "\" target=\"_blank\">", "Second URL Link", "</a>"
  )
}


build_nebraska_ggplot <- function(){
  ggplot() +
    # County lines
    geom_sf(data = ne_county,
            fill = ffn_colors$offwhite,
            color = alpha(ffn_colors$lightgray, 0.5)) +
    # LD35 connector line
    geom_segment(data = connector_ld35,
                 aes(x = x_start, y = y_start,
                     xend = x_end, yend = y_end),
                 color = ffn_colors$darkgray,
                 linewidth = 0.75) +
    # LD polygons
    ggiraph::geom_sf_interactive(data = ne_legis,
                                 aes(data_id = District,
                                     tooltip = tooltip_html(District, 
                                                            Sen.Page, 
                                                            Sen.Page, 
                                                            Full.Name),
                                     onclick = onclick_js),
                                 fill = NA,
                                 color = ffn_colors$gold,
                                 linewidth = 0.5) +
    # LD35 label
    geom_sf_text_interactive(data = label_ld35_geom,
                             aes(label = District,
                                 data_id = District,
                                 onclick = onclick_js),
                             color = ffn_colors$darkgray,
                             fontface = "bold") +
    # LD labels (Greater NE, except 35)
    geom_sf_text_interactive(data = greaterne_label_geom,
                             aes(label = District,
                                 data_id = District,
                                 onclick = onclick_js),
                             color = ffn_colors$darkgray,
                             fontface = "bold") +
    coord_sf(expand = FALSE) +
    theme_void(base_family = "Plus Jakarta Sans")
}

build_metro_ggplot <- function(base_data, label_data, title){
  
  ggplot() +
    # LD polygons
    ggiraph::geom_sf_interactive(data = base_data,
                                 aes(data_id = District,
                                     tooltip = tooltip_html(District, 
                                                            Sen.Page, 
                                                            Sen.Page, 
                                                            Full.Name),
                                     onclick = onclick_js),
                                 fill = ffn_colors$offwhite,
                                 color = ffn_colors$gold,
                                 linewidth = 0.5) +
    # LD labels
    geom_sf_text_interactive(data = label_data,
                             aes(label = District,
                                 data_id = District,
                                 onclick = onclick_js),
                             color = ffn_colors$darkgray,
                             fontface = "bold") +
    coord_sf(expand = FALSE) +
    theme_void(base_family = "Plus Jakarta Sans") +
    labs(title = title) +
    theme(plot.title = element_text(hjust = 0.5,
                                    family = "Plus Jakarta Sans"))
}
