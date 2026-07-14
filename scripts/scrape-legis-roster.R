library(tidyverse)
library(pdftools)

roster_url <- "https://nebraskalegislature.gov/FloorDocs/Current/PDF/Roster/roster.pdf"

pdf <- pdf_text(roster_url)

roster <- pdf[38] |> # select PDF page range where their names are printed
  str_split("\n") |> 
  unlist() %>% 
  str_split_fixed("\\s{1,2}", 3) %>%
  .[4:20,3] |> 
  str_trim() |> 
  str_split_fixed("   +", 3) |> 
  as.character() |> 
  as_tibble() |> 
  filter(value != "") |> 
  mutate(District = str_extract(value, "^\\d+") |> as.integer()) |> 
  mutate(value = str_remove(value, "^\\d+") |> str_trim()) |> 
  mutate(clean = str_remove(value, "\\s+(Jr\\.)$"),
         last_name = str_extract(clean, "(?:[vV]on\\s)?[A-Z][A-Za-z-]+$"),
         first_initial = str_sub(clean, 1, 1)) |> 
  group_by(last_name) |> 
  mutate(n = n()) |> 
  ungroup() |> 
  mutate(Abbr.Name = if_else(n > 1,
                        paste0("Sen. ",first_initial, ". ", last_name),
                        paste0("Sen. ", last_name)),
         Full.Name = paste0("Sen. ", clean)) |> 
  select(District, Abbr.Name, Full.Name) |> 
  mutate(Sen.Page = paste0("https://nebraskalegislature.gov/senators/landing-pages/index.php?District=",
                           District))

saveRDS(roster, file = "./data/senators_roster.rds")
