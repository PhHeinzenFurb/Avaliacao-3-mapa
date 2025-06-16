library(wbstats)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(gganimate)
library(gifski)
library(ggthemes)
library(extrafont)

# códigos a serem utilizados
# Exportações e Importações de Bens e Serviços (% do PIB) 
# (NE.EXP.GNFS.ZS,NE.IMP.GNFS.ZS)

my_ids <- c("Exportacao (%PIB)" = "NE.EXP.GNFS.ZS",
            "Importacao (%PIB)" = "NE.IMP.GNFS.ZS")

# extraindo dados do banco mundial
exp_imp_df <- wb_data(
  indicator = my_ids,
  country = "countries_only",
  start_date = 1980, end_date = 2023
)

# extraindo dados geoespaciais dos paises
europe_map <- ne_countries(scale = "small", type = "countries",
                          continent = "Europe") |>
  select(iso_a2, name, subregion)

# juntando exp_imp_df e world_map
exp_imp_map_eu <- exp_imp_df |>
  inner_join(europe_map, by = c("iso2c" = "iso_a2")) |>
  st_as_sf()

# utilizando Lambert Conformal Conic para o mapa da europa
exp_imp_map_lamb <- st_transform(exp_imp_map_eu, crs = 3035)

font_import()

# criando plot de mapa
p <- exp_imp_map_lamb |>
  filter(date >= 1985,
         date <= 2000) |>
  ggplot() +
  geom_sf(aes(fill = `Importacao (%PIB)`)) +
  labs(
    title = "Taxas de Importação na Europa entre 1985 - 2000 (% do PIB)",
    subtitle = "Ano: {current_frame}"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    )
  ) +
  transition_manual(date) +
  scale_fill_fermenter(
    name = "(% PIB)",
    breaks = seq(0, 200, 30),
    direction = 1,
    palette = "YlGnBn"
  )

# ajustando o fps
animate(p, fps = 10)
