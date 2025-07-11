library(wbstats)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(gganimate)
library(gifski)
library(ggthemes)

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
world_map <- ne_countries(scale = "small", type = "countries") |>
  select(iso_a2, name, subregion)

# juntando exp_imp_df e world_map
exp_imp_map <- exp_imp_df |>
  inner_join(world_map, by = c("iso2c" = "iso_a2")) |>
  st_as_sf()

# listando código para projeção de Robinson
robinson_code <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs"

# utilizando Lambert Conformal Conic para o mapa da europa
exp_imp_map_proj <- st_transform(exp_imp_map, crs = robinson_code)

df_not_na <- na.omit(exp_imp_map_proj)

# criando plot de mapa
p <- exp_imp_map_proj |>
  ggplot() +
  geom_sf(aes(fill = `Importacao (%PIB)`)) +
  labs(
    title = "Taxas de Importação entre 1985 - 2000 (% do PIB)",
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
  ) +
  theme_map()

# ajustando o fps
animate(p, fps = 5)
