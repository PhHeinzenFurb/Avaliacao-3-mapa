library(wbstats)
library(tidyverse)
library(rnaturalearth)
library(gganimate)
library(sf)
library(gifski)

internet_df <- wbstats::wb_data(
  indicator = "IT.NET.USER.ZS",
  country = "countries_only",
  start_date = 2001, end_date = 2022 
)

# selecionando colunas
internet_world_df <- internet_df |>
  dplyr::select(, c(1, 4:5))

# renomeando colunas
names(internet_world_df) <-
  c("iso2c", "Year", "users")

# fazendo o download do shapefile do mapa
world_sf <- rnaturalearth::ne_countries(
  type = "countries", scale = "small"
) |> dplyr::select(iso_a2, name)

# juntando as duas tabelas
internet_world_df <- dplyr::left_join(
  world_sf, internet_world_df,
  by = c("iso_a2" = "iso2c")
)

# ajustando a projecao para aliviar as distorcoes
robinson_crs <- "+proj=robin +lon_0=0 +y_0=0 +ellps=WGS84 + datum=WGS84 +units=m +no_defs"
internet_world_df_robinson <- internet_world_df |>
  sf::st_transform(robinson_crs)
plot(sf::st_geometry(internet_world_df_robinson))

# transformando a coluna Year em numerico
internet_world_df_robinson$Year <- as.numeric(internet_world_df_robinson$Year)

p <- ggplot(filter(internet_world_df_robinson)) +
  geom_sf(aes(fill = users)) +
  theme_minimal() +
  labs(
    title = "Acesso a internet",
    subtitle = "Data: {frame_along}"
  ) +
  transition_manual(Year)


  
  
  
  
  
  
  
