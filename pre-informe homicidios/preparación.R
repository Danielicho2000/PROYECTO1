library(dplyr)
library(lubridate)
library(ggplot2)
library(sf)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(patchwork)
base_1<-read.csv("mdi_homicidiosintencionales_pm_2014_2025.csv")
base_2<-read.csv("mdi_homicidiosintencionalse_pm_2026_enero_febrer.csv")

base<-bind_rows(base_1,base_2)

base_mod <- base %>%
  mutate(
    fecha_infraccion = as.Date(fecha_infraccion, format = "%Y/%m/%d"),
    longitud = as.numeric(gsub(",", ".", as.character(coordenada_x))),
    latitud = as.numeric(gsub(",", ".", as.character(coordenada_y)))
  ) %>%
  filter(!is.na(longitud) & !is.na(latitud)) %>%
  filter(longitud > -82 & longitud < -75) %>%
  filter(latitud > -6 & latitud < 2)
#generar una columna nueva por semanas o días y volver a gráficar el conteo

conteo_mensual <- base_mod %>%
  mutate(mes_año = floor_date(fecha_infraccion, "month")) %>% 
  group_by(mes_año) %>%
  summarise(total_eventos = n(), .groups = 'drop')

ggplot(conteo_mensual, aes(x = mes_año, y = total_eventos)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue", size = 1.5) +
  geom_vline(xintercept = as.numeric(as.Date("2026-01-01")), 
             linetype = "dashed", 
             color = "red", 
             size = 1) +
  labs(title = "Evolución Mensual de Homicidios (2014 - 2026)",
       subtitle = "Validación de empalme entre bases históricas y recientes",
       x = "Mes y Año",
       y = "Total de Homicidios Mensuales") +
  theme_minimal()


mapa_frecuencia <- leaflet(data = base_mod) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addHeatmap(
    lng = ~longitud, 
    lat = ~latitud,
    blur = 15,       
    radius = 10,      
    max = 0.05
  )
mapa_frecuencia


#el objetivo es ver si luego de un evento identificado, que tan probable es que
#dentro de 500m a la redonda y en un lapso de 30 días, se de otro evento

distancia_umbral <- 500  
tiempo_umbral <- 30
base_sf <- base_mod %>%
  mutate(event_id = row_number()) %>% 
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326)
base_con_vecinos<-base_sf %>%
  mutate(
    n_vecinos_proximos = sapply(1:n(), function(i) {
      # Punto actual
      actual <- base_sf[i, ]
      
      # Filtrar potenciales vecinos: 
      vecinos <- base_sf %>%
        filter(
          event_id != actual$event_id,
          fecha_infraccion > actual$fecha_infraccion,
          fecha_infraccion <= (actual$fecha_infraccion + days(tiempo_umbral))
        )
     
      if(nrow(vecinos) == 0) return(0)
      
      distancias <- st_distance(actual, vecinos)
      sum(as.numeric(distancias) <= distancia_umbral)
    })
  )
summary(base_con_vecinos$n_vecinos_proximos)
#ver que motivación es más probable de suceder en el área de cada distrito
analisis_motivacion <- base_con_vecinos %>%
  st_drop_geometry() %>%
  group_by(distrito, presunta_motivacion) %>%
  summarise(
    total_eventos = n(),
    promedio_vecinos_afectados = mean(n_vecinos_proximos),
    max=max(n_vecinos_proximos, na.rm = TRUE),
    prob=(sum(n_vecinos_proximos>0)/n())*100,
    .groups='drop'
  ) %>%
  filter(total_eventos >= 5) %>% 
  arrange(desc(prob))

#objetos_a_guardar <- list(
#  base_limpia = base_mod,
#  base_analisis_espacial = base_con_vecinos,
#  resumen_motivacion = analisis_motivacion
#)


##saveRDS(objetos_a_guardar, "analisis_homicidios_2026.rds")
#dc<- readRDS("analisis_homicidios_2026.rds")
resumen_distritos <- dc$resumen_motivacion
base_detalle <- dc$base_analisis_espacial


###############################################################################
#Mapas y covariables
library(rnaturalearth)
library(osmdata)
mi_caja <- getbb("Ecuador")
mi_consulta <- opq(bbox = mi_caja,timeout = 900) %>%
  add_osm_feature(key = "amenity", 
                  value = c("hospital", "bar","nightclub","pub", "bank",
                            "atm",  "bus_station","restaurant","police",
                             "social_facility","prison"))
datos_osm <- osmdata_sf(mi_consulta)
mis_puntos <- datos_osm$osm_points
covariables_sf <- mis_puntos %>%
  filter(!is.na(amenity)) %>%
  select(amenity, geometry) %>%
  st_transform(32717)
lista_amenities <- split(covariables_sf, covariables_sf$amenity)
########medimos la fuerza de las covariables
homicidios_utm <- st_transform(base_sf, 32717)
covariables_utm <- st_transform(covariables_sf, 32717)

#EJEMPLO
discotecas_utm <- covariables_utm %>% filter(amenity == "nightclub")


distancias <- st_distance(homicidios_utm, discotecas_utm)
homicidios_utm$dist_discoteca <- apply(distancias, 1, min)

modelo <- lm(base_con_vecinos$n_vecinos_proximos ~ dist_discoteca, data = homicidios_utm)
summary(modelo)

#######con todas las amenities
tipos <- unique(covariables_utm$amenity)
homicidios_con_intensidad_utm <- st_transform(base_con_vecinos, 32717)
resultados_comparativos <- lapply(tipos, 
                                  function(tipo_actual) {
 
  puntos <- covariables_utm %>% filter(amenity == tipo_actual)
  
 
  distancias <- st_distance(homicidios_con_intensidad_utm, puntos)
  dist_min <- apply(distancias, 1, min)
  
  #  modelo rápido
  mod <- lm(base_con_vecinos$n_vecinos_proximos ~ dist_min, data = homicidios_con_intensidad_utm)
  
  
  data.frame(
    Amenity = tipo_actual,
    Fuerza_t_value = summary(mod)$coefficients[2,3], # t-value
    Significancia = summary(mod)$coefficients[2,4],  # p-value
    R_cuadrado = summary(mod)$r.squared
  )
})
ranking_peligrosidad <- do.call(rbind, resultados_comparativos) %>%
  arrange(Fuerza_t_value)

print(ranking_peligrosidad)
##### UBICACIONES DE LAS AMENITIES CERCANAS A LAS MAYORES INTENSIDADES 
base_para_mapa <- base_con_vecinos %>%
  st_transform(4326) %>%
  mutate(
    lng_map = st_coordinates(.)[,1],
    lat_map = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry()
leaflet() %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addHeatmap(
    data = base_para_mapa,
    lng = ~lng_map, 
    lat = ~lat_map,
    intensity = ~n_vecinos_proximos, # Pesa los puntos según la recurrencia
    blur = 20, 
    radius = 12, 
    max = max(base_para_mapa$n_vecinos_proximos, na.rm = TRUE)
  ) %>%
  addCircleMarkers(
    data = (bares_puntos %>% st_transform(4326)),
    color = "yellow", 
    radius = 2, 
    label = ~amenity,
    group = "Amenidades"
  )
###GUARDAR LAS AMENITIES MÁS FUERTES
categorias_impacto <-c(
  "bus_station", "parking_entrance", "restaurant", "toilets", 
  "hospital", "nightclub", "bar", "social_facility", "bank", "atm", "police"
)
amenities_impacto <- covariables_utm %>% 
  filter(amenity %in% categorias_impacto)
umbral_alto <- quantile(homicidios_con_intensidad_utm$n_vecinos_proximos, 0.90)
picos_violencia <- homicidios_con_intensidad_utm %>% 
  filter(n_vecinos_proximos >= umbral_alto)
buffer_picos <- st_buffer(picos_violencia, dist = 300)
amenities_criticas_ubicacion <- st_join(amenities_impacto, buffer_picos, left = FALSE)
amenities_final_save <- amenities_criticas_ubicacion %>%
  st_transform(4326) %>% 
  mutate(
    longitud_amenity = st_coordinates(.)[,1],
    latitud_amenity = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(amenity, n_vecinos_proximos, distrito, provincia, longitud_amenity, latitud_amenity) %>%
  distinct() 
write.csv(amenities_final_save, "amenities_alto_impacto_ubicaciones.csv", row.names = FALSE)

cat("Se han exportado", nrow(amenities_final_save), "ubicaciones de amenidades críticas.")


###GUARDAR LAS AMENITIES CERCANAS A LAS MAYORES INTENSIDADES
picos_criticos <- homicidios_con_intensidad_utm %>% 
  filter(n_vecinos_proximos > 8) 
buffer_picos <- st_buffer(picos_criticos, dist = 400)


amenidades_en_picos <- st_join(covariables_utm, buffer_picos, left = FALSE)


df_exportar <- amenidades_en_picos %>%
  st_transform(4326) %>%
  mutate(
    amenidad_lon = st_coordinates(.)[,1],
    amenidad_lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(amenity, n_vecinos_proximos, distrito, amenidad_lon, amenidad_lat)

write.csv(df_exportar, "amenidades_en_zonas_criticas_2026.csv", row.names = FALSE)

cat("Se han guardado", nrow(df_exportar), "amenidades ubicadas en focos de alta violencia.")
