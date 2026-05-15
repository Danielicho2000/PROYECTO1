###############################################################################
# ANÁLISIS ESPACIO-TEMPORAL DE HOMICIDIOS Y COVARIABLES (AMENIDADES)
# Autor: DANIEL LARA
# Descripción: Este script limpia datos de homicidios, evalúa la recurrencia 
# espacio-temporal (efecto near-repeat) y analiza la relación espacial con 
# amenidades extraídas de OpenStreetMap (OSM).
###############################################################################

# =============================================================================
# 1. LIBRERÍAS Y PARÁMETROS GENERALES
# =============================================================================
# Instalación condicional de paquetes (opcional, por si la tutora no los tiene)
# pacman::p_load(dplyr, lubridate, ggplot2, sf, leaflet, leaflet.extras, htmlwidgets, patchwork, rnaturalearth, osmdata)

library(dplyr)
library(lubridate)
library(ggplot2)
library(sf)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(patchwork)
library(rnaturalearth)
library(osmdata)

# --- PARÁMETROS CONFIGURABLES (Cambiar aquí para adaptar el análisis) ---
crs_geo     <- 4326     # WGS84 (Latitud/Longitud ideal para mapas web)
crs_proy    <- 32717    # UTM Zona 17S (Ideal para medir distancias en metros en Ecuador)
dist_umbral <- 500      # Distancia en metros para considerar un evento "vecino"
dias_umbral <- 30       # Días de diferencia para considerar un evento "vecino"
buffer_pico <- 400      # Metros de radio para extraer amenidades en zonas críticas
# -------------------------------------------------------------------------

# =============================================================================
# 2. CARGA Y LIMPIEZA DE DATOS
# =============================================================================
# Lectura de bases
base_1 <- read.csv("mdi_homicidiosintencionales_pm_2014_2025.csv")
base_2 <- read.csv("mdi_homicidiosintencionalse_pm_2026_enero_febrer.csv")
base_cruda <- bind_rows(base_1, base_2)
rm(list=c("base_1","base_2"))

# Limpieza y filtrado espacial
base_limpia <- base_cruda %>%
  mutate(
    fecha_infraccion = as.Date(fecha_infraccion, format = "%Y/%m/%d"),
    # Reemplazar comas por puntos en coordenadas
    longitud = as.numeric(gsub(",", ".", as.character(coordenada_x))),
    latitud  = as.numeric(gsub(",", ".", as.character(coordenada_y)))
  ) %>%
  # Eliminar NAs y filtrar por el Bounding Box aproximado de Ecuador
  filter(!is.na(longitud) & !is.na(latitud)) %>%
  filter(longitud > -82 & longitud < -75) %>%
  filter(latitud > -6 & latitud < 2)
rm("base_cruda")
saveRDS(base_limpia, "base_original.rds")
write.csv(base_limpia, "base_limpia.csv")
# =============================================================================
# 3. ANÁLISIS TEMPORAL BÁSICO: MENSUAL, SEMANAL, DIARIOS y POR HORAS
# =============================================================================
##MENSUAL
conteo_mensual <- base_limpia %>%
  mutate(mes_año = floor_date(fecha_infraccion, "month")) %>% 
  group_by(mes_año) %>%
  summarise(total_eventos = n(), .groups = 'drop')

grafico_temporal <- ggplot(conteo_mensual, aes(x = mes_año, y = total_eventos)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue", size = 1.5) +
  geom_vline(xintercept = as.numeric(as.Date("2026-01-01")), 
             linetype = "dashed", color = "red", size = 1) +
  labs(title = "Evolución Mensual de Homicidios (2014 - 2026)",
       subtitle = "Validación de empalme entre bases históricas y recientes",
       x = "Mes y Año",
       y = "Total de Homicidios Mensuales") +
  theme_minimal()

print(grafico_temporal)

##SEMANAL
conteo_semanal <- base_limpia %>%
  mutate(semana = floor_date(fecha_infraccion, "week")) %>% 
  group_by(semana) %>%
  summarise(total_eventos = n(), .groups = 'drop')

grafico_semanal<-ggplot(conteo_semanal, aes(x = semana, y = total_eventos)) +
  geom_line(color = "steelblue", size = 0.8) +
  geom_vline(xintercept = as.numeric(as.Date("2026-01-01")), 
             linetype = "dashed", color = "red", size = 1) +
  labs(title = "Evolución Semanal de Homicidios (2014 - 2026)",
       x = "Semana", y = "Total de Homicidios") +
  theme_minimal()
print(grafico_semanal)

##DIARIOS
conteo_diario <- base_limpia %>%
  mutate(dia_semana = wday(fecha_infraccion, label = TRUE, abbr = FALSE, week_start = 1)) %>% 
  group_by(dia_semana) %>%
  summarise(total_eventos = n(), .groups = 'drop')

grafico_dia_semana <- ggplot(conteo_diario, aes(x = dia_semana, y = total_eventos)) +
  geom_col(fill = "steelblue", alpha = 0.8) + 
  geom_text(aes(label = total_eventos), vjust = -0.5, color = "darkblue", fontface = "bold") + 
  labs(title = "Total de Homicidios por Día de la Semana (2014 - 2026)",
       subtitle = "¿Qué días presentan mayor incidencia?",
       x = "Día de la Semana", 
       y = "Total de Homicidios") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(face = "bold", size = 11)
  )

print(grafico_dia_semana)

##POR HORAS
# Conteo acumulado por hora del día (0 a 23)
conteo_por_hora <- base_limpia %>%
  mutate(hora_del_dia = hour(hms(hora_infraccion))) %>% 
  filter(!is.na(hora_del_dia)) %>%
  group_by(hora_del_dia) %>%
  summarise(total_eventos = n(), .groups = 'drop')

grafico_horas <- ggplot(conteo_por_hora, aes(x = hora_del_dia, y = total_eventos)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  scale_x_continuous(breaks = 0:23) + 
  labs(title = "Acumulado de Homicidios por Hora del Día (2014 - 2026)",
       x = "Hora del día (0-23 hrs)",
       y = "Total de Homicidios") +
  theme_minimal()

print(grafico_horas)
rm(list=c("conteo_diario","conteo_mensual","conteo_por_hora","conteo_semanal"))
# =============================================================================
# 4. ANÁLISIS ESPACIO-TEMPORAL (VECINOS PRÓXIMOS)
# =============================================================================
base_sf <- base_limpia %>%
  mutate(event_id = row_number()) %>% 
  st_as_sf(coords = c("longitud", "latitud"), crs = crs_geo)

base_con_vecinos <- base_sf %>%
  mutate(
    n_vecinos_proximos = sapply(1:n(), function(i) {
      actual <- base_sf[i, ]
      
      # Filtrar eventos futuros dentro de la ventana de tiempo
      vecinos <- base_sf %>%
        filter(
          event_id != actual$event_id,
          fecha_infraccion > actual$fecha_infraccion | 
            (fecha_infraccion == actual$fecha_infraccion & event_id > actual$event_id),
          fecha_infraccion <= (actual$fecha_infraccion + days(dias_umbral))
        )
      
      if(nrow(vecinos) == 0) return(0)
      
      # Calcular distancias y contar los que caen dentro del umbral espacial
      distancias <- st_distance(actual, vecinos)
      sum(as.numeric(distancias) <= dist_umbral)
    })
  )
saveRDS(base_con_vecinos,"base_con_vecinos.rds")
# Resumen de motivaciones en zonas críticas
analisis_motivacion <- base_con_vecinos %>%
  st_drop_geometry() %>%
  group_by(distrito, presunta_motivacion) %>%
  summarise(
    total_eventos = n(),
    promedio_vecinos_afectados = mean(n_vecinos_proximos),
    max_vecinos = max(n_vecinos_proximos, na.rm = TRUE),
    prob_recurrencia_pct = (sum(n_vecinos_proximos > 0) / n()) * 100,
    .groups = 'drop'
  ) %>%
  filter(total_eventos >= 5) %>% 
  arrange(desc(prob_recurrencia_pct))
saveRDS(analisis_motivacion,"analisis_motivacion.rds")
# =============================================================================
# 5. DESCARGA DE COVARIABLES (OPEN STREET MAP)
# =============================================================================
# Advertencia: Para todo Ecuador esto puede tomar bastante RAM.
mi_caja <- getbb("Ecuador")
amenidades_buscar <- c(
  # --- Sustento (Sustenance) ---
  "bar", "bbq", "biergarten", "cafe", "drinking_water", 
  "fast_food", "food_court", "ice_cream", "pub", "restaurant",
  
  # --- Educación (Education) ---
  "college", "conference_centre", "driving_school", "kindergarten", 
  "language_school", "library", "music_school", "school", "university",
  
  # --- Transporte (Transportation) ---
  "bicycle_parking", "bicycle_repair_station", "bicycle_rental", 
  "boat_rental", "bus_station", "car_sharing", "car_wash", 
  "charging_station", "ferry_terminal", "fuel", "motorcycle_parking", 
  "parking", "parking_entrance", "parking_space", "taxi",
  
  # --- Finanzas (Financial) ---
  "atm", "bank", "bureau_de_change",
  
  # --- Salud y Cuidado (Healthcare) ---
  "baby_hatch", "clinic", "dentist", "doctors", "hospital", 
  "nursing_home", "pharmacy", "social_facility", "veterinary",
  
  # --- Entretenimiento, Artes y Cultura (Entertainment, Arts & Culture) ---
  "arts_centre", "brothel", "casino", "cinema", "community_centre", 
  "events_venue", "fountain", "gambling", "nightclub", "planetarium", 
  "public_bookcase", "social_centre", "stripclub", "studio", "theatre",
  
  # --- Servicios Públicos (Public Service) ---
  "courthouse", "fire_station", "police", "post_box", "post_office", 
  "prison", "ranger_station", "townhall",
  
  # --- Instalaciones y Otros (Facilities) ---
  "bench", "clock", "device_charging_station", "gavage", "marketplace", 
  "monastery", "photo_booth", "place_of_mourning", "place_of_worship", 
  "public_bath", "public_building", "refugee_site", "shelter", 
  "telephone", "toilets", "town_square", "vending_machine", 
  "waste_basket", "waste_disposal", "waste_transfer_station", 
  "water_point", "watering_place"
) #https://wiki.openstreetmap.org/wiki/Key:amenity


mi_consulta <- opq(bbox = mi_caja, timeout = 900) %>%
  add_osm_feature(key = "amenity", value = amenidades_buscar)

#datos_osm <- osmdata_sf(mi_consulta)
#saveRDS(datos_osm, "datos_osm_ecuador.rds")
datos_osm <-readRDS("datos_osm_ecuador.rds")

# Extraer puntos y proyectar a UTM para medir distancias en metros
covariables_utm <- datos_osm$osm_points %>%
  filter(!is.na(amenity)) %>%
  select(amenity, geometry) %>%
  st_transform(crs_proy)
#saveRDS(covariables_utm, "covariables_utm.rds")
#covariables_utm <-readRDS("covariables_utm.rds")
# =============================================================================
# 6. MODELADO ESPACIAL BÁSICO (Distancia a Amenidades)
# =============================================================================
# Proyectar homicidios a UTM
homicidios_utm <- st_transform(base_con_vecinos, crs_proy)
tipos_amenidad <- unique(covariables_utm$amenity)

# Loop para iterar modelos lineales por cada tipo de amenidad
resultados_comparativos <- lapply(tipos_amenidad, function(tipo_actual) {
  
  puntos_amenidad <- covariables_utm %>% filter(amenity == tipo_actual)
  
  if(nrow(puntos_amenidad) == 0) return(NULL)
  
  # Distancia mínima de cada homicidio a la amenidad más cercana
  distancias <- st_distance(homicidios_utm, puntos_amenidad)
  dist_min <- 0.001*apply(distancias, 1, min) #en KM
  
  # Modelo GLM POISSON
  mod <- glm(n_vecinos_proximos ~ dist_min, 
                  data = homicidios_utm, 
                  family = poisson(link = "log"))
  res_summary <- summary(mod)$coefficients
  
  data.frame(
    Amenidad = tipo_actual,
    Estimador_Log = res_summary[2, 1], # Beta en escala logarítmica
    Error_Std = res_summary[2, 2],
    Z_value = res_summary[2, 3],       # En GLM se usa Z en lugar de T
    Significancia_p = res_summary[2, 4],
    AIC = AIC(mod)                # Criterio de información para comparar modelos
  )
})

# Consolidar ranking
ranking_peligrosidad <- bind_rows(resultados_comparativos) %>%
  arrange(Z_value) 
print(ranking_peligrosidad)

# =============================================================================
# 7. MAPAS INTERACTIVOS
# =============================================================================
# Preparar datos para el mapa de intensidad
base_para_mapa <- base_con_vecinos %>%
  mutate(
    lng_map = st_coordinates(.)[,1],
    lat_map = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry()

# Preparar capa de puntos de ejemplo 
puntos <- covariables_utm %>% 
  st_transform(crs_geo)

mapa_intensidad <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addHeatmap(
    data = base_para_mapa,
    lng = ~lng_map, 
    lat = ~lat_map,
    intensity = ~n_vecinos_proximos,
    blur = 20, 
    radius = 12, 
    max = max(base_para_mapa$n_vecinos_proximos, na.rm = TRUE)
  ) %>%
  addCircleMarkers(
    data = puntos,
    color = "red", # Cambié el color a rojo para que se vea mejor sobre fondo blanco
    radius = 2, 
    label = ~amenity,
    group = "Covariables"
  )
mapa_intensidad 

# =============================================================================
# 8. EXTRACCIÓN Y EXPORTACIÓN DE ZONAS CRÍTICAS
# =============================================================================
# Definir umbral crítico (Por ejemplo: Más de 8 vecinos o el percentil 90)
picos_criticos <- homicidios_utm %>% filter(n_vecinos_proximos > 8) 

# Crear áreas de influencia (Buffers) alrededor de los puntos más violentos
buffer_picos <- st_buffer(picos_criticos, dist = buffer_pico)

# Encontrar amenidades dentro de esas zonas
amenidades_en_picos <- st_join(covariables_utm, buffer_picos, left = FALSE)

# Preparar para exportar
df_exportar <- amenidades_en_picos %>%
  st_transform(crs_geo) %>%
  mutate(
    amenidad_lon = st_coordinates(.)[,1],
    amenidad_lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(amenity, n_vecinos_proximos, distrito, amenidad_lon, amenidad_lat) %>%
  distinct() # Evitar duplicados si buffers se superponen

# Exportar resultados
write.csv(df_exportar, "amenidades_en_zonas_criticas_2026.csv", row.names = FALSE)
cat("¡Éxito! Se han guardado", nrow(df_exportar), "amenidades ubicadas en focos de alta violencia.\n")