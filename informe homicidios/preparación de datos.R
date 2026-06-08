# =============================================================================
# 1. LIBRERÍAS Y PARÁMETROS GENERALES
# =============================================================================
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
library(htmlwidgets)

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
base_1 <- read.csv("D:\\PROYECTO1\\mdi_homicidiosintencionales_pm_2014_2025.csv")
base_2 <- read.csv("D:\\PROYECTO1\\mdi_homicidiosintencionalse_pm_2026_enero_febrer.csv")
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
# =============================================================================
# 3. ANÁLISIS ESPACIO-TEMPORAL (VECINOS PRÓXIMOS)
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

# =============================================================================
# 4. DESCARGA DE COVARIABLES (OPEN STREET MAP)
# =============================================================================
# mi_caja <- getbb("Ecuador")
# amenidades_buscar <- c(
#   # --- Sustento (Sustenance) ---
#   "bar", "bbq", "biergarten", "cafe", "drinking_water", 
#   "fast_food", "food_court", "ice_cream", "pub", "restaurant",
#   
#   # --- Educación (Education) ---
#   "college", "conference_centre", "driving_school", "kindergarten", 
#   "language_school", "library", "music_school", "school", "university",
#   
#   # --- Transporte (Transportation) ---
#   "bicycle_parking", "bicycle_repair_station", "bicycle_rental", 
#   "boat_rental", "bus_station", "car_sharing", "car_wash", 
#   "charging_station", "ferry_terminal", "fuel", "motorcycle_parking", 
#   "parking", "parking_entrance", "parking_space", "taxi",
#   
#   # --- Finanzas (Financial) ---
#   "atm", "bank", "bureau_de_change",
#   
#   # --- Salud y Cuidado (Healthcare) ---
#   "baby_hatch", "clinic", "dentist", "doctors", "hospital", 
#   "nursing_home", "pharmacy", "social_facility", "veterinary",
#   
#   # --- Entretenimiento, Artes y Cultura (Entertainment, Arts & Culture) ---
#   "arts_centre", "brothel", "casino", "cinema", "community_centre", 
#   "events_venue", "fountain", "gambling", "nightclub", "planetarium", 
#   "public_bookcase", "social_centre", "stripclub", "studio", "theatre",
#   
#   # --- Servicios Públicos (Public Service) ---
#   "courthouse", "fire_station", "police", "post_box", "post_office", 
#   "prison", "ranger_station", "townhall",
#   
#   # --- Instalaciones y Otros (Facilities) ---
#   "bench", "clock", "device_charging_station", "gavage", "marketplace", 
#   "monastery", "photo_booth", "place_of_mourning", "place_of_worship", 
#   "public_bath", "public_building", "refugee_site", "shelter", 
#   "telephone", "toilets", "town_square", "vending_machine", 
#   "waste_basket", "waste_disposal", "waste_transfer_station", 
#   "water_point", "watering_place"
# ) #https://wiki.openstreetmap.org/wiki/Key:amenity
# 
# 
# mi_consulta <- opq(bbox = mi_caja, timeout = 900) %>%
#   add_osm_feature(key = "amenity", value = amenidades_buscar)

#datos_osm <- osmdata_sf(mi_consulta)
datos_osm <-readRDS("datos_osm_ecuador.rds") #
# Extraer puntos y proyectar a UTM para medir distancias en metros
# covariables_utm <- datos_osm$osm_points %>%
#   filter(!is.na(amenity)) %>%
#   select(amenity, geometry) %>%
#   st_transform(crs_proy)
covariables_utm <-readRDS("covariables_utm.rds") #ubicación de las covariables en la región de estudio
# =============================================================================
# 5. Distancia a Amenidades
# =============================================================================
# Proyectar homicidios a UTM
homicidios_utm <- st_transform(base_con_vecinos, crs_proy)
ubica_amenidad <- unique(covariables_utm$amenity)
saveRDS(ubica_amenidad,"ubicacion_cov.rds")

#obtener distancias
base_distancias <- homicidios_utm %>%
  st_drop_geometry() %>%
  select(event_id, distrito, presunta_motivacion, n_vecinos_proximos)

# Bucle para calcular la distancia mínima a cada tipo de amenidad en metros
for(tipo_actual in tipos_amenidad) {
  puntos_amenidad <- covariables_utm %>% filter(amenity == tipo_actual)
  
  if(nrow(puntos_amenidad) > 0) {
    # Calcular matriz de distancias
    distancias <- st_distance(homicidios_utm, puntos_amenidad)
    # Extraer la distancia mínima a la amenidad más cercana para cada homicidio
    dist_minima <- apply(distancias, 1, min)
    
    # Crear una nueva columna en la base con el nombre de la amenidad
    nombre_columna <- paste0("dist_", tipo_actual)
    base_distancias[[nombre_columna]] <- as.numeric(dist_minima)
  }
}
saveRDS(base_distancias,"base_distancias.rds")
# =============================================================================
# 6. MAPA DE CALOR DE LOS HOMICIDIOS
# =============================================================================
base_con_vecinos<-readRDS("base_con_vecinos.rds")
base_para_mapa <- base_con_vecinos %>%
  mutate(
    lng_map = st_coordinates(.)[,1],  
    lat_map = st_coordinates(.)[,2]   
  ) %>%
  st_drop_geometry()

# Construcción del Mapa
mapa_completo <- leaflet(base_para_mapa) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addHeatmap(
    lng = ~lng_map, lat = ~lat_map,
    blur = 20, radius = 15, max = 0.05, 
    group = "Calor: Homicidios"
  ) %>%
  addCircleMarkers(
    lng = ~lng_map, lat = ~lat_map,
    radius = 4,
    color = "#ff4500", # Color naranja/rojo fuego
    stroke = FALSE, fillOpacity = 0.6,
    # Un popup interactivo para ver detalles al hacer clic
    popup = ~paste("<b>Fecha:</b>", fecha_infraccion, "<br>",
                   "<b>Motivación:</b>", presunta_motivacion, "<br>",
                   "<b>Distrito:</b>", distrito),
    clusterOptions = markerClusterOptions(), 
    group = "Puntos: Homicidios"
  ) %>%
  
  # CONTROL DE CAPAS
  addLayersControl(
    overlayGroups = c("Calor: Homicidios", "Puntos: Homicidios"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup("Puntos: Homicidios") 

mapa_completo

# Guardar el widget
saveWidget(
  widget = mapa_completo, 
  file = "mapa_homicidios_ecuador_corregido.html", 
  selfcontained = TRUE 
)
