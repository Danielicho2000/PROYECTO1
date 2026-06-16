library(dplyr)
library(sf)
library(ggplot2)
library(hms)
library(leaflet)
library(leafgl)
library(leaflet.extras)
library(htmlwidgets)
library(lubridate)
library(patchwork)
library(osmdata)
library(hms)
# --- PARÁMETROS CONFIGURABLES (Cambiar aquí para adaptar el análisis) ---
crs_geo     <- 4326     # WGS84 (Latitud/Longitud ideal para mapas web)
crs_proy    <- 32717    # UTM Zona 17S (Ideal para medir distancias en metros en Ecuador)
dist_umbral <- 200      # Metros (El riesgo se concentra en la misma intersección/cuadra)
dias_umbral <- 7        # Días (Evaluamos si el choque genera riesgo a corto plazo)
buffer_pico <- 300      # Metros de radio para extraer amenidades en zonas críticas
datos<-readRDS("Base_Final.rds")
# base_estudio_espacial <- datos %>%
#   select(
#     # 1. Variables Espaciales 
#     LATITUD_Y, 
#     LONGITUD_X,
#     PROVINCIA, 
#     CANTON, 
#     PARROQUIA, 
#     DIRECCION,
#     # 2. Variables Temporales 
#     ANIO, 
#     FECHA, 
#     HORA, 
#     FERIADO,
#     # 3. Variables Dependientes 
#     LESIONADOS, 
#     FALLECIDOS,
#     # 4. Variables Categóricas del Evento 
#     CAUSA_PROBABLE, 
#     TIPO_DE_SINIESTRO,
#     # 5. Agregados de Vehículos
#     SUMA_DE_VEHICULOS, 
#     AUTOMOVIL, 
#     BICICLETA, 
#     BUS, 
#     CAMION, 
#     CAMIONETA, 
#     MOTOCICLETA
#   ) %>%
#   # Filtro vital: descartar registros sin coordenadas, ya que impiden el cálculo de distancias
#   filter(!is.na(LATITUD_Y) & !is.na(LONGITUD_X))%>%
#   mutate(
#     FECHA = as.Date(as.numeric(FECHA), origin = "1899-12-30"),
#     HORA = as_hms(as.numeric(HORA) * 86400)
#   )%>%
#   filter(!is.na(FECHA) & !is.na(HORA))
#saveRDS(base_estudio_espacial,"Base_estudio_original.rds")
#rm(list=c("datos","base_estudio_espacial"))
base_cruda<-readRDS("Base_estudio.rds")
# base_limpia<-base_cruda%>%
#   # Asegurar que R las lea como números y no como texto
#   mutate(
#     LONGITUD = as.numeric(LONGITUD_X),
#     LATITUD = as.numeric(LATITUD_Y)
#   ) %>%
#   # Filtrar para quedarse solo con accidentes en Ecuador Continental
#   filter(
#     LONGITUD >= -81.5 & LONGITUD <= -75.0,  
#     LATITUD >= -5.5 & LATITUD <= 1.5,
#     LONGITUD != 0 & LATITUD != 0 # Eliminar la Isla Null
#   )
#saveRDS(base_limpia,"base_limpia.rds")
base_limpia_corregida <- base_limpia %>%
  filter(!(
    LONGITUD_X >= -78.5405 & LONGITUD_X <= -78.5395 &  
      as.Date(FECHA) >= as.Date("2024-06-17") &          
      as.Date(FECHA) <= as.Date("2024-06-25")            
  ))

base_limpia_corregida <- base_limpia_corregida %>%
  filter(!(
   
    LATITUD_Y >= -3.5  & LATITUD_Y <= -2.1 &   # Rango de latitud desde el sur hasta Santa Elena
      LONGITUD_X <= -80.1                         # Todo lo que esté a la izquierda de Machala/Guayaquil en ese tramo
  )) %>%
  filter(LONGITUD_X > -81.05)
saveRDS(base_limpia_corregida,"base_limpia.rds")
mapa_completo_corregido <- base_limpia_corregida %>% 
  st_as_sf(coords = c("LONGITUD_X", "LATITUD_Y"), crs = 4326)

mapa_ubic <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    data = mapa_completo_corregido,  
    color = "darkred", 
    stroke = FALSE,      
    fillOpacity = 0.5,
    radius = 2           
  )

mapa_ubic
#### MAPA DE CALOR DE ACCIDENTES
mapa_calor <- leaflet(base_limpia) %>%
  # Usar un fondo oscuro (DarkMatter) hace que el "calor" resalte mucho más
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  # Agregamos la capa de calor
  addHeatmap(
    lng = ~LONGITUD_X, 
    lat = ~LATITUD_Y,
    intensity = 1,      # Peso de cada punto (cada accidente vale 1)
    blur = 15,          # Qué tan difuminado quieres el borde del calor
    max = 0.05,         # Ajusta este valor si el mapa se ve "quemado" (todo rojo)
    radius = 10         # Radio de influencia visual de cada punto
  )
# mostrar mapa de calor
mapa_calor
##### GUARDAR MAPAS
saveWidget(mapa_ubic, file = "mapa_puntos_siniestros.html", selfcontained = TRUE)
saveWidget(mapa_calor, file = "mapa_calor_siniestros.html", selfcontained = TRUE)
# =============================================================================
# 3. ANÁLISIS TEMPORAL BÁSICO
# =============================================================================
## DIARIOS
conteo_diario <- base_limpia %>%
  mutate(dia_semana = wday(FECHA, label = TRUE, abbr = FALSE, week_start = 1)) %>% 
  group_by(dia_semana) %>%
  summarise(total_eventos = n(), .groups = 'drop')

grafico_dia_semana <- ggplot(conteo_diario, aes(x = dia_semana, y = total_eventos)) +
  geom_col(fill = "darkorange", alpha = 0.8) + 
  geom_text(aes(label = total_eventos), vjust = -0.5, color = "darkred", fontface = "bold") + 
  labs(title = "Total de Siniestros por Día de la Semana",
       x = "Día de la Semana", y = "Siniestros") + theme_minimal()
print(grafico_dia_semana)
## POR HORAS
conteo_por_hora <- base_limpia %>%
  mutate(hora_del_dia = hour(HORA)) %>% 
  filter(!is.na(hora_del_dia)) %>%
  group_by(hora_del_dia) %>%
  summarise(total_eventos = n(), .groups = 'drop')

grafico_horas <- ggplot(conteo_por_hora, aes(x = hora_del_dia, y = total_eventos)) +
  geom_line(color = "darkorange", size = 1) + geom_point(color = "darkred", size = 2) +
  scale_x_continuous(breaks = 0:23) + 
  labs(title = "Acumulado de Siniestros por Hora del Día",
       x = "Hora del día", y = "Total de Siniestros") + theme_minimal()
print(grafico_horas)
## SERIE TEMPORAL MENSUAL (Evolución histórica)
conteo_mensual <- base_limpia %>%
  # Agrupar por el primer día de cada mes para crear la serie temporal
  mutate(mes_anio = floor_date(FECHA, "month")) %>% 
  group_by(mes_anio) %>%
  # Sumar el total de siniestros por mes
  summarise(total_siniestros = n(), .groups = 'drop')

grafico_temporal <- ggplot(conteo_mensual, aes(x = mes_anio, y = total_siniestros)) +
  # Línea principal y puntos
  geom_line(color = "darkorange", size = 1) +
  geom_point(color = "darkred", size = 2) +
  # Añadir una línea de tendencia suavizada (opcional, muy útil para ver si el problema empeora)
  geom_smooth(method = "loess", color = "steelblue", linetype = "dashed", se = FALSE, size = 0.8) +
  # Etiquetas y títulos
  labs(title = "Evolución Mensual de Siniestros de Tránsito",
       subtitle = "Serie temporal histórica y línea de tendencia (azul)",
       x = "Mes y Año",
       y = "Total de Siniestros") +
  # Formato del eje X para que las fechas se lean claro 
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    plot.title = element_text(face = "bold")
  )

# Mostrar el gráfico
print(grafico_temporal)
# =============================================================================
# 4. ANÁLISIS ESPACIO-TEMPORAL (VECINOS PRÓXIMOS - NEAR REPEAT)
# =============================================================================
base_sf <- base_limpia %>%
  mutate(event_id = row_number()) %>%
  st_as_sf(coords = c("LONGITUD_X", "LATITUD_Y"), crs = crs_geo) %>%
  # Proyectamos a UTM antes de medir para que la distancia sea real en metros
  st_transform(crs_proy)
# 
# vecinos_espaciales <- st_is_within_distance(base_sf, base_sf, dist = dist_umbral)
# fechas <- base_sf$FECHA
# ids <- base_sf$event_id
# n_vecinos <- sapply(1:nrow(base_sf), function(i) {
#   # Extraemos los índices de los accidentes que cayeron en el radio de este accidente
#   idx_cercanos <- vecinos_espaciales[[i]]
#   
#   # Si el único punto cercano es él mismo, tiene 0 vecinos a futuro
#   if(length(idx_cercanos) <= 1) return(0)
#   
#   # Filtramos las fechas y los IDs SOLO de esos pocos puntos cercanos
#   fechas_cercanas <- fechas[idx_cercanos]
#   ids_cercanos <- ids[idx_cercanos]
#   
#   # Condición Temporal: 
#   # a) No es el mismo ID
#   # b) Ocurrió DESPUÉS de este accidente (o el mismo día pero reportado luego)
#   # c) Ocurrió DENTRO de los días de umbral permitidos
#   es_futuro_valido <- (ids_cercanos != ids[i]) & 
#     (fechas_cercanas > fechas[i] | (fechas_cercanas == fechas[i] & ids_cercanos > ids[i])) & 
#     (fechas_cercanas <= (fechas[i] + days(dias_umbral)))
#   
#   # Contamos cuántos cumplieron las reglas
#   sum(es_futuro_valido, na.rm = TRUE)
# })
# base_con_vecinos <- base_sf %>% mutate(n_vecinos_proximos = n_vecinos)
# saveRDS(base_con_vecinos,"base_con_vecinos.rds")
base_vecinos<-readRDS("base_con_vecinos.rds")
# =============================================================================
# 5. DESCARGA DE COVARIABLES (OPEN STREET MAP)
# =============================================================================
covariables_utm<-readRDS("covariables_utm.rds")
siniestros_utm <- st_transform(base_vecinos, crs_proy)
tipos_amenidad <- unique(covariables_utm$amenity)
# Umbral crítico adaptado: Más de 3 choques recurrentes cercanos
picos_criticos <- siniestros_utm %>% filter(n_vecinos_proximos >= 3) 

buffer_picos <- st_buffer(picos_criticos, dist = buffer_pico)
amenidades_en_picos <- st_join(covariables_utm, buffer_picos, left = FALSE)

base_amenidades <- amenidades_en_picos %>%
  st_transform(crs_geo) %>%
  mutate(
    amenidad_lon = st_coordinates(.)[,1],
    amenidad_lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(amenity, n_vecinos_proximos, amenidad_lon, amenidad_lat) %>%
  distinct()
# =============================================================================
# 6. BASE DE DISTANCIAS DE EVENTOS A LAS AMENIDADES MÁS CERCANAS
# =============================================================================
amenidades_utm <- base_amenidades %>%
  st_as_sf(coords = c("amenidad_lon", "amenidad_lat"), crs = crs_geo) %>%
  st_transform(crs_proy)
transito_utm <- st_transform(base_vecinos, crs_proy)
tipos_amenidad <- unique(amenidades_utm$amenity)
for (tipo_actual in tipos_amenidad) {
  
  # Filtrar únicamente los puntos de la amenidad actual
  puntos_sub <- amenidades_utm %>% filter(amenity == tipo_actual)
  
  # PASO CLAVE 1: st_nearest_feature encuentra el ÍNDICE del punto más cercano
  idx_cercano <- st_nearest_feature(transito_utm, puntos_sub)
  
  # PASO CLAVE 2: st_distance con 'by_element = TRUE' calcula la distancia 
  # SOLO entre el accidente y su vecino más cercano (operación en parejas).
  dist_metros <- st_distance(transito_utm, puntos_sub[idx_cercano, ], by_element = TRUE)
  
  nombre_columna <- paste0("dist_", tipo_actual, "_km")
  
  #  resultado transformado a Kilómetros (multiplicando por 0.001)
  transito_utm[[nombre_columna]] <- as.numeric(dist_metros) * 0.001
}
saveRDS(transito_utm,"transito_utm.rds")

