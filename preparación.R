library(dplyr)
library(lubridate)
library(ggplot2)
library(sf)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(dplyr)
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

objetos_a_guardar <- list(
  base_limpia = base_mod,
  base_analisis_espacial = base_con_vecinos,
  resumen_motivacion = analisis_motivacion
)

# Guardamos la lista completa en un archivo .rds
saveRDS(objetos_a_guardar, "analisis_homicidios_2026.rds")
dc<- readRDS("analisis_homicidios_2026.rds")
resumen_distritos <- dc$resumen_motivacion
base_detalle <- dc$base_analisis_espacial
