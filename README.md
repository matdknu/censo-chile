# Censo de población y vivienda 2024

Obtención y limpieza de datos del Censo 2024 de Chile.

- El script `descargar.R` obtiene todos los resultados oficiales [desde el sitio de estadísticas del Censo 2024](https://censo2024.ine.gob.cl/estadisticas/) usando [web scraping](https://bastianolea.rbind.io/tags/web-scraping/).
- El script `limpiar.R` carga los datos descargados que vienen en Excel, y los transforma a dataframes listos para análisis de datos.

## Fuente
- https://censo2024.ine.gob.cl/estadisticas/