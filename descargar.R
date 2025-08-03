enlace <- "https://censo2024.ine.gob.cl/estadisticas/"

# extraer enlaces de descarga
enlaces <- rvest::read_html(enlace) |> # descargar sitio
  rvest::html_elements(".fusion-button") |> # botones
  rvest::html_attr("href") |> # enlaces
  stringr::str_subset("xls") # sólo excel


# extraer nombres de archivos
archivos <- stringr::str_extract(enlaces, "(?<=\\d{2}/\\d{2}/).*")

# crear carpeta
dir.create("datos/originales", showWarnings = F) 

# por cada posición de los enlaces (x va del 1 al n)
seq_along(enlaces) |> 
  purrr::map(
    ~{message("descargando ", enlaces[.x]) # enlace correspondiente a x
      
      # descargar archivo en el enlace
      download.file(enlaces[.x],
                    # nombre del archivo a guardar
                    destfile = paste0("datos/originales/", 
                                      archivos[.x]) # nombre de archivo de x
      )
      # Sys.sleep(1) # espera post-descarga
    })
