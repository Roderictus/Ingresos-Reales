library (httr)

save_dir <- "C:/Proyectos/Ingresos-Reales/DATA/DBF ZIPS"  # Replace with your desired path

base_url <- "https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos/"

download_files <- function(base_url, filename, save_dir) {
  url <- paste0(base_url, filename)
  filepath <- file.path(save_dir, filename)
  
  response <- GET(url, write_disk(filepath), timeout(600))  # 600 seconds timeout
  
  if (http_error(response)) {
    cat(paste("Error downloading:", filename, "\n"))
  } else {
    # Print message to indicate progress
    cat(paste("Downloaded:", filename, "\n"))
  }
}


# Loop for years 2005 to 2020
#for (year in 2005:2020) {
#  for (trim in 1:4) {
for (year in 2011:2020) {
  for (trim in 1:4) {
    # Exception for missing 2020 2nd trimester
    if (year == 2020 && trim == 2) next
    
    filename <- paste0(year, "trim", trim, "_dbf.zip")
    download_files(base_url, filename, save_dir)
  }
}
new_base_url <- base_url
for (year in 2020:2023) {
  for (trim in 1:4) {
    # Construct new filename
    filename <- paste0("enoe_n_", year, "_trim", trim, "_dbf.zip")
    download_files(new_base_url, filename, save_dir)
  }
}
filename <- "enoe_2023_trim1_dbf.zip"
download_files(base_url, filename, save_dir)

##############################      UNZIP     ##################################



