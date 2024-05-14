library(tidyverse)

import_csv<-function(path_to_file, farm_name){
  read_csv(paste0(path_to_file), 
           col_types = cols(.default = col_character()))%>%
    mutate(source_file_path = paste0(path_to_file))%>%
    select(-contains('...'))%>%
    clean_names()
}

#from 01_single_import.R---------

import_lame <- function(filename, farm_name) {
  data <- import(paste0("sourcedata/",
                        filename, "_lame.csv"),
                 fill = TRUE) |>
    clean_names() |>
    select(-c(technician, v14)) |>
    mutate(farm = farm_name)
  return(data)
}


import_dry <- function(filename, farm_name) {
  read_csv(paste0("sourcedata/", filename, "_dry_cull.csv"), 
           show_col_types = FALSE) |>
    clean_names() |>
    # change the x16 as needed might be 17 if included FTREM
    select(-c(protocols, technician, x14)) |>
    mutate(farm = farm_name,
           # needed as some columns shifted due to commas in rem feild
           dim = as.numeric(dim))
}

import_until_condition <- function(file, stop_sequence) {
  lines <- readLines(file)
  rows_to_import <- numeric()
  
  for (i in 1:length(lines)) {
    if (grepl(stop_sequence, lines[i])) {
      rows_to_import <- 1:(i - 2)
      break
    }
  }
  
  imported_data <- read.csv(file, header = TRUE, 
                            nrows = length(rows_to_import))
  return(imported_data)
}

import_and_transform <- function(file, farm_name, stop_sequence, column_name) {
  import_until_condition(file, stop_sequence) |> 
    clean_names() %>% 
    mutate(farm = farm_name,
           ftyearmon = my(dates),
           ftyearmon = as.yearmon(ftyearmon),
           !!sym(column_name) := milking) %>% 
    select(farm, ftyearmon, !!sym(column_name))
}