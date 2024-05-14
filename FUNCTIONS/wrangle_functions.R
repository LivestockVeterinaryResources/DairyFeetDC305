library(tidyverse)

wrangle_events_basic<-function(df){
  df%>%
    rename(birth = birth_date_item,
           frsh = fresh_date_item,
           breed = breed_item) |> 
    mutate(cowid = paste0(id,' - ', birth), #consider adding birth data to decrease chances of duplicate id
           ftdim = dim,
           
           birth = mdy(birth, quiet = TRUE),# quiet stops warnings
           frsh = mdy(frsh, quiet = TRUE),
           ftdat = mdy(date),
           ftmon = month(ftdat),
           ftyear = year(ftdat),
           ftyearmon = as.yearmon(ftdat)
    )
}

wrangle_duplicates<-function(df, grp_vars){
  df%>%
    distinct()%>%
    group_by({{grp_vars}})%>%
    summarize(ct_rows = sum(n()), 
              remark = paste0(remark, collapse = ','), 
              protocols = paste0(protocols, collapse = ','))%>%
    ungroup()
}
