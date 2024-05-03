#creating table for freq of leg----
tablegs <- lameleg %>%
    filter(ftdat >= startdat) %>%
    select(site, cowid, leg) %>%
    mutate(leg = fct_relevel(leg, 
                             c("LF", "RF", "BF",
                               "RH", "LH", "BH", "Other"))) %>% 
    tbl_cross(row = site,
              col = leg,
              percent = "row",
              margin = c("row"),
              label = list(site ~ "Site",
                           leg ~ "Leg") ) %>% 
    bold_labels() %>% 
    # highlights Other as Orange
    as_flex_table() %>%
    bg(j = 8, part = "body", bg ="orange") %>% 
    bold(i = 1, bold = TRUE, part = "header") %>% 
    bold(i = 2, bold = TRUE, part = "header") %>%
    bold(i =  4, bold = TRUE, part = "body") %>% 
    autofit() %>% 
    fit_to_width(max_width= 6.5)
  
