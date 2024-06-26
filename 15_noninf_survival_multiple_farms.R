# Survival post lesion Dx

#Survival:Blocks----
survall <- lamecull %>%
  # filter(farm == regfarm) %>% 
  # filter out the specific lesion
  mutate(censordat = as.numeric(censordat)) %>% 
  mutate(lifexnoninf = case_when(lifexlame == 0 ~ 0,
                                 lifexnoninf == 1 ~ 1,
                                 lifexnoninf == 2 ~ 2,
                                 lifexnoninf >2 ~3,
                                 TRUE ~ NA),
         lifexnoninf_cat = case_when(lifexlame == 0 ~ "Never any lesion",
                                     lifexnoninf == 1 ~ "Once",
                                     lifexnoninf == 2 ~ "Twice",
                                     lifexnoninf == 3 ~ "3 or more times",
                                     TRUE ~ NA) ) |> 
  mutate(lifexnoninf_cat = fct_reorder(lifexnoninf_cat, 
                                       lifexnoninf, .na_rm = TRUE))

# just graph 1 year
survall_1y <- survall |>
  filter(ftdat >= startdat) 

fitKM <- survfit(Surv(censordat, culled) ~ lifexnoninf, data = survall_1y)
km<- ggsurvplot(fitKM, 
                pval = FALSE, 
                conf.int = TRUE,
                censor = FALSE,
                fun = "pct",
                size = 1,
                linetype = c(1, 2, 3, 4), # Specify dashed lines for each group
                palette = c("#fde725ff", "#440154ff", "#3b528bff",
                            "#21908cff"), 
                legend = "bottom",
                legend.title = "Lifetime Lesion #",
                legend.labs = c("Never any lesion",
                                "First",
                                "Second",
                                "3 or more"),
                #   "Combined"), for add.all
                xlab = "Days to cull after lifetime lesion #",
                ylab = "% of Cows Alive",
                xlim = c(0,180),
                ylim = c(50,100),
                #add.all = TRUE,
                tables.theme = theme_cleantable(),
                break.time.by = 30
                ) 

# tables for survival

fit_farm1 <- survfit(Surv(censordat, culled) ~ lifexnoninf_cat, 
                     data = survall_1y)


tbl_surv_farm1 <- tbl_survfit(fit_farm1, 
                              times = c(30,120, 180),
                              label = " ",
                              reverse = TRUE, 
                              label_header = "{time} Days") %>% 
  as_flex_table() %>% # makes formatting and pdf better
  add_header_row(top = TRUE,
                 values = c("Lifetime Lesion History",
                            "% Culled (Confidence Interval) at",
                            "", "")) %>% 
  bold(i = 1, bold = TRUE, part = "header") %>% # bolds headers
  bold(i = 2, bold = TRUE, part = "header") %>% 
  merge_at(i = 1:2, j = 1, part = "header") %>% #merges 1st row
  merge_at(i = 1, j = 2:4, part = "header") %>% # merges top columns
  delete_rows(i = 1, part = "body")|>
  fit_to_width(max_width= 6.5) |> 
  hline_bottom(part = "body")


