# Survival post lesion Dx

#Survival:Blocks----
survall <- lamecull %>%
  # filter(farm == regfarm) %>% 
  # filter out the specific lesion
  filter(ftdat >= startdat) %>% 
  mutate(censordat = as.numeric(censordat)) %>% 
  mutate(lifexdd = case_when(lifexlame == 0 ~ 0,
                                 lifexdd == 1 ~ 1,
                                 lifexdd == 2 ~ 2,
                                 lifexdd >2 ~ 3,
                             TRUE ~ NA),
         lifexdd_cat = case_when(lifexlame == 0 ~ "Never any lesion",
                                     lifexdd == 1 ~ "Once",
                                     lifexdd == 2 ~ "Twice",
                                     lifexdd == 3 ~ "3 or more times",
                                 TRUE ~ NA) ) |> 
  mutate(lifexdd_cat = fct_reorder(lifexdd_cat, lifexdd, .na_rm = TRUE))

fitKM <- survfit(Surv(censordat, culled) ~ lifexdd, data = survall)
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
                #legend = c(0.9,0.9),
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


# Tables
fit_farm1 <- survfit(Surv(censordat, culled) ~ lifexdd_cat, 
                     data = survall)

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

