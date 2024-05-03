# Graph Trimming data

# Trimming: Graph distribution of trims----
lact.labs <- c("1" = "1st Lactation", 
               "2" = "2nd Lactation",
               "3+" = "3+ Lactation")

# combined faceted by farm
trimdistgfarm <- lame4 %>% 
  filter(ftdat > startdat) %>%
  select(farm, cowid, ftdim, lifexlame, lact) %>% 
  mutate(lifexlame = case_when(lifexlame == 0 ~ "0",
                               lifexlame == 1 ~ "1",
                               lifexlame == 2 ~ "2",
                               TRUE ~ "3"),
         lctgp = case_when(lact == 1 ~ "1",
                           lact == 2 ~ "2",
                           TRUE ~ "3+")) %>% 
  #ridgeline graph
  ggplot(aes(x = ftdim, y = lifexlame))+
  geom_density_ridges(aes( fill = as.factor(lifexlame)),
                      alpha = 0.7, color = "black", 
                      from = 0, to = 500,
                      scale = 2,
                      jittered_points = TRUE, point_alpha = 0.5,
                      position = position_points_jitter(width = 0.05,
                                                        height = 0),
                      point_shape = '|', point_size = 2)+
  scale_fill_viridis_d(option = "D",
                       breaks = c("0", "1", "2", "3"),
                       name = "Lameness History",
                       labels = c("0" = "Never",
                                  "1" = "Once",
                                  "2" = "Twice",
                                  "3" = "Chronic"),
                       # for legend positions to move needs to be horizontal
                       guide = guide_legend(direction = "horizontal",
                                            title.position = "top"))+ 
  # zooms into 0-500
  coord_cartesian(xlim = c(0,500))+
  # changes chow close labels are to axis
  scale_x_continuous(expand = c(0, 0))+
  scale_y_discrete(expand = c(0.01, 0.01),
                   labels = NULL)+
  labs(x = "Days in Milk at Trim Date", y = NULL)+
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
  # more moving of legend
  theme(legend.position = "bottom",
        legend.title.align = 0.5,
        legend.justification = "center")+
  facet_grid(cols = vars(lctgp), 
             labeller = labeller(lctgp = lact.labs))

# Time to Lameness Graph----

# set up variables
trimtolame <- lamecull %>% 
  filter (lifexlame == 0) %>%
  filter(ftdat > startdat) %>% 
  select (cowid, farm, ftdat, lact, birth, frsh, lifexlame, site,
          time2nextftdat, time2nextlame, censordat, trim_type) %>% 
  mutate(nextrmlame = if_else(is.na(time2nextlame),0,1),
         lctgp = case_when(lact == 1 ~ "1",
                           lact == 2 ~ "2",
                           TRUE ~ "3+"))
# KM graph
trim2lame <- trimtolame

fitKM <- survfit(Surv(censordat, nextrmlame) ~ lctgp, data = trim2lame) 

km<- ggsurvplot(fitKM, 
                pval = FALSE, 
                conf.int = TRUE,
                censor = FALSE,
                fun = "pct",
                size = 1,
                linetype = c(1,2,3), # for dashed lines but then legends if wacky
                palette = c("#440154ff", "#3b528bff", "#fde725ff"),
                legend = "bottom",
                legend.title = "Lactation",
                legend.labs = c("1st",
                                "2nd", "3+"),
                #   "Combined"), for add.all
                xlab = "Days to Lameness after Trim",
                ylab = "Percentage of Healthy Cows",
                xlim = c(0,180),
                ylim = c(50,100),
                break.time.by = 30)
km$plot <- km$plot+
  geom_vline(xintercept = 30, color = "dark grey",
             linewidth = 1, alpha = 0.75, 
             linetype = "dashed")+
  geom_vline(xintercept = 90, color = "dark grey",
             linewidth = 1, alpha = 0.75, 
             linetype = "dashed")+
  geom_vline(xintercept = 150, color = "dark grey",
             linewidth = 1, alpha = 0.75, 
             linetype = "dashed")+
  theme(axis.title.y = element_text(vjust=1))


# time to lameness table----
# changed models to create 2 tables for each farm
trim2lame_farm1 <- trimtolame 


fit_farm1 <- survfit(Surv(censordat, nextrmlame) ~ lctgp, 
                     data = trim2lame_farm1)

# farm 1
tbl_time_2_lame_farm1 <- tbl_survfit(fit_farm1,
            times = c(30, 60, 120, 180),
            label = " ",
            reverse = TRUE,
            label_header = "{time} Days") %>% 
  as_flex_table() %>% # makes formatting and pdf better
  add_header_row(top = TRUE,
                 values = c("Lactation",
                            "% Lame (Confidence Interval) at",
                            "",
                            "",
                            "")) %>% 
  bold(i = 1, bold = TRUE, part = "header") %>% # bolds headers
  bold(i = 2, bold = TRUE, part = "header") %>% 
  merge_at(i = 1:2, j = 1, part = "header") %>% #merges 1st row
  merge_at(i = 1, j = 2:5, part = "header") %>% # merges top columns
  autofit() %>% 
  fit_to_width(max_width= 6) |> 
  delete_rows( i= 1, part = "body") |> 
  hline_bottom(part = "body")

# Fit the survival model using the 'survfit' function

trim2lame <- trim2lame |> 
  mutate(trim_type = fct_relevel(trim_type,
                                 c("Dry", "Mid", "Early", "Late")))

fitKM <- survfit(Surv(censordat, nextrmlame) ~ lctgp, data = trim2lame)


km_trim <- ggsurvplot(fitKM, 
                      pval = FALSE, 
                      facet.by = c("trim_type"),
                      panel.labs = list(trim_type = c("Dry: (-60 to + 7 d from DRY)",
                                                      "Mid: (114-120 DIM)",
                                                      "Early: (<114 DIM)",
                                                      "Late: (>120 DIM)")),
                      conf.int = TRUE,
                      ncol = 2,
                      censor = FALSE,
                      fun = "pct",
                      size = 1,
                      linetype = c(1, 1, 1, 1,
                                   2, 2, 2, 2, 
                                   3, 3, 3, 3 
                      ), # Specify dashed lines for each group
                      palette = c("#440154ff", "#3b528bff","#fde725ff"),
                      legend = "bottom",
                      legend.title = "Lactation",
                      legend.labs = lact.labs,
                      xlab = "Days to Lameness after Trim",
                      ylab = "Percentage of Healthy Cows",
                      xlim = c(0, 180),
                      ylim = c(50, 100),
                      break.time.by = 30,
                      short.panel.labs = TRUE) +
  # Set the fill colors and linetypes for each group in the legend and overide
  # the default where lines and colours are separate
  # to get codes library(RColorBrewer) brewer.pal(n=5,"Accent")
  scale_fill_manual(values = c("#440154ff","#3b528bff", "#fde725ff"),
                    guide = guide_legend(override.aes = 
                                           list(linetype = c(1, 2, 3),
                                                fill = c("#440154ff","#3b528bff",
                                                         "#fde725ff")))) +
  guides(linetype = "none") # Remove the linetype legend


# by site
km_trim_site <- ggsurvplot(fitKM, 
                      pval = FALSE, 
                      facet.by = c("site"),
                      conf.int = TRUE,
                      ncol = 2,
                      censor = FALSE,
                      fun = "pct",
                      size = 1,
                      linetype = c(1, 1, 2, 2, 3, 3 
                      ), # Specify dashed lines for each group
                      palette = c("#440154ff", "#3b528bff","#fde725ff"),
                      legend = "bottom",
                      legend.title = "Lactation",
                      legend.labs = lact.labs,
                      xlab = "Days to Lameness after Trim",
                      ylab = "Percentage of Healthy Cows",
                      xlim = c(0, 180),
                      ylim = c(50, 100),
                      break.time.by = 30,
                      short.panel.labs = TRUE) +
  # Set the fill colors and linetypes for each group in the legend and overide
  # the default where lines and colours are separate
  # to get codes library(RColorBrewer) brewer.pal(n=5,"Accent")
  scale_fill_manual(values = c("#440154ff","#3b528bff", "#fde725ff"),
                    guide = guide_legend(override.aes = 
                                           list(linetype = c(1, 2, 3),
                                                fill = c("#440154ff","#3b528bff",
                                                         "#fde725ff")))) +
  guides(linetype = "none") # Remove the linetype legend

