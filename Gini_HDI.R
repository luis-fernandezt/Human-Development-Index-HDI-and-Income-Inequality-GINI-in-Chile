# Select country from HDI and Gini index and join ####
hdi.cl <- sdt_hdi %>% filter(country =="Chile")
hdi.cl <- hdi.cl[,c(1:2)]
colnames(hdi.cl) <- c("year", "ratio")
hdi.cl$index <- c("hdi.cl")
str(hdi.cl)

gini.cl <- sdt_gini %>% filter(country =="Chile")
gini.cl <- gini.cl[,c(1:2)]
colnames(gini.cl) <- c("year", "ratio")
gini.cl$index <- c("gini.cl")
str(gini.cl)

sdt_idh_gini <- rbind(hdi.cl, gini.cl)
rm(hdi.cl, gini.cl)
str(sdt_idh_gini)

# Plot Gini and HD index #### 
cols <- c("gini.cl" = "orangered", "hdi.cl" = "darkcyan")

gg3.gini.hdi <- 
ggplot(sdt_idh_gini, aes(x=year, y=ratio, group=index, color=index)) +
  geom_line(mapping = aes(y=ratio), size = 2, sdt_idh_gini %>% filter(!is.na(ratio))) +
  geom_point(mapping = aes(y=ratio), fill= "white", size = 11, sdt_idh_gini %>% filter(!is.na(ratio))) +
    scale_colour_manual(values = cols, name= "") +
  
    geom_text(aes(label=format(round(ratio, 3), decimal.mark = ",", sep_mark = ".")), color = "white", 
            size= 3,  hjust = 0.5, vjust = 0.5, fontface= "bold", show.legend = F) + 
  
  theme_light() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic"),
        legend.text = element_text(color = "black", size = 12)) +
  
  scale_x_date(date_breaks = '1 year', date_labels = "%Y", expand = expansion(mult = c(0.02,0.01))) +
  scale_y_continuous(trans = 'identity', breaks = c(0,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) +
  #ylim(0.25,1) +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Year", 
       y = "Ratio", 
       title = "Gini index and IDH, Chile", 
       subtitle = "1987 - 2019", 
       caption = "Worldbank.org (Gini index) | Undp.org (IDH)")

gg3.gini.hdi

#ggsave(plot = gg3.gini.hdi, filename = './gg3_gini_hdi_cl.png',
 #     units = 'mm', width = 279, height = 216, dpi = 300)

# Join boths DF by country names and export to *.xlsx ####
rnk.idh.2017 <- as.data.frame(rnk.idh.2017)
rnk.idh.2017$HDI <- as.numeric(rnk.idh.2017$HDI)

Rnk.Gini.hdi.2017 <- merge.data.frame(rnk.idh.2017, rnk.gini.2017)
str(Rnk.Gini.hdi.2017)

#write_xlsx(Rnk.Gini.hdi.2017, "Rnk-Gini-hdi-2017.xlsx", col_names = TRUE)

# Plot all countries by Gini and HDI, year 2017 in the same file ####
gg4 <- 
Rnk.Gini.hdi.2017 %>%
  filter(!is.na(Gini)) %>%
  arrange(desc(Gini)) %>%
  mutate(Country=factor(Country, Country)) %>%  
  ggplot( aes(x=Country, y=Gini, fill=Gini) ) +
  geom_bar(stat="identity", fill="#69b3a2") +
  geom_bar(aes(x=Country, y=Gini, fill=Gini), stat="identity", fill="orangered", Rnk.Gini.hdi.2017 
           %>% filter(Country == "Chile")) +
  geom_text(aes(label=format(round(Gini, 3), decimal.mark = ",", sep_mark = ".")), color = "white", 
            size= 3,  hjust = 1.1, vjust = 0.5, fontface= "bold", show.legend = F) +
  coord_flip() +
  theme_light() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none") +
  xlab("") +
  ylab("Gini Index") +
  ylim(0,1)

gg5 <- 
Rnk.Gini.hdi.2017 %>%
  filter(!is.na(HDI)) %>%
  arrange(HDI) %>%
  mutate(Country=factor(Country, Country)) %>%  
  ggplot( aes(x=Country, y=HDI, fill=HDI) ) +
  geom_bar(stat="identity", fill="#69b3a2") +
  geom_bar(aes(x=Country, y=HDI, fill=HDI), stat="identity", fill="orangered", Rnk.Gini.hdi.2017 
           %>% filter(Country == "Chile")) +
  geom_text(aes(label=format(round(HDI, 3), decimal.mark = ",", sep_mark = ".")), color = "white", 
            size= 3,  hjust = 1.1, vjust = 0.5, fontface= "bold", show.legend = F) +
    coord_flip() +
  theme_light() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none") +
  xlab("") +
  ylab("HDI Index") +
  ylim(0,1)

gg6.bar <- ggarrange(gg4, gg5 + rremove("x.text"), ncol = 2, nrow = 1)

gg6.bar <- annotate_figure(gg6.bar, top = text_grob("Gini and Human Development index | Year 2017", color = "black", face = "bold", size = 16),
                       bottom = text_grob("Worldbank.org (Gini index) | Undp.org (IDH)",
                                          color = "grey", hjust = 1.01, x = 1, face = "italic", size = 10))

gg6.bar

#ggsave(plot = gg6.bar, filename = './gg4_Rnk_Gini_HDI.png',
 #      units = 'mm', width = 216, height = 279, dpi = 300)

