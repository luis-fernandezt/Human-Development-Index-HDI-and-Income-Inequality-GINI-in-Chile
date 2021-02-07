# Gini index from Worldbank.org ####
gini <- read_csv("API_SI.POV.GINI_DS2_en_csv_v2_1926631.csv", col_types = cols(X3 = col_double()), skip = 4)
gini <- as.data.frame(gini)
str(gini)

# Select countries to plot
Chile.gini <- gini %>%  filter(`Country Name` %in% c("Chile"))
Chile.gini <- t(Chile.gini[,32:ncol(Chile.gini)])
colnames(Chile.gini) <- c("gini")
Chile.gini <- dplyr::as_data_frame(Chile.gini, rownames = "year")
Chile.gini$gini <- as.numeric(Chile.gini$gini/100)
Chile.gini$year <- as.Date(as.character(Chile.gini$year), format = "%Y")
Chile.gini$country <- c("Chile")
Chile.gini <- as.data.frame(Chile.gini)
# Chile.gini <- Chile.gini[-(c(34:35)),]
str(Chile.gini)

Salvador.gini <- gini %>%  filter(`Country Name` %in% c("El Salvador"))
Salvador.gini <- t(Salvador.gini[,32:ncol(Salvador.gini)])
colnames(Salvador.gini) <- c("gini")
Salvador.gini <- dplyr::as_data_frame(Salvador.gini, rownames = "year")
Salvador.gini$gini <- as.numeric(Salvador.gini$gini/100)
Salvador.gini$year <- as.Date(as.character(Salvador.gini$year), format = "%Y")
Salvador.gini$country <- c("El Salvador")
Salvador.gini <- as.data.frame(Salvador.gini)
#Salvador.gini <- Salvador.gini[-(c(34:35)),]
str(Salvador.gini)

Spain.gini <- gini %>%  filter(`Country Name` %in% c("Spain"))
Spain.gini <- t(Spain.gini[,32:ncol(Spain.gini)])
colnames(Spain.gini) <- c("gini")
Spain.gini <- dplyr::as_data_frame(Spain.gini, rownames = "year")
Spain.gini$gini <- as.numeric(Spain.gini$gini/100)
Spain.gini$year <- as.Date(as.character(Spain.gini$year), format = "%Y")
Spain.gini$country <- c("Spain")
Spain.gini <- as.data.frame(Spain.gini)
# Spain.gini <- Spain.gini[-(c(34:35)),]
str(Spain.gini)

sdt_gini <- rbind(Chile.gini, Salvador.gini, Spain.gini)
sdt_gini <- as.data.frame(sdt_gini)
str(sdt_gini)

rm(Chile.gini, Salvador.gini, Spain.gini)

# Plot Gini index ####
gg1.gini <- 
ggplot(sdt_gini, aes(x=year, y=gini, group=country, color=country)) +
  geom_line(size = 1.3, data = sdt_gini %>% filter(!is.na(gini))) +
  geom_point(fill= "white", size = 11, data = sdt_gini %>% filter(!is.na(gini))) +
  geom_text(aes(label=format(round(gini, 3), decimal.mark = ",", sep_mark = ".")), color = "white", 
            size= 3,  hjust = 0.5, vjust = 0.5, fontface= "bold", show.legend = F) +
  
  theme_light() +
  theme(legend.position = "bottom", #c(0.2,0.8), 
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic"),
        legend.text = element_text(color = "black", size = 12)) +
  
  scale_colour_viridis_d(name = "", option = 'C', begin = 0, end = 0.8, direction = -1, alpha = 0.9) +
  scale_x_date(date_breaks = '1 year', date_labels = "%Y", expand = expansion(mult = c(0.02,0.00))) +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Year", 
       y = "Ratio", 
       title = "Gini Index", 
       subtitle = "World Bank stimate 1990-2017 (+-)", 
       caption = "Worldbank.org (Gini index)")

gg1.gini

#ggsave(plot = gg1.gini, filename = './gg1.gini.png',
 #     units = 'mm', width = 279, height = 216, dpi = 300)

# Rankig countries Gini index ####
rnk.gini.2017 <- gini[,c(1,62)]
rnk.gini.2017 <- rnk.gini.2017 %>% filter(!is.na(`2017`))

rnk.gini.2017 <- rnk.gini.2017[order(rnk.gini.2017$`2017`, decreasing = F), ]
row.names(rnk.gini.2017) <- c(1:67)
colnames(rnk.gini.2017) <- c("Country", "Gini")
rnk.gini.2017$Gini <- rnk.gini.2017$Gini/100
rnk.gini.2017
