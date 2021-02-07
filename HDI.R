# Human Develop Index (HDI) from Hdr.undp.org ####
hdi <- read_csv("Human Development Index (HDI).csv", skip = 5)
hdi <- as.data.frame(hdi)

# Remove columns that have NAs
hdi$X4 <- NULL
hdi$X6 <- NULL
hdi$X8 <- NULL
hdi$X10 <- NULL
hdi$X12 <- NULL
hdi$X14 <- NULL
hdi$X16 <- NULL
hdi$X18 <- NULL
hdi$X20 <- NULL
hdi$X22 <- NULL
hdi$X24 <- NULL
hdi$X26 <- NULL
hdi$X28 <- NULL
hdi$X30 <- NULL
hdi$X32 <- NULL
hdi$X34 <- NULL
hdi$X36 <- NULL
hdi$X38 <- NULL
hdi$X40 <- NULL
hdi$X42 <- NULL
hdi$X44 <- NULL
hdi$X46 <- NULL
hdi$X48 <- NULL
hdi$X50 <- NULL
hdi$X52 <- NULL
hdi$X54 <- NULL
hdi$X56 <- NULL
hdi$X58 <- NULL
hdi$X60 <- NULL
hdi$X62 <- NULL

# Select countries to plot
Chile.idh <- hdi %>%  filter(Country %in% c("Chile"))
Chile.idh <- t(Chile.idh[,3:ncol(Chile.idh)])
colnames(Chile.idh) <- c("idh")
Chile.idh <- dplyr::as_data_frame(Chile.idh, rownames = "year")
Chile.idh$idh <- as.numeric(Chile.idh$idh)
Chile.idh$year <- as.Date(as.character(Chile.idh$year), format = "%Y")
Chile.idh$country <- c("Chile")
str(Chile.idh)

Salvador.idh <- hdi %>%  filter(Country %in% c("El Salvador"))
Salvador.idh <- t(Salvador.idh[,3:ncol(Salvador.idh)])
colnames(Salvador.idh) <- c("idh")
Salvador.idh <- dplyr::as_data_frame(Salvador.idh, rownames = "year")
Salvador.idh$idh <- as.numeric(Salvador.idh$idh)
Salvador.idh$year <- as.Date(as.character(Salvador.idh$year), format = "%Y")
Salvador.idh$country <- c("El Salvador")
str(Salvador.idh)

Spain.idh <- hdi %>%  filter(Country %in% c("Spain"))
Spain.idh <- t(Spain.idh[,3:ncol(Spain.idh)])
colnames(Spain.idh) <- c("idh")
Spain.idh <- dplyr::as_data_frame(Spain.idh, rownames = "year")
Spain.idh$idh <- as.numeric(Spain.idh$idh)
Spain.idh$year <- as.Date(as.character(Spain.idh$year), format = "%Y")
Spain.idh$country <- c("Spain")
str(Spain.idh)

sdt_hdi <- rbind(Chile.idh, Salvador.idh, Spain.idh)
sdt_hdi <- as.data.frame(sdt_hdi)
str(sdt_hdi)

rm(Chile.idh, Chile.idh, Salvador.idh, Spain.idh, LAC, OCDE)

# Plot Human Develop index ####
gg2.hdi <- 
ggplot(sdt_hdi, aes(x=year, y=idh, group=country, color=country)) +
  geom_line(size = 1.3, data = sdt_hdi %>% filter(!is.na(idh))) +
  geom_point(fill= "white", size = 11, data = sdt_hdi %>% filter(!is.na(idh))) +
  geom_text(aes(label=format(round(idh, 3), decimal.mark = ",", sep_mark = ".")), color = "white", 
            size= 3,  hjust = 0.5, vjust = 0.5, fontface= "bold", show.legend = F) +
  
  theme_light() +
  theme(legend.position = "bottom", #c(0.2,0.8), 
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic"),
        legend.text = element_text(color = "black", size = 12)) +
  
  scale_colour_viridis_d(name = "", option = 'C', begin = 0, end = 0.8, direction = -1, alpha = 0.9) +
  scale_x_date(date_breaks = '1 year', date_labels = "%Y", expand = expansion(mult = c(0.02,0.02))) +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Year", 
       y = "Ratio", 
       title = "Human Development Index (HDI)", 
       subtitle = "Reports 1990-2019", 
       caption = "Hdr.undp.org/ (HDI index)")

gg2.hdi

#ggsave(plot = gg2.hdi, filename = './gg2.hdi.png',
 #     units = 'mm', width = 279, height = 216, dpi = 300)


# Rankig countries HD index ####
rnk.idh.2017 <- hdi[,c(2,30)]
rnk.idh.2017 <- rnk.idh.2017 %>% filter(!is.na(`2017`))
rnk.idh.2017 <- rnk.idh.2017[order(rnk.idh.2017$`2017`, decreasing = T), ]
rnk.idh.2017 <- rnk.idh.2017[-(c(205:206)),]
row.names(rnk.idh.2017) <- c(1:204)
colnames(rnk.idh.2017) <- c("Country", "HDI")
rnk.idh.2017
