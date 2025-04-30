# load packages
library(readr)
library(utils)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(writexl)
library(readr)
library(ggpubr)
library(stringr)

################################################################################
#                            Gini index from Worldbank.org                    #
################################################################################

#BD ENE 2022 12 NDE.SAV, 31.73 MB
download.file("https://api.worldbank.org/v2/en/indicator/SI.POV.GINI?downloadformat=csv", "GINI.zip", mode="wb")
zipfile <- "./GINI.zip"
unzip(zipfile)
rm(zipfile)

# Gini index from Worldbank.org ####
gini <- read_csv("API_SI.POV.GINI_DS2_en_csv_v2_19376.csv", col_types = cols(X3 = col_double()), skip = 4)
gini <- as.data.frame(gini)
str(gini)

# Select countries to plot
Chile.gini <- gini %>%  filter(`Country Name` %in% c("Chile"))
Chile.gini <- t(Chile.gini[,38:ncol(Chile.gini)])
colnames(Chile.gini) <- c("gini")
Chile.gini <- dplyr::as_data_frame(Chile.gini, rownames = "year")
Chile.gini$gini <- as.numeric(Chile.gini$gini/100)
Chile.gini$year <- as.Date(as.character(Chile.gini$year), format = "%Y")
Chile.gini$country <- c("Chile")
Chile.gini <- as.data.frame(Chile.gini)
# Chile.gini <- Chile.gini[-(c(34:35)),]
str(Chile.gini)

Salvador.gini <- gini %>%  filter(`Country Name` %in% c("El Salvador"))
Salvador.gini <- t(Salvador.gini[,38:ncol(Salvador.gini)])
colnames(Salvador.gini) <- c("gini")
Salvador.gini <- dplyr::as_data_frame(Salvador.gini, rownames = "year")
Salvador.gini$gini <- as.numeric((Salvador.gini$gini)/100)
Salvador.gini$year <- as.Date(as.character(Salvador.gini$year), format = "%Y")
Salvador.gini$country <- c("El Salvador")
Salvador.gini <- as.data.frame(Salvador.gini)
#Salvador.gini <- Salvador.gini[-(c(34:35)),]
str(Salvador.gini)

Spain.gini <- gini %>%  filter(`Country Name` %in% c("Spain"))
Spain.gini <- t(Spain.gini[,38:ncol(Spain.gini)])
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

#rm(Chile.gini, Salvador.gini, Spain.gini)

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
       subtitle = "World Bank stimate 1990-2022", 
       caption = "Worldbank.org (Gini index)")

#gg1.gini
ggsave(plot = gg1.gini, filename = './gg1.gini.png', units = 'mm', width = 279, height = 216, dpi = 300)

# Rankig countries Gini index ####
rnk.gini <- gini[,c(1,67)]
rnk.gini <- rnk.gini %>% filter(!is.na(`2022`))
rnk.gini <- rnk.gini[order(rnk.gini$`2022`, decreasing = F), ]
names(rnk.gini) <- c("Country", "Gini")
rnk.gini$gini <- rnk.gini$gini/100
rnk.gini

################################################################################
#                   Human Develop Index (HDI) from Hdr.undp.org                #
################################################################################

download.file("https://hdr.undp.org/sites/default/files/2023-24_HDR/HDR23-24_Composite_indices_complete_time_series.csv",
              "Human Development Index (HDI).csv", mode="wb")

hdi <- read_csv("Human Development Index (HDI).csv")
hdi <- as.data.frame(hdi)

hdi <- hdi |> select(2:38)

# Select countries to plot
Chile.idh <- hdi %>%  filter(country %in% c("Chile"))
Chile.idh <- t(Chile.idh[,5:ncol(Chile.idh)])
colnames(Chile.idh) <- c("idh")
Chile.idh <- dplyr::as_data_frame(Chile.idh, rownames = "year")
Chile.idh$idh <- as.numeric(Chile.idh$idh)
Chile.idh$year <- as.numeric(unlist(str_extract_all(Chile.idh$year, "\\d+")))
Chile.idh$year <- as.Date(as.character(Chile.idh$year), format = "%Y")
Chile.idh$country <- c("Chile")
str(Chile.idh)

Salvador.idh <- hdi %>%  filter(country %in% c("El Salvador"))
Salvador.idh <- t(Salvador.idh[,5:ncol(Salvador.idh)])
colnames(Salvador.idh) <- c("idh")
Salvador.idh <- dplyr::as_data_frame(Salvador.idh, rownames = "year")
Salvador.idh$idh <- as.numeric(Salvador.idh$idh)
Salvador.idh$year<- as.numeric(unlist(str_extract_all(Salvador.idh$year, "\\d+")))
Salvador.idh$year <- as.Date(as.character(Salvador.idh$year), format = "%Y")
Salvador.idh$country <- c("El Salvador")
str(Salvador.idh)

Spain.idh <- hdi %>%  filter(country %in% c("Spain"))
Spain.idh <- t(Spain.idh[,5:ncol(Spain.idh)])
colnames(Spain.idh) <- c("idh")
Spain.idh <- dplyr::as_data_frame(Spain.idh, rownames = "year")
Spain.idh$idh <- as.numeric(Spain.idh$idh)
Spain.idh$year <- as.numeric(unlist(str_extract_all(Spain.idh$year, "\\d+")))
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
       subtitle = "Reports 1990-2022", 
       caption = "Hdr.undp.org/ (HDI index)")

#gg2.hdi
ggsave(plot = gg2.hdi, filename = './gg2.hdi.png', units = 'mm', width = 279, height = 216, dpi = 300)

# Rankig countries HD index ####
rnk.idh <- hdi[,c(1,37)]
rnk.idh <- rnk.idh %>% filter(!is.na(`hdi_2022`))
rnk.idh <- rnk.idh[order(rnk.idh$hdi_2022, decreasing = T), ]
rnk.idh <- rnk.idh[-(c(205:206)),]
row.names(rnk.idh) <- c(1:204)
colnames(rnk.idh) <- c("Country", "HDI")
rnk.idh

################################################################################
#                             GINI and HDI                                     #
################################################################################

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
       subtitle = "1987 - 2022", 
       caption = "Worldbank.org (Gini index) | Undp.org (IDH)")

#gg3.gini.hdi

ggsave(plot = gg3.gini.hdi, filename = './gg3_gini_hdi_cl.png',  units = 'mm', width = 279, height = 216, dpi = 300)

# Join boths DF by country names and export to *.xlsx ####
rnk.idh <- as.data.frame(rnk.idh)
rnk.idh$HDI <- as.numeric(rnk.idh$HDI)

Rnk.Gini.hdi <- merge.data.frame(rnk.idh, rnk.gini)
str(Rnk.Gini.hdi)

write_xlsx(Rnk.Gini.hdi, "./Rnk-Gini-hdi.xlsx", col_names = TRUE)

# Plot all countries by Gini and HDI, year 2017 in the same file ####
gg4 <- 
  Rnk.Gini.hdi %>%
  filter(!is.na(Gini)) %>%
  arrange(desc(Gini)) %>%
  mutate(Country=factor(Country, Country)) %>%  
  ggplot( aes(x=Country, y=Gini, fill=Gini) ) +
  geom_bar(stat="identity", fill="#69b3a2") +
  geom_bar(aes(x=Country, y=Gini, fill=Gini), stat="identity", fill="orangered", Rnk.Gini.hdi 
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
  ylab("Gini Index")
  #ylim(0,1)

gg5 <- 
  Rnk.Gini.hdi %>%
  filter(!is.na(HDI)) %>%
  arrange(HDI) %>%
  mutate(Country=factor(Country, Country)) %>%  
  ggplot( aes(x=Country, y=HDI, fill=HDI) ) +
  geom_bar(stat="identity", fill="#69b3a2") +
  geom_bar(aes(x=Country, y=HDI, fill=HDI), stat="identity", fill="orangered", Rnk.Gini.hdi 
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

gg6.bar <- annotate_figure(gg6.bar, top = text_grob("Gini and Human Development index | Year 2022", color = "black", face = "bold", size = 16),
                       bottom = text_grob("Worldbank.org (Gini index) | Undp.org (IDH)",
                                          color = "grey", hjust = 1.01, x = 1, face = "italic", size = 10))

#gg6.bar

ggsave(plot = gg6.bar, filename = './gg4_Rnk_Gini_HDI.png', units = 'mm', width = 216, height = 279, dpi = 300)
