list.of.packages <- c("tidyverse", "dplyr", "readr", "ggplot2", "writexl", "ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,c("Package")] )]
if(length(new.packages)) install.packages(new.packages)
rm(list.of.packages)
rm(new.packages)

library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(writexl)
library(ggpubr)