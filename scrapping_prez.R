library(xml2)
library(rvest)
library(tidyverse)
library(lubridate)

############### pobieranie sondaży wszystkich kandydatów ###############

url <- xml2::read_html("http://ewybory.eu/wybory-prezydenckie-2020/sondaze-prezydenckie/")
sond_prez <- rvest::html_table(url, fill = TRUE)[[1]]%>%
  .[-(1:2),-c(10:ncol(.))]
  
colnames(sond_prez) <- c("agencja", "pub", "data_bad", "RB", "MKB", "WKK", "SzH", "AD", "KB")

sond_prez <- mutate(sond_prez, agencja = as.character(agencja),
             pub_d = str_split_fixed(sond_prez$pub, "\\.", n = 2)[,1],
             pub_mies = str_split_fixed(sond_prez$pub, "\\.", n = 2)[,2],
             pub_data = NA,
             bad_ = str_split_fixed(data_bad, "-", n = 2)[,2],
             bad_data = NA)

sond_prez[,4:9] <- apply(sond_prez[,4:9], 2, as.numeric)

sond_prez$bad_[sond_prez$bad_ == ""] <- sond_prez$data_bad[sond_prez$bad_ == ""]
sond_prez$bad_[sond_prez$bad_ == "?"] <- NA

sond_prez <- mutate(sond_prez, bad_d = str_split_fixed(bad_, "\\.", n = 2)[,1],
             bad_mie = str_split_fixed(bad_, "\\.", n = 2)[,2])

sond_prez$pub_data <- as.Date(with(sond_prez, paste("2020", pub_mies, pub_d ,sep="-")), "%Y-%m-%d")
sond_prez$bad_data <- as.Date(with(sond_prez, paste("2020", bad_mie, bad_d ,sep="-")), "%Y-%m-%d")

sond_prez <- select(sond_prez,-c(data_bad, pub, pub_d, pub_mies, bad_, bad_d, bad_mie ))%>%
  arrange(pub_data)

sond_prez$bad_data[is.na(sond_prez$bad_data)] <- sond_prez$pub_data[is.na(sond_prez$bad_data)]-4

sond_prez$agencja[sond_prez$agencja == "Kantar/CAPI"] <- "Kantar"
sond_prez$agencja[sond_prez$agencja == "Kantar/CATI"] <- "Kantar"
sond_prez$agencja[sond_prez$agencja == "Kantar/CATI (10.05)"] <- "Kantar"
sond_prez$agencja[sond_prez$agencja == "IBRiS (10.05)"] <- "IBRiS"
rm("url")


