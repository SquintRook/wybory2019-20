library(xml2)
library(rvest)
library(tidyverse)
library(lubridate)

url <- xml2::read_html("http://ewybory.eu/wybory-prezydenckie-2020/sondaze-prezydenckie/")
df <- rvest::html_table(url, fill = TRUE)[[1]]%>%
  .[-(1:2),-c(10:ncol(.))]
  
colnames(df) <- c("agencja", "pub", "data_bad", "RB", "MKB", "WKK", "SzH", "AD", "KB")

df <- mutate(df, agencja = as.character(agencja),
             pub_d = str_split_fixed(df$pub, "\\.", n = 2)[,1],
             pub_mies = str_split_fixed(df$pub, "\\.", n = 2)[,2],
             pub_data = NA,
             bad_ = str_split_fixed(data_bad, "-", n = 2)[,2],
             bad_data = NA)

df[,4:9] <- apply(df[,4:9], 2, as.numeric)

df$bad_[df$bad_ == ""] <- df$data_bad[df$bad_ == ""]
df$bad_[df$bad_ == "?"] <- NA

df <- mutate(df, bad_d = str_split_fixed(bad_, "\\.", n = 2)[,1],
             bad_mie = str_split_fixed(bad_, "\\.", n = 2)[,2])

df$pub_data <- as.Date(with(df, paste("2020", pub_mies, pub_d ,sep="-")), "%Y-%m-%d")
df$bad_data <- as.Date(with(df, paste("2020", bad_mie, bad_d ,sep="-")), "%Y-%m-%d")

df <- select(df,-c(data_bad, pub, pub_d, pub_mies, bad_, bad_d, bad_mie ))%>%
  arrange(pub_data)

df$bad_data[is.na(df$bad_data)] <- df$pub_data[is.na(df$bad_data)]-4

df$agencja[df$agencja == "Kantar/CAPI"] <- "Kantar"
df$agencja[df$agencja == "Kantar/CATI"] <- "Kantar"
df$agencja[df$agencja == "Kantar/CATI (10.05)"] <- "Kantar"
df$agencja[df$agencja == "IBRiS (10.05)"] <- "IBRiS"
rm("url")
