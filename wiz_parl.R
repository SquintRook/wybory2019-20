library(readxl)
library(tidyverse)
library(stringr)
library(lubridate)
library(zoo)
library(metagen)
library(ggridges)

select <- dplyr::select 

################ sondaże ################

sondaze <- read_excel("sondaze.xlsx")%>%
  as.data.frame()%>%
  mutate(pop_lewica = as.numeric(pop_lewica),
         pop_ko = as.numeric(pop_ko),
         pop_psl = as.numeric(pop_psl),
         pop_pis = as.numeric(pop_pis),
         pop_konf = as.numeric(pop_konf))

okres <- sondaze$okres%>%
  str_split_fixed("-", n = 2)%>%
  as.data.frame()%>%
  mutate(V2 = ifelse(V2 == "",NA,as.character(V2)),
         V1 = ifelse(V1 == "?", NA, as.character(V1)))

okres$V2[is.na(okres$V2)] <- as.character(okres$V1[is.na(okres$V2)])
okres <- okres[,2]

repeat.before <-  function(x) {   # repeats the last non NA value. Keeps leading NA
  ind = which(!is.na(x))      # get positions of nonmissing values
  if(is.na(x[1]))             # if it begins with a missing, add the 
    ind = c(1,ind)        # first position to the indices
  rep(x[ind], times = diff(   # repeat the values at these indices
    c(ind, length(x) + 1) )) # diffing the indices + length yields how often 
}                               # they need to be repeated

sondaze <- cbind(okres, sondaze[,-4])%>%
  mutate(data = as.Date(okres, format = "%d.%m.%Y"))%>%
  select(-c(pub, okres))%>%
  map_df(rev)
  
sondaze[,3:7] <- apply(sondaze[,3:7], 2, as.numeric)%>%
  apply(2, function(x){x*0.01})

sondaze <- sondaze[order(sondaze$data),]%>%
  as.data.frame()

alph <- 0.4
grub_lin <- 1

ggplot(data = sondaze, aes(x = data))+
  geom_point(aes(y = pop_pis, color = "PIS"), alpha = alph-0.1)+
  geom_smooth(aes(y = pop_pis, color = "PIS"), se = FALSE, size = grub_lin)+
  geom_point(aes(y = pop_ko, color = "KO"), alpha = alph)+
  geom_smooth(aes(y = pop_ko, color = "KO"), se = FALSE, size = grub_lin)+
  geom_point(aes(y = pop_lewica, color = "Lewica"), alpha = alph)+
  geom_smooth(aes(y = pop_lewica, color = "Lewica"), se = FALSE, size = grub_lin)+
  geom_point(aes(y = pop_psl, color = "PSL"), alpha = alph)+
  geom_smooth(aes(y = pop_psl, col = "PSL"), se = FALSE, size = grub_lin)+
  geom_point(aes(y = pop_konf, color = "Konfederacja"), alpha = alph)+
  geom_smooth(aes(y = pop_konf, col = "Konfederacja"), se = FALSE, size = grub_lin)+
  scale_color_manual(values = c( "chocolate2", "darkgoldenrod4","red", "blue4", "chartreuse3"))+
  labs(title = "Wyniki sondaży dla wiodących partii w Polsce",
       caption = " Twitter: @SquintRook, dane: ewybory.eu",
       subtitle = "linie ciągłe to regresje lokalne, zaznaczony okres to wybory europarlamentarne")+
  ylab("Procent poparcia")+
  xlab("Data kończąca przeprowadzenie sondażu")+
  scale_y_continuous(labels = scales::percent)+
  coord_cartesian(xlim = as.Date(c("2018-01-01", "2019-10-10")))+
  geom_vline(xintercept = as.Date("2019-05-25"), color = "blue", size = 1.5, alpha = 0.2)+
  annotate(geom = "text",x = as.Date("2019-04-25"),
           y = 0.18, label = "Wybory europarlamentarne", size = 2.9)+
  theme_minimal()

################ frekwencja ################
freq <- read_excel("frekwencja.xlsx")%>%
  as.data.frame()%>%
  .[-c(8,9,10,16,29),]

lm(filter(freq, rodzaj == "Wybory parlamentarne")$frekwencja[-c(1)]~
    freq$frekwencja[(which(freq$rodzaj == "Wybory parlamentarne")-1)] )%>%
  summary()

ggplot(data = freq, aes(x = rok))+
  geom_point(aes(y = frekwencja, color = rodzaj), size = 2)+
  geom_line(aes(y = frekwencja, color = rodzaj), size = 1)+
  theme(legend.position="bottom")+
  scale_y_continuous(labels = scales::percent)+
  labs(title = "Frekwencja w ważniejszych wyborach w Polsce",
       caption = "Twitter: @SquintRook, źródło: wikipedia")+
  theme(legend.title = element_blank())+
  scale_x_continuous(breaks = seq(1990,2020,2))

cor(filter(freq, rodzaj == "Wybory parlamentarne")$frekwencja[-c(1,3,5)], 
    filter(freq, rodzaj == "Wybory prezydenckie")$frekwencja)

freq_par <- filter(freq, rodzaj == "Wybory parlamentarne")$frekwencja[-c(1,3,5)]
freq_prez <- filter(freq, rodzaj == "Wybory prezydenckie")$frekwencja
frek_lag<- freq$frekwencja[which(freq$frekwencja %in% filter(freq, rodzaj == "Wybory parlamentarne")$frekwencja[-c(1,3,5)])-1]

lm(freq_par ~ freq_prez + frek_lag)%>%
  predict(newdata =  data.frame(freq_prez = 0.4896, frek_lag = 0.54))


############### prognozy sondaży vs rzeczywistość #############

od_kiedy <- "2019-07-19"
wyniki_oficjalne <- data.frame(grupa = c("po_konf", "pop_psl", "pop_lewica", "pop_ko", "pop_pis"),
                               wyniki = c(0.068, 0.086, 0.126, 0.274, 0.436))

sondaze_ostatnie <- filter(sondaze, data> od_kiedy)%>%
  gather(group, Value, colnames(sondaze[,3:(ncol(sondaze)-1)]))%>%
  drop_na()%>%
  mutate(group = factor(group, levels = c("pop_konf", "pop_psl", "pop_lewica", "pop_ko", "pop_pis")))

n_prognoz <- table(sondaze_ostatnie$group)

ggplot(data = sondaze_ostatnie, aes(x = Value, y = group, fill = ..x..))+
  geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.00001, jittered_points = FALSE,
                               position = position_points_jitter(height = 0.1),
                               point_size = 1, point_color = "black")+
  scale_fill_continuous(type = "viridis", name = "Prognozowane poparcie",labels = scales::percent,
                        option = "C")+
  scale_x_continuous(labels = scales::percent)+
  ylab("")+
  xlab("Poparcie")+
  labs(title = "Prognozy sondaży vs. Faktyczne poparcie",
       subtitle = paste("Jak sprawdziły się prognozy z sondaży od", od_kiedy),
       caption = "Twitter: @SquintRook, dane: ewybory.eu")+
  scale_y_discrete(labels = c(paste("Konfederacja (n = ", n_prognoz[1],")"),
                              paste("PSL (n = ", n_prognoz[2],")"), 
                              paste("Lewica (n = ", n_prognoz[3],")"),
                              paste("KO (n = ", n_prognoz[4],")"),
                              paste("PiS (n = ", n_prognoz[5],")")))+
  geom_segment(data = wyniki_oficjalne, aes(x = wyniki, xend = wyniki, yend = c(1:5)+.9,
                                            y = c(1:5)), color = "red", size = 1)+
  annotate("text",y = c(1.5,2.5,3.5,4.5,5.5), x = wyniki_oficjalne$wyniki+0.05, 
           label = c("6.8%", "8.6%", "12.6%", "27.4%", "43.6%"), size = 3)

               