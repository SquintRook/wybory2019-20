library(ggplot2)

source("scrapping_prezydenckie.R")

########## Wizualizacja szeregów czasowych #######

alph <- 0.4      # przejrzystość punktów
conf <- FALSE    # czy chcemy przedział ufności
spn <- 0.4       # jak bardzo ma wygładzać geom_smooth


ggplot(data = df)+
  scale_x_date(breaks = "1 weeks",
               date_labels = "%d/%m",
               limits = c(min(df$bad_data), as.Date("2020-05-10")))+
  geom_point(aes(x = bad_data, y = RB/100, color = "R. Biedroń"), alpha = alph)+
  geom_smooth(aes(x = bad_data, y = RB/100, color = "R. Biedroń"), se = conf, span = spn)+
  geom_point(aes(x = bad_data, y = WKK/100, color = "W. Kosiniak-Kamysz"), alpha = alph)+
  geom_smooth(aes(x = bad_data, y = WKK/100, color = "W. Kosiniak-Kamysz"), se = conf, span = spn)+
  geom_point(aes(x = bad_data, y = SzH/100, color = "S. Hołownia"), alpha = alph)+
  geom_smooth(aes(x = bad_data, y = SzH/100, color = "S. Hołownia"), se = conf, span = spn)+
  geom_point(aes(x = bad_data, y = AD/100, color = "A. Duda"), alpha = alph)+
  geom_smooth(aes(x = bad_data, y = AD/100, color = "A. Duda"), se = conf, span = spn)+
  geom_point(aes(x = bad_data, y = KB/100, color = "K. Bosak"), alpha = alph)+
  geom_smooth(aes(x = bad_data, y = KB/100, color = "K. Bosak"), se = conf, span = spn)+
  geom_point(aes(x = bad_data, y = MKB/100, color = "M. Kidawa-Błońska"), alpha = alph)+
  geom_smooth(aes(x = bad_data, y = MKB/100, color = "M. Kidawa-Błońska"), se = conf, span = spn)+
  labs(x = "", y = "poparcie", caption = "źródło: ewybory.eu, twitter: @mateusz_dadej",
       title = "Wyniki sondaży dla głównych kandydatów w wyborach prezydenckich")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0,0.6,0.1))+
  theme_minimal()+
  theme(legend.position = "bottom", legend.title = element_blank())+
  scale_color_manual(values = c("coral2", "tan3", "steelblue3", "darkorchid3", "gold1", "springgreen3"))


