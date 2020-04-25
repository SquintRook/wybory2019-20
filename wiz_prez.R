library(ggplot2)

source("scrapping_prez.R")
# alternatively:
# https://raw.githubusercontent.com/SquintRook/wybory2019-20/master/scrapping_prez.R 

########## Wizualizacja szeregów czasowych #######

alph <- 0.4      # przejrzystość punktów
conf <- FALSE    # czy chcemy przedział ufności
spn <- 0.8       # jak bardzo ma wygładzać geom_smooth
point_size <- 1.4

sond_prez$agencja[sond_prez$agencja %in% c("CBOS", "Maison & Partners", "PPG", "United Survey", "IBSP")] <- "inne"

ggplot(data = sond_prez)+
  scale_x_date(breaks = "1 weeks",
               date_labels = "%d/%m",
               limits = c(min(sond_prez$bad_data), as.Date("2020-05-10")))+
  geom_point(aes(x = bad_data, y = RB/100, color = "R. Biedroń", shape = agencja), size = point_size, alpha = alph)+
  geom_smooth(aes(x = bad_data, y = RB/100, color = "R. Biedroń"), se = conf, span = spn)+
  geom_point(aes(x = bad_data, y = WKK/100, color = "W. Kosiniak-Kamysz", shape = agencja), size = point_size, alpha = alph)+
  geom_smooth(aes(x = bad_data, y = WKK/100, color = "W. Kosiniak-Kamysz"), se = conf, span = spn)+
  geom_point(aes(x = bad_data, y = SzH/100, color = "S. Hołownia", shape = agencja), size = point_size, alpha = alph)+
  geom_smooth(aes(x = bad_data, y = SzH/100, color = "S. Hołownia"), se = conf, span = spn)+
  geom_point(aes(x = bad_data, y = AD/100, color = "A. Duda", shape = agencja), size = point_size, alpha = alph)+
  geom_smooth(aes(x = bad_data, y = AD/100, color = "A. Duda"), se = conf, span = spn)+
  geom_point(aes(x = bad_data, y = KB/100, color = "K. Bosak", shape = agencja), size = point_size, alpha = alph)+
  geom_smooth(aes(x = bad_data, y = KB/100, color = "K. Bosak"), se = conf, span = spn)+
  geom_point(aes(x = bad_data, y = MKB/100, color = "M. Kidawa-Błońska", shape = agencja), size = point_size, alpha = alph)+
  geom_smooth(aes(x = bad_data, y = MKB/100, color = "M. Kidawa-Błońska"), se = conf, span = spn)+
  labs(x = "", y = "", caption = "twitter: @mateusz_dadej, źródło: ewybory.eu",
       title = "Wyniki sondaży dla głównych kandydatów w wyborach prezydenckich")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0,0.6,0.1))+
  theme_minimal()+
  theme(legend.position = "bottom", legend.title = element_blank())+
  scale_color_manual(values = c("coral2", "tan3", "steelblue3", "darkorchid3", "gold1", "springgreen3"))

########## to samo ale bez dudy #############

ggplot(data = select(sond_prez, - AD))+
  scale_x_date(breaks = "1 weeks",
               date_labels = "%d/%m",
               limits = c(min(sond_prez$bad_data), as.Date("2020-05-10")))+
  geom_point(aes(x = bad_data, y = RB/100, color = "R. Biedroń", shape = agencja), size = point_size, alpha = alph)+
  geom_smooth(aes(x = bad_data, y = RB/100, color = "R. Biedroń"), se = conf, span = spn)+
  geom_point(aes(x = bad_data, y = WKK/100, color = "W. Kosiniak-Kamysz", shape = agencja), size = point_size, alpha = alph)+
  geom_smooth(aes(x = bad_data, y = WKK/100, color = "W. Kosiniak-Kamysz"), se = conf, span = spn)+
  geom_point(aes(x = bad_data, y = SzH/100, color = "S. Hołownia", shape = agencja), size = point_size, alpha = alph)+
  geom_smooth(aes(x = bad_data, y = SzH/100, color = "S. Hołownia"), se = conf, span = spn)+
  geom_point(aes(x = bad_data, y = KB/100, color = "K. Bosak", shape = agencja), size = point_size, alpha = alph)+
  geom_smooth(aes(x = bad_data, y = KB/100, color = "K. Bosak"), se = conf, span = spn)+
  geom_point(aes(x = bad_data, y = MKB/100, color = "M. Kidawa-Błońska", shape = agencja), size = point_size, alpha = alph)+
  geom_smooth(aes(x = bad_data, y = MKB/100, color = "M. Kidawa-Błońska"), se = conf, span = spn)+
  labs(x = "", y = "", caption = "twitter: @mateusz_dadej, źródło: ewybory.eu",
       title = "Wyniki sondaży dla kandydatów w wyborach prezydenckich z pominięciem PAD")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0,0.3,0.05))+
  theme_minimal()+
  theme(legend.position = "right", legend.title = element_blank())+
  scale_color_manual(values = c("tan3", "steelblue3", "darkorchid3", "gold1", "springgreen3"))

######## ridge plot #############

library(ggplot2movies)

pivot_longer(sond_prez, cols = c("RB", "MKB", "WKK", "SzH", "AD", "KB"))%>%
  mutate(value = value/100)%>%
  ggplot(aes(x = value, y = name, group = name))+
  geom_density_ridges_gradient(aes(fill = ..x..), scale = 1.5, rel_min_height = 0.00001, jittered_points = FALSE)+
  scale_fill_continuous(type = "viridis", name = "", option = "C",labels = scales::percent)+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1L), breaks = seq(0,0.7, 0.1))+
  scale_y_discrete(labels = c("A. Duda", "K. Bosak", "M. Kidawa-Błońska", "R. Biedroń", "S. Hołownia", "W. Kosianiak-Kamysz"))+
  labs(title = "Rozkład poparcia dla kandydatów na prezydenta w wyborach 2020", y = "",
       x = "poparcie", caption = "Twitter: @SquintRook, dane: ewybory.eu",
       subtitle = "Wyniki sondaży wyborczych prowadzonych od listopada 2019 do konca kwietnia 2020")+
  theme_minimal()
  
  
