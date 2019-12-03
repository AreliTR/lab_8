# lab_8
+ Se descargó la malla llamada **waste_vs_gdp**  
+ Se eliminaron los valores **NA** de la columna **poblacion_total**, y se filtraron los años **mayores a 1999**, quedando el periodo **2000-2013**  
+ Se agregó una columna llamada **continente**, con la finalidad de **agrupar** los países en esta variable 
+ Se agregó otra columna en la que **divide** la población por **millones de personas**

waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

names (waste_vs_gdp) = c("pais", "codigo", "ano", "KRPPD", "PIB_2011", "poblacion_total")

waste_vs_gdp <-
  waste_vs_gdp %>% drop_na(`poblacion_total`) %>%
  filter(ano > "1999") %>% 
  print()

waste_by_person <- 
  waste_vs_gdp %>% 
  mutate(continente = case_when(pais %in% c("Angola", "Algeria", "Benin", "Cape Verde", "Cameroon", "Comoros", 
                                              "Cote d'Ivoire", "Egypt", "Eritrea", "Gabon", "Gambia", "Ghana", 
                                              "Guinea", "Guinea-Bissau", "Equatorial Guinea", "Kenya", "Liberia", 
                                              "Libya", "Madagascar", "Morocco", "Mauritius", "Mauritania", 
                                              "Mozambique", "Namibia", "Nigeria", "Congo", "Democratic Republic of Congo",
                                              "Saint Helena", "Sao Tome and Principe", "Senegal", "Seychelles", 
                                              "Sierra Leone", "Somalia", "South Africa", "Sudan", "Tanzania", "Togo", 
                                              "Tunisia", "Djibouti") ~ "Africa",
                                pais %in% c("Anguilla", "Antigua and Barbuda", "Argentina", "Aruba", "Bahamas", 
                                              "Barbados", "Belize", "Bermuda", "Brazil", "Canada", "Cayman Islands", 
                                              "Chile", "Colombia", "Costa Rica", "Cuba", "Curacao", "Dominica", 
                                              "Dominican Republic", "Ecuador", "El Salvador", "Falkland Islands", 
                                              "French Guiana", "Grenada", "Guadeloupe", "Guatemala", "Guyana", 
                                              "Haiti", "Honduras", "Jamaica", "Martinique", "Mexico", "Montserrat", 
                                              "Nicaragua", "Panama", "Peru", "Puerto Rico", "Saint Kitts and Nevis", 
                                              "Saint Lucia", "Saint Pierre and Miquelon", "Saint Vincent and the Grenadines", 
                                              "Sint Maarten (Dutch part)", "Suriname", "Trinidad and Tobago", 
                                              "Turks and Caicos Islands", "United States", "Uruguay", "Venezuela") ~ "America",
                                pais %in% c("Bahrain", "Bangladesh", "Brunei", "Cambodia", "China", "Cocos Islands", "Fiji", 
                                              "Hong Kong", "India", "Indonesia", "Iran", "Iraq", "Israel", "Japan", "Jordan", 
                                              "Kuwait", "Lebanon", "Macao", "Malaysia", "Maldives", "Myanmar", "North Korea", 
                                              "Oman", "Pakistan", "Palestine", "Philippines", "Qatar", "Russia", "Saudi Arabia", 
                                              "Singapore", "South Korea", "Sri Lanka", "Syria", "Taiwan", "Thailand", "Turkey", 
                                              "United Arab Emirates", "Vietnam", "Yemen") ~ "Asia",
                                pais %in% c("Albania", "Belgium", "Bosnia and Herzegovina", "British Virgin Islands", 
                                              "Bulgaria", "Channel Islands", "Croatia", "Cyprus", "Denmark", "Estonia", 
                                              "Faeroe Islands", "Finland", "France", "Georgia", "Germany", "Gibraltar", 
                                              "Greece", "Greenland", "Guernsey", "Iceland", "Ireland", "Italy", "Latvia", 
                                              "Lithuania", "Malta", "Monaco", "Montenegro", "Netherlands", "Netherlands Antilles", 
                                              "Norway", "Poland", "Portugal", "Reunión", "Romania", "Slovenia", "Spain", "Sweden",
                                              "Ukraine", "United Kingdom") ~ "Europa",
                                pais %in% c("Australia", "Christmas Island", "Cook Islands", "French Polynesia", "Guam", "Kiribati", "Marshall Islands", "Micronesia (country)", "Nauru", "New Caledonia", "New Zealand", "Niue", "Norfolk Island", "Northern Mariana Islands", "Palau", "Papua New Guinea", "Samoa", "Solomon Islands", "Tokelau", "Tonga", "Tuvalu", "Vanuatu") ~ "Oceania",
                                TRUE ~ NA_character_))%>% 
  print()

waste_by_person_1 <- 
  waste_by_person %>%
  group_by(continente, ano) %>%
  summarise(pob_total = sum(poblacion_total, na.rm = T)) %>%
  drop_na(`continente`) %>% 
  print()

waste_by_person_1 <- 
  waste_by_person_1 %>% 
  mutate(pob_tot_millones = round((pob_total/1000000), 2)) %>% 
  print()
  
  + Se realizó el siguiente **gráfico**:
  
  ggplot(na.omit(waste_by_person_1), aes(ano, pob_tot_millones, group = continente)) + 
  geom_line(aes(color = continente), size = 2) + 
  geom_segment(aes(xend = 2014, yend = pob_tot_millones, color = continente), linetype = 4) +
  scale_x_continuous(limits = c(2000, 2013), breaks = seq(2000, 2013, 1)) +
  scale_fill_viridis(discrete = TRUE) +
  geom_point(size = 4) + 
  geom_text(aes(x = 2014, label = continente), hjust = 0) + 
  transition_reveal(ano) + 
  coord_cartesian(clip = 'off') + 
  labs(title = 'Crecimiento poblacional por continente, 2000 - 2013', x = 'Año', y = 'Millones de personas') + 
  theme_minimal(base_family = "Avenir") + 
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "dodgerblue4"),
        axis.ticks.length.x = unit(.25, "cm"),
        plot.margin = margin(5.5, 40, 5.5, 5.5),
        axis.text.x = element_text(size = 9))
