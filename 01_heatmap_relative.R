library(dplyr)
library(ggplot2)

df <- read.csv('birth.csv', 
               nrows = 392, 
               stringsAsFactors = F)

df <- df %>% 
  filter(Month != 'Total', Month != 'Unknown',
         Reliability != 'Provisional figure') %>% 
  select(-Country.or.Area, -Area, -Record.Type, 
         -Reliability, -Source.Year, -Value.Footnotes) %>% 
  mutate(Month = factor(Month, levels = unique(Month), ordered = T)) %>% 
  group_by(Year) %>% 
  mutate(Total = sum(Value), 
         Share = Value/Total * 100)

png(filename = 'heatmap_relative.png', width = 1000, height = 800)

ggplot(df)+
  geom_tile(aes(x = Month, y = Year, fill = Share))+
  # viridis::scale_fill_viridis(direction = -1, option = 'D',
  #                             begin = 0.01, end = 0.89,
  #                             breaks = c(6, 7, 8, 9, 10),
  #                             limits = c(6, 10), 
  #                             alpha = 0.8)+
  scale_fill_distiller(palette = 3, direction = 1,
                       breaks = c(6, 7, 8, 9, 10),
                       limits = c(6, 10))+
  scale_x_discrete(position = 'top', 
                   labels = c('січень', 'лютий', 'березень', 'квітень',
                              'травень', 'червень', 'липень', 'серпень',
                              'вересень', 'жовтень', 'листопад', 'грудень'),
                   expand = c(0, 0))+
  scale_y_reverse(expand = c(0, 0),
                  breaks = c(1980, 1986, 1989, 1996, 1998, 2001, 
                             2005, 2007, 2008, 2010, 2014),
                  labels = c(1980, 1986, 1989, 1996, 1998, 2001, 
                             2005, 2007, 2008, 2010, 2014))+
  labs(title = 'Набільше дітей народжується у липні',
       subtitle = 'Розподіл народжених в Україні за місяцями, 1980-2014 роки',
       caption = 'Дані: UN Data | Візуалізація: Textura.in.ua')+
  guides(fill = guide_colorbar(nbin = 5, raster = F, ticks = F,
                               title = 'Частка народжених впродовж місяця від числа усіх народжених за рік, %',
                               title.position = 'top'))+
  theme_minimal()+
  theme(
    text = element_text(family = 'Ubuntu Condensed', face = 'plain', color = '#3A3F4A'),
    legend.position = 'top',
    legend.key.height = unit(7.5, "pt"),
    legend.key.width = unit(183, "pt"),
    legend.text = element_text(size = 15, color = '#5D646F'),
    legend.title = element_text(size = 15, color = '#5D646F'),
    axis.title = element_blank(),
    axis.text = element_text(size = 15, color = '#5D646F'),
    panel.grid = element_blank(),
    plot.title = element_text(face = 'bold', size = 40, margin = margin(b = 10, t = 10)),
    plot.subtitle = element_text(size = 20, face = 'plain', margin = margin(b = 10)),
    plot.caption = element_text(size = 15, margin = margin(b = 10, t = 10), color = '#5D646F'),
    plot.background = element_rect(fill = '#EFF2F4'),
    plot.margin = unit(c(1, 1, 1, 1), 'cm')
  )

dev.off()