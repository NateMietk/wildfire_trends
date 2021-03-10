library(rvest)
library(tidyverse)
library(stringr)

nifc_df <- read_html('https://www.nifc.gov/fireInfo/fireInfo_stats_totalFires.html') %>%
  html_nodes("table") %>%
  html_table() %>%
  tibble(.) %>%
  unnest() %>%
  slice(4:62) %>%
  mutate(year = as.integer(X1),
         fire_freq = as.double(gsub(',', '', X2)),
         burn_area = as.double(gsub('\\*|,', '', X3))) %>%
  dplyr::select(year, fire_freq, burn_area)

ranked_area <- nifc_df %>%
  arrange(desc(burn_area)) %>%
  slice(1:10) %>%
  mutate(rank_order = row_number())

mean_70_00 <- nifc_df %>%
  filter(year >= 1970 & year < 2000) %>%
  group_by() %>%
  summarise(mean_1 = mean(burn_area))

mean_00_10 <- nifc_df %>%
  filter(year >= 2000) %>%
  group_by() %>%
  summarise(mean_2 = mean(burn_area))
  
nifc_df <- nifc_df %>%
  group_by(year) %>%
  mutate(mean_1 = ifelse(year >= 1970 & year < 2000, mean_70_00$mean_1, NA),
         mean_2 = ifelse(year >= 2000, mean_00_10$mean_2, NA))

p1 <- nifc_df %>%
  ggplot(aes(x = year)) +
  geom_bar(aes(y = burn_area/1000000), stat = "identity", fill = 'red') +
  geom_text(data = ranked_area, aes(y = burn_area/1000000, label = rank_order), vjust=-0.25) +
  theme_pub() +
  ylab('Burned area (in millions of acres)') +
  xlab('Year') +
  scale_x_continuous(limits=c(1970, 2020), breaks=seq(1970,2020, by = 5))

p2 <- p1 +
  geom_line(aes(y = mean_1/1000000)) +
  geom_line(aes(y = mean_2/1000000))

                        
ggsave('results/nifc_top_fire_area_nomean.pdf', p1, width = 9, height = 5, dpi = 600, scale = 3, units = "cm")
ggsave('results/nifc_top_fire_area.pdf', p2, width = 9, height = 5, dpi = 600, scale = 3, units = "cm")

p3 <- nifc_df %>%
  ggplot(aes(x = year)) +
  geom_bar(aes(y = fire_freq), stat = "identity", fill = 'red') +
  theme_pub() +
  ylab('Fire frequency') +
  xlab('Year') +
  scale_x_continuous(limits=c(1970, 2020), breaks=seq(1970,2020, by = 5))
ggsave('results/nifc_fire_frequency.jpg', p3, width = 9, height = 5, dpi = 600, scale = 3, units = "cm")

get_noaa_climate <- function(url) {
  region <- strsplit(url, '/') %>%
    lapply(`[`, 7) %>%
    unlist %>%
    str_sub(., start = 0, end = 3)
  region <- case_when(
    region == '101' ~ 'northeast',
    region == '102' ~ 'midwest',
    region == '103' ~ 'ohiovalley',
    region == '104' ~ 'southeast',
    region == '105' ~ 'rockies',
    region == '106' ~ 'south',
    region == '107' ~ 'southwest',
    region == '108' ~ 'northwest',
    region == '109' ~ 'west')
  
  out_name <- paste0('avg_temp_', region)
  df <- read_csv(url) 
  df <- df %>%
    slice(5:nrow(df)) %>%
    rename(year = names(.)[1], avg_temp = names(.)[2]) %>%
    mutate(year = as.integer(str_sub(year, 1, 4)),
           !!ensym(out_name) := as.numeric(avg_temp)) %>%
    dplyr::select(year, !!ensym(out_name))
    return(df)
}

# Get Average Temperature
url_list <- list('https://www.ncdc.noaa.gov/cag/regional/time-series/101-tavg-3-8-1949-2019.csv?base_prd=true&begbaseyear=1901&endbaseyear=2019', #northeast
                 'https://www.ncdc.noaa.gov/cag/regional/time-series/102-tavg-3-8-1949-2019.csv?base_prd=true&begbaseyear=1901&endbaseyear=2019', #upper midwest
                 'https://www.ncdc.noaa.gov/cag/regional/time-series/103-tavg-3-8-1949-2019.csv?base_prd=true&begbaseyear=1901&endbaseyear=2019', #ohio valley
                 'https://www.ncdc.noaa.gov/cag/regional/time-series/104-tavg-3-8-1949-2019.csv?base_prd=true&begbaseyear=1901&endbaseyear=2019', #southeast
                 'https://www.ncdc.noaa.gov/cag/regional/time-series/105-tavg-3-8-1949-2019.csv?base_prd=true&begbaseyear=1901&endbaseyear=2019', #north rockies
                 'https://www.ncdc.noaa.gov/cag/regional/time-series/106-tavg-3-8-1949-2019.csv?base_prd=true&begbaseyear=1901&endbaseyear=2019', #south
                 'https://www.ncdc.noaa.gov/cag/regional/time-series/107-tavg-3-8-1949-2019.csv?base_prd=true&begbaseyear=1901&endbaseyear=2019', #southwest
                 'https://www.ncdc.noaa.gov/cag/regional/time-series/108-tavg-3-8-1949-2019.csv?base_prd=true&begbaseyear=1901&endbaseyear=2019', #northwest
                 'https://www.ncdc.noaa.gov/cag/regional/time-series/109-tavg-3-8-1949-2019.csv?base_prd=true&begbaseyear=1901&endbaseyear=2019') #west

summertemp_burnarea_df <- lapply(url_list, FUN=function(x) get_noaa_climate(x)) %>%
  bind_cols() %>%
  dplyr::select(year...1, contains("avg_temp")) %>%
  rename(year = year...1) %>%
  mutate(avg_temp_us = rowMeans(.[2:10])) %>%
  left_join(., nifc_df, by = 'year') %>%
  mutate(avg_temp_us = (avg_temp_us - 32) * 5/9,
         avg_temp_5yr= zoo::rollapply(avg_temp_us, 5, mean, align='right', fill=NA),
         avg_temp_10yr= zoo::rollapply(avg_temp_us, 10, mean, align='right', fill=NA),
         avg_burn_10yr= zoo::rollapply(burn_area, 10, mean, align='right', fill=NA)) %>%
  filter(year > 1969)

p3 <- summertemp_burnarea_df %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = avg_temp_5yr), fill = 'black', size = 2) +
  scale_y_continuous(position = "right") +
  theme_pub() +
  ylab('Average Temperature (degree C)') +
  xlab('Year') +
  scale_x_continuous(limits=c(1970, 2025), breaks=seq(1970,2025, by = 5)) +
  theme(rect = element_rect(fill = "transparent"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave('results/noaa_avg_temp.pdf', p3, width = 16, height = 7, dpi = 600, scale = 3, units = "cm")


library(latticeExtra)
obj1 <- xyplot(avg_burn_10yr/100000 ~ year, summertemp_burnarea_df , type = "l" , lwd=2, col = 'royalblue',
               ylab = list("10-yr running mean burned area (in 100000s of acres)", fontsize = 12, col = 'royalblue'),
               ylab.right = list("10-yr running mean temperature (degrees C)", fontsize = 12, col = 'red'),
               par.settings = simpleTheme(col = 1))
obj2 <- xyplot(avg_temp_10yr_c ~ year, summertemp_burnarea_df, type = "l", lwd=2, col = 'red')
doubleYScale(obj1, obj2)

doubleYScale(obj1, obj2)

trellis.device(device="png", filename="results/burnedarea_to_temp.png", width = 4650, height = 2250, units= 'cm', res = 300)
print(doubleYScale(obj1, obj2))
dev.off()