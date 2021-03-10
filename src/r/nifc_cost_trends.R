
library(pdftools)
library(tidyverse)
library(reshape)

download.file('https://www.nifc.gov/fireInfo/fireInfo_documents/SuppCosts.pdf',
              file.path(ics209_input_dir, 'SuppCosts.pdf'))

nifc_table <- pdf_text(file.path(ics209_input_dir, 'SuppCosts.pdf'))

clean_table <- function(table){
  table <- str_split(table, "\n", simplify = TRUE)
  country_name <- table[1, 1] %>% 
    stringr::str_squish()
  table_start <- 1
  table_end <- stringr::str_which(table, "The Department of Interior agencies include: Bureau of Indian Affairs, Bureau of Land Management; National")
  table <- table[(table_start + 1 ):(table_end - 1)]
  table <- str_replace_all(table, "\\s{2,}", "|") %>%
    str_replace_all(., ",", "")
  
  
  text_con <- textConnection(table)
  write.csv(table, 'src/tables/nifc_totals.csv')
  data_table <- read.csv('src/tables/nifc_totals.csv', sep = "|")
  data_table <- transform(data_table, n = colsplit(X.x, split = "\\|", names = c('x', "Year", "Fires", "Acres", "Forest Service", "DOI Agencies", "Total"))) %>%
    dplyr::slice(2:35) %>%
    dplyr::select(year = n.Year,
                  n_fires = n.Fires,
                  acres = n.Acres,
                  total_costs = n.Total) %>%
    as_tibble() %>%
    mutate_all(funs(str_replace(., "\\$", ""))) %>%
    mutate_all(funs(as.numeric(as.character(.)))) %>%
    mutate(year = as.integer(year))
  unlink('src/tables/nifc_totals.csv')
}

nifc <- map_df(nifc_table, clean_table)

p1 <- nifc %>%
  ggplot(aes(x = year, y = total_costs/1000000000)) +
  geom_line() +
  theme_pub() +
  xlab('Year') +
  ylab('Suppression costs (in billions of US $)') +
  theme(axis.text.x = element_text(size = 12, angle = 65, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16)) 

pct_increase <- nifc %>%
  filter(year == 1985 | year == 2018) %>%
  dplyr::select(year, total_costs) %>%
  spread(year, total_costs) %>%
  mutate(diff = `2018`/`1985`)

ggsave('results/nifc_cost_trends.pdf', p1, width = 6, height = 5, dpi = 600, scale = 3, units = "cm")


p1 <- nifc %>%
  ggplot(aes(x = year, y = acres)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_pub() +
  xlab('Year') +
  ylab('Suppression costs (in billions of US $)') +
  theme(axis.text.x = element_text(size = 12, angle = 65, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16)) 

pct_increase <- nifc %>%
  filter(year == 1985 | year == 2018) %>%
  dplyr::select(year, total_costs) %>%
  spread(year, total_costs) %>%
  mutate(diff = `2018`/`1985`)

ggsave('results/nifc_cost_trends.pdf', p1, width = 6, height = 5, dpi = 600, scale = 3, units = "cm")
