getwd()
asset_csv <- readr::read_csv(file = "./data/resources/new_csv_file/Assets.csv")

asset_csv$Category[1]
string_example = asset_csv$Description[1]

str_split(string_example)
          
# asset_csv$Description[2]
# 
# 
# asset_dir <- paste0("./data/resources/custom")
# asset_csv_files <- fs::dir_ls(asset_dir)
# asset_files <- asset_csv_files %>% 
#   map_dfr(readr::read_csv)
# asset_files

#library(bbplot)

breaks <- c(0, 10, 20, 30, 40, 50, 60)
tags <- c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60 mins")
nyc_census_tracts_opendatanyc %>% 
  as_tibble() %>% select(., GEOID) %>% 
  filter(., GEOID == "36061024500") %>% 
  inner_join(., y=nyc_trvl_times_adj, by = c("GEOID" = "origin")) %>%
  left_join(., y=resource_ct_by_geoid, by = c("destination" = "resource_geoid")) %>% 
  mutate(.,  minutes_bin = cut(minutes, breaks = breaks, include.lowest = TRUE, 
                               right = FALSE, labels = tags)) %>% 
  ggplot(data = ., mapping = aes(x = minutes_bin, y = count)) + 
  geom_col(fill="#1380A1") +
  geom_hline(yintercept = 0, size=1, colour = "#333333") +
  bbc_style() +
  labs(
    title = "# of Resources by Travel Time",
    subtitle = "Further Away => Less Impact"
  )
