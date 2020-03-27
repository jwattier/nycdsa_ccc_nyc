getwd()
asset_csv <- readr::read_csv(file = "./data/resources/new_csv_file/Assets.csv")

asset_csv$Category[1]
string_example = asset_csv$Description[1]

str_split(string_example
          
asset_csv$Description[2]


asset_dir <- paste0("./data/resources/custom")
asset_csv_files <- fs::dir_ls(asset_dir)
asset_files <- asset_csv_files %>% 
  map_dfr(readr::read_csv)
asset_files
