roads <- sf::st_read(dsn = "data-raw/TRAN_Nevada_State_Shape/Shape/",
                     layer = "Trans_RoadSegment")

# Extract only the US Highways and Interstates.
# Eliminate Business Loops and Alternative Routes
# Combine Road Segments and Simplify the Geometry
road_sub <- roads |>
  filter(!is.na(interstate) | !is.na(us_route)) |>
  select(interstate, us_route, geometry) |>
  pivot_longer(cols = c(interstate, us_route),
               names_to = "type",
               values_to = "road") |>
  select(-type) |>
  filter(!str_detect(road, pattern = "^[[:alpha:]]")) |>
  mutate(road = str_remove(road, pattern = ",.*$"),
         road = as.numeric(road)) |>
  filter(road < 100) |>
  group_by(road) |>
  summarize(geometry = st_union(geometry)) |>
  st_simplify(dTolerance = 1000)

st_write(road_sub, dsn = "data-raw/simple_features", layer = "nv_road_simp",
         driver = "ESRI Shapefile")
