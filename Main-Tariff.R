# Libraries ---------------------------------------------------------------
pacman::p_load(
  tidyverse,
  sf,
  ggthemes,
  cowplot,
  skimr,
  spData,
  spdep,
  scales,
  leaflet
)

# Uni Filter --------------------------------------------------------------
PUBUKPRNS =  c(
  "10003270",  # Imperial College London
  "10007774",  # University of Oxford
  "10007788",  # University of Cambridge
  "10007784",  # University College London
  "10003645",  # King's College London
  "10007790",  # University of Edinburgh
  "10007798",  # University of Manchester
  "10007786",  # University of Bristol
  "10004063",  # London School of Economics & Political Science
  "10007163",  # University of Warwick
  "10006840",  # University of Birmingham
  "10007794",  # University of Glasgow
  "10007795",  # University of Leeds
  "10007158",  # University of Southampton 
  "10007157",  # University of Sheffield
  "10007143",  # Durham University
  "10007154",  # University of Nottingham
  "10007775",  # Queen Mary and Westfield College, University of London
  "10007803",  # University of St Andrews
  "10007850",  # University of Bath
  "10007799",  # University of Newcastle Upon Tyne
  "10006842",  # University of Liverpool
  "10007792",  # University of Exeter
  "10007768", # Lancs
  "10007167",  # University of York
  "10007814",  # Cardiff University
  "10007802", # reading
  "10007805", # Strathclyde
  "10007806", # Sussex
  "10005343",  # The Queen's University of Belfast
  "10004113",  # Loughborough University
  "10000961",  # Brunel University
  "10007780"   # School of Oriental and African Studies
)

# Load Data ---------------------------------------------------------------

location = read_csv("Data/LOCATION.csv") %>% select(UKPRN, LATITUDE, LONGITUDE) %>% distinct(UKPRN, .keep_all = TRUE)
kiscourse = read_csv("Data/KISCOURSE.csv") %>% select(PUBUKPRN, UKPRN, CRSEURL, KISCOURSEID, KISMODE, TITLE, KISAIMCODE)
kisaim = read_csv("Data/KISAIM.csv")
tariff = read_csv("Data/TARIFF.csv") %>% select(-starts_with("TAR"))
inst = read_csv("Data/INSTITUTION.csv") %>% select(LEGAL_NAME, PUBUKPRN, UKPRN)


myData = kiscourse %>%
  # filter(PUBUKPRN %in% PUBUKPRNS) %>%
  left_join(inst) %>% 
  left_join(kisaim) %>%
  left_join(location)


# View(myData)
# skim(myData)

# Clean Tariff ------------------------------------------------------------
# Fill missing tariff values with PUBUKPRN-level means
tariff_fill <- tariff %>%
  # filter(PUBUKPRN %in% PUBUKPRNS) %>%
  group_by(PUBUKPRN) %>%
  summarise(
    across(starts_with("T"), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# Impute NAs, then summarise to course level
tariff_clean <- tariff %>%
  # filter(PUBUKPRN %in% PUBUKPRNS) %>%
  left_join(tariff_fill, by = "PUBUKPRN", suffix = c("", "_fill")) %>%
  mutate(across(
    starts_with("T") & !ends_with("_fill"),
    ~ coalesce(.x, get(paste0(cur_column(), "_fill")))
  )) %>%
  select(-ends_with("_fill")) %>%
  group_by(PUBUKPRN, UKPRN, KISCOURSEID) %>%
  summarise(
    across(starts_with("T"), ~ round(mean(.x, na.rm = TRUE), 2)),
    .groups = "drop"
  )

# View(tariff_clean)

# Full Data ---------------------------------------------------------------

myData2 = myData %>%
  left_join(tariff_clean) %>%
  rename("DEGREE_TITLE" = TITLE) %>%
  mutate(
    TotalGrades = rowSums(across(starts_with("T"))),
    TopGrades = ((T160 + T176 + T192 + T208 + T224 + T240) / TotalGrades) * 100
  ) %>%
  mutate(
    LATITUDE = ifelse(PUBUKPRN == "10003270", 51.4988, LATITUDE),
    LONGITUDE = ifelse(PUBUKPRN == "10003270", -0.1749, LONGITUDE)
  )



Counties = st_read("Data/CTYUA_DEC_2023_UK_BGC.shp")

SF.DF <- myData2 %>%
  filter(!is.na(LONGITUDE), !is.na(LATITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(Counties))   # reproject to BNG to match shapefile


# -------------------------------------------------------------------------
names(Counties)

mySfData = SF.DF %>%
  st_join(Counties %>% select(CTYUA23CD, CTYUA23NM))

# Aggregate to County Level
COUNTY = mySfData %>%
  st_drop_geometry() %>%
  group_by(CTYUA23CD, CTYUA23NM) %>%
  summarise(
    PropTopEntrants = mean(TopGrades, na.rm = TRUE),
    Unis = paste(unique(LEGAL_NAME), collapse = ",  "),
    .groups="drop")

# SF
COUNTY.SF = Counties %>%
  select(CTYUA23CD, CTYUA23NM) %>%
  left_join(COUNTY)
UK = Counties %>% st_union()

## Plot
# COUNTY.SF %>%
#   ggplot() +
#   geom_sf(aes(fill = PropTopEntrants), colour = "black", linewidth = 0.5) + 
#   geom_sf(data = UK, fill = NA, colour = "black", linewidth = 0.5) + 
#   scale_fill_distiller(
#     palette = "RdBu", direction = 1, name = "Prop. Top Entrants") +
#   theme_map()


# Leaflet learning --------------------------------------------------------

# # Create a JS format
# st_write(COUNTY.SF, "County.geojson") # Like a DF

df = COUNTY.SF %>% st_transform(crs = 4326)
df <- df %>% st_make_valid()

colours = colorNumeric(palette = "Blues", domain = df$PropTopEntrants)
cols2 = colorBin(palette = "YlOrRd", bins = 5, domain = df$PropTopEntrants, na.color = "#f2f2f2")

# -------------------------------------------------------------------------

leaflet(df, 
        options = leafletOptions(
          minZoom = 5,
          maxZoom = 12)) %>%
  addPolygons(
    fillColor = ~cols2(PropTopEntrants),
    color = "black",
    weight = 1,
    fillOpacity = 0.7,
    popup = ~paste0(
      "<b>", CTYUA23NM, "</b><br>",
      "<b>Universities:</b> ", ifelse(is.na(Unis), "No data", Unis), "<br>",
      "<b>(Average) Proportion of Entrants w/ A*A*A+:</b> ", 
      ifelse(is.na(PropTopEntrants), "No data", paste0(round(PropTopEntrants, 2), "%"))
    )
  ) %>%
  addLegend(
    pal = cols2,
    values = ~PropTopEntrants,
    title = paste("Proportion of students entering<br>
                university with 160+ tariff points<br>
                (equivalent to A*A*A or greater)"),
    na.label = ""
  )
  






