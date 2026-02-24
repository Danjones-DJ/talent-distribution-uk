# Libraries ---------------------------------------------------------------
pacman::p_load(
  tidyverse,
  sf,
  ggthemes,
  cowplot,
  skimr,
  spData
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

location = read_csv("LOCATION.csv") %>% select(UKPRN, LATITUDE, LONGITUDE) %>% distinct(UKPRN, .keep_all = TRUE)
kiscourse = read_csv("KISCOURSE.csv") %>% select(PUBUKPRN, UKPRN, CRSEURL, KISCOURSEID, KISMODE, TITLE, KISAIMCODE)
kisaim = read_csv("KISAIM.csv")
tariff = read_csv("TARIFF.csv") %>% select(-starts_with("TAR"))
inst = read_csv("INSTITUTION.csv") %>% select(LEGAL_NAME, PUBUKPRN, UKPRN)


myData = kiscourse %>%
  filter(PUBUKPRN %in% PUBUKPRNS) %>%
  left_join(inst) %>% 
  left_join(kisaim) %>%
  left_join(location)
  

# View(myData)
# skim(myData)

# Clean Tariff ------------------------------------------------------------
# Fill missing tariff values with PUBUKPRN-level means
tariff_fill <- tariff %>%
  filter(PUBUKPRN %in% PUBUKPRNS) %>%
  group_by(PUBUKPRN) %>%
  summarise(
    across(starts_with("T"), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# Impute NAs, then summarise to course level
tariff_clean <- tariff %>%
  filter(PUBUKPRN %in% PUBUKPRNS) %>%
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
  left_join(tariff_clean)

# View(myData2)
# skim(myData2)

# -------------------------------------------------------------------------
SF.DF <- myData2 %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 27700, remove = FALSE)



# Load Geojson ------------------------------------------------------------

UK = st_read("MSOA_DEC_2021_EW_NC_v3_-8834577846191935829.geojson")
plot(UK)



# -------------------------------------------------------------------------

