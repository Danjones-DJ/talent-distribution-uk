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

location = read_csv("Data/LOCATION.csv") %>% select(UKPRN, LATITUDE, LONGITUDE) %>% distinct(UKPRN, .keep_all = TRUE) %>% rename(PUBUKPRN = UKPRN)
kiscourse = read_csv("Data/KISCOURSE.csv") %>% select(PUBUKPRN, UKPRN, CRSEURL, KISCOURSEID, KISMODE, TITLE, KISAIMCODE)
kisaim = read_csv("Data/KISAIM.csv")
salary = read_csv("Data/GOSALARY.csv") %>% select(PUBUKPRN, UKPRN, KISCOURSEID, GOSALPOP, GOINSTLQ, GOINSTMED, GOINSTUQ)
inst = read_csv("Data/INSTITUTION.csv") %>% select(LEGAL_NAME, PUBUKPRN, UKPRN)
names(salary)

myData = kiscourse %>%
  # filter(PUBUKPRN %in% PUBUKPRNS) %>%
  left_join(inst) %>% 
  left_join(kisaim) %>%
  left_join(location)


salary2 = salary %>% 
  drop_na() %>%
  group_by(PUBUKPRN, UKPRN, KISCOURSEID) %>%
  mutate(
    LQ = signif(weighted.mean(GOINSTLQ, GOSALPOP, na.rm=TRUE), 2),
    Median = signif(weighted.mean(GOINSTMED, GOSALPOP, na.rm=TRUE), 2),
    UQ = signif(weighted.mean(GOINSTUQ, GOSALPOP, na.rm=TRUE), 2)
    )

# -------------------------------------------------------------------------
myData2 = myData %>%
  left_join(salary2) %>% drop_na(Median) %>%
  mutate(
    LATITUDE = ifelse(PUBUKPRN == "10003270", 51.4988, LATITUDE),
    LONGITUDE = ifelse(PUBUKPRN == "10003270", -0.1749, LONGITUDE)
  )

Counties = st_read("Data/CTYUA_DEC_2023_UK_BGC.shp")

myDataAsSF = myData2 %>%
  filter(!is.na(LONGITUDE), !is.na(LATITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(Counties))  

myDataAsSF2 = myDataAsSF %>%
  st_join(Counties %>% select(CTYUA23CD, CTYUA23NM))

# Aggregate to County Level
COUNTY.Sal = myDataAsSF2 %>%
  st_drop_geometry() %>%
  group_by(CTYUA23CD, CTYUA23NM) %>%
  summarise(
    MeanLQ = mean(LQ, na.rm = TRUE),
    MeanMedian = mean(Median, na.rm = TRUE),
    MeanUQ = mean(UQ, na.rm = TRUE),
    Unis = paste(unique(LEGAL_NAME), collapse = ",  "),
    .groups="drop")

COUNTY.SAL.FULL = Counties %>%
  select(CTYUA23CD, CTYUA23NM) %>%
  left_join(COUNTY.Sal)


# Plot --------------------------------------------------------------------
## Set-up


df = COUNTY.SAL.FULL %>% st_transform(crs = 4326)
df <- df %>% st_make_valid()

## Colours
myColoursLQ = colorBin(palette = "YlOrRd", bins = 5, domain = df$MeanLQ, na.color = "#f2f2f2")
myColoursMED = colorBin(palette = "YlOrRd", bins = 5, domain = df$MeanMedian, na.color = "#f2f2f2")
myColoursMED = colorNumeric(palette = "YlOrRd", domain = df$MeanMedian, na.color = "#f2f2f2")
myColoursUQ = colorBin(palette = "YlOrRd", bins = 5, domain = df$MeanUQ, na.color = "#f2f2f2")

## Plot
leaflet(df, 
        options = leafletOptions(
          minZoom = 5,
          maxZoom = 12)) %>%
  addPolygons(
    fillColor = ~myColoursMED(MeanMedian),
    color = "black",
    weight = 1,
    fillOpacity = 0.7,
    popup = ~paste0(
      "<b>", CTYUA23NM, "</b><br>",
      "<b>Universities:</b> ", ifelse(is.na(Unis), "No data", Unis), "<br>",
      "<b>(Average) Salary:</b> ", 
      ifelse(is.na(MeanMedian), "No data", paste0("£", format(MeanMedian, big.mark = ",", scientific = FALSE)))
    )
  ) %>%
  addLegend(
    pal = myColoursMED,
    values = ~MeanMedian,
    title = paste("Salary Outcomes by Region"),
    na.label = ""
  )







