# Libraries ---------------------------------------------------------------
pacman::p_load(
  tidyverse,
  sf,
  leaflet,
  GWmodel,    # GWR
  sp,         # GWmodel needs sp objects
  RColorBrewer
)

# ── 1. Build the merged county-level analytical dataset ──────────────────

# From Script 1: PropTopEntrants per county
COUNTY_ENTRANTS <- mySfData %>%          # already created in Script 1
  st_drop_geometry() %>%
  group_by(CTYUA23CD, CTYUA23NM) %>%
  summarise(
    PropTopEntrants = mean(TopGrades, na.rm = TRUE),
    .groups = "drop"
  )

# From Script 2: Salary outcomes per county
COUNTY_SALARY <- myDataAsSF2 %>%         # already created in Script 2
  st_drop_geometry() %>%
  group_by(CTYUA23CD, CTYUA23NM) %>%
  summarise(
    MeanUQ  = mean(UQ,     na.rm = TRUE),
    MeanMed = mean(Median, na.rm = TRUE),
    .groups = "drop"
  )

# Merge onto county geometries & drop incomplete rows
GWR_SF <- Counties %>%
  select(CTYUA23CD, CTYUA23NM) %>%
  left_join(COUNTY_ENTRANTS, by = c("CTYUA23CD", "CTYUA23NM")) %>%
  left_join(COUNTY_SALARY,   by = c("CTYUA23CD", "CTYUA23NM")) %>%
  st_transform(crs = 27700) %>%   # British National Grid — metres needed for bandwidth
  st_make_valid() %>%
  drop_na(PropTopEntrants, MeanUQ)   # GWR cannot handle NAs

# Quick sanity check
cat("Counties with complete data:", nrow(GWR_SF), "\n")


# ── 2. OLS baseline ──────────────────────────────────────────────────────

ols <- lm(MeanUQ ~ PropTopEntrants, data = GWR_SF)
summary(ols)
# Gives global relationship — check sign, magnitude, R²


# ── 3. Convert to sp (required by GWmodel) ───────────────────────────────

GWR_SP <- as(GWR_SF, "Spatial")


# ── 4. Select optimal bandwidth via cross-validation ─────────────────────

bw_cv <- bw.gwr(
  formula  = MeanUQ ~ PropTopEntrants,
  data     = GWR_SP,
  approach = "CV",        # cross-validation; swap "AIC" for AICc-based
  kernel   = "bisquare",
  adaptive = TRUE         # adaptive k-nearest neighbours — better for uneven county sizes
)

cat("Optimal adaptive bandwidth (k neighbours):", bw_cv, "\n")


# ── 5. Fit GWR ───────────────────────────────────────────────────────────

gwr_model <- gwr.basic(
  formula  = MeanUQ ~ PropTopEntrants,
  data     = GWR_SP,
  bw       = bw_cv,
  kernel   = "bisquare",
  adaptive = TRUE,
  F123.test = TRUE   # outputs local F-tests for coefficient stationarity
)

print(gwr_model)
# Key outputs: global R², local R² range, bandwidth used,
# and Monte Carlo test of whether spatial variation in β is significant


# ── 6. Extract results back to sf ────────────────────────────────────────

gwr_results <- gwr_model$SDF %>%
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  rename(
    beta_Entrants = PropTopEntrants,
    local_R2      = Local_R2
  ) %>%
  mutate(
    se_Entrants = PropTopEntrants_SE,
    t_Entrants  = beta_Entrants / se_Entrants,
    sig         = abs(t_Entrants) > 1.96
  )

# Rejoin county names — row order is preserved by GWmodel so cbind is safe
gwr_results <- gwr_results %>%
  bind_cols(
    GWR_SF %>% st_drop_geometry() %>% select(CTYUA23CD, CTYUA23NM)
  )


# ── 7. Leaflet visualisation ─────────────────────────────────────────────

## 7a. Local slope (β) — does entrant quality predict earnings here?
pal_beta <- colorNumeric(
  palette = "RdBu",
  domain  = gwr_results$beta_Entrants,
  reverse = TRUE
)

leaflet(gwr_results,
        options = leafletOptions(minZoom = 5, maxZoom = 12)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor  = ~pal_beta(beta_Entrants),
    color      = ~ifelse(sig, "black", "#aaaaaa"),  # bold border = significant
    weight     = ~ifelse(sig, 1.5, 0.4),
    fillOpacity = 0.75,
    popup = ~paste0(
      "<b>", CTYUA23NM, "</b><br>",
      "β (Entrants → UQ Salary): <b>", round(beta_Entrants, 1), "</b><br>",
      "t-value: ", round(t_Entrants, 2), "<br>",
      "Significant (|t|>1.96): ", ifelse(sig, "✅ Yes", "❌ No"), "<br>",
      "Local R²: ", round(local_R2, 3)
    )
  ) %>%
  addLegend(
    pal    = pal_beta,
    values = ~beta_Entrants,
    title  = "Local β<br>(Entrant Quality → UQ Salary)",
    position = "bottomright"
  ) %>%
  addControl(
    "<b>Note:</b> Black borders = statistically significant (|t| > 1.96)",
    position = "bottomleft"
  )


## 7b. Local R² — model fit across space
pal_r2 <- colorNumeric(palette = "Blues", domain = gwr_results$local_R2)

leaflet(gwr_results,
        options = leafletOptions(minZoom = 5, maxZoom = 12)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor   = ~pal_r2(local_R2),
    color       = "grey40",
    weight      = 0.5,
    fillOpacity = 0.75,
    popup = ~paste0(
      "<b>", CTYUA23NM, "</b><br>",
      "Local R²: <b>", round(local_R2, 3), "</b><br>",
      "β: ", round(beta_Entrants, 1)
    )
  ) %>%
  addLegend(
    pal    = pal_r2,
    values = ~local_R2,
    title  = "Local R²",
    position = "bottomright"
  )


# ── 8. Summary diagnostics ───────────────────────────────────────────────

cat("\n── OLS vs GWR comparison ──────────────────────────────\n")
cat("OLS global R²:      ", round(summary(ols)$r.squared, 3), "\n")
cat("GWR global R²:      ", round(gwr_model$GW.diagnostic$gw.R2, 3), "\n")
cat("GWR AICc:           ", round(gwr_model$GW.diagnostic$AICc, 1), "\n")
cat("OLS AIC:            ", round(AIC(ols), 1), "\n\n")

cat("── Local β range ──────────────────────────────────────\n")
print(summary(gwr_results$beta_Entrants))

cat("\n── Significant counties ───────────────────────────────\n")
options(tibble.print_max = Inf)

gwr_results %>%
  st_drop_geometry() %>%
  filter(sig) %>%
  select(CTYUA23NM, beta_Entrants, t_Entrants, local_R2) %>%
  arrange(desc(beta_Entrants)) %>%
  print()