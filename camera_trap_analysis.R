# ============================================================
#  Wild Impact Research — Camera Trap Database & Visualisation
#  Sites: Nyekweri Forest (Kenya) & Okavango Delta (Botswana)
# ============================================================

library(DBI)
library(RSQLite)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggtext)
library(lubridate)
library(sf)
library(ggmap)          # basemap tiles
library(ggspatial)      # north arrow & scale bar
library(patchwork)      # panel layouts
library(RColorBrewer)
library(viridis)
library(scales)
library(tidyr)

# ── Paths ────────────────────────────────────────────────────
nyekweri_csv <- "WildImpact/Nyekweri_forest_data.csv"
okavango_csv <- "WildImpact/Okavango_delta_data.csv"
coords_xlsx  <- "WildImpact/Wild_Impact_Research_Task_Camera_Coords.xlsx"

# ============================================================
#  1. LOAD & CLEAN RAW DATA
# ============================================================

ny_raw <- read.csv(nyekweri_csv, fileEncoding = "latin1", stringsAsFactors = FALSE)
ok_raw <- read.csv(okavango_csv, fileEncoding = "latin1", stringsAsFactors = FALSE)
coords  <- read_excel(coords_xlsx)

# Add site label and bind
ny_raw$site <- "Nyekweri Forest"
ok_raw$site <- "Okavango Delta"
raw <- bind_rows(ny_raw, ok_raw) |> filter(!is.na(Filename))

# Standardise column names to snake_case
names(raw) <- c("filename","camera","date","time","temp_raw",
                "species","scientific_name","count","confidence",
                "class","reasons","site")

# Parse date-time; extract hour for diel analysis
raw <- raw |>
  mutate(
    datetime    = ymd_hms(paste(date, time)),
    date        = ymd(date),
    hour        = hour(datetime),
    month       = month(datetime, label = TRUE, abbr = TRUE),
    year        = year(datetime),
    temp_c      = as.numeric(gsub("[^0-9.-]", "", temp_raw)),
    # Normalise confidence (typo "HIgh" → "High"; unknowns → "Unknown")
    confidence  = case_when(
      tolower(confidence) %in% c("high","high") ~ "High",
      tolower(confidence) == "medium"            ~ "Medium",
      tolower(confidence) == "low"               ~ "Low",
      TRUE                                       ~ "Unknown"
    ),
    # Diel period
    diel = case_when(
      hour >= 6  & hour < 12 ~ "Morning",
      hour >= 12 & hour < 18 ~ "Afternoon",
      hour >= 18 & hour < 21 ~ "Evening",
      TRUE                   ~ "Night"
    ),
    diel = factor(diel, levels = c("Morning","Afternoon","Evening","Night")),
    # Wildlife flag: exclude humans, cattle, false triggers
    is_wildlife = !species %in% c("Human","Domestic cattle",
                                  "Unknown (False trigger)","Unknown Mammal")
  )

# Clean coordinates
coords <- coords |>
  rename(site_name = Site, camera_label = `Camera number`,
         lat = latitude, lon = longitude) |>
  mutate(
    # Normalise camera ID to match data (e.g. "ID89" → "CAM89", "Cam169" → "CAM169")
    camera = toupper(gsub("^ID|^Cam", "CAM", camera_label)),
    site   = case_when(
      grepl("Nyekweri", site_name) ~ "Nyekweri Forest",
      TRUE                          ~ "Okavango Delta"
    )
  )

# ============================================================
#  2. BUILD INTERACTIVE HTML DATABASE
# ============================================================

library(DT)
library(htmlwidgets)

# Add a human-readable site label and clean up for display
db_table <- obs |>
  select(
    `Image Filename`   = filename,
    `Camera ID`        = camera_id,
    Site               = site,
    Date               = date,
    Time               = time,
    `Temp (°C)`        = temp_c,
    Species            = species,
    `Scientific Name`  = scientific_name,
    Count              = count,
    Confidence         = confidence,
    Class              = class,
    `Diel Period`      = diel_period,
    `Is Wildlife`      = is_wildlife,
    `ID Reasoning`     = reasons
  ) |>
  mutate(
    `Is Wildlife` = if_else(`Is Wildlife` == 1, "Yes", "No"),
    Date          = as.character(Date)
  )

# Colour-code confidence column
conf_colours <- c(High = "#d4edda", Medium = "#fff3cd",
                  Low = "#f8d7da", Unknown = "#e2e3e5")

dt <- datatable(
  db_table,
  filter    = "top",          # per-column filter boxes
  rownames  = FALSE,
  extensions = c("Buttons","FixedHeader","Scroller"),
  options   = list(
    dom          = "Bfrtip",
    buttons      = list("csv","excel","pdf","copy"),
    pageLength   = 20,
    scrollX      = TRUE,
    fixedHeader  = TRUE,
    scroller     = TRUE,
    scrollY      = 500,
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color':'#2d6a4f','color':'white'});",
      "}"
    )
  ),
  caption = htmltools::tags$caption(
    style = "caption-side: top; text-align: left; font-size: 16px;
             font-weight: bold; color: #1a1a1a; padding-bottom: 8px;",
    "Wild Impact Research — Camera Trap Observation Database"
  )
) |>
  formatStyle(
    "Confidence",
    backgroundColor = styleEqual(
      names(conf_colours), unname(conf_colours)
    )
  ) |>
  formatStyle(
    "Is Wildlife",
    backgroundColor = styleEqual(c("Yes","No"), c("#d4edda","#f8d7da"))
  ) |>
  formatStyle(
    "Site",
    backgroundColor = styleEqual(
      c("Nyekweri Forest","Okavango Delta"),
      c("#d4edda","#d0e8f1")
    )
  ) |>
  formatStyle(
    columns     = names(db_table),
    fontSize    = "13px",
    fontFamily  = "Arial, sans-serif"
  )

saveWidget(dt, "camera_trap_database.html", selfcontained = TRUE)
message("Interactive HTML database saved: camera_trap_database.html")

# ============================================================
#  3. THEME & PALETTE
# ============================================================

theme_wild <- theme_minimal(base_size = 12) +
  theme(
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "#f7f7f7", colour = NA),
    panel.grid.major   = element_line(colour = "#e0e0e0"),
    panel.grid.minor   = element_blank(),
    text               = element_text(colour = "#1a1a1a"),
    axis.text          = element_text(colour = "#444444"),
    axis.title         = element_text(colour = "#1a1a1a", face = "bold"),
    plot.title         = element_text(colour = "#1a1a1a", face = "bold", size = 13),
    plot.subtitle      = element_text(colour = "#555555", size = 10),
    legend.background  = element_rect(fill = "white", colour = NA),
    legend.text        = element_text(colour = "#1a1a1a"),
    legend.title       = element_text(colour = "#1a1a1a", face = "bold"),
    strip.text         = element_text(colour = "white", face = "bold"),
    strip.background   = element_rect(fill = "#2d6a4f", colour = NA),
    plot.caption       = element_text(colour = "#888888", size = 8)
  )

theme_map <- theme_void() +
  theme(
    plot.background   = element_rect(fill = "white", colour = NA),
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.title        = element_text(colour = "#1a1a1a", face = "bold",
                                     size = 13, hjust = 0.5),
    plot.subtitle     = element_text(colour = "#555555", size = 10, hjust = 0.5),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.text       = element_text(colour = "#1a1a1a"),
    legend.title      = element_text(colour = "#1a1a1a", face = "bold"),
    plot.caption      = element_text(colour = "#888888", size = 7)
  )

# Site & confidence palettes
site_pal <- c("Nyekweri Forest" = "#2d6a4f", "Okavango Delta" = "#1d3557")
conf_pal <- c("High" = "#2d6a4f", "Medium" = "#f4a261", "Low" = "#e63946", "Unknown" = "#aaaaaa")
class_pal <- c("Mammal" = "#e07b39", "Aves" = "#457b9d", "Unknown" = "#aaaaaa")

# Species colour palettes (one per site, vivid)
ny_species <- sort(unique(obs$species[obs$site == "Nyekweri Forest"]))
ok_species <- sort(unique(obs$species[obs$site == "Okavango Delta"]))
ny_pal <- setNames(
  colorRampPalette(brewer.pal(8, "Set2"))(length(ny_species)), ny_species)
ok_pal <- setNames(
  colorRampPalette(brewer.pal(8, "Set1"))(length(ok_species)), ok_species)


# ============================================================
#  4. GRAPHS
# ============================================================

# ── 4.1  Species richness bar — Nyekweri ─────────────────────
ny_spp <- obs |>
  filter(site == "Nyekweri Forest", is_wildlife == 1) |>
  count(species, sort = TRUE)

p1 <- ggplot(ny_spp, aes(x = reorder(species, n), y = n, fill = species)) +
  geom_col(width = 0.75, show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.3, colour = "#e0e0e0", size = 3.5) +
  scale_fill_manual(values = ny_pal) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(title   = "Species Detection Frequency",
       subtitle = "Nyekweri Forest · Wildlife only",
       x = NULL, y = "Number of detections") +
  theme_wild

# ── 4.2  Species richness bar — Okavango ─────────────────────
ok_spp <- obs |>
  filter(site == "Okavango Delta", is_wildlife == 1) |>
  count(species, sort = TRUE)

p2 <- ggplot(ok_spp, aes(x = reorder(species, n), y = n, fill = species)) +
  geom_col(width = 0.75, show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.3, colour = "#e0e0e0", size = 3.5) +
  scale_fill_manual(values = ok_pal) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(title   = "Species Detection Frequency",
       subtitle = "Okavango Delta · Wildlife only",
       x = NULL, y = "Number of detections") +
  theme_wild

# ── 4.3  Diel activity — both sites ──────────────────────────
diel_data <- obs |>
  filter(is_wildlife == 1) |>
  count(site, diel_period) |>
  mutate(diel_period = factor(diel_period,
                              levels = c("Morning","Afternoon","Evening","Night")))

p3 <- ggplot(diel_data, aes(x = diel_period, y = n, fill = site)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_text(aes(label = n), position = position_dodge(0.8),
            vjust = -0.5, colour = "#e0e0e0", size = 3.5) +
  scale_fill_manual(values = site_pal, name = "Site") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title    = "Diel Activity Pattern",
       subtitle  = "When are animals most active?",
       x = "Time of Day", y = "Detections") +
  theme_wild

# ── 4.4  Confidence distribution ─────────────────────────────
conf_data <- obs |>
  filter(!is.na(confidence)) |>
  count(site, confidence) |>
  mutate(confidence = factor(confidence, levels = c("High","Medium","Low","Unknown")))

p4 <- ggplot(conf_data, aes(x = site, y = n, fill = confidence)) +
  geom_col(position = "fill", width = 0.6) +
  geom_text(aes(label = n), position = position_fill(vjust = 0.5),
            colour = "#1a1a2e", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = conf_pal, name = "Confidence") +
  scale_y_continuous(labels = percent_format()) +
  labs(title    = "Identification Confidence by Site",
       subtitle  = "Proportion of detections at each confidence level",
       x = NULL, y = "Proportion") +
  theme_wild

# ── 4.5  Class breakdown (Mammal vs Aves) ────────────────────
class_data <- obs |>
  filter(is_wildlife == 1) |>
  mutate(class = if_else(class %in% c("Mammal","Aves"), class, "Unknown")) |>
  count(site, class)

p5 <- ggplot(class_data, aes(x = class, y = n, fill = class)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.5, colour = "#e0e0e0", size = 4) +
  scale_fill_manual(values = class_pal) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  facet_wrap(~site) +
  labs(title    = "Taxonomic Class Breakdown",
       subtitle  = "Mammals vs birds per site",
       x = "Class", y = "Detections") +
  theme_wild

# ── 4.6  Monthly detection trend ─────────────────────────────
monthly <- obs |>
  filter(is_wildlife == 1, !is.na(month)) |>
  mutate(month = factor(month, levels = c("Jan","Feb","Mar","Apr","May","Jun",
                                          "Jul","Aug","Sep","Oct","Nov","Dec"))) |>
  count(site, month)

p6 <- ggplot(monthly, aes(x = month, y = n, colour = site, group = site)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_colour_manual(values = site_pal, name = "Site") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title    = "Monthly Detection Trend",
       subtitle  = "Wildlife detections across the survey period",
       x = "Month", y = "Detections") +
  theme_wild

# ── 4.7  Individual count heatmap per camera ─────────────────
camera_spp <- obs |>
  filter(is_wildlife == 1) |>
  count(camera_id, species) |>
  complete(camera_id, species, fill = list(n = 0))

p7 <- ggplot(camera_spp, aes(x = camera_id, y = species, fill = n)) +
  geom_tile(colour = "#1a1a2e", linewidth = 0.4) +
  geom_text(aes(label = ifelse(n > 0, n, "")),
            colour = "#1a1a2e", size = 3, fontface = "bold") +
  scale_fill_viridis_c(option = "plasma", begin = 0.05, end = 0.95,
                       name = "Detections") +
  labs(title    = "Camera × Species Detection Heatmap",
       subtitle  = "All cameras · Wildlife only",
       x = "Camera", y = NULL) +
  theme_wild +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ── 4.8  Hourly radial plot ───────────────────────────────────
hourly <- obs |>
  filter(is_wildlife == 1) |>
  count(hour, site)

p8 <- ggplot(hourly, aes(x = hour, y = n, fill = site)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  scale_x_continuous(breaks = seq(0, 23, 3),
                     labels = paste0(seq(0, 23, 3), ":00")) +
  scale_fill_manual(values = site_pal, name = "Site") +
  labs(title    = "Hourly Detection Pattern",
       subtitle  = "24-hour activity profile",
       x = "Hour of Day", y = "Detections") +
  theme_wild +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ── 4.9  Temperature vs detections scatter ───────────────────
p9 <- obs |>
  filter(is_wildlife == 1, !is.na(temp_c)) |>
  ggplot(aes(x = temp_c, y = count, colour = site)) +
  geom_jitter(width = 0.3, height = 0.1, alpha = 0.7, size = 2.5) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2, linewidth = 1.2) +
  scale_colour_manual(values = site_pal, name = "Site") +
  labs(title    = "Temperature vs Animal Count",
       subtitle  = "Each point = one detection event",
       x = "Temperature (°C)", y = "Animals in frame") +
  theme_wild

# ── 4.10 Top species count (individuals detected) ────────────
top_count <- obs |>
  filter(is_wildlife == 1) |>
  group_by(site, species) |>
  summarise(total_individuals = sum(count, na.rm = TRUE), .groups = "drop") |>
  group_by(site) |>
  slice_max(total_individuals, n = 6)

p10 <- ggplot(top_count, aes(x = reorder(species, total_individuals),
                              y = total_individuals, fill = site)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = total_individuals), hjust = -0.3,
            colour = "#e0e0e0", size = 3.5) +
  scale_fill_manual(values = site_pal) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  coord_flip() +
  facet_wrap(~site, scales = "free") +
  labs(title    = "Top Species by Total Individuals Counted",
       subtitle  = "Sum of all animals visible per detection event",
       x = NULL, y = "Total individuals") +
  theme_wild

# ── Save multi-panel figure 1: Species & Activity ────────────
fig1 <- (p1 | p2) / (p3 | p4) / (p5 | p6)
ggsave("fig1_species_activity.png", fig1,
       width = 16, height = 18, dpi = 200, bg = "#1a1a2e")

# ── Save multi-panel figure 2: Camera & Temporal ─────────────
fig2 <- p7 / (p8 | p9) / p10
ggsave("fig2_camera_temporal.png", fig2,
       width = 16, height = 18, dpi = 200, bg = "#1a1a2e")

message("Figures 1 & 2 saved.")

# ============================================================
#  5. GIS — MAPS
# ============================================================

library(rnaturalearth)
library(rnaturalearthdata)
library(prettymapr)   # fallback scale/north if ggspatial has issues

# ── Natural Earth base layers ────────────────────────────────
africa      <- ne_countries(continent = "africa", scale = "medium", returnclass = "sf")
kenya       <- ne_countries(country = "Kenya",    scale = "medium", returnclass = "sf")
botswana    <- ne_countries(country = "Botswana", scale = "medium", returnclass = "sf")

# Admin level 1 (provinces/counties) for context
kenya_adm1    <- ne_states(country = "Kenya",    returnclass = "sf")
botswana_adm1 <- ne_states(country = "Botswana", returnclass = "sf")

# Rivers and lakes for Okavango context
rivers <- ne_download(scale = "medium", type = "rivers_lake_centerlines",
                      category = "physical", returnclass = "sf")
lakes  <- ne_download(scale = "medium", type = "lakes",
                      category = "physical", returnclass = "sf")

# ── Camera sf objects ────────────────────────────────────────
cam_sf <- cam |>
  left_join(obs |> filter(is_wildlife == 1) |>
              count(camera_id, name = "detections"), by = "camera_id") |>
  mutate(detections = replace_na(detections, 0)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

cam_richness <- obs |>
  filter(is_wildlife == 1) |>
  group_by(camera_id) |>
  summarise(richness = n_distinct(species), .groups = "drop")

cam_sf <- cam_sf |>
  left_join(cam_richness, by = "camera_id") |>
  mutate(richness = replace_na(richness, 0))

ny_sf <- cam_sf |> filter(site == "Nyekweri Forest")
ok_sf <- cam_sf |> filter(site == "Okavango Delta")

# ── Bounding boxes ───────────────────────────────────────────
pad_bbox <- function(sf_obj, pct = 0.15) {
  bb <- st_bbox(sf_obj)
  xpad <- (bb["xmax"] - bb["xmin"]) * pct
  ypad <- (bb["ymax"] - bb["ymin"]) * pct
  c(xmin = unname(bb["xmin"] - xpad), xmax = unname(bb["xmax"] + xpad),
    ymin = unname(bb["ymin"] - ypad), ymax = unname(bb["ymax"] + ypad))
}

ny_bb <- pad_bbox(ny_sf, pct = 0.4)
ok_bb <- pad_bbox(ok_sf, pct = 0.4)

# ── Stadia Maps basemap tiles (free, no billing required) ────
# Register once: create a free API key at https://client.stadiamaps.com
# Then run:  ggmap::register_stadiamaps("YOUR_KEY_HERE")
# The tile style "stamen_terrain" works well for wildlife fieldwork sites.
# If you do not yet have a key, set use_tiles <- FALSE and the maps will
# render with Natural Earth polygons only (still professional).


use_tiles <- FALSE   # ← set TRUE once you have a Stadia key

if (use_tiles) {
  library(ggmap)
  
  ny_basemap <- get_stadiamap(
    bbox = setNames(as.numeric(ny_bb), c("left","bottom","right","top")),
    zoom = 13L, maptype = "stamen_terrain")
  
  ok_basemap <- get_stadiamap(
    bbox = setNames(as.numeric(ok_bb), c("left","bottom","right","top")),
    zoom = 12L, maptype = "stamen_terrain")
}

# ── 5.1  Nyekweri Forest — standalone (as requested first) ───
map_ny <- ggplot() +
  # Country fill
  geom_sf(data = kenya, fill = "#d4edda", colour = "#aaaaaa", linewidth = 0.4) +
  # Admin boundaries for county context
  geom_sf(data = kenya_adm1, fill = NA, colour = "#bbbbbb", linewidth = 0.2,
          linetype = "dashed") +
  # Lakes
  geom_sf(data = lakes, fill = "#a8dadc", colour = "#457b9d", linewidth = 0.3) +
  # Rivers
  geom_sf(data = rivers, colour = "#457b9d", linewidth = 0.4, alpha = 0.6) +
  # Camera trap points
  geom_sf(data = ny_sf,
          aes(size = detections, fill = richness),
          shape = 21, colour = "#1a1a1a", stroke = 0.8, alpha = 0.9) +
  geom_sf_text(data = ny_sf, aes(label = camera_id),
               nudge_y = 0.004, fontface = "bold", size = 3.2, colour = "#1a1a1a") +
  scale_fill_viridis_c(option = "YlGn", name = "Species\nRichness",
                       begin = 0.3, end = 0.95) +
  scale_size_continuous(range = c(5, 14), name = "Total\nDetections") +
  annotation_north_arrow(location = "tl",
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "br") +
  coord_sf(xlim = ny_bb[c("xmin","xmax")], ylim = ny_bb[c("ymin","ymax")],
           expand = FALSE) +
  labs(title    = "Nyekweri Forest — Camera Trap Network",
       subtitle = "Narok County, Kenya  ·  Dot size = detections  ·  Colour = species richness",
       caption  = "Basemap: Natural Earth  ·  CRS: WGS84 (EPSG:4326)") +
  theme_map

ggsave("map_nyekweri_standalone.png", map_ny,
       width = 10, height = 9, dpi = 200, bg = "white")
message("Nyekweri standalone map saved.")

# ── 5.2  Okavango Delta map ───────────────────────────────────
map_ok <- ggplot() +
  geom_sf(data = botswana, fill = "#fff3cd", colour = "#aaaaaa", linewidth = 0.4) +
  geom_sf(data = botswana_adm1, fill = NA, colour = "#cccccc", linewidth = 0.2,
          linetype = "dashed") +
  geom_sf(data = lakes, fill = "#a8dadc", colour = "#457b9d", linewidth = 0.3) +
  geom_sf(data = rivers, colour = "#457b9d", linewidth = 0.5, alpha = 0.7) +
  geom_sf(data = ok_sf,
          aes(size = detections, fill = richness),
          shape = 21, colour = "#1a1a1a", stroke = 0.8, alpha = 0.9) +
  geom_sf_text(data = ok_sf, aes(label = camera_id),
               nudge_y = 0.006, fontface = "bold", size = 3.2, colour = "#1a1a1a") +
  scale_fill_viridis_c(option = "YlOrRd", name = "Species\nRichness",
                       begin = 0.2, end = 0.9) +
  scale_size_continuous(range = c(5, 14), name = "Total\nDetections") +
  annotation_north_arrow(location = "tl",
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "br") +
  coord_sf(xlim = ok_bb[c("xmin","xmax")], ylim = ok_bb[c("ymin","ymax")],
           expand = FALSE) +
  labs(title    = "Okavango Delta — Camera Trap Network",
       subtitle = "Ngamiland, Botswana  ·  Dot size = detections  ·  Colour = species richness",
       caption  = "Basemap: Natural Earth  ·  CRS: WGS84 (EPSG:4326)") +
  theme_map

# ── 5.3  Africa context inset ─────────────────────────────────
ny_centroid <- st_coordinates(st_centroid(st_union(ny_sf)))
ok_centroid <- st_coordinates(st_centroid(st_union(ok_sf)))

map_inset <- ggplot() +
  geom_sf(data = africa, fill = "#f0f0f0", colour = "#cccccc", linewidth = 0.3) +
  geom_point(aes(x = ny_centroid[1], y = ny_centroid[2]),
             colour = "#2d6a4f", size = 4, shape = 18) +
  geom_point(aes(x = ok_centroid[1], y = ok_centroid[2]),
             colour = "#e63946", size = 4, shape = 18) +
  geom_text(aes(x = ny_centroid[1] + 6, y = ny_centroid[2] + 1),
            label = "Nyekweri", colour = "#2d6a4f", size = 2.5, fontface = "bold") +
  geom_text(aes(x = ok_centroid[1] + 6, y = ok_centroid[2] - 1),
            label = "Okavango", colour = "#e63946", size = 2.5, fontface = "bold") +
  coord_sf(xlim = c(-20, 55), ylim = c(-36, 38), expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", colour = "#333333", linewidth = 0.8),
    panel.background = element_rect(fill = "#d6eaf8", colour = NA)
  )

# ── 5.4  Combined two-site panel (patchwork) ──────────────────
fig_maps <- (map_ny | map_ok) +
  plot_annotation(
    title    = "Wild Impact Research — Camera Trap Survey Sites",
    subtitle = "Left: Nyekweri Forest, Kenya  ·  Right: Okavango Delta, Botswana",
    caption  = "Wild Impact Research Project  ·  Natural Earth basemap  ·  CRS: WGS84",
    theme    = theme(
      plot.background = element_rect(fill = "white", colour = NA),
      plot.title      = element_text(colour = "#1a1a1a", face = "bold",
                                     size = 16, hjust = 0.5),
      plot.subtitle   = element_text(colour = "#555555", size = 11, hjust = 0.5),
      plot.caption    = element_text(colour = "#888888", size = 8)
    )
  )

ggsave("fig3_combined_maps.png", fig_maps,
       width = 18, height = 10, dpi = 200, bg = "white")

# ── Inset embedded using cowplot (uncomment if installed) ─────
# library(cowplot)
# final_map <- ggdraw(fig_maps) +
#   draw_plot(map_inset, x = 0.44, y = 0.05, width = 0.10, height = 0.22)
# ggsave("fig3_combined_maps_with_inset.png", final_map,
#        width = 18, height = 10, dpi = 200, bg = "white")

ggsave("fig3_africa_inset.png", map_inset,
       width = 3, height = 4, dpi = 200, bg = "white")

message("All map figures saved.")

# ============================================================
#  6. SUMMARY STATISTICS TABLE (printed to console)
# ============================================================

summary_tbl <- obs |>
  filter(is_wildlife == 1) |>
  group_by(site) |>
  summarise(
    cameras          = n_distinct(camera_id),
    total_detections = n(),
    species_richness = n_distinct(species),
    total_individuals = sum(count, na.rm = TRUE),
    pct_high_conf    = round(mean(confidence == "High", na.rm = TRUE) * 100, 1),
    date_range       = paste(min(date, na.rm = TRUE), "to", max(date, na.rm = TRUE))
  )

cat("\n======== SUMMARY TABLE ========\n")
print(as.data.frame(summary_tbl))
cat("================================\n")

message("\nAll done! Output files:\n",
        "  camera_trap.db\n",
        "  fig1_species_activity.png\n",
        "  fig2_camera_temporal.png\n",
        "  map_nyekweri_standalone.png\n",
        "  fig3_combined_maps.png\n",
        "  fig3_africa_inset.png")
