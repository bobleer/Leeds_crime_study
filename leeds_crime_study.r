################################################################################
# The study of Leeds crimes in space and time
# University of Sheffield
# MSc Data Science
# Registration No: 180234862
# Item page: https://github.com/bobleer/Leeds_crime_study
################################################################################

library("tidyverse")
library("dplyr")
library("grid")
library("gridExtra")
library("leaflet")
library("rgdal")
library("tmap")
library("ggcorrplot")
library("ggmap")
library("ggplot2")
library("cowplot")
library("zoo")

options(scipen = 200)

################################################################################
# Show all crime data in the UK (The huge data need a ton of RAM.)
################################################################################

#Set file directory
all_crimes_dir <- "./all-street/"
#Traversal
all_crimes_csv <-
  list.files(
    path = all_crimes_dir,
    pattern = "*.csv",
    recursive = TRUE,
    all.files = TRUE,
    full.names = TRUE
  )
#Set output filename
all_combined_filename <- "[Combined]all-street_crimes.csv"
#First apply read.csv, then rbind
read_all_csv <- lapply(all_crimes_csv,
                       function(x)
                         read.csv(x, stringsAsFactors = F, header = T))
#Output combined csv
all_crimes_path <- paste0(all_crimes_dir, all_combined_filename)
#Save combined csv
do.call(rbind, read_all_csv) %>%
  write.csv(all_crimes_path)
#Read combined csv
all_combined_crimes <- read.csv(all_crimes_path, header = TRUE)
#Sum by month
all_crimes_by_month <-
  all_combined_crimes %>% group_by(Month) %>% summarise(crime_sum = n())

ggplot() +
  geom_point(
    aes(x = as.Date(as.yearmon(Month)),
        y = crime_sum),
    size = all_crimes_by_month$crime_sum / 60000,
    color = "#999999",
    data = all_crimes_by_month
  ) +
  geom_smooth(
    aes(x = as.Date(as.yearmon(Month)),
        y = crime_sum),
    data = all_crimes_by_month,
    method = lm,
    size = 0.7,
    color = "#ffffff",
    fill = "#FF635C",
    se = TRUE
  ) +
  geom_hline(
    yintercept = max(all_crimes_by_month$crime_sum),
    color = "#eeeeee",
    size = 1,
    alpha = 0.5,
    linetype = "dashed"
  ) +
  ggtitle("The trend of crimes in the UK") +
  scale_x_date(
    date_labels = "%Y-%m",
    date_breaks = "3 months",
    date_minor_breaks = "1 months"
  ) +
  xlab("Month (over 36 months)") +
  ylab("The number of crimes") +
  theme(
    plot.title = element_text(
      size = 16,
      color = "#ffffff",
      hjust = 0,
      face = "bold"
    ),
    axis.title = element_text(
      size = 14,
      color = "#888888",
      hjust = 0.5,
      face = "bold"
    ),
    axis.text.x = element_text(
      size = 12,
      color = "#ffffff",
      hjust = 0.5,
      angle = 45,
      face = "bold"
    ),
    axis.text.y = element_text(
      size = 12,
      angle = 45,
      face = "bold",
      color = "#ffffff"
    ),
    plot.background = element_rect(fill = "#2E2E2E",
                                   color = NA),
    plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "inches"),
    panel.background = element_rect(fill = "#5E5E5E"),
    panel.grid = element_line(color = "#888888"),
    panel.grid.minor = element_line(color = "#727272")
  )

leaflet(all_combined_crimes) %>% addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())

################################################################################
# Read West Yorkshire crime data and combine with Leeds LSOA data
################################################################################

leedsShape <-
  readOGR(dsn = "./leeds_lsoa", layer = "england_lsoa_2011")

deprivation2015 <-
  read.csv("./Deprivation2015.csv",
           header = TRUE)

deprivation2015_sl <- deprivation2015 %>%
  select(
    LSOA.name..2011.,
    LSOA.code..2011.,
    Total.population..mid.2012..excluding.prisoners.,
    Income.Score..rate.,
    Employment.Score..rate.,
    Education..Skills.and.Training.Score,
    Living.Environment.Score,
    Crime.Score
  )

names(deprivation2015_sl)[names(deprivation2015_sl) == "LSOA.name..2011."] <-
  "LSOA_name"
names(deprivation2015_sl)[names(deprivation2015_sl) == "LSOA.code..2011."] <-
  "LSOA_code"
names(deprivation2015_sl)[names(deprivation2015_sl) == "Total.population..mid.2012..excluding.prisoners."] <-
  "Total_population"
names(deprivation2015_sl)[names(deprivation2015_sl) == "Income.Score..rate."] <-
  "Income"
names(deprivation2015_sl)[names(deprivation2015_sl) == "Employment.Score..rate."] <-
  "Employment"
names(deprivation2015_sl)[names(deprivation2015_sl) == "Education..Skills.and.Training.Score"] <-
  "Education"
names(deprivation2015_sl)[names(deprivation2015_sl) == "Living.Environment.Score"] <-
  "Living_ENV"
names(deprivation2015_sl)[names(deprivation2015_sl) == "Crime.Score"] <-
  "Crime_Score"

leedsShape@data <- left_join(leedsShape@data,
                             deprivation2015_sl,
                             by = c('code' = 'LSOA_code'))

transformed_leedsShape <- spTransform(leedsShape,
                                      CRS(
                                        "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
                                      ))

tmap_mode("view")

tm_shape(leedsShape) + tm_borders(lwd = 2, alpha = 0.5, col = "#666666") + tm_basemap(leaflet::providers$OpenMapSurfer.Roads)

leeds_pop_map <- tm_shape(leedsShape) +
  tm_fill(
    "Total_population",
    alpha = 0.7,
    style = "kmeans",
    title = "Population@Leeds"
  ) +
  tm_borders(alpha = 0.5, col = "#eeeeee") +
  tm_view(view.legend.position = c("right", "bottom")) +
  tm_basemap(leaflet::providers$OpenMapSurfer.Roads)

leeds_pop_map

leeds_income_map <- tm_shape(leedsShape) +
  tm_fill("Income",
          alpha = 0.7,
          style = "kmeans",
          title = "Income@Leeds") +
  tm_borders(alpha = 0.5, col = "#eeeeee") +
  tm_view(view.legend.position = c("right", "bottom")) +
  tm_basemap(leaflet::providers$OpenMapSurfer.Roads)

leeds_income_map

leeds_employment_map <- tm_shape(leedsShape) +
  tm_fill("Employment",
          alpha = 0.7,
          style = "kmeans",
          title = "Employment@Leeds") +
  tm_borders(alpha = 0.5, col = "#eeeeee") +
  tm_view(view.legend.position = c("right", "bottom")) +
  tm_basemap(leaflet::providers$OpenMapSurfer.Roads)

leeds_employment_map

leeds_education_map <- tm_shape(leedsShape) +
  tm_fill("Education",
          alpha = 0.7,
          style = "kmeans",
          title = "Education@Leeds") +
  tm_borders(alpha = 0.5, col = "#eeeeee") +
  tm_view(view.legend.position = c("right", "bottom")) +
  tm_basemap(leaflet::providers$OpenMapSurfer.Roads)

leeds_education_map

leeds_living_map <- tm_shape(leedsShape) +
  tm_fill("Living_ENV",
          alpha = 0.7,
          style = "kmeans",
          title = "Living Environment<br>@Leeds") +
  tm_borders(alpha = 0.5, col = "#eeeeee") +
  tm_view(view.legend.position = c("right", "bottom")) +
  tm_basemap(leaflet::providers$OpenMapSurfer.Roads)

leeds_living_map

################################################################################
# Merge all 36 months data from West Yorkshire Police
################################################################################

#Set file directory
crimes_dir = "./west-yorkshire-street_crimes/"
crimes_csv <-
  list.files(path = crimes_dir,
             pattern = "*.csv",
             full.names = TRUE)
#Set output filename
combined_filename = "[Combined]west-yorkshire-street_crimes.csv"
#First apply read.csv, then rbind
read_all_csv <- lapply(crimes_csv,
                       function(x)
                         read.csv(x, stringsAsFactors = F, header = T))
#Save combined csv
do.call(rbind, read_all_csv) %>%
  write.csv(paste0(crimes_dir, combined_filename))

################################################################################
# Visualise Leeds crime data by LSOA
################################################################################

combined_crimes <-
  read.csv(paste0(crimes_dir, combined_filename), header = TRUE)

numCrimesByLSOA <- combined_crimes %>%
  select(LSOA.code, Crime.ID) %>%
  group_by(LSOA.code) %>%
  summarise(Num_crimes = n())



leedsShape@data <- left_join(leedsShape@data,
                             numCrimesByLSOA,
                             by = c('code' = 'LSOA.code'))
leedsShape@data[is.na(leedsShape@data$Num_crimes)] <- 0

tmap_mode("view")
leeds_crime_map <- tm_shape(leedsShape) +
  tm_fill("Num_crimes",
          alpha = 0.7,
          style = "kmeans",
          title = "Crimes@Leeds<br>Over past 36 months") +
  tm_borders(alpha = 0.5, col = "#eeeeee") +
  tm_view(view.legend.position = c("right", "bottom")) +
  tm_basemap(leaflet::providers$OpenMapSurfer.Roads)

leeds_crime_map

################################################################################
# Correlation among the crime and other social elements
################################################################################

nation_data <- deprivation2015_sl %>%
  select(LSOA_name,
         Total_population,
         Income,
         Employment,
         Education,
         Living_ENV,
         Crime_Score)
rownames(nation_data) <- nation_data[, 1]
nation_data <- nation_data[, -1]
nation_p.mat <- cor_pmat(nation_data)
cor_nation <- cor(nation_data)


leeds_data_by_nation <- leedsShape@data %>%
  select(LSOA_name,
         Total_population,
         Income,
         Employment,
         Education,
         Living_ENV,
         Crime_Score)
rownames(leeds_data_by_nation) <- leeds_data_by_nation[, 1]
leeds_data_by_nation <- leeds_data_by_nation[, -1]
leeds_by_nation_p.mat <- cor_pmat(leeds_data_by_nation)
cor_leeds_by_nation <- cor(leeds_data_by_nation)

leeds_data <- leedsShape@data %>%
  select(LSOA_name,
         Total_population,
         Income,
         Employment,
         Education,
         Living_ENV,
         Num_crimes)
rownames(leeds_data) <- leeds_data[, 1]
leeds_data <- leeds_data[, -1]
leeds_p.mat <- cor_pmat(leeds_data)
cor_leeds <- cor(leeds_data)

ggcorrplot(
  title = "The UK crimes p-value significance",
  cor_nation,
  method = "square",
  outline.color = "white",
  type = "full",
  ggtheme = ggplot2::theme_get,
  lab = TRUE,
  lab_size = 6,
  tl.srt = 45,
  pch.cex = 24,
  p.mat = nation_p.mat,
  colors = c("#6D9EC1", "white", "#E46726")
) + theme(text = element_text(face = "bold"))

ggcorrplot(
  title = "Leeds crimes p-value significance (Data from deprivation2015)",
  cor_leeds_by_nation,
  method = "square",
  outline.color = "white",
  type = "full",
  ggtheme = ggplot2::theme_get,
  lab = TRUE,
  lab_size = 6,
  tl.srt = 45,
  pch.cex = 24,
  p.mat = leeds_by_nation_p.mat,
  colors = c("#6D9EC1", "white", "#E46726")
) + theme(text = element_text(face = "bold"))

ggcorrplot(
  title = "Leeds crimes p-value significance",
  cor_leeds,
  method = "square",
  outline.color = "white",
  type = "full",
  ggtheme = ggplot2::theme_get,
  lab = TRUE,
  lab_size = 6,
  tl.srt = 45,
  pch.cex = 24,
  p.mat = leeds_p.mat,
  colors = c("#6D9EC1", "white", "#E46726")
) + theme(text = element_text(face = "bold"))

numCrimesbyLSOA <- combined_crimes %>%
  select(Month, Crime.type, LSOA.code) %>%
  group_by(Month, Crime.type, LSOA.code) %>%
  na.omit() %>%
  summarise(Num_crimes = n())

leedsShape <-
  readOGR(dsn = "./leeds_lsoa", layer = "england_lsoa_2011")

numCrimesbyType <- left_join(leedsShape@data,
                             numCrimesbyLSOA,
                             by = c('code' = 'LSOA.code')) %>%
  group_by(Month, Crime.type) %>%
  summarise(crime_sum = sum(Num_crimes))

numCrimesbyMonth <- numCrimesbyType %>%
  select(Month, crime_sum) %>%
  group_by(Month) %>%
  summarise(crime_sum = sum(crime_sum))

dataset_path <- "./Leeds_City_Council_Spending/"

leeds_council_dataset <- list.files(path = dataset_path,
                                    pattern = "*.csv",
                                    full.names = TRUE)

read_all_csv <- lapply(leeds_council_dataset,
                       function(x)
                         read.csv(x, stringsAsFactors = F, header = T))
combined = ''
for (leeds_council_month in leeds_council_dataset) {
  single_month_data <- read.csv(leeds_council_month, header = TRUE)
  combining <-
    select(single_month_data, Effective.Date, Amount)[which(single_month_data$Benificiary.Name == "P & Cc For West Yorkshire"),]
  combined <- rbind(combined, combining)
}

leeds_council_spending <- combined %>%
  group_by(Effective.Date) %>%
  summarise(Amount_sum = sum(as.numeric(Amount))) %>%
  na.omit()

leeds_council_spending <-
  leeds_council_spending[order(as.Date(leeds_council_spending$Effective.Date, format = "%d/%m/%Y")),]

crime_and_spending <-
  cbind(numCrimesbyMonth, leeds_council_spending)[-3]
rownames(crime_and_spending) <- crime_and_spending[, 1]
crime_and_spending <- crime_and_spending[, -1]
cor(crime_and_spending)
summary(lm(formula = crime_sum ~ Amount_sum, data = crime_and_spending))

ggplot(data = crime_and_spending,
       aes(x = crime_sum,
           y = as.integer(Amount_sum))) +
  geom_point(size = 4.5,
             color = "#999999") +
  geom_smooth(
    method = lm,
    size = 0.7,
    color = "#ffffff",
    fill = "#2469FF",
    se = TRUE
  ) +
  ggtitle("The correlation between the number of crimes and the amount of police in Leeds") +
  xlab("The number of crimes@Leeds (over past 36 months)") +
  ylab("The amount of spending@West Yorkshire Police") +
  theme(
    plot.title = element_text(
      size = 16,
      color = "#ffffff",
      hjust = 0,
      face = "bold"
    ),
    axis.title = element_text(
      size = 14,
      color = "#888888",
      hjust = 0.5,
      face = "bold"
    ),
    axis.text.x = element_text(
      size = 12,
      color = "#ffffff",
      hjust = 0.5,
      angle = 45,
      face = "bold"
    ),
    axis.text.y = element_text(
      size = 12,
      angle = 45,
      face = "bold",
      color = "#ffffff"
    ),
    plot.background = element_rect(fill = "#2E2E2E",
                                   color = NA),
    plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "inches"),
    panel.background = element_rect(fill = "#5E5E5E"),
    panel.grid = element_line(color = "#888888"),
    panel.grid.minor = element_line(color = "#727272")
  )

################################################################################
# Visualise Leeds crime data by month
################################################################################

crimes_csv <-
  list.files(path = crimes_dir,
             pattern = "*.csv",
             full.names = TRUE)
# remove combined csv
crimes_csv <-
  crimes_csv[-(grep(pattern = "Combined", x = crimes_csv))]

for (single_month in crimes_csv) {
  crimes <- read.csv(single_month, header = TRUE)
  crimes_month <- substr(single_month, 33, 39)
  
  numCrimesByLSOA <- crimes %>%
    select(LSOA.code, Longitude, Latitude, Crime.type) %>%
    group_by(Crime.type, LSOA.code) %>%
    summarise(Num_crimes = n())
  
  leeds_crimes_location <- left_join(leedsShape@data,
                                     numCrimesByLSOA,
                                     by = c('code' = 'LSOA.code')) %>%
    na.omit()
  
  crimes_location <- crimes %>%
    select(LSOA.code, Longitude, Latitude, Crime.type)
  
  leeds_crimes_location <- left_join(leedsShape@data,
                                     crimes_location,
                                     by = c('code' = 'LSOA.code')) %>%
    na.omit()
  
  plot01 <- qmplot(
    Longitude,
    Latitude,
    data = leeds_crimes_location,
    geom = "blank",
    darken = .7,
    zoom = 12,
    maptype = "toner",
    legend = "bottomright"
  ) +
    stat_density_2d(
      aes(fill = ..level..),
      bins = 50,
      geom = "polygon",
      alpha = .2,
      color = NA
    ) +
    scale_fill_gradient2(
      paste0("Leeds Crimes\nPropensity\n", crimes_month),
      low = "white",
      mid = "yellow",
      high = "red",
      midpoint = 100
    ) +
    theme(
      plot.background = element_rect(fill = "#5E5E5E",
                                     color = "#5E5E5E"),
      panel.background = element_rect(fill = "#5E5E5E",
                                      color = "#5E5E5E")
    )
  
  crime_colors <- c(
    "Anti-social behaviour" = "#A172FF",
    "Bicycle theft" = "#72A1FF",
    "Burglary" = "#0074D9",
    "Criminal damage and arson" = "#39CCCC",
    "Drugs" = "#1B7B00",
    "Other crime" = "#111111",
    "Other theft" = "#EEEEEE",
    "Possession of weapons" = "#85144b",
    "Public order" = "#F012BE",
    "Robbery" = "#3333cc",
    "Shoplifting" = "#01FF70",
    "Theft from the person" = "#E4FF00",
    "Vehicle crime" = "#FFB800",
    "Violence and sexual offences" = "#FF4136"
  )
  
  plot02 <- qmplot(
    Longitude,
    Latitude,
    data = leeds_crimes_location,
    geom = "blank",
    darken = .7,
    zoom = 12,
    maptype = "toner",
    legend = "bottomright"
  ) +
    stat_bin2d(
      aes(Longitude, Latitude,
          fill = Crime.type),
      size = 0,
      bins = 150,
      alpha = 1,
      drop = TRUE,
      na.rm = TRUE,
      show.legend = FALSE,
      data = leeds_crimes_location
    ) +
    scale_fill_manual(values = crime_colors) +
    theme(
      plot.background = element_rect(fill = "#5E5E5E",
                                     color = "#5E5E5E"),
      panel.background = element_rect(fill = "#5E5E5E",
                                      color = "#5E5E5E")
    )
  
  crime_type_sum <- leeds_crimes_location %>%
    select(Crime.type) %>%
    group_by(Crime.type) %>%
    summarise(Num_crimes = n()) %>%
    na.omit()
  
  crime_type_sum$Percent <- round(crime_type_sum$Num_crimes /
                                    sum(crime_type_sum$Num_crimes) * 100,
                                  2)
  
  plot03 <- ggplot(crime_type_sum,
                   aes(
                     x = reorder(Crime.type, Num_crimes),
                     y = as.numeric(Num_crimes),
                     fill = Crime.type
                   )) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_text(
      label = paste0(crime_type_sum$Percent, '%'),
      hjust = -0.05,
      color = "#ffffff",
      fontface = "bold"
    ) +
    geom_hline(
      yintercept = max(crime_type_sum$Num_crimes),
      color = "#eeeeee",
      size = 1
    ) +
    scale_fill_manual(values = crime_colors) +
    scale_y_continuous(limits = c(0, max(crime_type_sum$Num_crimes) * 1.05),
                       breaks = sort(c(
                         seq(
                           min(crime_type_sum$Num_crimes),
                           max(crime_type_sum$Num_crimes),
                           length.out = 6
                         ),
                         max(crime_type_sum$Num_crimes)
                       ))) +
    geom_text(
      aes(
        x = 0,
        y = max(crime_type_sum$Num_crimes) * 1.05,
        label = paste0("Leeds Crime Types\n", crimes_month),
        hjust = 1,
        vjust = 0
      ),
      fontface = "bold.italic",
      size = 8,
      color = "#2E2E2E",
      show.legend = FALSE,
      nudge_x = 1.2
    ) +
    coord_flip() +
    labs(x = "", y = "the number of crimes") +
    theme(
      plot.title = element_text(
        size = 12,
        color = "#888888",
        hjust = 0,
        face = "bold"
      ),
      axis.title = element_text(
        size = 11,
        color = "#888888",
        hjust = 0.95,
        face = "bold"
      ),
      axis.text.x = element_text(
        size = 11,
        color = "#ffffff",
        hjust = 0
      ),
      axis.text.y = element_text(size = 11,
                                 color = "#ffffff"),
      plot.background = element_rect(fill = "#2E2E2E",
                                     color = NA),
      panel.background = element_rect(fill = "#5E5E5E"),
      panel.grid = element_line(color = "#888888")
    )
  
  #ggplot2.multiplot(plot01, plot02, plot03, cols = 3)
  jpeg(
    filename = paste0("./images/", crimes_month, "_leeds_crimes.jpeg"),
    width = 1920,
    height = 580,
    units = "px",
    pointsize = 24,
    bg = "#2E2E2E"
  )
  print(
    cowplot::ggdraw(grid.arrange(plot01, plot02, plot03, ncol = 3)) +
      theme(
        plot.background = element_rect(fill = "#5E5E5E",
                                       color = "#5E5E5E"),
        panel.background = element_rect(fill = "#5E5E5E",
                                        color = "#5E5E5E")
      )
  )
  dev.off()
  
}

################################################################################

plot04 <- ggplot() +
  geom_area(aes(
    x = as.Date(as.yearmon(Month)),
    y = crime_sum,
    group = reorder(Crime.type,-crime_sum),
    fill = reorder(Crime.type,-crime_sum)
  ),
  data = numCrimesbyType) +
  geom_point(aes(x = as.Date(as.yearmon(Month)),
                 y = crime_sum),
             color = "#2E2E2E",
             data = numCrimesbyMonth) +
  geom_smooth(
    aes(x = as.Date(as.yearmon(Month)),
        y = crime_sum),
    data = numCrimesbyMonth,
    method = lm,
    size = 0.7,
    color = "white",
    se = TRUE
  ) +
  geom_hline(
    yintercept = max(numCrimesbyMonth$crime_sum),
    color = "#eeeeee",
    size = 1,
    alpha = 0.5,
    linetype = "dashed"
  ) +
  geom_text(
    aes(
      x = as.Date(as.yearmon(Month)),
      y = crime_sum,
      label = crime_sum
    ),
    data = numCrimesbyMonth,
    hjust = 1,
    vjust = 0.5,
    angle = 90,
    color = "#ffffff",
    size = 4
  ) +
  scale_fill_manual(values = crime_colors,
                    name = "Leeds crime types") +
  scale_x_date(
    date_labels = "%Y-%m",
    date_breaks = "3 months",
    date_minor_breaks = "1 months"
  ) +
  xlab("Month (over 36 months)") +
  ylab("The number of crime") +
  theme(
    plot.title = element_text(
      size = 12,
      color = "#888888",
      hjust = 0,
      face = "bold"
    ),
    axis.title = element_text(
      size = 12,
      color = "#888888",
      hjust = 0.5,
      face = "bold"
    ),
    axis.text.x = element_text(
      size = 12,
      color = "#ffffff",
      hjust = 0.5,
      angle = 45,
      face = "bold"
    ),
    axis.text.y = element_text(
      size = 12,
      angle = 45,
      face = "bold",
      color = "#ffffff"
    ),
    legend.title = element_text(size = 12,
                                hjust = 0,
                                face = "bold"),
    legend.text = element_text(#size = 12,
      hjust = 0,
      face = "bold"),
    plot.background = element_rect(fill = "#2E2E2E",
                                   color = NA),
    plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "inches"),
    panel.background = element_rect(fill = "#5E5E5E"),
    panel.grid = element_line(color = "#888888"),
    panel.grid.minor = element_line(color = "#727272")
  )

plot05 <-
  ggplot(
    numCrimesbyType,
    aes(
      x = as.Date(as.yearmon(Month)),
      y = crime_sum,
      group = reorder(Crime.type,-crime_sum),
      fill = reorder(Crime.type,-crime_sum),
      color = reorder(Crime.type,-crime_sum)
    )
  ) +
  geom_point(show.legend = FALSE) +
  geom_smooth(
    method = lm,
    color = "white",
    size = 0.7,
    show.legend = FALSE
  ) +
  facet_wrap( ~ reorder(Crime.type,-crime_sum),
              scales = "free",
              ncol = 2) +
  scale_discrete_manual(aesthetics = c("colour", "fill"),
                        values = crime_colors) +
  xlab("") +
  ylab("") +
  theme(
    axis.text.x = element_text(
      color = "#ffffff",
      hjust = 0,
      face = "bold"
    ),
    axis.text.y = element_text(face = "bold",
                               color = "#ffffff"),
    plot.background = element_rect(fill = "#2E2E2E",
                                   color = NA),
    plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "inches"),
    panel.background = element_rect(fill = "#5E5E5E"),
    panel.grid = element_line(color = "#888888"),
    panel.grid.minor = element_line(color = "#727272"),
    strip.background = element_rect(fill = "#242424"),
    strip.text = element_text(color = "#ffffff",
                              face = "bold")
  )

filename = "./images/leeds_crimes_36_months.jpeg"

jpeg(
  filename = filename,
  width = 1919,
  height = 1014,
  units = "px",
  pointsize = 12,
  bg = "#2E2E2E"
)
print(
  cowplot::ggdraw(grid.arrange(
    plot05, plot04, ncol = 2, widths = c(1, 2.5)
  )) +
    theme(
      plot.background = element_rect(fill = "#5E5E5E",
                                     color = "#5E5E5E"),
      panel.background = element_rect(fill = "#5E5E5E",
                                      color = "#5E5E5E")
    )
)
dev.off()
