# Packages
require(tidyverse)
require(icesDatras)
require(DATRAS)
require(icesVocab)
require(sf)
require(readxl)
require(data.table)
require(RColorBrewer)
require(surveyIndex)

#Set location (of MasterTable, ICES data, and output folder)
setwd("")

outPath <- "Output/Survey Indices/"

#Read in master table
MasterTable <- read_xlsx("MasterTable.xlsx")

# Set year
year <- 2026

# Select group and species (Aphia ID)
Group <- "Sepiidae"

sp        <- unique(MasterTable$Species[MasterTable$Family == Group])
# Get aphia IDs
aphias     <- findAphia(sp, latin =  T)
aphias_df <- data.frame(aphia = aphias, SpeciesName = sp)
aphias_df$aphia <- as.numeric(aphias_df$aphia)

# Adjustment for Sepiidae: Sepia -> Sepia spp
#aphias_df$SpeciesName <- ifelse(aphias_df$SpeciesName %in% c("Alloteuthis", "Alloteuthis media", "Alloteuthis subulata"), "Alloteuthis spp.", aphias_df$SpeciesName)
aphias_df$SpeciesName <- ifelse(aphias_df$SpeciesName == "Sepia", "Sepia spp.", aphias_df$SpeciesName)

# Select surveys for the whole group, making sure to use the correct names
icesDatras::getSurveyList()

surveys   <- unique(strsplit(paste(MasterTable$Surveys[MasterTable$Family == Group],collapse=","), ",")[[1]])

# Notes
# SWC-IBTS historical, SCOWCGFS from 2011 onwards
# ROCKALL historical, SCOROC from 2011 onwards
# BTS-VIII beam trawl in BoB
# PT-IBTS
# Spanish surveys: SP-ARSA, SP-NORTH, SP-PORC
# SCOROC survey 

# Select time range
years     <- 2000:year
quarters  <- 1:4

# Get survey data from DATRAS
for(surv in surveys){
  # Survey data
  surv_dat <- DATRAS::getDatrasExchange(survey = surv, years = years, quarters = quarters)
  
  # Bind surveys together into one DATRASraw file
  if(surv == surveys[1]){
    surv_list <- surv_dat
  }else{
    # Some data is duplicate among surveys (SP-ARSA), remove
    surv_dat <- subset(surv_dat,!haul.id %in% surv_list[["HH"]]$haul.id)
    surv_list <- c(surv_list, surv_dat)
  }
  rm(surv_dat)
}

# Add spatial information (area)
# Read ICES areas
ICES_areas <- read_sf("ICES_areas/ICES_Areas_20160601_cut_dense_3857.shp") |>
  st_transform(crs = 4326)

# Read ICES rectangles as sf
ICES_rectangles_sf <- read_sf("ICES_rectangles/ICES_Statistical_Rectangles_Eco.shp") |>
  st_transform(crs = 4326)

# Create centroid coordinates for each rectangle
ICES_rect_centroids <- ICES_rectangles_sf |>
  st_centroid()

coords <- st_coordinates(ICES_rect_centroids)

ICES_rect_centroids <- ICES_rect_centroids |>
  mutate(CentroidLong = coords[, 1],
    CentroidLat  = coords[, 2]) |>
  st_drop_geometry() |>
  select(ICESNAME, AREA_KM2, CentroidLong, CentroidLat)

# Join centroid info to HH data
surv_list[["HH"]] <- surv_list[["HH"]] |>
  left_join(ICES_rect_centroids,by = c("StatRec" = "ICESNAME")) |>
  mutate(ShootLong = ifelse(is.na(ShootLong), CentroidLong, ShootLong),
    ShootLat  = ifelse(is.na(ShootLat),  CentroidLat,  ShootLat)) |>
  rename(StatRecArea = AREA_KM2) |>
  select(-CentroidLong, -CentroidLat)

# If coordinates are still missing, remove these hauls
surv_list <- subset(surv_list, !is.na(ShootLat))

# Spatial join to ICES areas
sf_use_s2(FALSE)

HH_sf <- st_as_sf(
  surv_list[["HH"]],
  coords = c("ShootLong", "ShootLat"),
  crs = 4326
)

surv_list[["HH"]]$Area <- st_join(HH_sf, ICES_areas)$Area_27

# In case rectangle is missing, get it from haul position
tmp <- st_join(HH_sf, ICES_rectangles_sf[, c("ICESNAME", "AREA_KM2")])

surv_list[["HH"]]$StatRec <- ifelse(
  is.na(surv_list[["HH"]]$StatRec),
  tmp$ICESNAME,
  surv_list[["HH"]]$StatRec)

# Also add rectangle to HL
surv_list[["HL"]] <- surv_list[["HL"]] %>%
  dplyr::left_join(surv_list[["HH"]] %>% dplyr::select(haul.id, StatRec),by = "haul.id")


# Align survey names with intended results
surv_list[["HH"]] <- surv_list[["HH"]]  %>%
  mutate(SurveyName = case_when(Survey == "NS-IBTS" & Quarter == 1 ~ "IBTS Q1",
                                Survey == "NS-IBTS" & Quarter %in% c(3,4) ~ "IBTS Q3", #IBTS-Q3 sometimes goes into early October
                                Survey == "SP-ARSA" & Quarter == 1 ~ "SP-ARSA Q1",
                                Survey == "SP-ARSA" & Quarter == 4 ~ "SP-ARSA Q4",
                                Survey == "SCOWCGFS" & Quarter == 1 ~ "SCOWCGFS Q1",
                                Survey == "SCOWCGFS" & Quarter == 4 ~ "SCOWCGFS Q4",
                                Survey == "NIGFS" & Quarter == 1 ~ "NIGFS Q1",
                                Survey == "NIGFS" & Quarter == 4 ~ "NIGFS Q4",
                                Survey == "BTS" & Quarter == 1 & !Area %in% c("4.a","4.b","4.c") ~ "BTS Q1",
                                Survey == "BTS" & Quarter == 3 & !Area %in% c("4.a","4.b","4.c") ~ "BTS Q3",
                                TRUE ~ Survey)) %>%
  filter(!(Quarter == 2 & Survey == "NS-IBTS")) #Occasional Norwegian IBTS in June, ignore

# Also join to HL
haul_survname <- unique(surv_list[["HH"]][, c("haul.id", "Survey", "SurveyName")])

surv_list[["HL"]] <- surv_list[["HL"]]  %>%
  left_join(haul_survname, by = c("haul.id", "Survey"))

# Keep only relevant species (aphias)
surv_list <- subset(surv_list, Valid_Aphia %in% aphias_df$aphia)

# Keep only valid hauls
surv_list <- subset(surv_list, HaulVal == "V")

# Save, read in next year and only extract the most recent year

save(surv_list,file= paste0("SurveyData_",Group,"_00-26.RData"))
load(file= paste0("SurveyData_",Group,"_00-26.RData"))

# Stratified survey index using HH directly, for both numbers and biomass
SurveyIdxCPUE <- function(hh){
  
  hh$cpue[hh$cpue == -9] <- NA
  hh$bpue[hh$bpue == -9] <- NA
  
  ysplit <- split(hh, hh$Year)
  
  res <- lapply(names(ysplit), function(y){
    
    d <- ysplit[[y]]
    
    # Mean per statistical rectangle
    byRec <- aggregate(
      cbind(cpue, bpue) ~ StatRec,
      data = d,
      FUN = mean,
      na.rm = TRUE)
    
    data.frame(
      Year    = as.numeric(y),
      index_n = mean(byRec$cpue, na.rm = TRUE),
      index_b = mean(byRec$bpue, na.rm = TRUE)
    )
  })
  
  res <- do.call(rbind, res)
  # Mean-standardize
  res$index_n_std <- res$index_n / mean(res$index_n, na.rm = TRUE)
  res$index_b_std <- res$index_b / mean(res$index_b, na.rm = TRUE)
  res
}

IndexDat <- data.frame()
TrendTable <- data.frame()

# Loop over areas
for(area in unique(MasterTable$Area[MasterTable$Family == Group])){
  print(area)
  # Loop over species
  Survey_index <- data.frame()
  for(sp2 in unique(aphias_df$SpeciesName)){
    sp2 <- gsub(" spp.", "", sp)
    # If species present for area
    if(sp %in% MasterTable$Species[MasterTable$Area == area]){
      # Get divisions and surveys from MasterTable
      divisions    <- strsplit(unique(MasterTable$Divisions[MasterTable$Species == sp2 & MasterTable$Area == area]), ",")[[1]]
      Surveys      <- strsplit(unique(MasterTable$Surveys[MasterTable$Species == sp2 & MasterTable$Area == area]), ",")[[1]]
      SurveyNames  <- strsplit(unique(MasterTable$SurveyGroups[MasterTable$Species == sp2 & MasterTable$Area == area]), ",")[[1]]
      
      # Subset species
      sp_dat   <- subset(surv_list, Valid_Aphia %in% aphias_df$aphia[aphias_df$SpeciesName == sp])
      
      #Subset areas and surveys
      sp_dat   <- subset(sp_dat, Area %in% divisions & Survey %in% Surveys)
      
      
      # Loop over surveys
      for(survey in SurveyNames){
        sp_dat1  <- subset(sp_dat, SurveyName == survey)
        if(nrow(sp_dat1[["HL"]]) > 0){
          # Calculate total numbers and weight per haul
          # Use HL table directly
          hl <- sp_dat1[["HL"]]
          
          # Keep unique hauls, if multiple total numbers found, we assume they are separate observations (e.g. male and female) and can be summed
          hl_unique <- hl %>%
            group_by(haul.id) %>%
            summarise(TotalNo = if (n_distinct(TotalNo) > 1) {
              sum(TotalNo, na.rm = TRUE)
            } else {first(TotalNo)},
            CatCatchWgt = if (n_distinct(CatCatchWgt) > 1) {
              sum(CatCatchWgt, na.rm = TRUE)
            } else {first(CatCatchWgt)},
            across(-c(TotalNo, CatCatchWgt), first),.groups = "drop")
          
          # CPUE = numbers per hour, BPUE = biomass per hour
          hl_unique$cpue <- hl_unique$TotalNo / (hl_unique$HaulDur / 60)
          hl_unique$bpue <- hl_unique$CatCatchWgt / (hl_unique$HaulDur / 60)
          
          # Merge into HH (because we also want hauls with 0 observations)
          hh <- sp_dat1[["HH"]]
          
          # Keep only haul ID + cpue
          cpue_dat <- hl_unique[, c("haul.id", "cpue", "bpue")]
          
          # Merge into HH
          hh <- merge(hh, cpue_dat, by = "haul.id", all.x = TRUE)
          
          # Zero catches become NA after merge -> set to 0
          hh$cpue[is.na(hh$cpue)] <- 0  
          hh$bpue[is.na(hh$bpue)] <- 0  
          
          # No hauls where cpue or bpue is -9 (NA)
          
          
          # Calculate index based on stratified mean
          idx     <- as.data.frame(SurveyIdxCPUE(hh))
          
          # Save properly
          idx$SpeciesName <- sp 
          idx$SurveyName  <- survey
          
          # Filter years between first observation of that species and the last observation of that survey
          idx <- idx %>% filter(Year >= min(as.character(sp_dat1[["HL"]]$Year)) & Year <= max(as.character(sp_dat1[["HH"]]$Year)))
          
          # Join to df
          Survey_index <- bind_rows(Survey_index, idx)
          rm(idx)
        }
      }
    }
  }
  
  if(nrow(Survey_index) > 0){
    
    # Get colors for surveys
    cols  <- brewer.pal(8, "Set1")
    
    # Plot numbers per hour
    Survey_plot <- ggplot(data = Survey_index) + 
      geom_line(aes(x = Year, y = index_n_std, group = SurveyName, color = SurveyName)) + 
      scale_color_manual(values = cols, name = "Survey") +
      #geom_ribbon(aes(x = Year,ymin = ci_lower, ymax = ci_upper, fill = SurveyName),  alpha = 0.3) +
      scale_fill_manual(values = cols, guide = "none") +
      scale_x_continuous(breaks = seq(min(Survey_index$Year), max(Survey_index$Year), by = 2)) +
      coord_cartesian(ylim = c(0, NA)) +
      ylab("Mean-standardized index") +
      theme_bw(base_size=12) + 
      scale_y_continuous(limits = c(-0.01, NA),expand = expansion(mult = c(0, 0.05))) +
      facet_wrap(~SpeciesName, scales = "free_y", ncol = round(length(unique(Survey_index$SpeciesName))/2)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            strip.background=element_blank(),
            strip.text=element_text(face="bold"),
            panel.grid.minor=element_blank(),
            legend.position="bottom",
            axis.title=element_text(face="bold"),
            axis.text=element_text(colour="black"))
    
    ggsave(paste0(outPath, Group,"_",area,"_Survey-Numbers.png"), width = 3000, height = 1500, units = "px")
    
    # Calculate mean of 3-year period and compare to previous 3-year period
    Survey_index_mean <- Survey_index %>%
      group_by(SurveyName) %>%
      mutate(final_year = as.numeric(max(Year))) %>%
      filter(final_year %in% c(year, year-1)) %>%
      mutate(mean_period = case_when(
        Year %in% (unique(final_year) - 2):unique(final_year) ~ "Current\n3-year period",
        Year %in% (unique(final_year) - 5):(unique(final_year) - 3) ~ "Previous\n3-year period",
        TRUE ~ NA_character_)) %>%
      filter(!is.na(mean_period)) %>%
      ungroup() %>%
      group_by(mean_period, SpeciesName, SurveyName) %>%
      summarise(mean_index = mean(index_n_std, na.rm = TRUE), .groups = "drop") %>%
      group_by(SpeciesName, SurveyName) %>%
      filter(n_distinct(mean_period) == 2) %>%
      ungroup() %>%
      mutate(mean_period = factor(mean_period,levels = c("Previous\n3-year period", "Current\n3-year period")))
    
    # Plot
    Survey_mean_plot <- ggplot(data = Survey_index_mean) + 
      geom_point(aes(x = mean_period, y = mean_index, col = SurveyName)) + 
      geom_line(aes(x = mean_period, y = mean_index, col = SurveyName, group = SurveyName)) + 
      scale_color_manual(values = cols, name = "Survey") +
      scale_fill_manual(values = cols, guide = "none") +
      ylab("Mean-standardized index") + xlab(NULL) +
      theme_bw(base_size=12) + 
      scale_y_continuous(limits = c(-0.01, NA),expand = expansion(mult = c(0, 0.05))) +
      facet_wrap(~SpeciesName, scales = "free_y", ncol = round(length(unique(Survey_index$SpeciesName))/2)) +
      theme(strip.background=element_blank(),
            strip.text=element_text(face="bold"),
            panel.grid.minor=element_blank(),
            legend.position="bottom",
            axis.title=element_text(face="bold"),
            axis.text=element_text(colour="black"))
    
    ggsave(paste0(outPath, Group,"_",area,"_MeanNumbers_Period.png"), width = 2000, height = 1500, units = "px")
    
    #Store trend results
    TrendTable_area <-  Survey_index_mean %>%
      mutate(mean_period = sub("\n.*", "", mean_period)) %>%
      pivot_wider(values_from = mean_index, names_from = mean_period) %>%
      mutate(Trend = case_when((Current-Previous)/Previous < -0.1 ~ "↘",
                               (Current-Previous)/Previous >= -0.1 & (Current-Previous)/Previous <= 0.1 ~ "-",
                               (Current-Previous)/Previous > 0.1 ~ "↗"),
             Area = area) %>%
      select(Area, SurveyName, SpeciesName, Trend) %>%
      pivot_wider(names_from = SpeciesName, values_from = Trend)
    
    # Plot biomass per hour
    Survey_plot <- ggplot(data = Survey_index) + 
      geom_line(aes(x = Year, y = index_b_std, group = SurveyName, color = SurveyName)) + 
      scale_color_manual(values = cols, name = "Survey") +
      #geom_ribbon(aes(x = Year,ymin = ci_lower, ymax = ci_upper, fill = SurveyName),  alpha = 0.3) +
      scale_fill_manual(values = cols, guide = "none") +
      scale_x_continuous(breaks = seq(min(Survey_index$Year), max(Survey_index$Year), by = 2)) +
      ylab("Mean-standardized index") +
      theme_bw(base_size=12) + 
      scale_y_continuous(limits = c(-0.01, NA),expand = expansion(mult = c(0, 0.05))) +
      facet_wrap(~SpeciesName, scales = "free_y", ncol = round(length(unique(Survey_index$SpeciesName))/2)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            strip.background=element_blank(),
            strip.text=element_text(face="bold"),
            panel.grid.minor=element_blank(),
            legend.position="bottom",
            axis.title=element_text(face="bold"),
            axis.text=element_text(colour="black"))
    
    ggsave(paste0(outPath, Group,"_",area,"_Survey-Biomass.png"), width = 3000, height = 1500, units = "px")
    
    # Calculate mean of 3-year period and compare to previous 3-year period
    Survey_index_mean <- Survey_index %>%
      group_by(SurveyName) %>%
      mutate(final_year = as.numeric(max(Year))) %>%
      filter(final_year %in% c(year, year-1)) %>%
      mutate(mean_period = case_when(
        Year %in% (unique(final_year) - 2):unique(final_year) ~ "Current\n3-year period",
        Year %in% (unique(final_year) - 5):(unique(final_year) - 3) ~ "Previous\n3-year period",
        TRUE ~ NA_character_)) %>%
      filter(!is.na(mean_period)) %>%
      ungroup() %>%
      group_by(mean_period, SpeciesName, SurveyName) %>%
      summarise(mean_index = mean(index_b_std, na.rm = TRUE), .groups = "drop") %>%
      group_by(SpeciesName, SurveyName) %>%
      filter(n_distinct(mean_period) == 2) %>%
      ungroup() %>%
      mutate(mean_period = factor(mean_period,levels = c("Previous\n3-year period", "Current\n3-year period")))
    
    # Plot mean biomass per hour
    Survey_mean_plot <- ggplot(data = Survey_index_mean) + 
      geom_point(aes(x = mean_period, y = mean_index, col = SurveyName)) + 
      geom_line(aes(x = mean_period, y = mean_index, col = SurveyName, group = SurveyName)) + 
      scale_color_manual(values = cols, name = "Survey") +
      scale_fill_manual(values = cols, guide = "none") +
      ylab("Mean-standardized index") + xlab(NULL) +
      theme_bw(base_size=12) + 
      scale_y_continuous(limits = c(-0.01, NA),expand = expansion(mult = c(0, 0.05))) +
      facet_wrap(~SpeciesName, scales = "free_y", ncol = round(length(unique(Survey_index$SpeciesName))/2)) +
      theme(strip.background=element_blank(),
            strip.text=element_text(face="bold"),
            panel.grid.minor=element_blank(),
            legend.position="bottom",
            axis.title=element_text(face="bold"),
            axis.text=element_text(colour="black"))
    
    ggsave(paste0(outPath, Group,"_",area,"_MeanBiomass_Period.png"), width = 2000, height = 1500, units = "px")
    
    # Bind together all indices and overall trends
    Survey_index$Region <- area
    IndexDat <- bind_rows(IndexDat, Survey_index)
    
    TrendTable <- bind_rows(TrendTable, TrendTable_area)
}}

# Save index data
write.csv(IndexDat, file = paste0(outPath,Group, "_IndexData.csv"))
write_xlsx(TrendTable, path = paste0(outPath,Group, "_TrendTable.xlsx"))
