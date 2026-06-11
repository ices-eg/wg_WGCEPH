# Packages
require(tidyverse)
require(icesDatras)
require(DATRAS)
require(icesVocab)
require(sf)
require(readxl)
require(writexl)
require(data.table)
require(RColorBrewer)
require(surveyIndex)

#Set location (of MasterTable, ICES data, and output folder)
setwd("W:/IMARES/DATA/ICES-WG/WGCEPH/2026/")

outPath <- "Results/"

#Read in master table
MasterTable <- read_xlsx("MasterTable.xlsx")

# Set year
year <- 2026

# Select group and species (Aphia ID)
Group <- "Loliginidae"

sp        <- unique(MasterTable$Species[MasterTable$Family == Group])
# Get aphia IDs
aphias     <- findAphia(sp, latin =  T)
aphias_df <- data.frame(aphia = aphias, SpeciesName = sp)
aphias_df$aphia <- as.numeric(aphias_df$aphia)

# Adjustment for Loliginids: all Alloteuthis grouped, Loligo -> Loligo spp
aphias_df$SpeciesName <- ifelse(aphias_df$SpeciesName %in% c("Alloteuthis", "Alloteuthis media", "Alloteuthis subulata"), "Alloteuthis spp.", aphias_df$SpeciesName)
aphias_df$SpeciesName <- ifelse(aphias_df$SpeciesName == "Loligo", "Loligo spp.", aphias_df$SpeciesName)

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


checkie <- surv_list[["HL"]]
checkie <- checkie %>% filter(Valid_Aphia == 140601 & Year == 2025 & Survey == "SP-ARSA")
nrow(checkie)

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

# Get map of Europe
library(giscoR)
eurPolsHires <- gisco_get_countries(
  year = 2020,
  resolution = "01",  # highest resolution
  country = c("Norway", "Sweden", "Finland", "Denmark", "United Kingdom", "Ireland","Germany", "Netherlands", "Belgium",
              "Luxembourg", "France", "Andorra", "Spain", "Portugal", "Marocco", "Switzerland")
)
eurPolsHires <- st_as_sf(eurPolsHires)
st_crs(eurPolsHires) <- 4326


IndexDat <- data.frame()
TrendTable <- data.frame()

# Loop over areas
for(area in unique(MasterTable$Area[MasterTable$Family == Group])){
  print(area)
  # Loop over species
  Survey_index <- data.frame()
  for(sp in unique(aphias_df$SpeciesName)){
    sp2 <- gsub(" spp.", "", sp)
    # If species present for area
    if(sp2 %in% MasterTable$Species[MasterTable$Area == area]){
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
    
    ggsave(paste0(outPath, Group,"/",Group,"_",area,"_Survey-Numbers.png"), width = 3000, height = 1500, units = "px")
    
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
    
    ggsave(paste0(outPath, Group,"/",Group,"_",area,"_MeanNumbers_Period.png"), width = 2000, height = 1500, units = "px")
    
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
    
    ggsave(paste0(outPath, Group,"/",Group,"_",area,"_Survey-Biomass.png"), width = 3000, height = 1500, units = "px")
    
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
    
    ggsave(paste0(outPath, Group,"/",Group,"_",area,"_MeanBiomass_Period.png"), width = 2000, height = 1500, units = "px")
    
    # Bind together all indices and overall trends
    Survey_index$Region <- area
    IndexDat <- bind_rows(IndexDat, Survey_index)
    
    TrendTable <- bind_rows(TrendTable, TrendTable_area)
    
    # Plot maps 
    SurveyNames      <- unique(unlist(strsplit(MasterTable$SurveyGroups[MasterTable$Area == area], ",")))
    # Loop over surveys
    for(survey in SurveyNames){
      hh_all <- data.frame()
      divisions    <- unique(unlist(strsplit(MasterTable$Divisions[MasterTable$Area == area], ",")))
      
      surv_dat  <- subset(surv_list, SurveyName == survey & Area %in% divisions)
      
      if(nrow(surv_dat[["HL"]]) > 0){
        for(aph in unique(surv_dat[["HL"]]$Valid_Aphia)){
          surv_sp_dat <- subset(surv_dat, Valid_Aphia == aph)
        
          if(nrow(surv_sp_dat[["HL"]]) > 0){
            # Calculate total numbers and weight per haul
            # Use HL table directly
            hl <- surv_sp_dat[["HL"]]
            
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
            
            # Merge into HH (because we also want hauls with 0 observations)
            hh <- surv_sp_dat[["HH"]]
            
            # Keep only haul ID + cpue
            cat_dat <- hl_unique[, c("haul.id",  "Valid_Aphia", "TotalNo", "CatCatchWgt")]
            
            # Merge into HH
            hh <- merge(hh, cat_dat, by = "haul.id", all.x = TRUE)
            
            # Zero catches become NA after merge -> set to 0
            hh$TotalNo[is.na(hh$TotalNo)] <- 0  
            hh$CatCatchWgt[is.na(hh$CatCatchWgt)] <- 0  
            hh$Valid_Aphia[is.na(hh$Valid_Aphia)] <- aph  
            
            #Add species name      
            hh <- hh %>% left_join(aphias_df, by = c("Valid_Aphia" = "aphia"))
  
            
          }
          hh_all <- bind_rows(hh_all, hh)  
        }
      
      # Add species name
      
      # Transform to shapefile
      hh_sf <- st_as_sf(hh_all, coords = c("ShootLong", "ShootLat"), crs = 4326) %>%
        mutate(Zero = ifelse(TotalNo == 0, "Zero", "Not Zero"))
      
      hh_sf$FillGroup <- ifelse(hh_sf$Zero == "Zero", "Zero", hh_sf$Country)
      hh_sf$PointSize <- ifelse(hh_sf$Zero == "Zero", 1, hh_sf$TotalNo)
      
      # Get final 6 years and create bounding box
      YearRange <- c(max(as.numeric(as.character(hh_sf$Year)))-5):max(as.numeric(as.character(hh_sf$Year)))
      
      # Get ICES divisions
      ICES_areas_plot <- ICES_areas %>% mutate(DivName = paste(SubArea, Division, sep = ".")) %>%
        filter(DivName %in% divisions)
      
      bb <- sf::st_bbox(ICES_areas_plot)
      
      # Plot
      Surv_map_plot <- ggplot() +
        geom_sf(data = ICES_areas_plot, fill = NA, color = "grey") +
        geom_sf(data = subset(hh_sf, Year %in% YearRange & Zero == "Zero"),shape = 4,size = 1,colour = "lightgrey",alpha = 0.3) +
        geom_sf(data = subset(hh_sf, Year %in% YearRange & Zero != "Zero"),aes(size = log(TotalNo), fill = Country, colour = Country),shape = 21,alpha = 0.5) + 
        scale_size(range = c(0.2, 3))+
        geom_sf(data = eurPolsHires, fill = "light grey") +
        theme_bw() +
        coord_sf(xlim = c(bb["xmin"], bb["xmax"]),ylim = c(bb["ymin"], bb["ymax"]),expand = TRUE) +
        scale_x_continuous(breaks = pretty(c(bb["xmin"], bb["xmax"]), n = 4)) +
        scale_y_continuous(breaks = pretty(c(bb["ymin"], bb["ymax"]), n = 4))+
        xlab(NULL) + ylab(NULL) +
        ggtitle(survey) +
        facet_grid(SpeciesName ~ Year)      +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Save
      ggsave(Surv_map_plot ,filename = paste0(outPath, Group,"/",Group,"_",area,"_",survey,"_Map.png"), units = "px", width = 3000, height = 3000)
      }
      }
  }
  }

# Save index data
write.csv(IndexDat, file = paste0(outPath, Group,"/",Group, "_IndexData.csv"))
write_xlsx(TrendTable, path = paste0(outPath, Group,"/",Group, "_TrendTable.xlsx"))
