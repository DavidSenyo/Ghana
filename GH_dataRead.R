
##############################################################
### data and libraries for Ghana map app
###############################################################
### Libraries
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(dplyr)
library(tidyr)
library(janitor)
library(kableExtra)
library(leaflet)
library(sf)
library(shinyjs)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)

library(packcircles)
library(viridis)
library(ggiraph)
library(ggforce)


################################################################################
### Data Read
# Load the shapefile
ghana_shapefile <- st_read("gadm41_GHA_shp/gadm41_GHA_2.shp")
ghana_pop <- readxl::read_excel("GHPopp.xlsx", sheet = 'Sheet1')
GH_EcoData <- readxl::read_excel("GH_EcoData.xlsx", sheet = 'Sheet2')
GH_SocioEco <- readxl::read_excel("GH_SocioEconomic.xlsx", sheet = 'Sheet1')


# Example data for tourist attractions
tourist_attractions <- data.frame(
  name = c("Cape Coast Castle", "Kakum National Park", "Mole National Park", "Kwame Nkrumah Mausoleum",
           "Elmina Castle", "Paga Crocodile Pond", "Osu Castle", "Independence Arch", "W.E.B. DuBois Centre", "Akosnachi Enterprise Gh."),
  lat = c(5.1053, 5.4231, 9.2490, 5.5471,
          5.0829, 10.9856, 5.5469, 5.5492, 5.5823, 5.5469),
  lon = c(-1.2466, -1.3267, -1.9961, -0.2089,
          -1.3479, -1.1092, -0.1827,  -0.1929, -0.1709, -0.2063)
)

# Example data for hospitals
regional_hospitals <- data.frame(
  name = c("Korle Bu Teaching Hospital", "Komfo Anokye Teaching Hospital", "Tamale Teaching Hospital", 
           "Cape Coast Teaching Hospital", "Ho Teaching Hospital", "Effia Nkwanta Regional Hospital", "Eastern Regional Hospital"),
  region = c("Greater Accra", "Ashanti", "Northern", "Central", "Volta", "Western", "Eastern"),
  lat = c(5.5687, 6.6944, 9.4154, 5.1060, 6.6143, 4.9268, 6.0982),
  lon = c(-0.2030, -1.6319, -0.8374, -1.2468, 0.4814, -1.7562, -0.2564)
)

# Example data for waterfalls
waterfalls <- data.frame(
  name = c("Wli Waterfalls", "Kintampo Waterfalls", "Boti Falls", "Tagbo Falls", 
           "Tsenku Falls", "Akaa Falls", "Ote Falls", "Fuller Falls", 
           "Wli Agumatsa Waterfalls", "Begoro Falls", "Boti Waterfalls", "Fuller Waterfalls", "Ote Falls"),
  lat = c(7.1302, 8.0542, 6.1926, 6.8507, 5.9369, 6.1807, 6.6800, 7.8446, 7.1302, 6.3937, 7.3408, 8.31116, 6.9327),
  lon = c(0.5727, -1.7284, -0.0753, 0.4544, -0.0903, -0.0835, -0.1246, -1.9072, 0.5727, -0.4158, 0.3539, -1.7915, 0.4348)
)

# Dataframe with population density information for capitals
capitals <- data.frame(
  region = c("Greater Accra", "Ashanti", "Central", "Eastern", "Northern", 
             "Western", "Volta", "Upper East", "Upper West", "Bono", 
             "Bono East", "Ahafo", "Savannah", "North East", "Western North"),
  capital = c("Accra", "Kumasi", "Cape Coast", "Koforidua", "Tamale", 
              "Sekondi-Takoradi", "Ho", "Bolgatanga", "Wa", "Sunyani", 
              "Techiman", "Goaso", "Damongo", "Nalerigu", "Sefwi Wiawso"),
  lat = c(5.6037, 6.6884, 5.1053, 6.0925, 9.4071, 4.9346, 6.6000, 10.7875, 10.0609, 
          7.3401, 7.5861, 6.9833, 9.0817, 10.5261, 6.1044),
  lon = c(-0.1870, -1.6244, -1.2466, -0.2591, -0.8533, -1.7778, 0.4700, -0.8514, 
          -2.5019, -2.3268, -1.9380, -2.5167, -1.8157, -0.5764, -2.4820),
  population_density = c(12500, 8500, 2100, 1800, 1700, 1500, 1300, 1200, 1100, 
                         900, 850, 800, 750, 700, 650)  # hypothetical population density data (people per sq km)
)

# Dataframe with population density information for major towns
major_towns <- data.frame(
  town = c("Takoradi", "Tema", "Obuasi", "Tamale", "Cape Coast", "Ho"),
  region = c("Western", "Greater Accra", "Ashanti", "Northern", "Central", "Volta"),
  lat = c(4.8845, 5.6694, 6.2080, 9.4008, 5.1060, 6.6093),
  lon = c(-1.7554, -0.0166, -1.6318, -0.8393, -1.2468, 0.4786),
  population_density = c(3000, 2900, 2800, 2700, 2600, 2500)  # hypothetical population density data (people per sq km)
)

# Example data for infrastructure projects
infrastructure_projects <- data.frame(
  name = c("Tema Port Expansion", "Pokuase Interchange", "Kumasi International Airport"),
  lat = c(5.6370, 5.6350, 6.7150),
  lon = c(-0.0180, -0.2450, -1.5910)
)

# Example data for cultural sites
cultural_sites <- data.frame(
  name = c("Cape Coast Castle", "Elmina Castle", "Larabanga Mosque"),
  lat = c(5.1053, 5.0843, 9.2550),
  lon = c(-1.2466, -1.3503, -1.0500)
)

# Example data for educational institutions
educational_institutions <- data.frame(
  name = c("University of Ghana", "Kwame Nkrumah University of Science and Technology", "University of Cape Coast",
           "University for Development Studies", "University of Health and Allied Sciences"),
  lat = c(5.6500, 6.6743, 5.1100, 9.3725, 6.6226),
  lon = c(-0.1860, -1.5714, -1.2983, -0.8852, 0.4720)
)

# Example data for rivers and lakes
rivers_and_lakes <- data.frame(
  name = c("Volta River", "Lake Bosumtwi", "Pra River", "River Densu", "Kulpawn River"),
  lat = c(6.2000, 6.5167, 5.1333, 5.5367, 10.1433),
  lon = c(0.0500, -1.4167, -1.1167, -0.3224, -1.4930)
)

# Example data for major roads and highways
major_roads <- data.frame(
  name = c("N1 Highway", "N6 Highway", "N2 Highway"),
  lat = c(5.5739, 6.6800, 8.6325),
  lon = c(-0.2218, -1.6244, 0.2655)
)



road_path_N1 <- list(
  c(5.289883, -2.784856),  
  c(4.973598612046922, -2.420877866429471),
  c(4.890494012430252, -1.9686891407261917),
  c(5.710117996315781, -0.004368087498940465),
  c(5.896206982512208, 0.5051755369318016),
  c(6.10087497729285, 1.077120823624885)
)

road_path_N6 <- list(
  c(6.534063372405459, -0.7792201474423552),
  c(6.687703141097121, -1.6184429908188787),
  c(6.688633680484984, -1.6175759880836793),
  c(6.529816995033872, -0.7349264045240242),
  c(6.523583040530491, -0.7133835425907515),
  c(6.496516389925158, -0.6912655054726249),
  c(6.458901200088311, -0.6257480119125383),
  c(6.447054952740229, -0.6067149974458028),
  c(6.43948349859193, -0.587341509590192),
  c(6.374105669291509, -0.5412294751888108),
  c(5.82368518405226, -0.3733753556242437)
)

road_path_N2 <- list(
  c(11.066000252090594, -0.1981948628798286),
  c(8.512389146690387, 0.2860615624697945),
  c(6.1549483178506375, 0.060448708050113965),
  c(5.686226052186951, -0.014531576261828367)
)



# Convert data frames to spatial objects
educational_institutions_sf <- st_as_sf(educational_institutions, coords = c("lon", "lat"), crs = st_crs(ghana_shapefile))
hospitals_sf <- st_as_sf(regional_hospitals, coords = c("lon", "lat"), crs = st_crs(ghana_shapefile))
tourist_attractions_sf <- st_as_sf(tourist_attractions, coords = c("lon", "lat"), crs = st_crs(ghana_shapefile))
waterfalls_sf <- st_as_sf(waterfalls, coords = c("lon", "lat"), crs = st_crs(ghana_shapefile))
capitals_sf <- st_as_sf(capitals, coords = c("lon", "lat"), crs = st_crs(ghana_shapefile))
major_towns_sf <- st_as_sf(major_towns, coords = c("lon", "lat"), crs = st_crs(ghana_shapefile))
infrastructure_projects_sf <- st_as_sf(infrastructure_projects, coords = c("lon", "lat"), crs = st_crs(ghana_shapefile))
cultural_sites_sf <- st_as_sf(cultural_sites, coords = c("lon", "lat"), crs = st_crs(ghana_shapefile))
rivers_and_lakes_sf <- st_as_sf(rivers_and_lakes, coords = c("lon", "lat"), crs = st_crs(ghana_shapefile))
major_roads_sf <- st_as_sf(major_roads, coords = c("lon", "lat"), crs = st_crs(ghana_shapefile))

pal <- colorFactor(topo.colors(15), domain = ghana_shapefile$NAME_1)


Political_parties <- readxl::read_excel("ghanaMpsxlsx.xlsx", sheet = 'Pparties')
Political_parties_sf <- st_as_sf(Political_parties, coords = c("lon", "lat"), crs = st_crs(ghana_shapefile))


mps_2020 <- readxl::read_xlsx("ghanaMpsxlsx_2020.xlsx") %>%
                    mutate(Year='2020')

mps_2012 <- readxl::read_xlsx("ghanaMpsxlsx_2012.xlsx") %>%
                   mutate(Year='2012')

mps_2016 <- readxl::read_xlsx("ghanaMpsxlsx_2016.xlsx") %>%
                    mutate(Year='2016')

mps_2012 <- readxl::read_xlsx("ghanaMpsxlsx_2012.xlsx") %>%
                    mutate(Year='2012')

mps_2008 <- readxl::read_xlsx("ghanaMpsxlsx_2008.xlsx") %>%
                    mutate(Year='2008')

mps_2004 <- readxl::read_xlsx("ghanaMpsxlsx_2004.xlsx") %>%
                    mutate(Year='2004')

mps_2000 <- readxl::read_xlsx("ghanaMpsxlsx_2000.xlsx") %>%
                    mutate(Year='2000')

mps_1996 <- readxl::read_xlsx("ghanaMpsxlsx_1996.xlsx") %>%
                    mutate(Year='1996')

mps_1992 <- readxl::read_xlsx("ghanaMpsxlsx_1992.xlsx") %>%
                    mutate(Year='1992')




MPs_GH <- union(mps_2020, mps_2016) %>%    
          union(mps_2012) %>%
          union(mps_2008) %>%
          union(mps_2004) %>%
          union(mps_2000) %>%
          union(mps_1996) %>%
          union(mps_1992)



######
##Functions
'%notin%' <- Negate('%in%')

#parlDiag function
parlDiag <- function(Parties, shares, cols = NULL, repr = c("absolute", "proportion")) {
  repr <- match.arg(repr)
  stopifnot(length(Parties) == length(shares))
  if (repr == "proportion") {
    stopifnot(sum(shares) == 1)
  }
  if (!is.null(cols)) {
    names(cols) <- Parties
  }
  
  # arc start/end in radians
  cc <- cumsum(c(-pi/2, switch(repr, "absolute" = (shares / sum(shares)) * pi, "proportion" = shares * pi)))
  cc[length(cc)] <- pi/2
  
  # get angle of arc midpoints
  meanAngles <- colMeans(rbind(cc[2:length(cc)], cc[1:length(cc)-1]))
  
  # unit circle
  labelX <- sin(meanAngles)
  labelY <- cos(meanAngles)
  
  # prevent bounding box < y=0
  labelY <- ifelse(labelY < 0.015, 0.015, labelY)
  
  # Create the ggplot
  p <- ggplot() + 
    coord_fixed() +
    expand_limits(x = c(-1.3, 1.3), y = c(0, 1.3)) + 
    theme_void() +
    theme(legend.position = "none") +
    
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.5, r = 1,
                     start = cc[1:length(shares)], 
                     end = c(cc[2:length(shares)], pi/2), fill = Parties), 
                 color = "black") +
    
    scale_fill_manual(values = cols) + 
    
    geom_segment(aes(x = 0.9 * labelX, y = 0.9 * labelY, 
                     xend = 1.15 * labelX, yend = 1.15 * labelY),
                 color = "black", size = 1) +
    
    geom_label(aes(x = 1.15 * labelX, y = 1.15 * labelY, 
                   label = switch(repr,
                                  "absolute" = sprintf("%s\n%i", Parties, shares),
                                  "proportion" = sprintf("%s\n%i%%", Parties, round(shares*100)))), 
               fontface = "bold", label.padding = unit(1, "points")) +
    
    geom_point(aes(x = 0.9 * labelX, y = 0.9 * labelY), color = "black", size = 2) +
    
    geom_text(aes(x = 0, y = 0, label = switch(repr, 
                                               "absolute" = sprintf("Total: %i MPs", sum(shares)), 
                                               "proportion" = "")),
              fontface = "bold", size = 7) 
  
  return(p)
}









