# Infant mortality
# Live births
# Enrollment in public funded childcare
# Child abuse

library(shiny)
library(tidyverse)
library(rvest)
library(geojsonio)
library(leaflet)

#Reading in the data sets
medianIncomes <- read_csv("MedianIncomes.csv")
nhoods <- geojson_read("CommunityDistricts.geojson",
                       what = "sp")

livebirths <- read_csv("LiveBirths.csv")
infantmortality <- read_csv("InfantMortality.csv")
childabuse <- read_csv("ChildAbuseAndNeglectInvestigations.csv")
childcare <- read_csv("EnrollmentInPubliclyFundedCareForChildrenUnder5.csv")

#Preparing Live Births
lb.2$X5 <- as.numeric(lb.2$X5)
lb.3<- lb.2 [ ,-1]
lb.2 <- livebirths%>%
  filter(`Live Births`== 2017,
         X3 == "Rate")

#Joining live births and median income
mi.filter <- medianIncomes%>%
  filter(TimeFrame== 2019)

join.1 <- mi.filter%>%
  left_join(lb.3,
            by= c("Fips" = "X5"))
join1.25 <- join.1 [ ,-4]
join1.3 <- join1.25[ ,-7]
    
join1.5 <- join1.3%>%
  rename("LBRate" = "X4",
         "Income" = "Data")

#Joining livebirths/median income and infant mortality

im.2 <- infantmortality[-1, -1]
im.3 <- im.2 %>%
  filter(`Infant Mortality` == 2017)%>%
  rename("Fips"= "X4",
         "DeathRate"= "X3",
         "Year"= `Infant Mortality`)
im.3$Fips<- as.numeric(im.3$Fips)

join2 <- join1.5%>%
  left_join(im.3,
            by="Fips")

#Joining mi/lb/im and child care
childcare2 <- childcare[-1,-1]
childcare3 <- childcare2%>%
  filter(`Enrollment in Publicly Funded Care for Children Under 5`== 2019)%>%
  rename("Fips" = "X4",
         "Enrollment" = "X3",
         "Year(CC)" = `Enrollment in Publicly Funded Care for Children Under 5`)
childcare3$Fips <- as.numeric(childcare3$Fips)


join3 <- join2%>%
  left_join(childcare3,
            by= "Fips")

#Joining mi/lb/im/cc and child abuse
childabuse2 <- childabuse[-1,-1]
childabuse3 <- childabuse2%>%
  filter(`Child Abuse and Neglect Investigations`== 2019)%>%
  rename("Year(CA)" = `Child Abuse and Neglect Investigations`,
         "Fips" = "X4",
         "Investigations" = "X3")
childabuse3$Fips <- as.numeric(childabuse3$Fips)


join4 <- join3%>%
  left_join(childabuse3,
            by= "Fips")

#------------------------------------------------#

#getting median incomes ready
nhoods.copy <- nhoods
medianIncomesFiltered <- medianIncomes %>%
  filter(`Household Type` == "All Households" &
           TimeFrame == 2017) %>%
  select(Location, Data, Fips)

nhoods.copy@data$boro_cd <- as.numeric(nhoods.copy@data$boro_cd)

#Joining neighborhoods and income; boro_cd = fips = neighborhood codes
nhoods.copy@data <- left_join(nhoods.copy@data,
                              medianIncomesFiltered,
                              by = c("boro_cd" = "Fips"))
nhoods.copy@data <- nhoods.copy@data %>%
  rename("Income" = "Data")

#leaflet
map <- nhoods.copy %>%
  leaflet()

#bins = income brackets
bins <- c(0,50000, 75000, 100000,150000, 200000, Inf)
colors <- colorBin(palette = "YlOrRd",
                   domain = nhoods.copy@data$Income,
                   bins = bins)

ui <- fluidPage(
  leafletOutput(outputId = "nyc_cd")
  
)

server <- function(input, output, session) {
  output$nyc_cd <- renderLeaflet({
    map %>% 
      addPolygons(fillColor = ~colors(Income),
                  color = "black",
                  opacity = 1,
                  fillOpacity = .7,
                  layerId = ~Location) %>%
      setView(-74, 40.7, 10) %>% 
      addLegend(pal = colors,
                values = nhoods.copy@data$Income)
  })
  
}

shinyApp(ui, server)