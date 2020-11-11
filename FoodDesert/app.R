# Infant mortality
# Live births
# Enrollment in public funded childcare
# Child abuse

library(shiny)
library(tidyverse)
library(rvest)
library(geojsonio)
library(leaflet)
library(ggrepel)

#Reading in the data sets
medianIncomes <- read_csv("MedianIncomes.csv")
nhoods <- geojson_read("CommunityDistricts.geojson",
                       what = "sp")

livebirths <- read_csv("LiveBirths.csv")
infantmortality <- read_csv("InfantMortality.csv")
childabuse <- read_csv("ChildAbuseAndNeglectInvestigations.csv")
childcare <- read_csv("EnrollmentInPubliclyFundedCareForChildrenUnder5.csv")

#Preparing Live Births
livebirths.filter <- livebirths %>%
  filter(`Live Births`== 2017,
         X3 == "Rate")
livebirths.filter$X5 <- as.numeric(livebirths.filter$X5)
livebirths.final<- livebirths.filter [ ,-1]

#Joining live births and median income
medianincome.filter <- medianIncomes %>%
  filter(TimeFrame== 2019)

join.incomebirths <- medianincome.filter %>%
  left_join(livebirths.final,
            by= c("Fips" = "X5"))
join.incomebirths.2 <- join.incomebirths [ ,-4]
join.incomebirths.3 <- join.incomebirths.2 [ ,-7]

join.incomebirths.final <- join.incomebirths.3 %>%
  rename("LBRate" = "X4",
         "Income" = "Data")

#Joining livebirths/median income and infant mortality
infantmortality.2 <- infantmortality[-1, -1]
infantmortality.filter <- infantmortality.2 %>%
  filter(`Infant Mortality` == 2017)%>%
  rename("Fips"= "X4",
         "DeathRate"= "X3",
         "Year"= `Infant Mortality`)
infantmortality.filter$Fips<- as.numeric(infantmortality.filter$Fips)

join.incomebirthsmortality <- join.incomebirths.final%>%
  left_join(infantmortality.filter,
            by="Fips")

#Joining mi/lb/im and child care
childcare.2 <- childcare[-1,-1]
childcare.filter <- childcare.2 %>%
  filter(`Enrollment in Publicly Funded Care for Children Under 5`== 2019) %>%
  rename("Fips" = "X4",
         "Enrollment" = "X3",
         "Year(CC)" = `Enrollment in Publicly Funded Care for Children Under 5`)
childcare.filter$Fips <- as.numeric(childcare.filter$Fips)

join.incomebirthsmortalitycare <- join.incomebirthsmortality %>%
  left_join(childcare.filter,
            by= "Fips")

#Joining mi/lb/im/cc and child abuse
childabuse.2 <- childabuse[-1,-1]
childabuse.filter <- childabuse.2 %>%
  filter(`Child Abuse and Neglect Investigations`== 2019)%>%
  rename("Year(CA)" = `Child Abuse and Neglect Investigations`,
         "Fips" = "X4",
         "Investigations" = "X3")
childabuse.filter$Fips <- as.numeric(childabuse.filter$Fips)

join.incomebirthsmortalitycareabuse <- join.incomebirthsmortalitycare %>%
  left_join(childabuse.filter,
            by= "Fips")

#Removing duplicate year columns
join.incomebirthsmortalitycareabuse.2 <- join.incomebirthsmortalitycareabuse [ ,-c(3,6,8,10,12)]
join.rename <- join.incomebirthsmortalitycareabuse.2 %>%
  filter(`Household Type` == "All Households") %>%
  rename("Neighborhood" = "Location",
         "MedianIncome" = "Income",
         "District" = "Fips",
         "LiveBirthRate" = "LBRate",
         "InfantMortalityRate" = "DeathRate",
         "EnrollmentInPublicFundedChildcare" = "Enrollment",
         "ChildAbuseAndNeglectInvestigations" = "Investigations")
join.final <- join.rename [ , -2]

#remove data that is not assigned to a community district 
join.final <- join.final %>% 
  filter(District < 1000)

#Making columns as numeric
join.final$MedianIncome <- as.numeric(join.final$MedianIncome)
join.final$LiveBirthRate <- as.numeric(join.final$LiveBirthRate)
join.final$InfantMortalityRate <- as.numeric(join.final$InfantMortalityRate)
join.final$EnrollmentInPublicFundedChildcare <- as.numeric(join.final$EnrollmentInPublicFundedChildcare)
join.final$ChildAbuseAndNeglectInvestigations <- as.numeric(join.final$ChildAbuseAndNeglectInvestigations)

#getting median incomes ready
nhoods.copy <- nhoods
medianIncomesFiltered <- medianIncomes %>%
  filter(`Household Type` == "All Households" &
           TimeFrame == 2017) %>%
  select(Location, Data, Fips)
#Joining neighborhoods and median income to make leaflet
nhoods.copy@data$boro_cd <- as.numeric(nhoods.copy@data$boro_cd)
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
  sidebarLayout(
    sidebarPanel(
      leafletOutput(outputId = "nyc_cd"),
      selectizeInput(inputId = "var2",
                     label = "Choose a y-variable and then click a community distrct on the map.",
                     choices = c("Live Birth Rate" = "LiveBirthRate",
                                 "Infant Mortality Rate" = "InfantMortalityRate",
                                 "Enrollment in Public Funded Childcare" = "EnrollmentInPublicFundedChildcare", 
                                 "Child Abuse and Neglect Investigations" = "ChildAbuseAndNeglectInvestigations"),
                     selected= "LiveBirthRate"),
    ),
    mainPanel(
      plotOutput(outputId = "plot1"),
      textOutput(outputId="textout")
    )
  )
  
 
)

server <- function(input, output, session) {
  
  click.map<- reactiveValues(clickedMarker= NULL)
  
  output$nyc_cd <- renderLeaflet({
    map %>% 
      addPolygons(fillColor = ~colors(Income),
                  color = "black",
                  opacity = 1,
                  fillOpacity = .7,
                  layerId = ~Location,
                  popup = ~Location) %>%
      setView(-73.9, 40.7, 9.9) %>% 
      addLegend(pal = colors,
                values = nhoods.copy@data$Income)
    
  })  
  
  output$plot1 <- renderPlot({
    
    cdChoice <- "Riverdale"
    ifelse(input$nyc_cd_shape_click$id != "", cdChoice <-input$nyc_cd_shape_click$id)
    
    joindistrictselected <- join.final %>%
      filter(Neighborhood == cdChoice)
    
    join.final %>%
      ggplot(aes_string(x = "MedianIncome",
                        y = input$var2)) +
      geom_point(size = 3) +
      geom_point(data = joindistrictselected,
                 size = 4,
                 color = "red") +
      geom_label_repel(data = joindistrictselected,
                       mapping = aes(label = Neighborhood),
                       show.legend = FALSE,
                       arrow = arrow(length = unit(0.03, "npc"), type = "closed", ends = "last"),
                       color = "black",
                       nudge_y = 10)
    
  })
  
  liveBirthText <- "Number of births in a given neighborhood out of total NYC population"
  infantMortalityText <- "Deaths of infants under 1 year per 1,000 births"
  enrollmentText <-"Number of children enrolled in publicly funded childcare"
  childAbuseText <- "Number of child abuse and neglect investigations"
  
  info <- reactiveValues()
  observe({
    info$x <- input$var2
    info$y <- ifelse(info$x == "LiveBirthRate", liveBirthText, 
                     ifelse(info$x == "InfantMortalityRate",infantMortalityText,
                            ifelse(info$x == "EnrollmentInPublicFundedChildcare", enrollmentText,
                                   childAbuseText)))
  })
  
  output$textout <- renderText({info$y})
  
  
}

shinyApp(ui, server)