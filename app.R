library(tidyr)
library(dplyr)
library(scales)
library(shiny)
library(leaflet)
library(ggvis)

# Constant values
weekdaysVector <- c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag")

mensas <- read.csv("data/mensas.csv", header = TRUE, stringsAsFactors = FALSE)
dishes <- read.csv("data/dishes.csv", header = TRUE, stringsAsFactors = FALSE)

# Make dates from date column
dishes$date <- as.Date(dishes$date, "%Y-%m-%d")

# Unnest labels
dishesPerLabel <- dishes %>%
  mutate(label = strsplit(labels, "\\|")) %>%
  unnest(label)

# Labels for the input field
labels <- sort(unique(dishesPerLabel$label))

# Mensas for Input field
mensas_list <- split(mensas$id,mensas$name)
ui <- fluidPage(title = "Mensa Hamburg Statistiken",
                headerPanel("Mensa Hamburg Statistiken (Stand: 24.12.2017)"),
                  tabsetPanel(type="tabs",
                   tabPanel("Graphs",
                      sidebarPanel(
                        selectInput(inputId = "mensaId", label = "Mensa", choices = mensas_list),
                        selectInput(inputId = "label", label = "Label", choices = labels)
                      ),
                      mainPanel(
                        h2("Anzahl der Gerichte pro Wochentag"),
                        ggvisOutput("boxplot"),
                        h2("Durschnittliche Anzahl an Gerichten pro Wochentag"),
                        ggvisOutput("average"),
                        h2("Median an Gerichten pro Wochentag"),
                        ggvisOutput("median"),
                        h2("Prozentualer Anteil der Tage mit Gerichten des Labels"),
                        ggvisOutput("percentage")
                      )          
                    ),
                    tabPanel("Map",
                      leafletOutput("mensaMap")
                    )
                )  
            )
  
server <- function(input, output) {
  numberOfDishesForMensaAndLabel <- reactive({dishesPerLabel %>%
    filter(mensaId == input$mensaId) %>%
    filter(label == input$label) %>%
    count(date)})
  allDatesOpen <- reactive({dishes %>% 
    filter(mensaId == input$mensaId) %>% 
    distinct(date) %>%
    mutate(weekday = weekdays(date))})
  allDatesOpenMerged <- reactive({
    result <- merge(allDatesOpen(),numberOfDishesForMensaAndLabel(), by.x = "date", by.y = "date", all = T)
    result[is.na(result)] <- 0
    result$weekday <- weekdays(result$date)
    result
  })
  mensaBoxPlot <- reactive({
    allDatesOpenMerged() %>%
      mutate(weekdayAsFactor = factor(weekday, levels = weekdaysVector)) %>%
      ggvis(x = ~weekdayAsFactor, y = ~n, fill := "steelblue") %>%
      layer_boxplots() %>%
      add_axis("x", title = "Wochentag") %>%
      add_axis("y", title = "Anzahl")
  })
  mensaBoxPlot %>% bind_shiny("boxplot")
  averagePlot <- reactive({
    meanDishes <- aggregate(n ~ weekday, allDatesOpenMerged(), mean)
    meanDishes$weekdayAsFactor <- factor(meanDishes$weekday, levels = weekdaysVector)
    meanDishes %>%
      ggvis(x = ~weekdayAsFactor, y = ~n, fill := "steelblue") %>%
      layer_bars() %>%
      add_axis("x", title = "Wochentag") %>%
      add_axis("y", title = "Durchschnittliche Anzahl")
  })
  averagePlot %>% bind_shiny("average")
  medianPlot <- reactive({
    medianDishes <- aggregate(n ~ weekday, allDatesOpenMerged(), median)
    medianDishes$weekdayAsFactor <- factor(medianDishes$weekday, levels = weekdaysVector)
    medianDishes %>%
      ggvis(x = ~weekdayAsFactor, y = ~n, fill := "steelblue") %>%
      layer_bars() %>%
      add_axis("x", title = "Wochentag") %>%
      add_axis("y", title = "Median")
  })
  medianPlot %>% bind_shiny("median")
  percentagePlot <- reactive({
    daysOpenPerWeekday <- allDatesOpen() %>% group_by(weekday) %>% summarise(count = n())
    noLabelDishesPerWeekday <- allDatesOpenMerged() %>% group_by(weekday) %>% filter(n == 0) %>% summarize(countNoLabel = n())
    likelihoodLabel <- merge(daysOpenPerWeekday, noLabelDishesPerWeekday, by.x = "weekday", by.y = "weekday", all = T) %>%
      mutate(likelihood = ifelse(is.na(countNoLabel), 1, 1 - (countNoLabel / count)))
    likelihoodLabel$weekdayAsFactor <- factor(likelihoodLabel$weekday, levels = weekdaysVector)
    likelihoodLabel %>%
      ggvis(x = ~weekdayAsFactor, y = ~likelihood, fill := "steelblue") %>%
      layer_bars() %>%
      add_axis("y", title = "Anteil an Tagen mit Label", format = "%") %>%
      add_axis("x", title = "Wochentag")
  })
  percentagePlot %>% bind_shiny("percentage")
  output$mensaMap <- renderLeaflet({
    leaflet(data = mensas) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(lat = ~latitude, lng = ~longitude, label = ~name, popup = ~paste(strong(name), br(), address, br(), zipcode, city))
  })
}
options(shiny.port = 8080)
shinyApp(ui = ui, server = server)