library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(shiny)

# Constant values
weekdaysVector <- c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

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
                  headerPanel("Mensa Hamburg Statistiken (Stand: 11.12.2017)"),
                  sidebarPanel(
                    selectInput(inputId = "mensaId", label = "Mensa", choices = mensas_list),
                    selectInput(inputId = "label", label = "Label", choices = labels)
                  ),
                  mainPanel(
                    plotOutput("average"),
                    plotOutput("percentage")
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
  output$average <- renderPlot({
    meanDishes <- aggregate(n ~ weekday, allDatesOpenMerged(), mean)
    meanDishes$weekdayAsFactor <- factor(meanDishes$weekday, levels = weekdaysVector)
    ggplot(meanDishes, aes(x = weekdayAsFactor)) + 
      geom_bar(aes(weight = n)) + 
      ggtitle("Durschnittliche Anzahl an Gerichten pro Wochentag") + 
      ylab("Durchschnittliche Anzahl") +
      xlab("Wochentag")
  })
  output$percentage <- renderPlot({
    daysOpenPerWeekday <- allDatesOpen() %>% group_by(weekday) %>% summarise(count = n())
    noLabelDishesPerWeekday <- allDatesOpenMerged() %>% group_by(weekday) %>% filter(n == 0) %>% summarize(countNoLabel = n())
    likelihoodLabel <- merge(daysOpenPerWeekday, noLabelDishesPerWeekday, by.x = "weekday", by.y = "weekday", all = T) %>%
      mutate(likelihood = ifelse(is.na(countNoLabel), 1, 1 - (countNoLabel / count)))
    likelihoodLabel$weekdayAsFactor <- factor(likelihoodLabel$weekday, levels = weekdaysVector)
    ggplot(likelihoodLabel, aes(x = weekdayAsFactor)) + 
      geom_bar(aes(weight = likelihood)) +
      ggtitle("Prozentualer Anteil der Tage mit Gerichten des Labels") + 
      scale_y_continuous(labels=percent) +
      ylab("Anteil an Tagen mit Label") +
      xlab("Wochentag")
  })
}
options(shiny.port = 8080)
shinyApp(ui = ui, server = server)