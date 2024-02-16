#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(httr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(shinyBS)

###############################################  prepare data
# currency
# get the currency name list
html <-
    read_html(
        "https://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/index.en.html"
    )
chunk <- html %>% html_nodes("tbody")
symbol <- chunk %>% html_nodes("[class='currency']") %>% html_text()
currency <-
    chunk %>% html_nodes("[class='alignLeft']") %>% html_text()
currencyDF <- tibble(symbol, currency)
symb_name <- paste(symbol, currency, sep = " - ")
currencyDF <-  currencyDF %>% add_column(symb_name)

############################################################ weather
# get bay area city names, center: SF
bayArea <-
    GET("https://www.metaweather.com/api/location/search/?lattlong=37.77,-122.41")
stop_for_status(bayArea)
json <- content(bayArea, as = "text", encoding = "UTF-8")
city <- fromJSON(json, flatten = TRUE) %>% select(title, woeid)

name <-
    c(
        "Snow",
        "Sleet",
        "Hail",
        "Thunderstorm",
        "Heavy Rain",
        "Light Rain",
        "Showers",
        "Heavy Cloud",
        "Light Cloud",
        "Clear"
    )
abbreviation <-
    c("sn", "sl", "h", "t", "hr", "lr", "s", "hc", "lc", "c")
link <- character(0)
for (i in abbreviation) {
    link <-
        c(
            link,
            paste(
                "https://www.metaweather.com/static/img/weather/",
                i,
                ".svg",
                sep = ""
            )
        )
}
weather_state <- tibble(name, abbreviation, link)

##################################################  BART station data

html <-
    read_html("http://api.bart.gov/docs/overview/abbrev.aspx")
chunk <- html %>% html_nodes("tbody")

stationAbbr <- chunk %>% html_nodes("tr") %>% html_text() %>%
    str_extract("^[0-9A-Za-z]+\r") %>% str_replace("\r", "")
stationName <- chunk %>% html_nodes("tr") %>% html_text() %>%
    str_extract("\n.*") %>% str_replace("\n", "")
stations <- tibble(abbr = stationAbbr, station = stationName)

# webscrapping missing these two stations name
b <-
    tibble(abbr = c("bery", "mlpt"),
           station = c("Berryessa", "Milpitas"))
stations <- stations %>% full_join(b) %>% arrange(station)

sch <-
    "Due to COVID-19 BART is temporarily running the following service hours:\n\nWeekdays (5:00 am - 9:00 pm)\nSaturday (8:00 am - 9:00 pm)\nSunday (8:00 am - 9:00 pm)"


################################################### shiny APP start

# Define UI for application that draws a histogram
ui <- navbarPage(
    "My Application",
    tabPanel("BART",
             fluidPage(
                 titlePanel("BART Arrival Estimate"),
                 sidebarLayout(
                     sidebarPanel(
                         h4("Next Train Arrival Time"),
                         selectInput(
                             inputId = "station",
                             label = "Station",
                             stations$station,
                             selected = stations$station[1]
                         ),
                         actionButton("nextBart", "Next BART", style = "background-color: #428bca; color: #fff;"),
                         hr(),
                         h4("Find the Trip Time"),
                         selectInput(
                             inputId = "origin",
                             label = "From",
                             stations$station,
                             selected = stations$station[1]
                         ),
                         selectInput(
                             inputId = "destination",
                             label = "To",
                             stations$station,
                             selected = stations$station[2]
                         ),
                         checkboxInput("detailRoute", label = "Route Info", value = FALSE),
                         actionButton("trip", "Trip Time", style = "background-color: #428bca; color: #fff;"),
                         actionButton("clear", "Clear"),
                         hr(),
                         h4("See the BART Map"),
                         actionButton("weekday", "Mon-Sat", style = "background-color: #428bca; color: #fff;"),
                         actionButton("weekend", "Sunday", style = "background-color: #428bca; color: #fff;"),
                         width = 3
                     ),
                     mainPanel(
                         verbatimTextOutput("OutOfService"),
                         textOutput("estimateNote"),
                         tableOutput("estimate"),
                         br(),
                         textOutput("tripNote"),
                         tableOutput("tripTime"),
                         br(),
                         textOutput("route_trainNote"),
                         tableOutput("route_train"),
                         br(),
                         textOutput("fareNote"),
                         tableOutput("fare"),
                         bsModal("bartMap1", "BART MAP", "weekday", size = "large", uiOutput("map1")),
                         bsModal("bartMap2", "BART MAP", "weekend", size = "large", uiOutput("map2"))
                     )
                 )
             )),
    tabPanel("Currency Exchange",
             fluidPage(
                 titlePanel("Check the Exchange Rate of Currency"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             inputId = "baseCurrency",
                             label = "Base Currency",
                             currencyDF$symb_name,
                             selected = currencyDF$symb_name[1]
                         ),
                         selectInput(
                             inputId = "exchange",
                             label = "Exchange to",
                             currencyDF$symb_name,
                             selected = currencyDF$symb_name[2]
                         ),
                         checkboxInput("today", label = "Today's Rate", value = TRUE),
                         hr(),
                         dateRangeInput(
                             'dateRange',
                             label = "Or select a time period:",
                             start = Sys.Date() - 10,
                             end = Sys.Date(),
                             max = Sys.Date(),
                             min = "2000-01-01"
                         ),
                         actionButton("check", "Check", style = "background-color: #428bca; color: #fff;"),
                         hr(),
                         numericInput("exchangeI", "Amount of Base Currency", value = 1),
                         actionButton("exchangeB", "Exchange", style = "background-color: #428bca; color: #fff;"),
                         actionButton("cash", "Cash"),
                         width = 3
                     ),
                     
                     mainPanel(
                         tags$style("#todayRate {font-size:30px;color:red;display:block; }"),
                         textOutput("todayRate"),
                         textOutput("detailRate"),
                         hr(),
                         plotOutput("ratePlot"),
                         br(),
                         tags$style("#exchangeO {font-size:x-large;color:green;display:block; }"),
                         tags$style(
                             "#exchangeO2 {font-size:x-large;color:green;display:block; }"
                         ),
                         textOutput("exchangeO"),
                         textOutput("exchangeO2")
                     )
                 )
             )),
    tabPanel("Weather",
             fluidPage(
                 titlePanel("Weather"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             inputId = "city",
                             label = "City",
                             city$title,
                             selected = city$title[1]
                         ),
                         hr(),
                         actionButton(
                             inputId = "weatherButton",
                             label = "Show Me Weather",
                             style = "background-color: #428bca; color: #fff;"
                         ),
                         width = 3
                     ),
                     mainPanel(
                         fluidRow(
                             column(2, h5("Date", style = "font-weight: bold")),
                             column(2, h5(" ")),
                             column(2, h5("Weather", style = "font-weight: bold")),
                             column(2, h5("Temperature", style = "font-weight: bold")),
                             column(2, h5("Wind Speed", style = "font-weight: bold")),
                             column(2, h5("Predictability", style = "font-weight: bold"))
                         ),
                         hr(),
                         fluidRow(
                             column(2, textOutput("date1")),
                             column(2, uiOutput("icon1")),
                             column(2, textOutput("weather1")),
                             column(2, uiOutput("temp1")),
                             column(2, textOutput("wind1")),
                             column(2, textOutput("predict1"))
                         ),
                         br(),
                         fluidRow(
                             column(2, textOutput("date2")),
                             column(2, uiOutput("icon2")),
                             column(2, textOutput("weather2")),
                             column(2, uiOutput("temp2")),
                             column(2, textOutput("wind2")),
                             column(2, textOutput("predict2"))
                         ),
                         br(),
                         fluidRow(
                             column(2, textOutput("date3")),
                             column(2, uiOutput("icon3")),
                             column(2, textOutput("weather3")),
                             column(2, uiOutput("temp3")),
                             column(2, textOutput("wind3")),
                             column(2, textOutput("predict3"))
                         ),
                         br(),
                         fluidRow(
                             column(2, textOutput("date4")),
                             column(2, uiOutput("icon4")),
                             column(2, textOutput("weather4")),
                             column(2, uiOutput("temp4")),
                             column(2, textOutput("wind4")),
                             column(2, textOutput("predict4"))
                         ),
                         br(),
                         fluidRow(
                             column(2, textOutput("date5")),
                             column(2, uiOutput("icon5")),
                             column(2, textOutput("weather5")),
                             column(2, uiOutput("temp5")),
                             column(2, textOutput("wind5")),
                             column(2, textOutput("predict5"))
                         ),
                         br(),
                         fluidRow(
                             column(2, textOutput("date6")),
                             column(2, uiOutput("icon6")),
                             column(2, textOutput("weather6")),
                             column(2, uiOutput("temp6")),
                             column(2, textOutput("wind6")),
                             column(2, textOutput("predict6"))
                         )
                     )
                 )
             ))
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    ############################################ BART part
    
    # real-time estimate button
    observeEvent(input$nextBart, {
        output$weekdayMap <- renderUI({
            tags$img(src = "https://www.bart.gov/sites/default/files/images/basic_page/system-map-weekday.png")
        })
        
        output$OutOfService <- NULL
        
        station <- input$station
        abbrS <- stations$abbr[which(stations$station == station)]
        endpoint <- str_glue(
            "http://api.bart.gov/api/etd.aspx?cmd=etd&orig={id}&key=MW9S-E7SL-26DU-VV8V&json=y",
            id = abbrS
        )
        
        bart <- GET(endpoint)
        stop_for_status(bart)
        json <- content(bart, as = "text", encoding = "UTF-8")
        
        warningMe <- fromJSON(json, flatten = TRUE)$root$message
        errorMe <-
            fromJSON(json, flatten = TRUE)$root$station$message.error
        
        if (!is.null(errorMe)) {
            errorMessage <- paste(errorMe, sch, sep = "\n\n")
            output$OutOfService <- renderText(errorMessage)
            
        } else if (warningMe != "") {
            warningMessage <- paste(warningMe, sch, sep = "\n\n")
            output$OutOfService <- renderText(warningMessage)
            
        } else{
            station <- fromJSON(json, flatten = TRUE)$root$station$name
            
            dest <-
                fromJSON(json, flatten = TRUE)$root$station$etd[[1]]$destination
            
            # loop here
            esti <-
                fromJSON(json, flatten = TRUE)$root$station$etd[[1]]$estimate[[1]]
            estiDF <- esti %>% add_column(destination = dest[1])
            estiDF <-
                estiDF %>% select(destination,
                                  minutes,
                                  platform,
                                  direction,
                                  length,
                                  color,
                                  delay)
            
            if (length(dest) > 1) {
                for (i in 2:length(dest)) {
                    lesti <-
                        fromJSON(json, flatten = TRUE)$root$station$etd[[1]]$estimate[[i]]
                    lestiDF <-
                        lesti %>% add_column(destination = dest[i])
                    lestiDF <-
                        lestiDF %>% select(destination,
                                           minutes,
                                           platform,
                                           direction,
                                           length,
                                           color,
                                           delay)
                    estiDF <- bind_rows(estiDF, lestiDF)
                }
            }
            
            estiDF <-
                estiDF %>% rename(
                    Destination = destination,
                    Minutes = minutes,
                    Platform = platform,
                    Direction = direction,
                    Length = length,
                    Line_Color = color,
                    Delay = delay
                )
            
            estimateNote <-
                paste("* next trains arrive in station: ", station)
            output$estimateNote <- renderText(estimateNote)
            
            output$estimate <- renderTable(estiDF)
        }
    })
    
    # trip button
    observeEvent(input$trip, {
        # origin and destination cannot be same
        req(input$origin != input$destination)
        
        if (!input$detailRoute) {
            output$route_trainNote <- NULL
            output$route_train <- NULL
        }
        
        orig <- input$origin
        dest <- input$destination
        
        origAbbr <- stations$abbr[which(stations$station == orig)]
        destAbbr <- stations$abbr[which(stations$station == dest)]
        
        endpoint <- str_glue(
            "http://api.bart.gov/api/sched.aspx?cmd=depart&orig={id1}&dest={id2}&date=now&key=MW9S-E7SL-26DU-VV8V&b=2&a=2&l=1&json=y",
            id1 = origAbbr,
            id2 = destAbbr
        )
        
        bart <- GET(endpoint)
        stop_for_status(bart)
        json <- content(bart, as = "text", encoding = "UTF-8")
        
        tripDF <-
            fromJSON(json, flatten = TRUE)$root$schedule$request$trip %>%
            select(
                "@origin",
                "@destination",
                "@origTimeMin",
                "@destTimeMin",
                "@tripTime",
                "fares.fare",
                "@destTimeDate"
            )
        
        tripDF <-
            tripDF %>% rename(
                Origin = "@origin",
                Destination = "@destination",
                Departure = "@origTimeMin",
                Arrival = "@destTimeMin",
                Time = "@tripTime",
                Fare = "fares.fare",
                Date = "@destTimeDate"
            )
        
        tripDF <-
            tripDF %>% mutate(
                Origin = replace(Origin, TRUE, orig),
                Destination = replace(Destination, TRUE, dest),
                Time = paste(Time, "Min.")
            )
        
        # just for detail route/path, optional
        if (input$detailRoute) {
            trip <- fromJSON(json, flatten = TRUE)$root$schedule$request$trip
            
            route <- character(0)
            
            for (i in 1:dim(trip)[1]) {
                a <- trip$leg[[i]] %>% select("@origin") %>% pull()
                a <-
                    c(a,
                      trip$leg[[i]] %>% select("@destination") %>% pull())
                b <- unique(a)
                
                r1 <-
                    stations$station[which(stations$abbr == tolower(b[1]))]
                if (length(b) > 1) {
                    for (i in 2:length(b)) {
                        r1 <- c(r1, stations$station[which(stations$abbr == tolower(b[i]))])
                    }
                }
                r1 <- paste(r1, collapse = " -> ")
                route <- c(route, r1)
            }
            tripDF <-
                tripDF %>% mutate(Route = route) %>% select(-Date)
            
            # extra train direction(terminal) info, which train to take
            trainDir <- character(0)
            for (i in 1:dim(trip)[1]) {
                trainTerm <-
                    fromJSON(json, flatten = TRUE)$root$schedule$request$trip$leg[[i]] %>%
                    select("@trainHeadStation") %>% pull()
                trainDir <-
                    c(trainDir, paste(trainTerm, collapse = " -> "))
            }
            route_trainDF <-
                tibble(Route = route, Tran_Direction = trainDir)
            
            output$route_trainNote <-
                renderText("* Detail trip, which trains to take")
            output$route_train <- renderTable(route_trainDF)
        }
        
        # fare (ticket price) info
        fareDF <-
            tripDF$Fare[[1]] %>% select("@name", "@amount") %>%
            rename(Payment = "@name", Amount = "@amount")
        
        note <-
            paste("* The estimeate trip time from <",
                  orig,
                  "> to <",
                  dest,
                  ">\n")
        
        output$tripNote <- renderText(note)
        output$tripTime <- renderTable(tripDF %>% select(-Fare))
        output$fareNote <-
            renderText("* Fare (Price for different payment methods)")
        output$fare <- renderTable(fareDF)
    })
    
    observeEvent(input$weekday, {
        output$map1 <- renderUI({
            tags$img(src = "https://www.bart.gov/sites/default/files/images/basic_page/system-map-weekday.png", style = "width:100%;")
        })
    })
    
    observeEvent(input$weekend, {
        output$map2 <- renderUI({
            tags$img(src = "https://www.bart.gov/sites/default/files/images/basic_page/system-map-sunday-091420.png", style = "width:100%;")
        })
    })
    
    # clean all output info
    observeEvent(input$clear, {
        output$estimateNote <- NULL
        output$estimate <- NULL
        output$tripNote <- NULL
        output$tripTime <- NULL
        output$route_trainNote <- NULL
        output$route_train <- NULL
        output$fareNote <- NULL
        output$fare <- NULL
    })
    
    #########################################  Currency part
    
    observeEvent(input$check, {
        
        output$exchangeO <- NULL
        output$exchangeO2 <- NULL
        
        # get input currencies
        base <- input$baseCurrency
        exchange <- input$exchange
        base <-
            currencyDF$symbol[which(currencyDF$symb_name == base)]
        exchange <-
            currencyDF$symbol[which(currencyDF$symb_name == exchange)]
        
        # get today's rate
        date1 <- Sys.Date() - 10
        date2 <- Sys.Date()
        todayEndPoint <-
            str_glue(
                "https://api.exchangeratesapi.io/latest?base={c1}&symbols={c2}",
                c1 = base,
                c2 = exchange
            )
        currency <- GET(todayEndPoint)
        stop_for_status(currency)
        json <- content(currency, as = "text", encoding = "UTF-8")
        t <- fromJSON(json, flatten = TRUE)$rates
        output$todayRate <-
            renderText(paste("Today's exchange rate is:", t[[1]]))
        
        # store today exchange rate for exchange part use conveniently
        todayRate <- t[[1]]
        
        detailR <-
            paste(" - Today's price: 1",
                  base,
                  "exchange",
                  round(t[[1]], 4),
                  exchange)
        output$detailRate <- renderText(detailR)
        
        if (input$today) {
            # get endpoint string
            sDaysEndPoint <-
                str_glue(
                    "https://api.exchangeratesapi.io/history?start_at={d1}&end_at={d2}&base={c1}&symbols={c2}",
                    c1 = base,
                    c2 = exchange,
                    d1 = date1,
                    d2 = date2
                )
            # plot past 7 days rate
            currency <- GET(sDaysEndPoint)
            stop_for_status(currency)
            json <-
                content(currency, as = "text", encoding = "UTF-8")
            x <- fromJSON(json, flatten = TRUE)$rates
            manyDays <- unlist(lapply(x, '[[', 1))
            
            dateP <- names(manyDays)
            price <- numeric(0)
            for (i in 1:length(manyDays)) {
                price <- c(price, manyDays[[i]])
            }
            
            p <-
                ggplot(mapping = aes(
                    x = dateP,
                    y = price,
                    group = 1
                )) +
                geom_line(color = "coral", size = 1) +
                labs(title = "Rate for last 7 days (only show business days)", x = "past days") +
                xlab("Previous days") + ylab("Exchange Rate")
            
            output$ratePlot <- renderPlot(p)
            
        } else{
            date1 <- input$dateRange[1]
            date2 <- input$dateRange[2]
            sDaysEndPoint <-
                str_glue(
                    "https://api.exchangeratesapi.io/history?start_at={d1}&end_at={d2}&base={c1}&symbols={c2}",
                    c1 = base,
                    c2 = exchange,
                    d1 = date1,
                    d2 = date2
                )
            
            currency <- GET(sDaysEndPoint)
            stop_for_status(currency)
            json <-
                content(currency, as = "text", encoding = "UTF-8")
            x <- fromJSON(json, flatten = TRUE)$rates
            manyDays <- unlist(lapply(x, '[[', 1))
            price <- numeric(0)
            for (i in 1:length(manyDays)) {
                price <- c(price, manyDays[[i]])
            }
            
            p <-
                ggplot(mapping = aes(x = 1:length(price), y = price)) +
                geom_line(color = "coral", size = 1) +
                labs(title = paste("Rate from", date1, "to", date2)) +
                xlab("Previous days") + ylab("Exchange Rate")
            
            output$ratePlot <- renderPlot(p)
        }
        
        # exchange money
        observeEvent(input$exchangeB, {
            amount <- input$exchangeI
            
            if (amount < 1 | is.na(amount)) {
                output$exchangeO <- NULL
                output$exchangeO2 <-
                    renderText("Please Enter Valid Amount! Minimum amount to exchange is: 1")
            } else{
                exchangeMessage <-
                    paste("You have exchanged",
                          round(amount * todayRate, 2),
                          exchange)
                output$exchangeO <- renderText(exchangeMessage)
                output$exchangeO2 <-
                    renderText("Please take your money, Thank You!")
            }
        })
        
        observeEvent(input$cash, {
            output$exchangeO <- NULL
            output$exchangeO2 <- NULL
        })
    })
    
    
    ########################################################### Weather part
    
    observeEvent(input$weatherButton, {
        woeid <- city$woeid[which(city$title == input$city)]
        endpoint <-
            str_glue("https://www.metaweather.com/api/location/{id}/",
                     id = woeid)
        
        weather <- GET(endpoint)
        stop_for_status(weather)
        json <- content(weather, as = "text", encoding = "UTF-8")
        x <- fromJSON(json, flatten = TRUE)[[1]]
        
        dfw <-
            x %>% select(
                -c(
                    id,
                    wind_direction_compass,
                    created,
                    wind_direction,
                    weather_state_abbr,
                    air_pressure
                )
            )
        
        dfw <-
            dfw %>% left_join(weather_state, by = c("weather_state_name" = "name"))
        
        for (i in 1:6) {
            local({
                li <- i
                
                id <- paste("date", li, sep = "")
                output[[id]] <- renderText(dfw$applicable_date[li])
                
                id <- paste("icon", li, sep = "")
                output[[id]] <- renderUI({
                    tags$img(src = dfw$link[li], width = 30)
                })
                
                id <- paste("weather", li, sep = "")
                output[[id]] <-
                    renderText(dfw$weather_state_name[li])
                
                id <- paste("temp", li, sep = "")
                output[[id]] <-
                    renderUI(paste(round(dfw$the_temp[li], 2), "\U2103"))
                
                id <- paste("wind", li, sep = "")
                output[[id]] <-
                    renderText(paste(round(dfw$wind_speed[li], 2), "mph"))
                
                id <- paste("predict", li, sep = "")
                output[[id]] <-
                    renderText(paste(dfw$predictability[li], "%"))
            })
        }
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
