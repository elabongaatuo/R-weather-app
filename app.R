library(shiny)
library(bslib)
library(shinyjs)
library(httr2)
library(lubridate)
library(shiny.semantic)

# Geocoding URL
geocoding_url <- "https://api.openweathermap.org/data/2.5/weather"
# One Call API URL
current_weather_url <- "https://api.openweathermap.org/data/3.0/onecall"
# Icons URL
icon_url <- "https://openweathermap.org/img/wn/"
# API key
readRenviron(".Renviron")
api_key <- Sys.getenv("OPENWEATHER_API") 

# UI Definition
ui <- semanticPage(
  useShinyjs(),
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=0.9"),
    tags$style(
      HTML("
      :root {
          --darkBG: #232B32;
        }
  
        body.inverted {
          background-color: var(--darkBG);
        }
       .equal-height-grid {
            display: flex;
        }
        
        .equal-height-grid > .column {
            display: flex;
            flex: 1;
            flex-direction: column;
        }
        
        .equal-height-segment {
            flex: 1;
            display: flex;
            flex-direction: column;
            justify-content: space-between; 
            padding: 1rem;
        }
        
        .equal-height-two-segment {
          display: flex;
          flex-direction: column;
          justify-content: space-between;
          padding: 1rem;
          height: 100%; 
        }
        
        .ui.grid .special-column {
            text-align: center; 
            margin: 0.5rem;
        }
        
        .ui.grid .ui.special-segment {
            height: 100%; 
        }
        
        .ui.raised.special-segment {
            margin: 0.5rem;
        }
        #search {
          display: block;           
          width: 100%;             
          height: 100%;             
          border: none;            
          background: transparent;  
          color: inherit;           
          padding: 0;               
          cursor: pointer;          
          z-index: 1;              
        }

        .alert-active {
          color: red !important;  
        }
        .bell-no-alert {
          color: gray !important;  
        }
        .shake {
          animation: shake 0.5s;
          animation-iteration-count: infinite;
        }
        @keyframes shake {
          0% { transform: translate(1px, 1px) rotate(0deg); }
          10% { transform: translate(-1px, -2px) rotate(-1deg); }
          20% { transform: translate(-3px, 0px) rotate(1deg); }
          30% { transform: translate(3px, 2px) rotate(0deg); }
          40% { transform: translate(1px, -1px) rotate(1deg); }
          50% { transform: translate(-1px, 2px) rotate(-1deg); }
          60% { transform: translate(-3px, 1px) rotate(0deg); }
          70% { transform: translate(3px, 1px) rotate(-1deg); }
          80% { transform: translate(-1px, -1px) rotate(1deg); }
          90% { transform: translate(1px, 2px) rotate(0deg); }
          100% { transform: translate(1px, -2px) rotate(-1deg); }
        }
      ")
    ),
    tags$script(src = "darkmode.js")
  ),
  div(id = "notFound", class = "ui modal",
      div(class = "header", "Location Not Found"),
      div(class = "content", "No such city/town exists. Check your spelling!"),
      div(class = "actions",
          div(class = "ui button", id = "closeNotFound", "OK"))
  ),
  div(id = "badRequest", class = "ui modal",
      div(class = "header", "Invalid Request"),
      div(class = "content", "Bad request. Please try again with valid details."),
      div(class = "actions",
          div(class = "ui button", id = "closeBadRequest", "OK"))
  ),
  div(class = "ui grid",
       # search bar area
       div(class = "sixteen wide column",
            div(class = "ui segment",
                div(class = "ui grid",
                    div(class = "two wide column",
                        button(
                          class = "ui button icon basic",
                          input_id = "darkmode",
                          label = NULL,
                          icon = icon("moon icon"))
                        ),
                    div(class="ten wide column",
                        textInput(
                          "location",
                          label = NULL,
                          placeholder = "Search for your preferred city"
                        )
                        
                    ),
                    div(class="two wide column",
                        tags$div(
                          class = "ui button",
                            id = "my-custom-button",
                            input_task_button("search", label = "Search", icon = icon("search"))
                          
                        )
                        ),
                    div(class="two wide column",
                        actionButton("show_alert",label = icon("bell"), class = "bell-no-alert"),
                        textOutput("alert_message")
                    )
                    ))),
      div(class = "sixteen wide column",
          div(class = "ui equal-height-grid grid ",
              div(class = "left floated center aligned four wide column",
                  div(class = "ui raised equal-height-two-segment segment",
                      style="flex: 1;",
                      div(class = "column center aligned",
                          div(class = "ui hidden section divider"),
                          span(class="ui large text",textOutput("city")),
                          div(class = "ui hidden section divider"),
                          span(class="ui big text",textOutput("currentTime")),
                          div(class = "ui hidden section divider"),
                          span(class="ui large text",textOutput("currentDate")),
                          div(class = "ui hidden section divider"),
                      )
                  )
                  ),
              div(class = "right floated center aligned twelve wide column",
                  div(class = "ui raised segment",
                      div(class = "ui horizontal equal width segments",
                          div(class = "ui equal-height-two-segment segment",
                              style="flex: 3;",
                              div(class = "column",
                                  span(class="ui big text centered",textOutput("currentTemp")),
                                  textOutput("feelsLike"),
                                  card( class = "ui mini",
                                        div(class = "content", icon(class = "large sun"),
                                            div(class = "sub header","Sunrise"),
                                            div(class = "description",textOutput("sunriseTime"))),
                                        
                                  ),
                                  card( class = "ui mini",
                                        div(class = "content", icon(class = "large moon"),
                                            div(class = "sub header","Sunset"),
                                            div(class = "description",textOutput("sunsetTime"))),
                                        
                                  ))
                          ),
                          div(class = "ui segment",
                              style="flex: 3;",
                              div(
                                class = "column center aligned",
                                div(class = "ui hidden divider"),
                                htmlOutput("currentWeatherIcon"),
                                span(class="ui large text", textOutput("currentWeatherDescription"))
                                
                              )),
                          div(class = "ui segment",
                              style="flex: 3;",
                              div(class = "column",
                                  div(class = "ui hidden divider"),
                                  card(class = "ui tiny",
                                        div(class = "content", icon(class = "big tint"),
                                            div(class = "sub header","Humidity"),
                                            div(class = "description",textOutput("currentHumidity"))),

                                  ),
                                  card(class = "ui tiny",
                                        div(class = "content", icon(class = "big tachometer alternate"),
                                            div(class = "sub header","Pressure"),
                                            div(class = "description",textOutput("currentPressure"))),

                                  ),
                                  div(class = "ui hidden divider"))),
                          div(class = "ui segment",
                              style="flex: 3;",
                              div(class = "column center aligned",
                                  div(class = "ui hidden divider"),
                                  card(class = "ui tiny",
                                       div(class = "content", icon(class = "big wind"),
                                           div(class = "sub header","Wind Speed"),
                                           div(class = "description",textOutput("currentWindSpeed"))),
                                       
                                  ),
                                  card(class = "ui tiny",
                                       div(class = "content", icon(class = "big umbrella"),
                                           div(class = "sub header","UV Index"),
                                           div(class = "description",textOutput("currentUV"))),
                                       
                                  ),
                                  div(class = "ui hidden divider"))
                          )
                          ),

                      )
                      
                      
                  )
                  
                  ),
            


              )),
      div(class = "sixteen wide column",
          div(class = "ui grid equal-height-grid",
              div(class = "left floated center aligned six wide column",
                  div(class = "ui raised segment special-segment equal-height-segment",
                      h4("5 Days Forecast: "),
                      div( class = "ui three column special-column grid",
                           #Day One
                           div(class = "row", 
                               div( class = "five wide column",textOutput("dailyDtOne")),
                               div( class = "three wide column",textOutput("dailyTempOne")),
                               div( class = "three wide column",
                                    htmlOutput("dailyIconOne"))
                           ),
                           #Day Two
                           div(class = "row", 
                               div( class = "five wide column",textOutput("dailyDtTwo")),
                               div( class = "three wide column",textOutput("dailyTempTwo")),
                               div( class = "three wide column",htmlOutput("dailyIconTwo"))
                           ),
                           #Day Three
                           div(class = "row", 
                               div( class = "five wide column",textOutput("dailyDtThree")),
                               div( class = "three wide column",textOutput("dailyTempThree")),
                               div( class = "three wide column",htmlOutput("dailyIconThree"))
                           ),
                           #Day Four
                           div(class = "row", 
                               div( class = "five wide column",textOutput("dailyDtFour")),
                               div( class = "three wide column",textOutput("dailyTempFour")),
                               div( class = "three wide column",htmlOutput("dailyIconFour"))
                           ),
                           #Day Five
                           div(class = "row", 
                               div( class = "five wide column",textOutput("dailyDtFive")),
                               div( class = "three wide column",textOutput("dailyTempFive")),
                               div( class = "three wide column",htmlOutput("dailyIconFive"))
                           )
                           
                      )
                      )),
              div(class = "right floated center aligned ten wide column",
                  div(class = "ui raised segment special-segment equal-height-segment",
                      h4("Hourly Forecast: "),
                      div( class = "ui grid",
                           style = "display: flex; flex-direction: row; align-items: center; justify-content: space-around; flex-wrap: wrap; height: 100%;",
                           #Hour One
                           div(class = "column",
                               textOutput("hourlyDtOne"),
                               htmlOutput("hourlyIconOne"),
                               textOutput("hourlyTempOne")
                           ),
                           
                           #Hour Two
                           div(class = "column",
                               textOutput("hourlyDtTwo"),
                               htmlOutput("hourlyIconTwo"),
                               textOutput("hourlyTempTwo")
                           ),
                           
                           #Hour Three
                           div(class = "column",
                               textOutput("hourlyDtThree"),
                               htmlOutput("hourlyIconThree"),
                               textOutput("hourlyTempThree")
                           ),
                           
                           #Hour Four
                           div(class = "column",
                               textOutput("hourlyDtFour"),
                               htmlOutput("hourlyIconFour"),
                               textOutput("hourlyTempFour")
                           ),
                           
                           #Hour Five
                           div(class = "column",
                               textOutput("hourlyDtFive"),
                               htmlOutput("hourlyIconFive"),
                               textOutput("hourlyTempFive")
                           ),
                           
                          
                           
                      ),)),
              
              ))
          
       )


# Helper functions
openweather_error_body <- function(resp) {
  resp |> resp_body_json() |> _$message 
}

openweather_json <- function(api_key, ...) { 
  request(current_weather_url) |> 
    req_url_query(..., `appid` = api_key, `units` = "metric") |> 
    req_error(body = openweather_error_body) |>
    req_perform() |> 
    resp_body_json()
}

openstreetmap_error_body <- function(location, api_key) {
  resp <- request(geocoding_url) |> 
    req_url_query(`q` = location, `appid` = api_key) |> 
    req_error(is_error = \(resp) FALSE) |>
    req_perform() |>  resp_status()
  resp
}

geocode <- function(location, api_key) {
  request(geocoding_url) |> 
    req_url_query(`q` = location, `appid` = api_key) |> 
    req_perform() |> 
    resp_body_json() |>
    coordinates()
}

coordinates <- function(body) {
  if(length(body) != 0) { 
    lat <- body$coord$lat
    lng <- body$coord$lon
    town <- body$name
    c(lat, lng, town)
  } else {
    "No such city exists!"
  }
}

parse_date <- function(timestamp) {
  datetime <- as_datetime(timestamp) 
  date <- paste(weekdays(datetime), ",", day(datetime), months(datetime))
  time <- format(as.POSIXct(datetime), format = "%I:%M %p")
  c(date, time)
}

display_icon <- function(icon_code) {
  paste0(icon_url, icon_code, "@2x.png")
}


# Server Logic
server <- function(input, output, session) {
  location <- reactive({
    query <- input$location
    if(openstreetmap_error_body(query, api_key) == "404"){
      runjs("$('#notFound').modal('show');")
      req(FALSE)
    }
    else if(openstreetmap_error_body(query, api_key) == "400"){
      runjs("$('#badRequest').modal('show');")
      req(FALSE)
    }
    coords <- geocode(query, api_key)
  }) %>% bindEvent(input$search)
  
  observeEvent(input$closeNotFound, {
    runjs("$('#notFound').modal('hide');")
  })
  
  observeEvent(input$closeBadRequest, {
    runjs("$('#badRequest').modal('hide');")
  })
  
  weather_data <- reactive({
    loc <- location()
    openweather_json(api_key, lat = loc[1], lon = loc[2])
  })
  
  
  alert_data <- reactive({
    weather_data()$alerts[[1]]
  }) |> bindEvent(input$search)
  
  observe({
    
    if (!is.null(alert_data())) {
      runjs("$('#show_alert').addClass('alert-active shake').removeClass('bell-no-alert');")
    } else {
      runjs("$('#show_alert').addClass('bell-no-alert').removeClass('alert-active shake');")
    }
  }) |> bindEvent(input$search)
  
  
  
  # Show notification when the user clicks the bell icon
  observeEvent(input$show_alert, {
    if (!is.null(alert_data())) {
      alert_message <- alert_data()$description
      showNotification(
        alert_message,
        type = "warning",
        duration = 10
      )
    } else {
      showNotification(
        "No alerts at the moment!",
        type = "message",
        duration = 2
      )
    }
    # Remove the shake class when the bell is clicked
    runjs("$('#show_alert').removeClass('shake');")
  })
  
  
  output$currentWeatherIcon <- renderText({
    icon_code <- weather_data()$current$weather[[1]]$icon
    src <- display_icon(icon_code)
    paste0('<img src="', src, '" width="150px" height="150px">')
  })
  
  output$city <- renderText({
    location()[3]
  })
  
  output$currentWeatherDescription <- renderText({
    weather_data()$current$weather[[1]]$description
  })
  
  get_date_time <- function(timestamp, offset) {
    parse_date(timestamp + offset)
  }
  
  output$currentDate <- renderText({
    dt <- weather_data()$current$dt
    offset <- weather_data()$timezone_offset
    get_date_time(dt, offset)[1]
  })
  
  output$currentTime <- renderText({
    dt <- weather_data()$current$dt
    offset <- weather_data()$timezone_offset
    get_date_time(dt, offset)[2]
  })
  
  output$sunriseTime <- renderText({
    dt <- weather_data()$current$sunrise
    offset <- weather_data()$timezone_offset
    get_date_time(dt, offset)[2]
  })
  
  output$sunsetTime <- renderText({
    dt <- weather_data()$current$sunset
    offset <- weather_data()$timezone_offset
    get_date_time(dt, offset)[2]
  })
  
  output$currentTemp <- renderText({
    paste(weather_data()$current$temp, "°C")
  })
  
  output$feelsLike <- renderText({
    paste("Feels Like: ",weather_data()$current$feels_like, "°C")
  })
  
  output$currentHumidity <- renderText({
    paste(weather_data()$current$humidity, "%")
  })
  
  output$currentPressure <- renderText({
    paste(weather_data()$current$pressure, "hPa")
  })
  
  output$currentWindSpeed <- renderText({
    paste(weather_data()$current$wind_speed, "km/h")
  })
  
  output$currentUV <- renderText({
    paste(weather_data()$current$uvi, "UV")
  })
  
  #DAILY FORECAST
  #Day One
  output$dailyDtOne <- renderText({
    dt <- weather_data()$daily[[1]]$dt
    offset <- weather_data()$timezone_offset
    get_date_time(dt, offset)[1]
  })
  
  output$dailyTempOne <- renderText({
    paste(weather_data()$daily[[1]]$temp$day,"°C")
  })
  
  output$dailyIconOne <- renderText({
    icon_code <- weather_data()$daily[[1]]$weather[[1]]$icon
    src <- display_icon(icon_code)
    paste0('<img src="', src, '" width="50px" height="50px">')
  })
  
  #Day Two
  output$dailyDtTwo <- renderText({
    dt <- weather_data()$daily[[2]]$dt
    offset <- weather_data()$timezone_offset
    get_date_time(dt, offset)[1]
  })
  
  output$dailyTempTwo <- renderText({
    paste(weather_data()$daily[[2]]$temp$day,"°C")
  })
  
  output$dailyIconTwo <- renderText({
    icon_code <- weather_data()$daily[[2]]$weather[[1]]$icon
    src <- display_icon(icon_code)
    paste0('<img src="', src, '" width="50px" height="50px">')
  })
  
  #Day Three
  output$dailyDtThree <- renderText({
    dt <- weather_data()$daily[[3]]$dt
    offset <- weather_data()$timezone_offset
    get_date_time(dt, offset)[1]
  })
  
  output$dailyTempThree <- renderText({
    paste(weather_data()$daily[[3]]$temp$day,"°C")
  })
  
  output$dailyIconThree <- renderText({
    icon_code <- weather_data()$daily[[3]]$weather[[1]]$icon
    src <- display_icon(icon_code)
    paste0('<img src="', src, '" width="50px" height="50px">')
  })
  
  #Day Four
  output$dailyDtFour <- renderText({
    dt <- weather_data()$daily[[4]]$dt
    offset <- weather_data()$timezone_offset
    get_date_time(dt, offset)[1]
  })
  
  output$dailyTempFour <- renderText({
    paste(weather_data()$daily[[4]]$temp$day,"°C")
  })
  
  output$dailyIconFour <- renderText({
    icon_code <- weather_data()$daily[[4]]$weather[[1]]$icon
    src <- display_icon(icon_code)
    paste0('<img src="', src, '" width="50px" height="50px">')
  })
  
  #Day Five
  output$dailyDtFive <- renderText({
    dt <- weather_data()$daily[[5]]$dt
    offset <- weather_data()$timezone_offset
    get_date_time(dt, offset)[1]
  })
  
  output$dailyTempFive<- renderText({
    paste(weather_data()$daily[[5]]$temp$day,"°C")
  })
  
  output$dailyIconFive <- renderText({
    icon_code <- weather_data()$daily[[5]]$weather[[1]]$icon
    src <- display_icon(icon_code)
    paste0('<img src="', src, '" width="50px" height="50px">')
  })
  
  ## HoURLY FORECAST
  #Hour One
  output$hourlyDtOne <- renderText({
    dt <- weather_data()$hourly[[1]]$dt
    offset <- weather_data()$timezone_offset
    get_date_time(dt, offset)[2]
  })
  
  output$hourlyTempOne <- renderText({
    paste(weather_data()$hourly[[1]]$temp,"°C")
  })
  
  output$hourlyIconOne <- renderText({
    icon_code <- weather_data()$hourly[[1]]$weather[[1]]$icon
    src <- display_icon(icon_code)
    paste0('<img src="', src, '" width="50px" height="50px">')
  })
  
  #Hour Two
  output$hourlyDtTwo <- renderText({
    dt <- weather_data()$hourly[[2]]$dt
    offset <- weather_data()$timezone_offset
    get_date_time(dt, offset)[2]
  })
  
  output$hourlyTempTwo <- renderText({
    paste(weather_data()$hourly[[2]]$temp,"°C")
  })
  
  output$hourlyIconTwo <- renderText({
    icon_code <- weather_data()$hourly[[2]]$weather[[1]]$icon
    src <- display_icon(icon_code)
    paste0('<img src="', src, '" width="50px" height="50px">')
  })
  
  #Hour Three
  output$hourlyDtThree <- renderText({
    dt <- weather_data()$hourly[[3]]$dt
    offset <- weather_data()$timezone_offset
    get_date_time(dt, offset)[2]
  })
  
  output$hourlyTempThree <- renderText({
    paste(weather_data()$hourly[[3]]$temp,"°C")
  })
  
  output$hourlyIconThree <- renderText({
    icon_code <- weather_data()$hourly[[3]]$weather[[1]]$icon
    src <- display_icon(icon_code)
    paste0('<img src="', src, '" width="50px" height="50px">')
  })
  
  #Hour Four
  output$hourlyDtFour <- renderText({
    dt <- weather_data()$hourly[[4]]$dt
    offset <- weather_data()$timezone_offset
    get_date_time(dt, offset)[2]
  })
  
  output$hourlyTempFour <- renderText({
    paste(weather_data()$hourly[[4]]$temp,"°C")
  })
  
  output$hourlyIconFour <- renderText({
    icon_code <- weather_data()$hourly[[4]]$weather[[1]]$icon
    src <- display_icon(icon_code)
    paste0('<img src="', src, '" width="50px" height="50px">')
  })
  
  #Hour Five
  output$hourlyDtFive <- renderText({
    dt <- weather_data()$hourly[[5]]$dt
    offset <- weather_data()$timezone_offset
    get_date_time(dt, offset)[2]
  })
  
  output$hourlyTempFive <- renderText({
    paste(weather_data()$hourly[[5]]$temp,"°C")
  })
  
  output$hourlyIconFive <- renderText({
    icon_code <- weather_data()$hourly[[5]]$weather[[1]]$icon
    src <- display_icon(icon_code)
    paste0('<img src="', src, '" width="50px" height="50px">')
  })
}

shinyApp(ui, server)



