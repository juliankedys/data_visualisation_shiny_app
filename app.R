library(shinyWidgets)
library(shiny)
library(shinythemes)
library(plotly)
library(dplyr)
library(DT)

ui <- shinyUI(
  navbarPage(
    title = "Film Dashboard",
    theme = shinytheme("yeti"),
    tags$head(tags$style(HTML('
      .dataTables_scrollBody {
          overflow-x: scroll;
      }

      #filmTable td:nth-child(10) {
          white-space: normal !important;
          width: 300px !important;
      }'))),
    tabPanel(
      tagList(icon("question"), "Help/About"),
      fluidPage(
        tags$img(src = "PP_monogram.jpg", height = 300, width = 300),
        p("Julian Kędys 148267"),
        h1("Welcome to my Film Dashboard"),
        p("This dashboard allows you to explore visualisations of a films dataset."),
        p("Please navigate further tabs for different visualisations."),
        h1("Available tabs:"),
        h2("1. Film Ratings Distribution"),
        p("The first of this app's tabs contains a bar chart representing the distribution of film ratings in the dataset. The Y axis stands for the count of films falling within given rating range and the X axis shows the rating ranges themselves. The bins for ratings each span half a rating point on a 0-10 scale."),
  
        p("When clicked on, each of the bars will result in a title of the film and its rating being shown beneath the chart. The films from each of the respective rating ranges are chosen at random and change with every interaction. Note that in order to display a different film for the same bar a different bar has to be clicked first before the original bar's random film changes. Sections of choice can be zoomed-in on by dragging a cursor across the area of interest. To revert to the original view, the user should double-click anywhere on the graph.  "),
        h2("2. Interactive Film Table"),
        p("This tab contains an interactive datatable providing various ways for searching through the entries. Each of the columns has a dedicated search bar underneath. Depending on the data type of the column the searchbar is either a text input field or a slider which allows the user to limit the scope of values of displayed results. To exemplify, feature 'adult' can be filtered by typing 'True' or 'False', whereas numerical columns such as 'vote_average' allow the user to set the minimum and maximum threshold for filtering the data on this characteristic. Note that the text fields are reactive and so any sufficiently unique phrase is enough to filter the data (e.g. just typing 'T' into the 'adult' column's searchbar would already effectively return the filtered data). Furthemore, column-bound searching tools are case-insensitive. On top of that, there are two global searchbars. One designated with 'Search film title' text which can be used for filtering the data using the films' titles, and the other for searching through the entire dataset with text-based queries. The former of the two, as opposed to the latter, is case-sensitive and its input is treated as a single phrase irrespective of the number of words it comprises. Consequently, typing 'big hero' in the first searchbar would yeild no results, yet if one searched 'Big Hero' then the corresponding film - Disney's 'Big Hero 6' - would appear. The other global search on the other hand would return any film containing either the word 'big' or 'hero' anywhere in its title, tagline, overview etc. upon the same query being entered. Moreover, any column can be used for sorting the table by the feature it contains. It can be done by clicking the column's name once for sorting in ascending order and the second time for descending order. The datatable can be navigated both by using the provided scrollbars or by moving two fingers across the touchpad. Finally, upon clicking on the datatable's rows, the most pertinent information is displayed about the films."),
        h2("3. Budget vs Ratings "),
        p("The following visualisation is a scatter plot showing correlation between average rating and budget of respective films. The Y axis represents the range of average ratings, while the Y axis shows the range of budgets spent on the films. "),
        p("There are four sliders available to the user. They are meant to filter the results based on these four criteria: minimum budget, maximum budget, minimum rating, maximum rating. Due to their reactive nature, the sliders update the plot on the fly as the user specifies their values. When hovered over, the points display information on the title of the film they represent and its exact budget and rating. Sections of choice can be zoomed-in on by dragging a cursor across the area of interest. To revert to the original view, the user should double-click anywhere on the graph."),
        h2("4. Runtime Histogram"),
        p("This tab contains a histogram showing distribution of the films' runtimes. The histogram's X axis denotes the count of films of specific runtime and its Y axis shows said runtimes."),
        p("Using a slider, the user can adjust the range (minimal and maximal value) of the runtimes depicted on the graph. The histogram should respond reactively. Sections of choice can be zoomed-in on by dragging a cursor across the area of interest. To revert to the original view, the user should double-click anywhere on the graph."),
        h2("5. Reviews by Year"),
        p("This tab shows a scatter plot of films defined by their rating (Y axis) and the year they were released in (X axis)."),
        p("This visualisation's interactivity is possible thanks to two sliders for setting the range of the years depicted and for setting the rating range. The user gets to define the minimal and the maximal value for both the features. Additionally, one can get to know what film each of the dots represents by hovering over it and causing the information on its title and exact rating to be displayed. Sections of choice can be zoomed-in on by dragging a cursor across the area of interest. To revert to the original view, the user should double-click anywhere on the graph."),
        h2("6. Interactive Film Rating System"),
        p("The following visualisation allows the user to compare their ratings of 10 randomly chosen films with their average ratings as stated in the dataset. Once all the rating are entered, the user can submit them using a dedicated button. After the ratings are registered, a bar chart will appear below showing the comparison. The visualisation and ratings can be reset by clicking the 'Clear Ratings and Plot' button. This will also result in randomly selecting 10 new films to be rated. "),
        h2("7. Vote Count vs Popularity Heatmap"),
        p("The following is an interactive heatmap composed of cells defined by their popularity and vote count ranges, while the colour of every individual cell is defined by the average rating of the film it represents. Values for both the popularity measure and the vote count are clustered into intervals ensuring equal size of the squares. "),
        p("This visualisation can be interacted with using two sliders: Year and Vote Count. The year is a singular value, while the vount count can be set as a range betwen minimal and maximal user-chosen values. Upon hovering over specific cells of the heatmap, information on vote count range within which the chosen film's vote count falls, popularity range containing the chosen film's popularity and its average rating will be displayed. The visualisation adapts reactively once the sliders are positioned by the user. Sections of choice can be zoomed-in on by dragging a cursor across the area of interest. To revert to the original view, the user should double-click anywhere on the graph."),
        p(""),
        p(""),
        p("*Note that loading either of these tabs: Budget vs Ratings, Reviews by Year may take some time, hence you should wait for a moment before the visualisation is displayed. In the meantime, an error will be displayed - please disregard it. ")
      )
    ),
    tabPanel(
      tagList(icon("bar-chart"), "Film Ratings Distribution"),
      fluidPage(
        plotlyOutput("ratingPlot"),
        verbatimTextOutput("infoFilm")
      )
    ),
    tabPanel(
      tagList(icon("table"), "Interactive Film Table"),
      fluidPage(
        textInput("searchText", "Search by film title:"),
        verbatimTextOutput("no_match"),
        DT::dataTableOutput("filmTable")
      )
    ),
    tabPanel(
      tagList(icon("dollar"), "Budget vs Ratings"),
      fluidPage(
        plotlyOutput("scatterPlot"),
        uiOutput("budget_min"),
        uiOutput("budget_max"),
        uiOutput("rating_min"),
        uiOutput("rating_max")
      )
    ),
    tabPanel(
      tagList(icon("clock"), "Runtime Histogram"),
      fluidPage(
        plotlyOutput("runtimePlot"),
        uiOutput("runtime_range")
      )
    ),
    tabPanel(
      tagList(icon("calendar"), "Reviews by Year"),
      fluidPage(
        plotlyOutput("reviewsByYearPlot"),
        uiOutput("year_range"),
        uiOutput("rating_range")
      )
    ),
    tabPanel(
      tagList(icon("thumbs-up"), "Interactive Film Rating System"),
      fluidPage(
        h3("Rate the following 10 films:"),
        uiOutput("user_rating_inputs"),
        actionButton("submit_ratings", "Submit Ratings"),
        actionButton("clear_ratings", "Clear Ratings and Plot"),
        plotlyOutput("user_rating_plot")
      )
    ),
    tabPanel(
      tagList(icon("fire"), "Vote Count vs Popularity Heatmap"),
      fluidPage(
        plotlyOutput("heatmapPlot"),
        uiOutput("year_slider"),
        uiOutput("vote_count_range")
      )
    )
  )
)





server <- shinyServer(function(input, output, session) {
  
  data <- read.csv("movies_metadata.csv")
  
  # ensuring that all numerical columns are numeric
  data <- data %>%
    mutate(
      vote_average = as.numeric(vote_average),
      budget = as.numeric(gsub(",", "", budget)), # removing the commas (if any)
      popularity = as.numeric(popularity),
      revenue = as.numeric(revenue),
      runtime = as.numeric(runtime),
      vote_count = as.numeric(vote_count),
      year = as.numeric(format(as.Date(release_date), "%Y")) # creating the year column from release_date
    )
  
  # NAs -> 0
  data$budget[is.na(data$budget)] <- 0 
  data$vote_average[is.na(data$vote_average)] <- 0
  data$year[is.na(data$year) | data$year < 1900 | data$year == 0] <- NA
  
  random_films <- reactiveValues(data = sample_n(data, 10))
  user_ratings <- reactiveValues(ratings = rep(NA, 10))
  
  output$user_rating_inputs <- renderUI({
    tagList(
      lapply(1:10, function(i) {
        fluidRow(
          column(6, p(random_films$data$title[i])),
          column(6, numericInput(inputId = paste0("user_rating_", i), 
                                 label = "Your rating", 
                                 value = NA, min = 0, max = 10, step = 0.1))
        )
      })
    )
  })
  
  # Saving the input ratings upon 'Submit Ratings' being clicked
  observeEvent(input$submit_ratings, {
    for(i in 1:10){
      user_ratings$ratings[i] <- input[[paste0("user_rating_", i)]]
    }
  })
  
  # Resetting the plots and ratings
  observeEvent(input$clear_ratings, {
    user_ratings$ratings <- rep(NA, 10)
    for(i in 1:10){
      updateNumericInput(session, paste0("user_rating_", i), value = NA)
    }
    random_films$data <- sample_n(data, 10) 
  })
  
  output$user_rating_plot <- renderPlotly({
    req(all(!is.na(user_ratings$ratings)))
    
    df <- data.frame(
      title = random_films$data$title,
      original_rating = random_films$data$vote_average,
      user_rating = user_ratings$ratings
    )
    
    df %>%
      plot_ly(x = ~title, y = ~original_rating, type = 'bar', name = 'Original Ratings') %>%
      add_trace(y = ~user_rating, name = 'User Ratings') %>%
      layout(yaxis = list(title = 'Ratings'), barmode = 'group')
  })
  
  output$year_slider <- renderUI({
    sliderInput("year_slider", "Year:",
                min = min(data$year, na.rm = TRUE), 
                max = max(data$year, na.rm = TRUE), 
                value = 2000,
                step = 1)
  })
  output$vote_count_range <- renderUI({
    sliderInput("vote_count_range", "Vote Count:",
                min = min(data$vote_count, na.rm = TRUE), 
                max = max(data$vote_count, na.rm = TRUE), 
                value = c(min(data$vote_count, na.rm = TRUE), max(data$vote_count, na.rm = TRUE)))
  })
  
  output$heatmapPlot <- renderPlotly({
    # Filtering the data based on input
    plot_data <- data %>%
      filter(year == input$year_slider,
             vote_count >= input$vote_count_range[1], vote_count <= input$vote_count_range[2]) 
    
    # Do the bins work now?
    plot_data <- plot_data %>%
      mutate(vote_count_bin = cut(vote_count, breaks = 18),  
             popularity_bin = cut(popularity, breaks = 18))  
    
    plot_data <- plot_data %>%
      group_by(vote_count_bin, popularity_bin) %>%
      summarise(vote_average = mean(vote_average, na.rm = TRUE),
                vote_count_min = min(vote_count, na.rm = TRUE),
                vote_count_max = max(vote_count, na.rm = TRUE),
                popularity_min = min(popularity, na.rm = TRUE),
                popularity_max = max(popularity, na.rm = TRUE))
    
    # the heatmap
    p <- plot_data %>%
      plot_ly(x = ~vote_count_bin, y = ~popularity_bin, z = ~vote_average, type = 'heatmap', colorscale = 'Viridis',
              text = ~paste('Vote count range:', vote_count_min, '-', vote_count_max, '<br>',
                            'Popularity range:', popularity_min, '-', popularity_max, '<br>',
                            'Average rating:', round(vote_average, 2)),
              hoverinfo = 'text') %>%
      layout(title = "Vote Count vs Popularity Heatmap", xaxis = list(title = "Vote Count"), yaxis = list(title = "Popularity"))
    
    p %>% layout(hovermode = "closest")
  })
  
  
  # sliders
  output$budget_min <- renderUI({
    sliderInput("budget_min", "Minimum Budget:", 
                min = 0, max = max(data$budget, na.rm = TRUE), value = 0)
  })
  output$budget_max <- renderUI({
    sliderInput("budget_max", "Maximum Budget:", 
                min = 0, max = max(data$budget, na.rm = TRUE), value = max(data$budget, na.rm = TRUE))
  })
  output$rating_min <- renderUI({
    sliderInput("rating_min", "Minimum Rating:", 
                min = 0, max = 10, value = 0)
  })
  output$rating_max <- renderUI({
    sliderInput("rating_max", "Maximum Rating:", 
                min = 0, max = 10, value = 10)
  })
  output$runtime_range <- renderUI({
    sliderInput("runtime_range", "Runtime range:",
                min = min(data$runtime, na.rm = TRUE), 
                max = max(data$runtime, na.rm = TRUE), 
                value = c(min(data$runtime, na.rm = TRUE), max(data$runtime, na.rm = TRUE)))
  })
  output$year_range <- renderUI({
    sliderInput("year_range", "Year range:",
                min = min(data$year, na.rm = TRUE), 
                max = max(data$year, na.rm = TRUE), 
                value = c(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE)))
  })
  output$rating_range <- renderUI({
    sliderInput("rating_range", "Rating range:",
                min = min(data$vote_average, na.rm = TRUE), 
                max = max(data$vote_average, na.rm = TRUE), 
                value = c(min(data$vote_average, na.rm = TRUE), max(data$vote_average, na.rm = TRUE)))
  })
  
  output$ratingPlot <- renderPlotly({
    p <- data %>%
      ggplot(aes(x = vote_average)) +
      geom_histogram(binwidth = 0.5, color = "black", fill = "lightblue") +
      labs(title = "Distribution of Film Ratings", x = "Rating", y = "Count") 
    
    ggplotly(p, source = "ratingPlot", tooltip = "text") %>% 
      layout(hovermode = "closest")
  })
  
  output$runtimePlot <- renderPlotly({
    runtime_data <- data %>%
      filter(runtime >= input$runtime_range[1], runtime <= input$runtime_range[2])
    
    p <- runtime_data %>%
      ggplot(aes(x = runtime)) +
      geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
      labs(title = "Distribution of Film Runtimes", x = "Runtime", y = "Count") 
    
    ggplotly(p, source = "runtimePlot", tooltip = "text") %>% 
      layout(hovermode = "closest")
  })
  
  output$scatterPlot <- renderPlotly({
    plot_data <- data %>%
      filter(budget >= input$budget_min, budget <= input$budget_max, 
             vote_average >= input$rating_min, vote_average <= input$rating_max)
    
    p <- plot_data %>%
      plot_ly(x = ~budget, y = ~vote_average, text = ~title, mode = 'markers', source = 'scatterPlot', marker = list(color = 'lightblue')) %>%
      layout(title = "Budget vs Ratings", xaxis = list(title = "Budget"), yaxis = list(title = "Rating"))
    
    p %>% layout(hovermode = "closest")
  })
  
  output$reviewsByYearPlot <- renderPlotly({
    plot_data <- data %>%
      filter(year >= input$year_range[1], year <= input$year_range[2],
             vote_average >= input$rating_range[1], vote_average <= input$rating_range[2])
    
    p <- plot_data %>%
      ggplot(aes(x = year, y = vote_average, text = paste("Title: ", title, "\nRating: ", vote_average))) +
      geom_point(color = "lightblue") +
      labs(title = "Reviews by Year", x = "Year", y = "Rating")
    
    ggplotly(p, tooltip = "text", source = "reviewsByYearPlot") %>% 
      layout(hovermode = "closest")
  })
  
  data_subset <- reactiveVal(data)
  
  observeEvent(input$searchText, {
    searchText <- input$searchText
    
    if (searchText == "") {
      data_subset(data)
    } else {
      data_subset(data %>%
                    filter(grepl(searchText, title, fixed = TRUE)))
    }
  })
  
  output$no_match <- renderText({
    if (nrow(data_subset()) == 0) {
      "No match found."
    } else {
      ""
    }
  })
  
  output$filmTable <- DT::renderDataTable({
    datatable(data_subset(),
              filter = 'top',
              options = list(pageLength = 10, search = list(caseInsensitive = TRUE)),
              rownames = FALSE,
              selection = 'single')
  })
  
  observeEvent(input$filmTable_rows_selected, {
    film <- data_subset()[input$filmTable_rows_selected, ]
    if (!is.null(film$homepage)) {
      details <- paste0("Title: ", film$title, 
                        "<br>Rating: ", film$vote_average, 
                        "<br>Homepage: <a href='", film$homepage, "'>", film$homepage, "</a>")
    } else {
      details <- paste0("Title: ", film$title, 
                        "<br>Rating: ", film$vote_average)
    }
    showModal(modalDialog(
      title = "Film Details",
      HTML(details),
      easyClose = TRUE
    ))
  })
  
  observeEvent(event_data("plotly_click", source = "ratingPlot"), {
    clickData <- event_data("plotly_click", source = "ratingPlot")
    if (!is.null(clickData)) {
      lower_bound <- clickData$x - 0.25
      upper_bound <- clickData$x + 0.25
      selected_films <- data %>%
        filter(vote_average >= lower_bound, vote_average < upper_bound)
      if (nrow(selected_films) > 0) {
        random_film <- sample_n(selected_films, 1)
        output$infoFilm <- renderText({
          paste0("Title: ", random_film$title, "\nRating: ", random_film$vote_average)
        })
      }
    }
  })
})

shinyApp(ui = ui, server = server)
