library(shiny)
library(mleap)
library(tibble)
library(magrittr)
library(purrr)

install_maven()
install_mleap()

sff_mleap_model <- mleap_load_bundle("sff.zip")


options(scipen = 999)


ui <- fluidPage(

    titlePanel("Reviews"),

    sidebarLayout(
        sidebarPanel(
            textInput("review", "Label")
        ),

        mainPanel(
           textOutput("prediction"),
           textOutput("percent")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    # prediction <- reactive({
    #   tibble(review = input$review, score = "") |>
    #     mleap_transform(sff_mleap_model, .) |>
    #     transpose() |>
    #     map(~
    #           list(
    #             prediction = ifelse(.x$prediction == 0, "Great", "Other"),
    #             max_prob = max(as.double(.x$probability$values)),
    #             min_prob = min(as.double(.x$probability$values))
    #           )
    #     )
    # })

    output$prediction <- renderText({
      
        tibble(review = input$review, score = "") |>
          mleap_transform(sff_mleap_model, .) |> 
          dplyr::glimpse() |> 
          capture.output()
      
      #out <- prediction()
      #as.character(out[[1]]$prediction)
    })
    
    output$percent <- renderText({
      #out <- prediction()
      #paste0(round(out[[1]]$max_prob * 100, 0), "%")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
