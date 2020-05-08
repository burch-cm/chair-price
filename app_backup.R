#
# 
#

library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)

pricing <- readxl::read_excel(here::here("./rawdata/Pricing.xlsx"))
product <- readxl::read_excel(here::here("./rawdata/Product.xlsx"))
requirement <- readxl::read_excel(here::here("./rawdata/Requirement Type.xlsx"))


ui <- dashboardPage(
    dashboardHeader(title = "Chair Price (DEMO)"),
    dashboardSidebar(),
    dashboardBody(
        useShinyjs(),
        fluidRow(
            # we know the requirement types
            column(4,
                   selectInput("requirement_type",
                               "Select Requirement Type",
                               choices = requirement$Requirement,
                               selected = "Seating")),
            column(4,
                   uiOutput("product_name")),
            column(4,
                   uiOutput("step_description"))
        )
    )
)

server <- function(input, output, session) {
    
    ##### reactively collect inputs #####
    
    product_choices <- reactive({
        req_id <- requirement[requirement$Requirement == input$requirement_type,
                              "ID"][[1]]
        product[product$`Requirement Type` == as.character(req_id), ]
    })
    
    step_desc_choices <- reactive({
        prod_key <- product[product$`Product Name` == input$selected_product,
                            "Product Key"][[1]]
        pricing[pricing$`Product Key` == prod_key, ]
    })
    
    ##### dynamically build UI based on reactive inputs #####
    
    output$product_name <- renderUI({
        selectInput("selected_product", "Select Product Name",
                    choices = product_choices()$`Product Name`)
    })
    
    output$step_description <- renderUI({
        # pull reactive value
        sdc <- step_desc_choices()
        # get unique step values
        step_choices <- unique(sdc$`Step Description`)
        # build a UI selector for each value in step values
        for (i in seq_along(step_choices)) {
            # get a unique input ID for each step choice
            input_id <- stringr::str_replace(tolower(step_choices[i]), " ", "_")
            # get the possible specification values per step choice
            spec_values <- sdc[sdc$`Step Desription` == step_choices[i], "Specification"][[1]]
            # create a select input box for each step choice
            selectInput(inputID = input_id,
                        label = step_choices[i],
                        choices = spec_values,
                        selected = spec_values[1])
        }
    })
        
}

shinyApp(ui, server)