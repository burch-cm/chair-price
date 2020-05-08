#
# 
#

library(shiny)
library(shinyjs)
library(dplyr)
library(tidyr)
library(readxl)
library(purrr)
library(DT)

##### load data #####

pricing <- readxl::read_excel(here::here("./rawdata/Pricing.xlsx"))
product <- readxl::read_excel(here::here("./rawdata/Product.xlsx"))
requirement <- readxl::read_excel(here::here("./rawdata/Requirement Type.xlsx"))

##### helper functions #####

make_select <- function(.name, .options) {
    .id <- stringr::str_replace(tolower(.name), " ", "_")
    y <- selectInput(
        inputId = .id,
        label = .name,
        choices = .options
    )
    return(y)
}

##### shiny app #####

ui <- fluidPage(
    useShinyjs(),
    titlePanel(title = "Chair Pricing DEMO"),
    fluidRow(
        # we know the requirement types
        column(5,
               selectInput("requirement_type",
                           "Select Requirement Type",
                           choices = requirement$Requirement,
                           selectize = FALSE),
               selectInput("product_name",
                           "Select Product Name",
                           choices = NULL,
                           selectize = FALSE),
               actionButton("update_ui",
                            "Update Choice"),
               br(),
               actionButton("showdatatable",
                            "Show Selections"),
               br(),
               DT::dataTableOutput("datatable")
               ),
        # need to dynamically generate the step description and selections
        column(7,
               uiOutput("step_description")
               )
    )
)

server <- function(input, output, session) {
    
    ##### collect reactive inputs #####
    
    selected_product <- reactive({
        # req_id <- requirement[requirement$Requirement == input$requirement_type,
        #                       "ID"][[1]]
        req_id <- requirement %>%
            filter(Requirement == input$requirement_type) %>%
            pull(ID)
        
        filter(product, `Requirement Type` == as.character(req_id))
    })
    
    prod_pricing <- eventReactive(input$update_ui, {
        req(!is.null(input$product_name))
        # prod_key <- product[product$`Product Name` == input$product_name,
        #                     "Product Key"][[1]]
        prod_key <- product %>%
            filter(`Product Name` == input$product_name) %>%
            pull(`Product Key`)
        
        filter(pricing, `Product Key` == prod_key)
    })
    
    all_inputs <- eventReactive(input$showdatatable, {
        x <- reactiveValuesToList(input)
        tibble(
            names = names(x),
            values = unlist(x, use.names = FALSE)
        )
    })
    
    
    ##### update dynamic list elements #####
    observeEvent(selected_product(), {
        # get procuct names for the given req type
        prod_names <- unique(selected_product()$`Product Name`)
        
        # update the product select input widget choice values
        updateSelectInput(session,
                          "product_name",
                          choices = prod_names)
        enable("show_selections")
    })
    
    ##### generate UI elements #####
    
    specs <- eventReactive(input$update_ui, {
        req(prod_pricing())
        
        prod_pricing() %>%
            select(`Step Description`, Specification) %>%
            group_by(`Step Description`) %>%
            nest() %>%
            mutate(obj = pmap(list(`Step Description`, data), make_select)) %>%
            pull(obj)
    })
    
    output$step_description <- renderUI({
        req(specs())
        tagList(specs())
    })
    
    output$datatable <- DT::renderDataTable({
        req(all_inputs())
        DT::datatable(all_inputs())
    })
    
}

shinyApp(ui, server)