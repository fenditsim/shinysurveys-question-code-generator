library(shiny)
library(shinythemes)
library(rclipboard)

ui <- fluidPage(
  rclipboardSetup(),
  theme = shinytheme("flatly"), 
  navbarPage(
    collapsible = TRUE,
    title = "Shinysurveys Question Code Generator",
    tabPanel(
      title = "Generator",
      sidebarLayout(
        sidebarPanel(width = 6,
                     selectInput(inputId = "type", label = "Question Type ", choices = list("Numeric"="numeric", "Multiple-Choice"="mc", "Text"="text", "Select"="select", "Yes or No"="y/n", "Slider"="slider", "Date"="date", "Matrix"="matrix")),
                     textAreaInput(inputId = "question", label = "Question(s) "),
                     conditionalPanel(condition = "input.type != 'text' && input.type != 'y/n' && input.type != 'slider' && input.type != 'date'", textAreaInput(inputId = "option", label = "What are the options? (Press Enter after inputting an option) "),),
                     conditionalPanel(condition = "input.type == 'slider'",
                                      textInput(inputId = "slider.min", label = "Minimum", width = "100%"),
                                      textInput(inputId = "slider.max", label = "Maximum", width = "100%"),
                                      textInput(inputId = "slider.value", label = "Value", width = "100%")
                     ),
                     conditionalPanel(condition = "input.type == 'date'",
                                      dateInput(inputId = "date.min", label = "Minimum Date", width = "100%", value = Sys.Date()-10),
                                      dateInput(inputId = "date.max", label = "Maximum Date", width = "100%", value = Sys.Date()+10),
                                      dateInput(inputId = "date.value", label = "Value", width = "100%", value = Sys.Date())
                     ),
                     textInput(inputId = "id", label = "Question's ID ", width = "100%"),
                     selectInput(inputId = "required", label = "Is this question required? ", selected = F, choices = list("Yes"=T, "No"=F)),
                     conditionalPanel(condition = "input.type == 'mc' || input.type == 'text' || input.type == 'select'", 
                                      selectInput(inputId = "dependence", label = "Does this question depend on another? ", selected = "No", choices = list("No"="No", "Yes"="Yes")),
                     ),
                     
                     conditionalPanel(condition = "input.dependence == 'Yes'",
                                      textInput(inputId = "dependence_value", label = " What is the dependent value? ", value = ""),
                                      textInput(inputId = "dependence_value_id", label = "What is ID for dependent value ", width = "100%"),
                                      selectInput(inputId = "dependence_value_required", label = "Is dependence question required? ", selected = "No", choices = list("Yes"=T, "No"=F)),
                     )
        ),
        mainPanel(width = 6,
                  rclipboardSetup(),
                  p("Click Copy button to copy the completed code below!"),
                  verbatimTextOutput(outputId = "question"),
                  uiOutput("clip"),
        )
      )
    ),
    tabPanel(
      title = "Reference",
      fluidRow(
        p("For more information about shinysurveys, please visit", a(href="https://shinysurveys.jdtrat.com/index.html", "here")),
        p("For more information about this shiny website, please visit", a(href="https://github.com/fendit/shinysurveys-question-code-generator", "here")),
        p("If you found any errors with this code generator, please report", a(href="https://github.com/fendit/shinysurveys-question-code-generator/issues", "here")),
        div(style = "position: absolute; bottom: 0;", 
            p("Author:", a("Fendi Tsim", href="https://github.com/fendit")),
            p("Date: 2023-08-24"))
      )
    ),
  )
  
  
  )

server <- function(input, output, session){
  result <- reactive({
    original <- paste0("q <- data.frame(question = ", if (input$type=="matrix") paste0("rep(c(",paste(shQuote(unlist(strsplit(input$question, "\n")), type = "cmd"), collapse = ", "), "), each = ", length(unlist(strsplit(input$option, "\n"))), ")") else paste0('"', input$question, '"'), ",", 
                       "\n                option = ", if (input$type=="text" | input$type == "slider" | input$type == "date") 'NA' else if (input$type=="y/n") 'c("Yes", "No")' else paste0("c(",paste(shQuote(unlist(strsplit(input$option, "\n")), type = "cmd"), collapse = ", "), ")"), ",", 
                       "\n                input_type = ", paste0('"', input$type, '"'), ",", 
                       "\n                input_id = ", paste0('"', input$id, '"'), ",", 
                       "\n                dependence = NA,", 
                       "\n                dependence_value = NA,", 
                       "\n                required = ", input$required,
                       "\n                )"
                       )
    
    dependence <- paste0("q[nrow(q)+1, ] <- c(question = ", paste0('"', input$question, '"'), ", ", 
                         "\n                    option = NA, ", 
                         paste0('\n                    input_type = "text"'), ", ", 
                         paste0('\n                    input_id = "', input$dependence_value_id, '"'), ", ", 
                         paste0('\n                    dependence = "', input$id, '"'), ", ", 
                         paste0('\n                    dependence_value = "', input$dependence_value, '"'),", ", 
                         "\n                    required = ", input$dependence_value_required,
                         "\n                    )"
                         )
    
    extend <- paste0('extendInputType(input_type = ', paste0('"', input$type, '"'),', {',
                     '\n    ', 'shiny::', if (input$type == 'slider') 'slider' else 'date' ,'Input(',
                     '\n        ', 'inputId = surveyID(),',
                     '\n        ', 'label = surveyLabel(),', 
                     '\n        ', 'min = ', if (input$type == 'slider') input$slider.min else paste0('as.Date("', input$date.min, '")'), ', ', 
                     '\n        ', 'max = ', if (input$type == 'slider') input$slider.max else paste0('as.Date("', input$date.max, '")'), ', ', 
                     '\n        ', 'value = ', if (input$type == 'slider') input$slider.value else paste0('as.Date("', input$date.value, '")'), 
                     '\n    ', ')',
                     '\n})'
                     )
    
    question <-if (input$type == 'slider' | input$type=='date') paste0(original, "\n", "\n", "# Register a ", input$type, " input with custom min & max" , "\n", extend) else{ if (input$dependence == 'No') original else paste0(original, "\n", "\n", "# For Dependence Value", "\n", dependence)}
  })
  
  output$question <- renderText({
    result()
  })
  
  output$clip <- renderUI({
    rclipButton(
      inputId = "clipbtn", 
      label = "Copy", 
      clipText = result(), 
      icon = icon("clipboard"))
  })
}

shinyApp(ui, server)