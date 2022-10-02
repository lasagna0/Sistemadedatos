library(shiny)
library(tidyverse)
library(rvest)
library(googlesheets4)

gs4_deauth()

setwd("./")

url <-  "https://www.barranquilla.gov.co/descubre/conoce-a-barranquilla/territorio"


df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T) %>% 
  lapply(., function(x) setNames(x, c("Barrios", "Barrios2")))

# combine all data frames into one
df <- do.call(rbind, df)

#create a list
barrios <- df$Barrios %>% 
  strsplit(split = ",") %>% 
  unlist()

barrios2 <- df$Barrios2 %>%
    strsplit(split = ",") %>% 
    unlist()

    # combine list into one
    barrios <- c(barrios, barrios2)




# which fields get saved 
fieldsAll <- c("name", "poblacion", "r_num_years")

# which fields are mandatory
fieldsMandatory <- c("name", "poblacion")

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# get current Epoch time
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# save the results to a file
saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data))
  
  write.csv2(x = data, file = file.path(responsesDir, fileName),
            row.names = FALSE, quote = TRUE)
}
# load all responses into a data.frame
loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv2, stringsAsFactors = FALSE)
  #data <- dplyr::rbind_all(data)
  data <- do.call(rbind, data)
  data
}

# directory where responses get stored
responsesDir <- file.path("./responses/")

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

# usernames that are admins
adminUsers <- c("admin", "prof", "lasagna0")

# info for sharing this app on facebook/twitter
share <- list(
  title = "Fundaciones que intervienen a poblaciones vulnerables de Barranquilla",
  url = "https://github.com/lasagna0",
  description = "Sistema de recopilación de datos de Probarranquilla – Gerencia de Información y Datos"
)

shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    title = "Fundaciones que intervienen a poblaciones vulnerables de Barranquilla",
    tags$head(
      tags$link(rel = "shortcut icon", type="image/x-icon", href="https://raw.githubusercontent.com/lasagna0/Sistemadedatos/main/4gRzmK13_400x400.jpg?token=GHSAT0AAAAAABZOZX5N4UCIMA5ZON3WJCXCYZ2A32Q"),

      # Facebook OpenGraph tags
      tags$meta(property = "og:title", content = share$title),
      tags$meta(property = "og:type", content = "website"),
      tags$meta(property = "og:url", content = share$url),
      tags$meta(property = "og:image", content = share$image),
      tags$meta(property = "og:description", content = share$description),
    
      # Twitter summary cards
      tags$meta(name = "twitter:card", content = "summary"),
      tags$meta(name = "twitter:site", content = paste0("@", share$twitter_user)),
      tags$meta(name = "twitter:creator", content = paste0("@", share$twitter_user)),
      tags$meta(name = "twitter:title", content = share$title),
      tags$meta(name = "twitter:description", content = share$description),
      tags$meta(name = "twitter:image", content = share$image)
    ),
    
    div(id = "header",
      h1("Fundaciones que intervienen a poblaciones vulnerables de Barranquilla"),
      h4("Sistema de recopilación de datos de Probarranquilla – Gerencia de Información y Datos")
      ),
    
    fluidRow(
      column(6,
        div(
          id = "form",
          
          textInput("name", labelMandatory("Nombre de la fundación"), ""),
          sliderInput("r_num_years", "Numero de años que ha intervenido", 0, 25, 0, ticks = FALSE),
          selectInput("poblacion", "Barrio que interviene",
                     barrios, multiple = T),
          actionButton("submit", "Submit", class = "btn-primary"),
          
          shinyjs::hidden(
            span(id = "submit_msg", "Submitting..."),
            div(id = "error",
                div(br(), tags$b("Error: "), span(id = "error_msg"))
            )
          )
        ),

        shinyjs::hidden(
          div(
            id = "thankyou_msg",
            h3("Gracias por tu respuesta!  #YoSoyProBarranquilla ")
          )
        )
      )
    )
  ),
  server = function(input, output, session) {
    
    # Enable the Submit button when all mandatory fields are filled out
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
    

# put input separeted by commas
    reactive({
      input$poblacion <- paste(input$poblacion, collapse = ", ")
    })

    # Gather all the form inputs (and add timestamp)
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- c(data, timestamp = epochTime())
      data <- t(data)
      data
    })    
    
    # When the Submit button is clicked, submit the response
    observeEvent(input$submit, {
      
      # User-experience stuff
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      # Save the data (show an error message in case of error)
      tryCatch({
        saveData(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      },
      finally = {
        shinyjs::enable("submit")
        shinyjs::hide("submit_msg")
      })
    })
    
    # submit another response
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })
    
    # render the admin panel

    
    # determine if current user is admin

    
    # Show the responses in the admin table
    
    
    # Allow user to download responses
    output$downloadBtn <- downloadHandler(
      filename = function() { 
        sprintf("datos.csv", humanTime())
      },
      content = function(file) {
        write.csv(loadData(), file, row.names = FALSE)
      }
    )    
  }
)


# save shinny app to a file
saveApp(app, "app.R")
