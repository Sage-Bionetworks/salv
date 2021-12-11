
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# This interface has been modified to be used specifically on Sage Bionetworks Synapse pages
# to log into Synapse as the currently logged in user from the web portal using the session token.
#
# https://www.synapse.org

library(shiny)
library(ggiraph)
library(waiter)

shinyUI(fluidPage(

  tags$head(
    singleton(
      includeScript("www/readCookie.js")
    )
  ),
  
  # Application title
  titlePanel("Synapse Access Level Visualization (SALV)"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    
    sidebarPanel(
      
      textInput(inputId = "synid_root",
                label = "Root synapse ID:",
                value = "syn26453932",
                placeholder = "syn12345"),
      
      textInput(inputId = "personage_name",
                label = "User or team name (optional):",
                value = "SALV Demo",
                placeholder = "username"),
      
      selectizeInput(inputId = "entity_type",
                     label = "Entity type(s):",
                     choices = c("folder", "file", "table", "link", "entityview", "dockerrepo"),
                     selected = "folder",
                     multiple = TRUE),
      
      actionButton(inputId = "plot",
                   label = "Plot")
      
    ),
    
    # main panel ------------------
    mainPanel(
      
      girafeOutput(outputId = "plot_of_permissions")
      
    )
  ),
  use_waiter(),
  waiter_show_on_load(
    html = tagList(
      img(src = "loading.gif"),
      h4("Retrieving Synapse information...")
    ),
    color = "#424874"
  )
))
