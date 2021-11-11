# Description:
# Author: Haley Hunter-Zinck
# Date: 2021-10-06

# setup ---------------

library(shiny)
library(glue)
library(plotly)
library(ggiraph)
source("permissions_fxns.R")

# ui -----------------
ui <- fluidPage(
  
  # title -------
  titlePanel("Synapse Access Level Visualization (SALV)"),
  
  # sidebar panel ----------
  sidebarLayout(
    
    sidebarPanel(
      
      textInput(inputId = "synid_root",
                  label = "Root synapse ID:",
                  value = "",
                  placeholder = "syn12345"),
      
      textInput(inputId = "personage_name",
                label = "User or team name:",
                value = "",
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
  )
)

# server -------------
server <- function(input, output) {
  
  observeEvent(input$plot, {
    
    if (!is_team(input$personage_name) && !is_user(input$personage_name)) {
      
      id <- showNotification("Plotting...", duration = NULL)
      g <- girafe(ggobj = plot_text(glue("Personage name '{input$personage_name}'\nis not a valid team or user name.")))
      removeNotification(id)
      
    } else if (!is_synapse_entity(input$synid_root)) {
      
      id <- showNotification("Plotting...", duration = NULL)
      g <- girafe(ggobj = plot_text(glue("Root synapse ID '{input$synid_root}'\n is not a valid Synapse ID.")))
      removeNotification(id)
      
    } else {
      
      id <- showNotification("Traversing file hierarchy...", duration = NULL)
      synapse_ids <- traverse(input$synid_root, 
                              include_types = unlist(input$entity_type))
      removeNotification(id)
      
      id <- showNotification("Querying permissions...", duration = NULL)
      if (is_team(input$personage_name)) {
        personage_id <- get_team_id(input$personage_name)
      } else {
        personage_id <- get_user_id(input$personage_name)
      }
      permissions <- get_permissions(personage_id = personage_id, 
                                     synapse_ids = synapse_ids)
      removeNotification(id)
      
      id <- showNotification("Plotting...", duration = NULL)
      synid_edges <- traverse_edges(NA, input$synid_root,  
                                    include_types = unlist(input$entity_type))
      name_entities <- get_entity_name(synapse_ids)
      if(is.null(nrow(synid_edges))) {
        g <- girafe(ggobj = plot_point(id = synid_edges[2], 
                                       node_group = permissions,
                                       node_label = name_entities,
                                       title = glue("{input$synid_root}, '{input$personage_id}'")))
      } else {
        g <- girafe(ggobj = plot_graph(edgelist = synid_edges[-1,],
                                       node_group = permissions,
                                       node_label = name_entities,
                                       title = glue("{input$synid_root}, '{input$personage_id}'")))
      }
    
      removeNotification(id)
    }
    
    output$plot_of_permissions <- renderGirafe(g)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
