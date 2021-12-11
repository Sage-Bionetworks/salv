
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# This server has been modified to be used specifically on Sage Bionetworks Synapse pages
# to log into Synapse as the currently logged in user from the web portal using the session token.
#
# https://www.synapse.org

library(shiny)
library(synapser)
library(glue)
library(plotly)
library(ggraph)
library(ggiraph)
library(igraph)
library(tidyverse)
library(RColorBrewer)
source("permissions_fxns.R")
# Waiter creates a loading screen in shiny
library(waiter)

shinyServer(function(input, output, session) {
  
  session$sendCustomMessage(type="readCookie", message=list())
  
  toListen <- reactive({
    list(input$cookie, input$plot)
  })
  
  observeEvent(toListen(), {
    
    if (!is.null(input$cookie)) {
    
      # If there's no session token, prompt user to log in
      if (input$cookie == "unauthorized") {
        waiter_update(
          html = tagList(
            img(src = "synapse_logo.png", height = "120px"),
            h3("Looks like you're not logged in!"),
            span("Please ", a("login", href = "https://www.synapse.org/#!LoginPlace:0", target = "_blank"),
                 " to Synapse, then refresh this page.")
          )
        )
      } else {
        ### login and update session; otherwise, notify to login to Synapse first
        id <- showNotification("Verifying Synapse credentials...", duration = NULL)
        tryCatch({
          synLogin(sessionToken = input$cookie, rememberMe = FALSE)
  
          ### update waiter loading screen once login successful
          waiter_update(
            html = tagList(
              img(src = "synapse_logo.png", height = "120px"),
              h3(sprintf("Welcome, %s!", synGetUserProfile()$userName))
            )
          )
          Sys.sleep(2)
          waiter_hide()
        }, error = function(err) {
          Sys.sleep(2)
          waiter_update(
            html = tagList(
              img(src = "synapse_logo.png", height = "120px"),
              h3("Login error"),
              span(
                "There was an error with the login process. Please refresh your Synapse session by logging out of and back in to",
                a("Synapse", href = "https://www.synapse.org/", target = "_blank"),
                ", then refresh this page."
              )
            )
          )
  
        })
        removeNotification(id)
  
        # Any shiny app functionality that uses synapse should be within the
        # input$cookie observer
        if (input$plot) {
          if (input$personage_name != "" && !(is_team(input$personage_name) || is_user(input$personage_name))) {
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
            synid_edges <- traverse_edges(NA, input$synid_root,  
                                          include_types = unlist(input$entity_type))
            name_entities <- get_entity_name(synapse_ids)
            removeNotification(id)
            
            id <- showNotification("Querying permissions...", duration = NULL)
            personage_id <- get_personage_id(if (is.null(input$personage_name)) "" else input$personage_name)
            permissions <- get_permissions(personage_id = personage_id, 
                                           synapse_ids = synapse_ids)
            removeNotification(id)
            
            id <- showNotification("Plotting...", duration = NULL)
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
        }
      }
    }
  })
})
