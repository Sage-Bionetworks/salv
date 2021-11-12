# Description: this script takes in a single user or team along with a 
# Synapse ID, the root, corresponding to a project or a folder.  The script 
# then traverses the file structure from the root and gathers the permission 
# level for the user/team in each descendant folder as well as the root folder 
# itself.
# 
# Author: Haley Hunter-Zinck
# Date: August 12, 2021

# setup -------------------------------------

library(synapser)
synLogin()
library(glue)
library(ggraph)
library(ggiraph)
library(igraph)
library(tidyverse)
library(RColorBrewer)


# constants
RANK_PERMISSIONS <- c("Administrator", "Can edit & delete", "Can edit", "Can download", "Can view", "None")
MAP_PERMISSIONS <- setNames(c("Can view", "Can download", "Can edit", "Can edit & delete", "Administrator"),
                            c("READ", "DOWNLOAD,READ", "CREATE,DOWNLOAD,READ,UPDATE", 
                                "CREATE,DELETE,DOWNLOAD,READ,UPDATE","CHANGE_PERMISSIONS,CHANGE_SETTINGS,CREATE,DELETE,DOWNLOAD,MODERATE,READ,UPDATE"))
COLOR_PERMISSIONS <- setNames(brewer.pal(n = length(RANK_PERMISSIONS), name = "Dark2"),
                              RANK_PERMISSIONS)

# functions  -------------------------------------

#' Depth first search for tree traversal of root folder directory.
#' 
#' @param synid_root Synapse ID of the root folder/project.
#' @return Vector of Synapse IDs of all descendants of the root 
#' @example 
#' traverse("syn12345")
traverse <- function(synid_root, 
                     include_types = "folder") {
  
  synid_desc <- c()
  synid_children <- as.list(synGetChildren(synid_root, 
                                           includeTypes = as.list(include_types)))
  
  if(!length(synid_children)) {
    return(synid_root)
  }
  
  for (synid_child in synid_children) {
    synid_desc <- append(synid_desc, traverse(synid_child$id, 
                                              include_types = as.list(include_types)))
  }
  
  return(c(synid_root, synid_desc))
}

#' Depth first search for tree traversal of root folder directory
#' to gather edges.
#' 
#' @param synid_grandparent Synapse ID of grandparent, if applicable.
#' @param synid_parent Synapse ID of the root folder/project
#' @return Matrix of Synapse IDs of all edges between descendants
#' of the root 
#' @example 
#' traverse_edges("syn12345", "syn23456")
traverse_edges <- function(synid_grandparent, 
                           synid_parent, 
                           include_types = "folder") {
  
  synid_edges <- c()
  ent_children <- as.list(synGetChildren(synid_parent, 
                                         includeTypes = as.list(include_types)))
  
  if(!length(ent_children)) {
    return(c(synid_grandparent, synid_parent))
  }
  
  for (ent_child in ent_children) {
    synid_edges <- rbind(synid_edges, traverse_edges(synid_parent, ent_child$id, 
                                                     include_types = as.list(include_types)))
  }
  return(rbind(c(synid_grandparent, synid_parent), synid_edges))
}

#' Get Synapse entity file name.
#' 
#' @param synapse_ids Vector of Synapse IDs
#' @return Vector of file names
#' @example get_entity_name("syn12345")
get_entity_name <- function(synapse_ids) {
  entity_names <- c()
  for (synapse_id in synapse_ids) {
    entity_names[synapse_id] <- synGet(synapse_id, downloadFile = F)$properties$name
  }
  return(entity_names)
}

#' Check whether a string represents a team name.
#' 
#' @param personage_name team or other Synapse entity name
#' @return TRUE if name relates to a Synapse team; otherwise FALSE
#' @example is_team("my_team_name")
is_team <- function(personage_name) {
  
  res <- tryCatch(length(synGetTeam(as.character(personage_name))), 
                  error = function(cond) {return(0)})
  
  return(as.logical(res))
}

#' Check whether a string represents a user name.
#' 
#' @param personage_name user or other Synapse entity name
#' @return TRUE if name relates to a Synapse user; otherwise FALSE
#' @example is_user("my_user_name")
is_user <- function(personage_name) {
  res <- tryCatch(length(synGetUserProfile(as.character(personage_name))), 
                  error = function(cond) {return(0)})
  
  return(as.logical(res))
}

#' Check whether a string represents a Synapse ID.
#' 
#' @param synapse_id Synapse ID for an entity
#' @return TRUE if the ID relates to a Synapse entity; otherwise FALSE
#' @example is_synapse_entity("synapse_id")
is_synapse_entity <- function(synapse_id) {
  res <- tryCatch(length(synGet(synapse_id, downloadFile = F)), 
                  error = function(cond) {return(0)})
  
  return(as.logical(res))
}

#' Get the user Synapse ID number from the user's Synapse user name.
#' 
#' @param user_name Synapse user name
#' @return Synapse user ID number
#' @example get_user_id("my_user_name")
get_user_id <- function(user_name) {
  
  if (is_user(user_name)) {
    return(synGetUserProfile(user_name)$ownerId)
  }
  
  return(NA)
}

#' Get the team Synapse ID number from the user's Synapse team name.
#' 
#' @param user_name Synapse team name
#' @return Synapse team ID number
#' @example get_team_id("my_team_name")
get_team_id <- function(team_name) {
  if (is_team(team_name)) {
    return(synGetTeam(team_name)$id)
  }
  
  return(NA)
}

get_personage_id <- function(personage_name) {
  
  if (personage_name == "") {
    return("")
  }
  
  if (is_team(personage_name)) {
    return(synGetTeam(personage_name)$id)
  } 
  
  if (is_user(personage_name)) {
    return(synGetUserProfile(personage_name)$ownerId)
  }
  
  return(NA)
}

get_personage_name <- function(personage_id) {
  
  if (personage_id == "273948") {
    return("Registered")
  }
  
  if (personage_id == "273949") {
    return("Public")
  }
  
  if (is_team(personage_id)) {
    return(synGetTeam(personage_id)$name)
  } 
  
  if (is_user(personage_id)) {
    return(synGetUserProfile(personage_id)$userName)
  }
  
  return(NA)
}

#' Get all the Synapse teams to which a user belongs.
#' @param user_id Synapse user ID number.  
#' @return_ids If TRUE, return Synapse team ID numbers; otherwise return 
#' Synapse team names.
#' @return vector of Synapse team names or ID numbers
get_user_teams <- function(user_id, return_ids = F) {
  
  team_names <- c()
  
  team_entities <- synRestGET(glue("/user/{user_id}/team/id"))
  team_ids <- unlist(team_entities$teamIds)
  
  if(return_ids) {
    return(team_ids)
  }
  
  if(length(team_ids)) {
    for(team_id in team_ids) {
      team_names <- append(team_names, synGetTeam(team_id)$name)
      
    }
  }
  
  return(team_names)
}

#' Get the ranking permission out of a list of standardized permission labels.
#' 
#' @param permissions vector of permission labels
#' @return permission label that has highest precedence
#' @example get_ranking_permission(c("Administrator", "Can edit & delete", "Can edit", "Can download", "Can view"))
get_ranking_permission <- function(permissions) {
  
  filtered <- permissions
  
  idx_remove = which(is.na(permissions) | !is.element(permissions, RANK_PERMISSIONS))
  if (length(idx_remove)) {
    filtered = permissions[-idx_remove]
  }

  if(!length(filtered)) {
    return(NA)
  }
  
  rank_idx <- match(filtered, RANK_PERMISSIONS)
  return(filtered[which(rank_idx == min(rank_idx))][1])
}

get_permissions_rest <- function(synapse_id, personage_id) {
  
  acl <- tryCatch({
    synRestGET(glue("/entity/{synapse_id}/acl"))
  }, error = function(cond) {
    return(synRestGET(tail(strsplit(cond[[1]], split = " ")[[1]], 1)))
  })
  
  personage_ids <- unlist(lapply(acl$resourceAccess, function(x) {return(x$principalId)}))
  
  idx <- which(personage_ids == personage_id)
  if (length(idx)) {
    return(acl$resourceAccess[[idx]]$accessType)
  }
  
  return(NULL)
}

get_permissions_by_personage <- function(synapse_id) {
  
  acl <- tryCatch({
    synRestGET(glue("/entity/{synapse_id}/acl"))
  }, error = function(cond) {
    return(synRestGET(tail(strsplit(cond[[1]], split = " ")[[1]], 1)))
  })
  
  personage_ids <- as.character(unlist(lapply(acl$resourceAccess, 
                                              function(x) {return(x$principalId)})))
  
  if (length(personage_ids)) {
    
    personage_names <- c()
    access_types <- c()
    
    for (i in 1:length(personage_ids)) {
      
      personage_names[i] <- get_personage_name(personage_ids[i])
      permissions_raw <- unlist(acl$resourceAccess[[i]]$accessType)
      access_types[i] <- MAP_PERMISSIONS[paste0(sort(permissions_raw), collapse = ",")]
    }
    
    idx <- order(personage_names)
    bnd <-  rbind(personage_names[idx], access_types[idx])
    return(paste0(apply(bnd, 2, paste0, collapse = ": "), collapse = "\n"))
  }

  return(NULL)
}


#' Get standardized permission label for a given user name and a 
#' Synapse ID representing a project, folder, file, table or other Synapse
#' entity.   
#' 
#' @param personage_id Synapse user ID or team ID
#' @param synapse_id Synapse ID of entity
#' @return standardized permission value
#' @example 
#' get_personage_entity_permissions(personage_id = "54321", synapse_id = "synid12345")
get_personage_entity_permission <- function(personage_id, synapse_id) {
  
  #permissions_raw <- unlist(synGetPermissions(synapse_id, personage_id))
  permissions_raw <- unlist(get_permissions_rest(synapse_id, personage_id))
  
  if (length(permissions_raw)) {
    permission_label <- MAP_PERMISSIONS[paste0(sort(permissions_raw), collapse = ",")]
    return(as.character(permission_label))
  }
  
  return("None")
}

#' Get standardized permission label for a given team name and a 
#' Synapse ID representing a project, folder, file, table or other Synapse
#' entity.   
#' 
#' @param personage_name Synapse user name or team name
#' @param synapse_id Synapse ID of entity
#' @return standardized permission value
#' @example 
#' get_team_entity_permissions(personage_name = "my_synapse_user_name", synapse_id = "synid12345")
get_team_entity_permission <- function(team_id, synapse_ids) {
  
  permissions <- c()
  for (synapse_id in synapse_ids) {
    permissions[synapse_id] <- get_personage_entity_permission(personage_id = team_id, 
                                  synapse_id = synapse_id)
  }
  
  return(permissions)
}

#' Get the ranking permission for a user on Synapse items across user's individually
#' assigned permissions in addition to permissions of any teams to which the user
#' belongs.
#' 
#' @param user_name Synapse user name
#' @param synapse_ids vector of Synapse IDs representing project, folder, or file
#' @return vector of ranking permissions with names representing the Synapse IDs
#' @example 
#' get_user_entity_ranking_permission(user_id = "54321", synapse_ids = c("syn12345", "syn67890"))
get_user_entity_ranking_permission <- function(user_id, synapse_ids) {
  
  permissions <- c()
  team_ids <- get_user_teams(user_id, return_ids = T)
  
  for (synapse_id in synapse_ids) {
    
    permissions_item <- c()
    for (personage_id in c(user_id, team_ids)) {
      permissions_item <- append(permissions_item, 
                                 get_personage_entity_permission(personage_id, synapse_id))
    }
    
    permissions[synapse_id] <- get_ranking_permission(permissions_item)
  }
  
  return(permissions)
}

#' Get ranking permissions on all Synapse IDs for a user or team.
#' 
#' @param personage_id Synapse user or team ID number
#' @param synapse_ids Vector of synapse IDs
#' @return vector of standardized permission labels for each Synapse entity
#' @example get_permissions("54321", c("syn12345", "syn23456"))
get_permissions <- function(personage_id, synapse_ids) {
  
  permissions <- c()
  
  if (personage_id == "") {
    for (i in 1:length(synapse_ids)) {
      permissions[i] <- get_permissions_by_personage(synapse_ids[i])
    }
    permissions <- setNames(permissions, synapse_ids)
  } else if(is_team(personage_id)) {
    permissions <- get_team_entity_permission(personage_id, synapse_ids)
  } else {
    permissions <- get_user_entity_ranking_permission(personage_id, synapse_ids)
  }
  
  return(permissions)
}

#' Plot a graph with associated with each node
#' 
#' @param edgelist matrix with two columns representing source and destination
#' @param node_group group associated with the node that will be used to assign 
#' node color
#' @param node_label text labels associated with each node
#' @param layout valid layout for ggraph
#' @return ggraph object
plot_graph <- function(edgelist, 
                       node_group, 
                       node_label, 
                       layout = "dendrogram", 
                       size = 5,
                       title = "") {
  
  group_column <- c()
  if (all(is.element(unique(node_group), RANK_PERMISSIONS))) {
    group_column <- factor(node_group, levels = RANK_PERMISSIONS)
  } else {
    group_column <- node_group
  }
  
  df_vertices <- data.frame(name = names(node_group),
                            label = node_label, 
                            group = group_column,
                            stringsAsFactors = T,
                            row.names = NULL)
  obj_igraph <- graph_from_data_frame(edgelist, 
                                    directed = T,
                                    vertices = df_vertices)
  obj_layout <- create_layout(obj_igraph, layout = layout)
  
  obj_ggraph <- ggraph(graph = obj_layout, 
                       circular = FALSE) + 
    geom_edge_elbow() +
    scale_colour_manual(values = COLOR_PERMISSIONS ) +
    geom_point_interactive(data = obj_layout,
                          aes(x = x, 
                              y = y, 
                              tooltip = glue("{label}\n{name}\n---\n{group}"),
                              color = group),
                          size = size) +
    theme_void() +
    labs(color = "Permissions",
         title = title)
  
  return(obj_ggraph)
}

plot_text <- function(label) {
 obj_ggplot <-  ggplot() + 
   annotate("text", x = 1, y = 1, size = 8, label = label) + 
   theme_void()
  
  return(obj_ggplot)
}

plot_point <- function(id,
                       node_label, 
                       node_group, 
                       size = 5,
                       title = "") {
  
  df <- data.frame(id = id, name = node_label, group = node_group, x = 1, y = 1)
  
  obj_ggplot <-  ggplot() + 
    scale_colour_manual(values = COLOR_PERMISSIONS) +
    geom_point_interactive(data = df,
                           aes(x = x, 
                               y = y, 
                               tooltip = glue("{name}\n{id}\n---\n{group}"),
                               color = group),
                           size = size) +
    theme_void() +
    labs(color = "Permissions", 
         title = title) 
  
  return(obj_ggplot)
}
