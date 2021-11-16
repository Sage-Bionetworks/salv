# Synapse Access Level Visualization (SALV)

## Description

SALV is an R Shiny app that allows users to explore Synapse user or team permission settings across folder hierarchies.  If a user is selected, the ranking permission across all teams to which the user belongs and the individual user's permissions, if set, is displayed.  If no user or team name is selected, permissions for all users and teams with assigned permissions on the Synapse entity are displayed.  

## Installation

Cache Synapse credentials in the .synapseConfig file.  Please see https://r-docs.synapse.org/articles/manageSynapseCredentials.html for more detailed instructions.

The SALV shiny app can be launched from your local machine and R installation.  Start R from the terminal or in RStudio and install all required packages with the following command in the interactive R prompt:

```
renv::restore()
```

## Launch

Once all packages have been installed, launch the app with the following commands in the interactive R prompt:

```
library(shiny)
runApp()
```

## Usage

The app has three inputs:
1. Root synapse ID: Synapse ID of the entity from which to start the folder hierarchy traverse.
2. User or team name (optional): Synapse user or team name for whom to query permissions. 
3. Entity type(s): select one or more entity types for which to display permissions. Default is 'folder'.  Other options include 'table', 'file', 'link, 'entityview', and 'dockerrepo'.  

After entering and selecting the above information, click 'Plot' to run the analysis and display an interactive graphic.  
