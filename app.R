# shiny app to explore PADRINO interactively

library(shiny)
library(shinythemes)
library(rlang)
library(tools)
library(dplyr)
library(RPadrino)
library(markdown)


pdb <- pdb_download(save = FALSE)

.pretty_names <- function(x) {

  temp <- gsub("_", " ", names(x))

  toTitleCase(temp)
}


ui <- fluidPage(

  navbarPage(
    "PADRINO Database", theme = shinytheme("lumen"),
    tabPanel("PADRINO Overview", fluid = TRUE, icon = icon("atlas"),
             fluidRow(
               column(6,
                      h4(p("Welcome to the PADRINO Shiny App!")),
                      h5(p("PADRINO is an open access database of published Integral Projection Models (IPMs). The goal of this app is to allow you to explore the models contained in the database. Analysis capabilities are available in the `RPadrino` R package.")))
             )
    ),
    tabPanel("Metadata", fluid = TRUE, icon = icon("seedling"),
             titlePanel("PADRINO Metadata"),
             fluidRow(
               column(6,
                      checkboxGroupInput(inputId  = "met_cols",
                                         label    = "Metadata Options",
                                         choices  = .pretty_names(pdb$Metadata),
                                         selected = c("species_accepted",
                                                      "tax_family",
                                                      "tax_kingdom",
                                                      "pub_year",
                                                      "duration",
                                                      "lat",
                                                      "lon",
                                                      "country",
                                                      "eviction_used",
                                                      "treatment",
                                                      "has_dd"
                                         )
                      )

               ),
               column(6,
                      selectizeInput(inputId = "sel_by",
                                     label   = "Summary Options",
                                     choices = .pretty_names(pdb$Metadata),
                                     multiple = TRUE,
                                     options  = list(placeholder = ' Metadata Column Names',
                                                     onInitialize = I('function() {this.setValue(""); }')))

                      )
             )

    )
  )


)


server <- function(input, output, session) {

  reactive({




  })

}
