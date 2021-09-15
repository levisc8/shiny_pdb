# shiny app to explore PADRINO interactively

library(shinydashboard)
library(rlang)
library(tools)
library(dplyr)
library(RPadrino)



pdb <- pdb_download(save = FALSE)

.pdb_2_pretty_names <- function(x) {

  temp <- gsub("_", " ", names(x))

  toTitleCase(temp)
}

.pretty_2_pdb_names <- function(x) {

  temp <- gsub(" ", "_", x)

  tolower(temp)

}


ui <- shinyUI(
  fluidPage(
    dashboardPage(
      skin = "green",

      dashboardHeader(title = "Padrino Database"),
      dashboardSidebar(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Metadata", tabName = "metadata", icon = icon("book"))
      ),

      dashboardBody(
        tabItems(
          tabItem(tabName = "home",
                  includeMarkdown("www/home.md")),


          tabItem(tabName = "metadata",
                  fluidRow(
                    tableOutput("sum_tab"),
                    box(title = "Select Columns to Summarize Metadata by",

                        selectizeInput(inputId = "gr_by",
                                       label   = "Summarize By",
                                       choices = names(pdb[[1]]),
                                       multiple = TRUE)

                    )
                  )
          )
        )

      )
    )
  )
)

server <- function(input, output) {

  output$sum_tab  <- renderTable({
    gr_by <- syms(input$gr_by)

    out_tab <- pdb$Metadata %>%
      group_by(!!! gr_by) %>%
      summarise(id = length(unique(ipm_id)),
                spp = length(unique(species_accepted)),
                pubs = length(unique(apa_citation))) %>%
      setNames(c(names(.)[1:(length(names(.)) - 3)],
                 "# of ipm_id's",
                 "# of Species",
                 "# of Publications"))

    out_tab
  })
}



shinyApp(ui = ui, server = server)
