# shiny app to explore PADRINO interactively

library(shinydashboard)
library(rlang)
library(tools)
library(dplyr)
library(RPadrino)
library(sf)
library(leaflet)



pdb <- pdb_download(save = FALSE)

pdb_jstor_doi_link <- function(pdb) {

  doi_jstor <- pdb$doi

  out <- character(length(doi_jstor))

  for(i in seq_along(doi_jstor)) {

    if(is.na(doi_jstor[i])) {

      out[i] <- NA_character_

      next
    }

    doi_jstor_link <- doi_jstor[i]

    if(grepl("jstor\\.org", doi_jstor_link)) {

      out[i] <- doi_jstor_link

    } else {

      out[i] <- paste0("https://doi.org/", doi_jstor_link)

    }

  }

  return(out)

}

pdb_map_lab_link <- function(pdb) {

  links <- pdb_jstor_doi_link(pdb)

  spp_nms <- pdb$species_accepted

  stopifnot(length(spp_nms) == length(links))

  labs <- vapply(seq_along(links),
                 function(ind, links, spps) {

                   if(is.na(links[ind])) {
                     return(spps[ind])
                   } else {
                     paste0('<a href = "',
                            links[ind],
                            '"> ',
                            spps[ind],
                            '</a>')
                   }
                 },
                 links = links, spps = spp_nms,
                 character(1L))

}


ui <- dashboardPage(
  skin = "green",

  dashboardHeader(title = "Padrino Database"),
  dashboardSidebar(id = "",
    menuItem("Home",     tabName = "home",     icon = icon("home")),
    menuItem("Metadata", tabName = "metadata", icon = icon("book")),
    menuItem("Maps",     tabName = "map",      icon = icon("map"))
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
      ), # End Metadata tab
      tabItem(tabName = "map",
              includeMarkdown("www/map.md"),
              fluidRow(
                leafletOutput("map_out"),
                box(
                  title = "Enter ipm_id's to Map",
                  textInput(
                    inputId = "ipm_ids",
                    label   = "IPM ID's",
                    value   = "all"),
                  actionButton(
                    inputId = "submit_ipm_id",
                    label = "Submit IDs"
                  )
                )
              )
      ) # End Map tab
    ) # End Dashboard body tab items

  ) # End dashboard body
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

  use_pdb <- eventReactive(
    eventExpr = input$submit_ipm_id,
    valueExpr = {

      ipm_ids <- input$ipm_ids

      if(ipm_ids == "all") {
        pdb %>%
          .$Metadata %>%
          select(ipm_id, species_accepted, doi, lon, lat) %>%
          mutate(species_accepted = gsub("_", " ", .$species_accepted)) %>%
          filter(!is.na(lat) & !is.na(lon)) %>%
          st_as_sf(crs = "WGS84",
                   coords = 4:5)
      } else {

        ipm_ids <- strsplit(ipm_ids, ", ") %>%
          unlist() %>%
          trimws()

        pdb_subset(pdb, ipm_ids) %>%
          .$Metadata %>%
          select(ipm_id, species_accepted, doi, lon, lat) %>%
          mutate(species_acceptcced = gsub("_", " ", .$species_accepted)) %>%
          filter(!is.na(lat) & !is.na(lon)) %>%
          st_as_sf(crs = "WGS84",
                   coords = 4:5)
      }
    }
  )

  output$map_out <- renderLeaflet({

    use_dat <- use_pdb()

    labs <- pdb_map_lab_link(use_dat)

    out <- leaflet(data = use_dat) %>%
      addTiles() %>%
      addMarkers(popup = labs,
                 clusterOptions = markerClusterOptions())

    out
  })
}



shinyApp(ui = ui, server = server)
