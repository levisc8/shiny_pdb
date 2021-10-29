# shiny app to explore PADRINO interactively

# Dependencies ---------------
library(RPadrino)
library(shinydashboard)
library(rlang)
library(dplyr)
library(sf)
library(leaflet)
library(mapview)
library(ggplot2)
library(gridExtra)
library(knitr)
library(rmarkdown)

# Download PADRINO upfront so it is not constantly re-downloading itself
pdb <- pdb_download(save = FALSE)


# Helper functions -----------------

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

pdb_make_map_dataset <- function(pdb, ipm_ids) {

  pdb_subset(pdb, ipm_ids) %>%
    .$Metadata %>%
    select(ipm_id, species_accepted, doi, lon, lat) %>%
    mutate(species_acceptcced = gsub("_", " ", .$species_accepted)) %>%
    filter(!is.na(lat) & !is.na(lon)) %>%
    st_as_sf(crs = "WGS84",
             coords = 4:5)
}

pdb_check_stochastic_models <- function(db, ids) {

  test <- pdb_subset(db, ids)

  if(nrow(test$EnvironmentalVariables) > 0) {

    rm_ids <- unique(test$EnvironmentalVariables$ipm_id)

    ids    <- ids[!ids %in% rm_ids]

    warning("Found stochastic models in requested data set. These will be removed!",
            call. = FALSE)

  }

  return(ids)

}

pdb_mat_to_df <- function(ps) {

  if(isTRUE(attr(ps, "has_par_sets"))) {

    lapply(ps, function(x){

      # x <- lapply(x, pdb_check_correct_ps_range)

      do.call(rbind, x)
    })

  } else {

    # ps <- pdb_check_correct_ps_range(ps)

    list(do.call(rbind, ps))
  }

}

pdb_prep_ps_par_sets <- function(pop_state, pdb) {

  use_pdb <- pdb_subset(pdb, names(pop_state))


  par_set_tab <- use_pdb$ParSetIndices

  par_set_ids <- unique(par_set_tab$ipm_id)

  for(i in seq_along(pop_state)) {

    # No par sets? Then keep on going.
    if(!names(pop_state)[i] %in% par_set_ids) {

      next
    }

    use_ps          <- pop_state[[i]]
    use_par_set_tab <- par_set_tab[par_set_tab$ipm_id == names(pop_state)[i], ]

    use_par_sets    <- as.list(use_par_set_tab$range) %>%
      lapply(function(x) eval(parse(text = x))) %>%
      setNames(use_par_set_tab$vr_expr_name)

    if(!all(is.na(use_par_set_tab$drop_levels))) {
      use_par_sets$drop_levels <- unique(eval(use_par_set_tab$drop_levels))
    }

    # replace the "n_z_parSetIndex" with "ipm_id_z_parSetIndex"

    # names(use_ps)  <- gsub("^n", names(pop_state[ind]), names(use_ps))

    par_set_inds <- ipmr:::.make_par_set_indices(use_par_sets)

    temp <- list()
    for(j in seq_along(par_set_inds)) {

      temp <- c(temp, list(use_ps[grepl(par_set_inds[j], names(use_ps))]))
      names(temp)[j] <- paste(names(pop_state)[i], par_set_inds[j], sep = "_")

    }

    attr(temp, "has_par_sets") <- TRUE
    pop_state[[i]] <- temp

  }

  return(pop_state)

}

pdb_calculate_input_ipm_ids <- function(pdb, input_ids) {

  ipm_ids <- strsplit(input_ids, ",") %>%
    unlist() %>%
    trimws()

  # browser()

  if(input_ids == "all") {

    ipm_ids <- unique(pdb$Metadata$ipm_id)

  } else if(!any(ipm_ids %in% pdb$Metadata$ipm_id)) { # Input is species/genus names

    spp_nms <- gsub(" ", "_", ipm_ids)

    use_ids <- character()

    # This approach Won't scale well when PDB gets bigger. Worry about this
    # later.

    for(i in seq_along(spp_nms)) {

      # use fuzzy matching of full names so users don't have to specify
      # varieties/sub-species
      temp <- pdb$Metadata$ipm_id[grepl(spp_nms[i],
                                        pdb$Metadata$species_accepted)] %>%
        unique()

      use_ids <- c(use_ids, temp)

    }

    ipm_ids <- use_ids[use_ids != ""] %>%
      unique()

  }

  if(!any(ipm_ids %in% pdb$Metadata$ipm_id)) {
    stop("The requested 'ipm_id's are not present in this version of PADRINO!")
  }

  return(ipm_ids)
}

pdb_download_report <- function(pdb, ids, dest) {

  use_db   <- pdb_subset(pdb, ids)

  rmd_path <- gsub("pdf", "rmd", dest)

  pdb_report(use_db,
             title = "",
             keep_rmd = TRUE,
             rmd_dest = rmd_path,
             output_format = "pdf",
             render_output = FALSE,
             map = TRUE)

  temp_report <- readLines(rmd_path, warn = FALSE)
  cit_ind     <- which(temp_report == "# Citations included in the `pdb` object")

  ipm_tab <- pdb_make_proto_ipm(use_db) %>%
    pdb_make_ipm() %>%
    pdb_make_ipm_report_table(db = use_db)

  pdb_insert_ipm_tab(temp_report, cit_ind, ipm_tab$txt) %>%
    pdb_clean_report_source() %>%
    writeLines(con = rmd_path)

  # output_format <- paste0(output_format, "_document")

  rmarkdown::render(input         = rmd_path,
                    output_format = "pdf_document",
                    envir         = ipm_tab$env)

}

# Cleans the indentation issues that might arise from pdb_report

pdb_clean_report_source <- function(report) {

  chunk_ind <- grepl("```", report)

  report[chunk_ind] <- trimws(report[chunk_ind])

  report

}


pdb_make_ipm_report_table <- function(ipms, db) {

  lams <- lambda(ipms)

  lam_tab <- list()

  for(i in seq_along(lams)) {

    spp <- db$Metadata$species_accepted[db$Metadata$ipm_id == names(lams)[i]]

    # Will need to add a naming function here to handle different metric
    # names whenever I do add that functionality (e.g. R_0, T, etc.)

    lam_tab[[i]] <- data.frame(
      spp_name = gsub("_", " ", spp),
      ipm_id = names(lams)[i],
      name   = names(lams[[i]]),
      lambda = round(lams[[i]], 3)
    )

  }

  lam_tab <- do.call(rbind, lam_tab)

  # Create evaluation environment for the rendering - We need to move the output
  # table, and the coordinates for the map to there so that they can be found
  # when the RMD file is knitted.

  ev_env <- new.env()
  ev_env$out_tab <- lam_tab
  ev_env$coords  <- db$Metadata[ , c("lat", "lon")] %>%
    .[complete.cases(.), ]


  # Same as above re: naming. col.names = c(...) should probably get its
  # own function.

  out_txt <- paste0(
    "\n\n# Demographic statistics for ",
    "`r paste(unique(out_tab$spp_name), collapse = ', ')`",
    "\n\n",
    "```{r echo = FALSE, message = FALSE, warning = FALSE}\n\n",
    'col_names <- c("Species Name", "ipm_id", "Lambda Name", "Lambda")\n\n',
    "kable(out_tab, col.names = col_names, row.names = FALSE)\n\n",
    "```"
  )

  list(env = ev_env,
       txt = out_txt)
}

pdb_insert_ipm_tab <- function(report, starting_index, ipm_table) {

  cit_ind  <- seq(starting_index, length(report), 1)
  cit_list <- report[cit_ind]
  temp_rep <- report[-cit_ind]

  c(temp_rep, ipm_table, cit_list)

}


# Re-scales the population state for plotting. This prevents the population
# time-series heat maps from getting swamped by large numbers in e.g. a seedbank
# and small transition probabilities everywhere else.

pdb_check_correct_ps_range <- function(pop_state) {

  rng <- range(pop_state$value)

  if(abs(diff(rng)) > 0.1) {
    if(max(pop_state$value) < 1) {
      pow <- 4
    } else {
      pow <- 0.1
    }
    pop_state$value <- pop_state$value ^ (pow)
  }

  return(pop_state)

}

# UI-------------

ui <- dashboardPage(
  skin = "green",

  dashboardHeader(title = "Padrino Database"),
  dashboardSidebar(id = "",
      menuItem("Home",           tabName = "home",      icon = icon("home"),
               selected = TRUE),
      menuItem("Metadata",       tabName = "metadata",  icon = icon("book")),
      menuItem("Maps",           tabName = "map",       icon = icon("map")),
      menuItem("Models",                                icon = icon("calculator"),
               menuSubItem("Tables",        tabName = "mod_tabs"),
               menuSubItem("Model Figures", tabName = "mod_figs"),
               menuSubItem("Reports",       tabName = "mod_reps"))

  ),

  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              includeMarkdown("www/home.md"),
              fluidRow(
                tableOutput("home_tab")
              )
      ),


      tabItem(tabName = "metadata",
              fluidRow(
                box(title = "Select Columns to Summarize Metadata by",

                    selectizeInput(inputId = "gr_by",
                                   label   = "Summarize By",
                                   choices = names(pdb[[1]]),
                                   multiple = TRUE)

                 ),
                dataTableOutput("sum_tab")
              )
      ), # End Metadata tab
      tabItem(tabName = "map",
              includeMarkdown("www/map.md"),
              fluidRow(
                box(
                  title = "Enter ipm_id's or Genus/Species Names to Map",
                  textInput(
                    inputId = "map_ids",
                    label   = "IPM ID's/Genus/Species Names",
                    value   = "all"),
                  actionButton(
                    inputId = "submit_map_id",
                    label = "Submit"
                  ),
                  downloadButton("map_dl")
                ),
                leafletOutput("map_out", height = 600)
              )
      ), # End Map tab
      tabItem(tabName = "mod_tabs",
              includeMarkdown("www/mods.md"),
              fluidRow(
                box(
                  title = "Enter ipm_id's or Genus/Species Names to Model",
                  textInput(
                    inputId = "mod_ids",
                    label   = "IPM ID's/Genus/Species Names",
                    value   = ""),
                  actionButton(
                    inputId = "submit_mod_id",
                    label = "Submit"
                  )
                ),
                dataTableOutput("demog_stats")
              )
      ),
      tabItem(tabName = "mod_figs",
              includeMarkdown("www/mods.md"),
              fluidRow(
                box(
                  title = "Enter ipm_id's or Genus/Species Names to Model",
                  textInput(
                    inputId = "fig_ids",
                    label   = "IPM ID's/Genus/Species Names",
                    value   = ""),
                  # checkboxInput("comp_evs", label = "Compute Eigenvectors?"),
                  actionButton(
                    inputId = "submit_fig_id",
                    label = "Submit"
                  )
                ),
                plotOutput("pop_TS",
                           height = "800px")
              )
      ),
      tabItem(tabName = "mod_reps",
              includeMarkdown("www/mods.md"),
              fluidRow(
                column(8,
                       box(
                         title = "Enter ipm_id's or Genus/Species Names to Generate Report",
                         textInput(
                           inputId = "rep_ids",
                           label   = "IPM ID's/Genus/Species Names",
                           value   = ""),
                         actionButton(
                           inputId = "submit_rep_id",
                           label = "Submit"
                         )
                       )
                ),
                column(4,
                       downloadButton("rep_dl")
                ),
                textOutput("rep_msg")
              )
      )
    ) # End Dashboard body tab items

  ) # End dashboard body
)

# Server ------

server <- function(input, output) {

  output$home_tab <- renderTable({

    kings <- pdb$Metadata %>%
      group_by(kingdom) %>%
      summarise(id = length(unique(ipm_id)),
                spp = length(unique(species_accepted)),
                pubs = length(unique(apa_citation)))

    tot <- pdb$Metadata %>%
      summarise(kingdom = "Totals",
                id = length(unique(ipm_id)),
                spp = length(unique(species_accepted)),
                pubs = length(unique(apa_citation)))


    tab <- rbind(kings, tot) %>%
      setNames(c(
        "Kingdom", "# of ipm_id's", "# of Species", "# of Publications"
      ))

    tab

  },
  striped = TRUE,
  bordered = TRUE)

  make_sum_tab <- reactive({

    gr_by <- syms(input$gr_by)

    out_tab <- pdb$Metadata %>%
      group_by(!!! gr_by) %>%
      summarise(id = length(unique(ipm_id)),
                spp = length(unique(species_accepted)),
                pubs = length(unique(apa_citation)),
                all_ids = paste(ipm_id, collapse = ", "))

    if(length(gr_by) == 0 ) {
      out_tab <- setNames(out_tab,
                          c("# of ipm_id's",
                            "# of Species",
                            "# of Publications",
                            "Associated ipm_id's"))
    } else {
      names(out_tab)[(length(gr_by) + 1):(ncol(out_tab))] <- c("# of ipm_id's",
                                                               "# of Species",
                                                               "# of Publications",
                                                               "Associated ipm_id's")
    }


    out_tab

  })

  make_col_widths <- reactive({

    gr_by <- syms(input$gr_by)

    n_cols <- length(gr_by) + 4

    wids <- c(rep("50px", n_cols), "250px")
    names(wids) <- rep("sWidth", n_cols)

    list(wids)
  })

  # Summary table for Metadata
  output$sum_tab  <- renderDataTable({

    out_tab <- make_sum_tab()

    out_tab
  },
  options = list(
    pageLength = 10,
    lengthMenu = seq(10, 50, by = 10),
    aoColumn = list(make_col_widths())))


  # Create dataset for maps
  map_pdb <- eventReactive(
    eventExpr = input$submit_map_id,
    valueExpr = {

      ipm_ids <- pdb_calculate_input_ipm_ids(pdb, input$map_ids)

      pdb_make_map_dataset(pdb, ipm_ids)

    },
    ignoreNULL = FALSE
  )

  # all_rv stores values that get updated by reactive events, but need to be
  # transferred across scopes (e.g. downloading a map created by renderLeaflet
  # with downloadHandler).

  all_rv <- reactiveValues(map_out = NULL)

  # Render the map, but store also store the result so downloadHandler can access
  # it for exporting.

  output$map_out <- renderLeaflet({

    use_dat <- map_pdb()

    labs <- pdb_map_lab_link(use_dat)

    all_rv$map <- leaflet(data = use_dat) %>%
      addTiles() %>%
      addMarkers(popup = labs,
                 clusterOptions = markerClusterOptions())

    all_rv$map
  })

  output$map_dl  <- downloadHandler(
    filename = "my_map.png",
    content  = function(file) {
      mapshot(all_rv$map, file = file)
    }
  )


  # This tracks which of the Model tabs has been submitted most recently.
  # Without it, one tab or another will have precedence and ID's won't update
  # if the subordinate one is submitted after the first one (though it would
  # work if the subordinate is submitted first and the user doesn't try to update
  # it afterwards (I don't think)).

  observe({
    input$submit_mod_id
    all_rv$submit_mod <- "1"
  })

  observe({
    input$submit_fig_id
    all_rv$submit_mod <- "2"
  })

  observe({
    input$submit_rep_id
    all_rv$submit_mod <- "3"
  })

  # Generates the requested IPMs based on inputs from user.

  mod_pdb <- eventReactive(
    eventExpr = {
      input$submit_mod_id
      input$submit_fig_id
    },
    valueExpr = {

      ids        <- switch(all_rv$submit_mod,
                           "1" = input$mod_ids,
                           "2" = input$fig_ids,
                           "3" = input$rep_ids)

      ipm_ids    <- pdb_calculate_input_ipm_ids(pdb, ids) %>%
        pdb_check_stochastic_models(db = pdb, ids = .)

      proto_list <- pdb_make_proto_ipm(pdb, ipm_ids)

      ipms       <- pdb_make_ipm(proto_list)

      conv_test  <- vapply(
        ipms,
        function(x, tol) all(is_conv_to_asymptotic(x, tolerance = tol)),
        logical(1L),
        tol = 1e-5)


    # If we have some that don't converge, try reiterating them
    if(any(!conv_test)) {

      reiterate_ids <- names(ipms[!conv_test])

      addl_args <- lapply(seq_along(reiterate_ids),
                          function(x) list(iterations = 200)) %>%
        setNames(reiterate_ids) %>%
        list()

      new_ipms <- pdb_make_proto_ipm(pdb, reiterate_ids) %>%
        pdb_make_ipm()

      for(i in seq_along(new_ipms)) {

        ipms[[reiterate_ids[i]]] <- new_ipms[[i]]

      }

    }

    ipms

  })


  output$demog_stats <- renderDataTable({

    mod <- mod_pdb()

    # Ps    <- get_ps(mod)
    # Fs    <- get_fs(mod)
    # Ns    <- get_Ns(mod)

    lam   <- lambda(mod)
    # R_0   <- R_0(Fs, Ns)
    # gen_T <- gen_T(R_0, lam)

    # if(input$comp_evs){
    #   r_evs <- right_ev(mod)
    #   l_ev  <- left_ev(mod)
    # }

    for(i in seq_along(lam)) {
      spp <- pdb$Metadata$species_accepted[pdb$Metadata$ipm_id == names(lam)[i]]
      lam[[i]] <- data.frame(
        spp_name = gsub("_", " ", spp),
        ipm_id = names(lam)[i],
        name   = names(lam[[i]]),
        lambda = round(lam[[i]], 3)
      )

    }

    do.call(rbind, lam)

  },
  options = list(
    pageLength = 10,
    lengthMenu = seq(10, 50, by = 10)))

  output$pop_TS <- renderPlot({

    mod <- mod_pdb()

    ps  <- pop_state(mod) %>%
      pdb_prep_ps_par_sets(pdb) %>%
      lapply(pdb_mat_to_df) %>%
      ipmr:::.flatten_to_depth(1L) %>%
      lapply(function(x) {
        temp <- ipm_to_df(x)
        pdb_check_correct_ps_range(temp)
      })


    plt_list <- list()

    for(i in seq_along(ps)) {

      pp     <- ps[[i]]
      n_bins <- max(pp$t_1)

      plt_list[[i]] <- ggplot(pp, aes(x = t, y = t_1)) +
        geom_tile(aes(fill = value)) +
        xlab("Time Step (t)") +
        ylab("Bin (z)") +
        scale_y_continuous(breaks = round(seq(0, n_bins, length.out = 4), 0),
                           labels = round(seq(0, n_bins, length.out = 4), 0)) +
        theme_bw() +
        theme(panel.background = element_blank(),
              panel.grid       = element_blank(),
              axis.text        = element_text(size = 16),
              axis.title       = element_text(size = 18)) +
        scale_fill_continuous("Population Count or Density") +
        ggtitle(names(ps)[i])

    }

    dims <- integer(2L)
    if(length(plt_list) > 4) {

      dims[1:2] <- round(sqrt(length(plt_list)), 0)

      # Sometimes sqrt(length) gets rounded down. In that case, floor + ceiling
      # the dimensions to make sure (n_row * n_col) > n_plots.
      # NB: will probably break for large n_plots, but that layout will look
      # heinous anyway. Maybe want to add a check/warning/error for when
      # n_plots > 25 or something?
      if(dims[1]^2 < length(plt_list)) {
        dims[1] <- ceiling(sqrt(length(plt_list)))
        dims[2] <- floor(sqrt(length(plt_list)))
      }

    } else {

      dims <- c(1, length(plt_list))

    }

    grid.arrange(grobs = plt_list, nrow = dims[1], ncol = dims[2])

  })

  output$rep_msg <- eventReactive(
    eventExpr = input$submit_rep_id,
    valueExpr = {

      "\n\nClick the Download button to prepare your report."
  })

  output$rep_dl  <- downloadHandler(
    filename = "report.pdf",
    content  = function(file) {

      ids       <- pdb_calculate_input_ipm_ids(pdb, input$rep_ids)
      use_db    <- pdb_subset(pdb, ids)

      pdb_download_report(
        use_db,
        ids,
        dest = file
      )

    }
  )

}



shinyApp(ui = ui, server = server)
