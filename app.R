# Packages
library(shiny)
library(dplyr)
library(tidyr)
library(rhdf5)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(dipsaus)
library(waiter)
library(DT)
library(bslib)
library(shinyBS)
library(shinyjs)


## Define constants
station_list = c(
  "1 Surprise",
  "2 Longoni",
  "3 Hajangoua",
  "4 Passe en S bouée 3",
  "5 Passe en S ext",
  "6 Saziley",
  "7 Dble barrière Sud",
  "8 Passe bateau",
  "9 Bouéni",
  "11 Tanaraki"
)

temp_min_expected = 25
temp_max_expected = 35

depth_min_expected = 2
depth_max_expected = 10


# Code dependencies
source("code/code_dependencies/00-read-write_HDF5.R")
source("code/code_dependencies/01-temporal_selection.R")
source("code/code_dependencies/02-graph_display.R")
source("code/code_dependencies/03-ACF_plot.R")
source("code/code_dependencies/04-ui.R")
source("code/code_dependencies/05-new_data_file_cleaning.R")


# Define UI for application
ui <- fluidPage(
  theme = custom_theme,
  waiterPreloader(),
  useWaiter(),
  useShinyjs(),
  
  navbarPage(
    title = "",
    windowTitle = "ReefTemp Monitor",
    tabPanel(
      "Data visualisation",
      pageWithSidebar(
        customHeaderPanel(windowTitle = "Mayotte ReefTemp Monitor", title = "Mayotte ReefTemp Monitor"),
        sidebarPanel(
          div(
            id = "data_selection",
            tags$h4("Informations spatiales"),
            selectInput(
              inputId = "station_selected",
              label = "Sélectionner la station",
              choices = c(station_list, "Toutes les stations")
            ),
            
            hr(),
            tags$h4("Information temporelles"),
            
            dateRangeInput(inputId = "date_range", label = "Sélectionner la plage temporelle"),
            selectInput(
              inputId = "temporal_res",
              label = "Sélectionner la résolution temporelle",
              choices = c("quotidien", "mensuel", "annuel")
            ),
            
            hr(),
            tags$h4("Variable"),
            
            selectInput(
              inputId = "variable",
              label = "Sélectionner la variable à afficher",
              choices = c("Température", "Profondeur")
            )
          ),
          
          
          hr(),
          fluidRow(
            actionButton(
              inputId = "start_display",
              label = "Afficher les données",
              class = "btn-success"
            ),
            br(),
            tags$i(
              "Veuillez noter qu'en fonction des données demandées, le traitement peut prendre beaucoup de temps!"
            ),
            align = "center",
            
            actionButton("resetAll", "Réinitialiser la sélection"),
            
            hr(),
            
            actionButton(
              inputId = "exit_button",
              label = "Quitter l'application",
              class = "btn-secondary"
            )
          )
          
        ),
        
        
        mainPanel(layout_column_wrap(height = "750px", card(
          full_screen = T, div(id = "data_display", tabsetPanel(
            # Show a plot
            tabPanel(title = "Série temporelle", plotOutput("temp_data")),
            # Download data
            
            tabPanel(
              title = "Téléchargement des données",
              
              verbatimTextOutput("df_preview"),
              hr(),
              textOutput(outputId = "dim_info"),
              
              downloadButton(outputId = "data_download", label = "Télécharger les données")
            ),
            tabPanel(
              title = "Autocorrélation temporelle",
              plotOutput("plot_acf", height = "650px"),
              hr(),
              uiOutput("acf_explanation")
            )
          ))
        )))
      )
    ),
    tabPanel(
      "Charger de nouvelles données",
      sidebarLayout(
        # Sidebar panel for inputs
        sidebarPanel(
          div(
            id = "data_upload",
            
            # Input: Select station
            selectInput(
              inputId = "station_new_data",
              label = "Sélectionner la station",
              choices = station_list
            ),
            
            # Input: Select a file
            fileInput(
              "file1",
              "Choisir le fichier CSV",
              multiple = FALSE,
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
            ),
            tags$hr(),
            fluidRow(
              column(
                3,
                
                # Input: Checkbox if file has header ----
                checkboxInput("header", "En-tête", TRUE),
                
                # Input: Select separator ----
                radioButtons(
                  "sep",
                  "Séparateur",
                  choices = c(
                    "Virgule" = ",",
                    "Point virgule" = ";",
                    "Tabulation" = "\t"
                  ),
                  selected = ";"
                ),
                
                # Input: Select Decimal ----
                radioButtons(
                  "decimal",
                  "Séparateur de décimal",
                  choices = c("Point" = '.', "Virgule" = ","),
                  selected = ','
                ),
              ),
              
              column(
                8,
                uiOutput("ColumnSelector_time"),
                uiOutput("ColumnSelector_temp"),
                uiOutput("ColumnSelector_depth"),
                uiOutput("depth_bar")
              )
            ),
            # Horizontal line ----
            tags$hr(),
            fluidRow(
              actionButton(
                inputId = "new_data_summary",
                label = "Générer un résumé des données",
                class = "btn-success"
              ),
              br(),
              actionButton(inputId = "resetupload", label = "Réinitialiser"),
              hr(),
              downloadButton(outputId = "HDF_DB_download", label = "Télécharger une sauvegarde des données brutes"),
              hr(),
              actionButton(
                inputId = "exit_button",
                label = "Quitter l'application",
                class = "btn-secondary"
              )
            )
          )
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
          layout_columns(
            card(full_screen = T, tableOutput("new_contents")),
            card(full_screen = T, plotOutput("new_contents_graph"))
          ),
          # Horizontal line ----
          tags$hr(),
          
          # Output: Validation file
          fluidRow(
            uiOutput("double_check"),
            uiOutput("new_data_validation"),
            align = "center"
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # define temporary file location
  temp = tempfile()
  
  # Output download full dataset button
  output$HDF_DB_download <- downloadHandler(
    filename = paste(paste("SAVE_full_dataset_", Sys.time(), sep = ""), ".h5", sep = ""),
    content = function(fname) {
      file.copy("data/data_station.h5", fname)
    }
  )
  
  # Outliers flag (True if any)
  outliers_flag = reactiveValues(value = FALSE)
  
  # Create the waiter
  loading_screen = Waiter$new(fadeout = T, html = spin_three_bounce())
  
  # Acf explanation
  output$acf_explanation = ACF_explanation()
  
  #Reset button
  observeEvent(input$resetAll, {
    reset("data_selection")
    reset("data_display")
    data_temp = reactiveVal({
      NULL
    })
    output$temp_data = renderPlot(NULL)
    output$plot_acf = renderPlot(NULL)
    output$df_preview = renderPrint("No data selected")
    output$dim_info = renderText(NULL)
    output$data_download = downloadHandler(
      filename = function() {
        paste(
          paste(
            input$station_selected,
            input$temporal_res,
            input$date_range[1],
            input$date_range[2],
            sep = "_"
          ),
          ".csv",
          sep = ""
        )
      },
      content = NULL
    )
  })
  
  observeEvent(input$resetupload, {
    outliers_flag$value = FALSE
    reset("data_upload")
    reset("file1")
    reset("double_verification")
    reset("data_addition")
    input_file$data = NULL
    output$new_contents_graph = renderPlot(NULL)
    output$new_data_validation = renderUI(NULL)
    output$double_check = renderUI(NULL)
  })
  
  #Read data from station
  observeEvent(input$start_display, {
    loading_screen$show()
    
    data_temp = reactiveVal({
      NULL
    })
    
    start = as.POSIXct(paste(input$date_range[1], "00:00:00", sep = " "))
    end = as.POSIXct(paste(input$date_range[2], "23:00:00", sep = " "))
    
    if (input$station_selected == "Toutes les stations") {
      for (station_selected in station_list) {
        print(station_selected)
        
        data_station = temporal_selection(
          dataset = station_read(station_name = station_selected),
          start = start,
          end = end,
          temporal_res = input$temporal_res
        )
        data_station$station_name = station_selected
        
        temp_df = rbind(data_temp(), data_station)
        data_temp(temp_df)
        
      }
      
    } else{
      data_station = temporal_selection(
        dataset = station_read(station_name = input$station_selected),
        start = start,
        end = end,
        temporal_res = input$temporal_res
      )
      data_station$station_name = input$station_selected
      
      
      temp_df = rbind(data_temp(), data_station)
      data_temp(temp_df)
      
      print(head(data_temp()))
    }
    
    output$temp_data <- renderPlot({
      graph_TS(data_temp = data_temp(), variable = input$variable)
    }, height = 650)
    
    loading_screen$hide()
    
    # Preview and download data
    
    dim_data = reactive({
      paste(
        "Les données à télécharger contiennent",
        nrow(na.omit(data_temp())),
        "enregistrements",
        sep = " "
      )
    })
    
    # Output summary table
    
    output$df_preview <- renderPrint({
      summary(na.omit(data_temp()))
    })
    
    # Output data information
    output$dim_info <- renderText(dim_data())
    
    # Output download button
    output$data_download <- downloadHandler(
      filename = function() {
        paste(
          paste(
            input$station_selected,
            input$temporal_res,
            input$date_range[1],
            input$date_range[2],
            sep = "_"
          ),
          ".csv",
          sep = ""
        )
      },
      content = function(fname) {
        write.table(
          data_temp(),
          fname,
          sep = ",",
          fileEncoding = "UTF8",
          row.names = F,
          dec = "."
        )
      }
    )
    
    
    
    
    ## Display ACF
    output$plot_acf <- renderPlot({
      temporal_autocor_plot(
        time_serie_data = data_temp(),
        variable = input$variable,
        station_selected = input$station_selected
      )
    }, height = 650)
  })
  
  # New data upload
  input_file = reactiveValues(data = NULL)
  
  observe({
    req(input$file1)
    try(input_file$data <- read.csv(
      input$file1$datapath,
      header = input$header,
      sep = input$sep,
      dec = input$decimal,
      na.strings = ""
    ))
  })
  
  #Preview data
  output$new_contents <- renderTable({
    head(input_file$data, 12)
  })
  
  # Interactive selectors
  output$ColumnSelector_time <- renderUI({
    popify(
      selectInput(
        inputId = "time_col",
        label = "Données temporelles",
        choices = colnames(input_file$data)
      ),
      title = "Sélection des données",
      placement = "right",
      content = "Sélectionner la colonne contenant les données <p>temporelles</p>."
    )
  })
  
  output$ColumnSelector_temp <- renderUI({
    popify(
      selectInput(
        inputId = "temp_col",
        label = "Données de température",
        choices = colnames(input_file$data)
      ),
      title = "Sélection des données",
      placement = "right",
      content = "Sélectionner la colonne contenant les données de température"
    )
  })
  
  output$ColumnSelector_depth <- renderUI({
    popify(
      selectInput(
        inputId = "depth_col",
        label = "Données de profondeur",
        choices = colnames(input_file$data)
      ),
      title = "Sélection des données",
      placement = "right",
      content = "Sélectionner la colonne contenant les données de profondeur"
    )
  })
  
  output$depth_bar <- renderUI({
    popify(
      checkboxInput(inputId = "depth_bar", label = "Profondeur en bar"),
      title = "Sélection de données",
      placement = "bottom",
      content = "Cochez cette case si les données de profondeur sont exprimées en bar au lieu de mètre."
    )
  })
  
  new_col_names = reactive({
    c(input$time_col, input$temp_col, input$depth_col)
  })
  
  # output$new_col_name <- renderPrint({
  #   new_col_names()
  # })
  
  # Generate summary
  observeEvent(input$new_data_summary, {
    selected_file = reactive({
      select(input_file$data, new_col_names())
    })
    
    ## Security if duplicates colnames
    if (length(unique(new_col_names())) != 3) {
      showModal(modalDialog(
        title = "ERREUR",
        tags$h3(
          "Le nombre de colonnes et le nombre de variables ne correspondent pas !"
        ),
        easyClose =  FALSE
      ))
    } else{
      ## Rename the columns
      new_data = selected_file() %>% rename(
        "Time" = input$time_col,
        "Temperature" = input$temp_col,
        "Depth" = input$depth_col
      )
      
      ## Clean the data
      new_data = na.omit(tail_cleaning(new_data, "#"))
      
      ## Round data to to digits
      new_data$Temperature = as.numeric(new_data$Temperature) %>% round(digits = 2)
      new_data$Depth = as.numeric(new_data$Depth) %>% round(digits = 2)
      #print(head(new_data))
      new_data = new_data[str_detect(rownames(new_data), "NA", negate = T), ]
      #print(tail(new_data))
      
      
      ## Security if decimal not correct (in this case, )
      if (dim(na.omit(new_data))[1] == 0) {
        showModal(modalDialog(
          title = "ERREUR",
          tags$h3("Aucune données numériques détectées, veuillez vérifier la sélection du séparateur de décimales !"),
          easyClose = FALSE
        ))
        
      } else{
        ## Round Hours and convert to time format
        if (isTRUE(time_formatting(new_data))) {
          showModal(modalDialog(
            title = "ERREUR",
            tags$h3("Veuillez vérifier la sélection des données !"),
            easyClose = FALSE
          ))
        } else{
          new_data = time_formatting(new_data) %>% arrange(Time)
          
          ## Convert depth to meters
          # 1 Bar = 10.1974 msw
          # Remove 1 bar for atmospheric pressure
          if (input$depth_bar) {
            new_data$Depth = (new_data$Depth - 1) * 10.1974
          }
          
          ## Plot data
          output$new_contents_graph <- renderPlot({
            graph_preview(new_data)
          }, height = 750)
          
          ## Check outliers
          temp_outliers = temperature_outliers_detection(dataframe = new_data,
                                                         lim_inf = temp_min_expected,
                                                         lim_sup = temp_max_expected)
          
          depth_outliers = depth_outliers_detection(dataframe = new_data,
                                                    lim_inf = depth_min_expected,
                                                    lim_sup = depth_max_expected)
          
          output$outlier_temp <- renderDataTable(temp_outliers)
          output$outlier_depth <- renderDataTable(depth_outliers)
          
          ## Summary message
          showModal(
            modalDialog(
              size = "l",
              title = "Validation des données",
              tags$h3(
                "Veuillez vérifier le résumé des données avant la validation !"
              ),
              br(),
              
              # Generate a summary of the dataset
              renderPrint({
                summary(new_data)
              }),
              # Show outliers
              if (nrow(depth_outliers) > 0) {
                outliers_flag$value = TRUE
                renderUI({
                  tagList(
                    tags$hr(),
                    tags$h4(
                      "Des valeurs aberrantes de profondeur sont détectées !"
                    ),
                    tags$h5(
                      "La profondeur devrait être comprise entre 2 et 10 mètres"
                    )
                  )
                })
              },
              if (nrow(depth_outliers) > 0) {
                renderUI({
                  dataTableOutput("outlier_depth")
                })
              },
              
              if (nrow(temp_outliers) > 0) {
                outliers_flag$value = TRUE
                renderUI({
                  tagList(
                    tags$hr(),
                    tags$h4(
                      "Des valeurs aberrantes de température sont détectées !"
                    ),
                    tags$h5(
                      "La température devrait être comprise entre 25°C et 35°C"
                    )
                  )
                })
              },
              if (nrow(temp_outliers) > 0) {
                renderUI({
                  dataTableOutput("outlier_temp")
                })
              },
              renderUI(tags$hr()),
              renderUI(renderPlot({
                graph_preview(new_data)
              })),
              easyClose = FALSE,
              footer = modalButton("Fermer le résumé")
            )
          )
          
          ## Button validation
          output$double_check = renderUI({
            checkboxInput(value = FALSE,
                          inputId = "double_verification",
                          label = "Les données sont vérifiées et prêtes à être importées")
          })
          
          output$new_data_validation = renderUI({
            actionButtonStyled(
              inputId = "validation_button",
              label = "AJOUTER LES DONNÉES",
              btn_type = "button",
              type = "success",
              class = "btn-lg"
            )
          })
          
        }
      }
      # Save new data as temporary file
      save(new_data, file = paste(temp, ".rda", sep = ""))
    }
  })
  
  # Validation new data addition
  observeEvent(input$validation_button, {
    ### Load the temporary new file
    load(paste(temp, ".rda", sep = ""))
    
    
    if (input$double_verification) {
      if (outliers_flag$value) {
        showModal(
          modalDialog(
            title = "Valeurs aberrantes détectées",
            tags$h3(
              "Des valeurs aberrantes ont été signalées. Êtes-vous sûr de vouloir ajouter ces données à la base de données ?"
            ),
            easyClose = FALSE,
            footer = tagList(
              modalButton("Annuler"),
              actionButton("data_addition", "Ajouter les données")
            )
          )
        )
      } else{
        ### Save the database => Download the previous hdf5 file
        click("HDF_DB_download")
        
        ### Write the new data in the current HDF5
        # rename columns
        new_data = new_data %>% rename(depth = Depth,
                                       temperature = Temperature,
                                       timestamp = Time)
        new_data$station_name = input$station_new_data
        # Merge previous data
        old_data = station_read(input$station_new_data)
        full_data = rbind(old_data, new_data) %>% rename(temperature_c = temperature, )
        full_data$timestamp = as.character(full_data$timestamp)
        full_data$timestamp = str_replace_all(full_data$timestamp, "-", "/")
        
        #write
        station_write(station_name = input$station_new_data,
                      dataset = full_data)
        
        # Acknowledge
        showNotification("Nouvelles données ajoutées")
        removeModal()
        click("resetupload")
      }
    } else{
      showNotification("Veuillez cocher la case de vérification !")
    }
  })
  
  observeEvent(input$data_addition, {
    ### Load the temporary new file
    load(paste(temp, ".rda", sep = ""))
    
    ### Save the database => Download the previous hdf5 file
    click("HDF_DB_download")
    
    ### Write the new data in the current HDF5
    # rename columns
    new_data = new_data %>% rename(depth = Depth,
                                   temperature = Temperature,
                                   timestamp = Time)
    new_data$station_name = input$station_new_data
    
    print(unique(new_data$station_name))
    
    # Merge previous data
    old_data = station_read(input$station_new_data)
    full_data = rbind(old_data, new_data) %>% rename(temperature_c = temperature, )
    full_data$timestamp = as.character(full_data$timestamp)
    full_data$timestamp = str_replace_all(full_data$timestamp, "-", "/")
    
    #write
    station_write(station_name = input$station_new_data,
                  dataset = full_data)
    
    # Acknowledge
    showNotification("Nouvelles données ajoutées")
    removeModal()
    click("resetupload")
  })
  
  # Exit button
  observeEvent(input$exit_button, {
    showModal(
      modalDialog(
        title = "Fermeture de l'application",
        tags$h3("Êtes-vous sûr de vouloir fermer l'application ?"),
        easyClose = T,
        footer = tagList(
          modalButton("Annuler"),
          actionButton("final_close", "Fermer l'application", class = "btn-success")
        )
      )
    )
  })
  
  observeEvent(input$final_close, {
    stopApp()
  })
}

# Run the application
shinyApp(ui = ui, server = server)