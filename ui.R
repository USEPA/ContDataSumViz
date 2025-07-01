# This is the user-interface definition of ContDataSumViz Shiny web application. You can
# run the application by clicking 'Run App' above.

app_jscode <-
  "shinyjs.disableTab = function(name) {
    var tab = $('.nav li a[data-value=' + name + ']');
    $(tab).css({'visibility' : 'hidden' })
   // $(tab).hide();
  }
  shinyjs.enableTab = function(name) {
    var tab = $('.nav li a[data-value=' + name + ']');
    $(tab).css({'visibility' : 'visible' })
    // $(tab).show();
  }"

# Define UI for application
options(spinner.color.background = "#ffffff", spinner.size = 1)
ui <- shinyUI(fluidPage(
  theme = "styles.css",
  useShinyjs(),
  shinyjs::extendShinyjs(text = app_jscode, functions = c("disableTab","enableTab")),
  tags$head(tags$script(src="script.js")),
  tags$head( tags$link(rel="stylesheet", type="text/css", href="app.css")),
  tags$head(tags$link(rel="icon", type="mage/x-icon", href="https://www.epa.gov/themes/epa_theme/images/favicon.ico")),
  tags$head(tags$link(rel="icon", type="mage/x-icon", href="favicon.ico")),

  # Start EPA formatting 
  tags$html(class = "no-js", lang="en"),
  head_tag(),
  tags$body(class="path-themes not-front has-wide-template", id="top",
            tags$script(src = 'https://cdnjs.cloudflare.com/ajax/libs/uswds/3.0.0-beta.3/js/uswds.min.js')),

  # Site Header
  custom_header(),

  # Individual Page Header
  page_header(),

  #titlePanel("ContDataSumViz: Visualization and Summary Statistics for Continuous Monitoring Data"),
  h2(id = "apptitle", 
     "ContDataSumViz: Visualization and Summary Statistics for Continuous Monitoring Data",
     style = "box-shadow: 0px 8px 8px rgba(0, 0, 0, 0.1) !important;"),
  
  mainPanel(
    width = 12,
    # spacing
    fluidRow(id = "one", ),
    # top controls
    fluidRow(
      div(id = "customBusy", class = "loading-modal")
    ),
    # fluidRow(
    #   div(span(style="float:right",
    #            a(tags$button(tags$i(class="fas fa-arrow-down"), "Download test data", class="btn btn-primary btn-sm", style = "font-size:16px;"), href="TestData.zip", target="_blank")))
    # ),
    fluidRow(
      column(
        width = 10,
        div("See User Guide for instruction to begin. To start over with a new dataset, refresh the page and upload a new dataset.", class = "text-info", style = "font-weight:bold;"),
        br()
      ),
      column(width = 2, 
             div(span(style="float:right",
                      a(tags$button(tags$i(class="fas fa-arrow-down"), "Download test data", class="btn btn-primary btn-sm", style = "font-size:16px;"), href="TestData.zip", target="_blank"))))
    ),
    fluidRow(
      column(width = 12,
             div(progressWorkflowModuleUI("statusWorkflow"), style = "margin-bottom: 20px"))
    ),
    fluidRow(
      p(),
      tabsetPanel(id="mainTabs",
                  selected = "uploadData",

                  tabPanel(
                    title="User Guide",
                    value="userguide",
                    fluidPage(
                      div(a(tags$button(tags$i(class="fas fa-arrow-down"), "Download full user guide", class="btn btn-primary btn-sm",  style = "font-size:16px;"), href="ContDataSumViz User Guide.docx", target="_blank"), style = "margin-bottom:20px;"),
                        includeHTML("Guidance_Tab.html")
                    ) # page
                  ),

                  tabPanel(
                    title="Upload Data",
                    value="uploadData",
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          div(class="panel panel-default", style="margin:10px;",
                              div(class="panel-heading", "Step 1: Upload continuous data", style="font-weight:bold;", icon("info-circle", style = "color:#2fa4e7", id="fileHelp")),
                              div(class="panel-body",
                                  tagList(
                                    bsPopover(id="fileHelp", title=HTML("<b>Helpful Hints</b>"), content = HTML("Files must contain only one mandatory header row containing column names. A site identifier, date and time (in one column or two), and one or more continuous parameters are required columns. Column(s) indicating the quality of each parameter observation can also be used to filter the data used in subsequent calculations, but this column is not required. </br></br>  Microsoft Excel corrupts .csv files when reopened by double clicking its icon or by using the File Open dialog. You can avoid this by using the Text or Data Import Wizard from the Excel Data Tab"),
                                              placement = "right", trigger = "hover"),
                                    fileInput("uploaded_data_file",
                                              label = HTML("<b>Upload your continuous data in .csv format</b>"),
                                              multiple = FALSE,
                                              buttonLabel=list(tags$b("Browse"),tags$i(class = "fa-solid fa-folder")),
                                              accept = c(
                                                "text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv"
                                              )
                                    )),
                                  uiOutput("displayFC")
                                  
                              )),
                          tagList(
                            uiOutput("display_paramselect"),
                            uiOutput("dt_warning"),
                            uiOutput("display_subsetTS"),
                            uiOutput("display_runmetasummary"),
                            uiOutput("display_actionButton_calculateDailyStatistics"),
                            uiOutput("display_actionButton_saveDailyStatistics"),
                            uiOutput("step5")
                          )
                        ),
                        mainPanel(
                          width = 9,
                          div(uiOutput("contents"), style = "overflow-x:auto;margin:0px 15px 0px 15px;"),
                          div(uiOutput("ts_right"), style = "margin-top:20px;height:700px;overflow-y:auto"),
                          br(),
                          div(uiOutput("display_fill_data"))
                        ) # mainPanel end
                      ) # sidebarLayout end
                    ) # fluidPage close
                  ), # tabPanel end
                  
                  tabPanel(
                    title="USGS & Daymet Exploration",
                    value="downloadData",
                    fluidPage(
                      fluidRow(
                        column(
                          width = 12,GageAndDaymetModuleUI("gageDaymetAndBase")
                        )# column close
                      ) # raw
                    ) # page
                  ),
                  tabPanel(
                    title="Discrete Data Exploration",
                    value="discreateDataEx",
                    column(
                      width = 12,
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          div(class="panel panel-default",style="margin:10px;",
                              div(class="panel-heading", "Upload discrete data in .csv format", style="font-weight:bold;", icon("info-circle", style = "color: #2fa4e7", id="discreteHelp")),
                              div(class="panel-body",
                                  tagList(
                                    bsPopover(id="discreteHelp", title="Discrete data rules", content = "The column headings for the parameter\\(s\\) you are matching must be the same in the discrete and continuous data files\\. The date\\(s\\) and time\\(s\\) of the discrete vs. continuous measurements do not need to match.",
                                              placement = "right", trigger = "hover"),
                                    fileInput("uploaded_discrete_file",
                                              label = NULL,
                                              multiple = FALSE,
                                              buttonLabel=list(tags$b("Browse"),tags$i(class = "fa-solid fa-folder")),
                                              accept = c(
                                                "text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv"
                                              ),

                                    ),
                                    hr(),
                                    uiOutput("baseParameters"),
                                    uiOutput("gage_daymet_discrete"),
                                    uiOutput("gage_select_discrete"),
                                    uiOutput("daymet_select_discrete"))
                              ),
                              div(uiOutput("discreteDateAndTimeBox"))
                          ),
                        ),
                        mainPanel(
                          width = 9,
                          fluidRow(width = 12, 
                                   shinydashboard::box(id="discrete_data_help", width=12, class="well",
                                                       h4("Dicrete Data Exploration"),
                                                       div(style="width:100%;", "Overlay high frequency sensor data with discrete data (e.g., grab samples) containing the same parameters in an interactive time series."),
                                                       br(),
                                                       div(style = "width:100%", "To begin, upload a file containing discrete data containing at least one matching parameter to the previously uploaded continuous data, and provide the date and time format."))),
                          fluidRow(width = 12, uiOutput("discreteHeader")),
                          fluidRow(column(width = 12,
                                          withSpinner(plotlyOutput("display_time_series_discrete"), type=1))
                          )
                        ) # mainPanel end
                      ) # sidebarLayout end
                    ), # column close
                  ),
                  # Data Exploration ----
                  tabPanel(
                    title="Continuous Data Exploration",
                    value="DataExploration",
                    fluidPage(
                      fluidRow(
                        tabsetPanel(
                          id = "tabset",
                          tags$head(tags$style(HTML(".radio-inline {margin-right: 40px;}"))),
                          ## DE, All Parameters ----
                          tabPanel("Any parameters",
                                   value = "all_parameters_tab", br(),
                                   tabsetPanel(
                                     id = "all_parameters_subtabs",
                                     ### DE, All, Summary Tables ----
                                     tabPanel("Summary tables",
                                              value = "tab_summary_tables",
                                              br(),
                                              br(),
                                              column(
                                                width = 12,
                                                SummaryTablesModuleUI("DataExpSummaryTbls")
                                              ), # column close
                                              br(),
                                     ), # tabPanel 1 end
                                     ### DE, All, TS Plots ----
                                     tabPanel("Daily summary plots",
                                              value = "tab_time_series", br(),
                                              column(
                                                width = 12,
                                                DataExplorationTSModuleUI(id="dataExpTS")
                                              ), # column close

                                     ), # tabPanel 2 end

                                     ### DE, All, TS Annual----
                                     tabPanel("Time series - Annual overlays",
                                              value = "tab_time_series_overlay", br(),
                                              column(
                                                width = 12,
                                                TsOverlayModuleUI(id="tsOverlayTab")
                                              ), # column close
                                     ), # tabPanel 3 end

                                     ### DE, All, Box Plots----
                                     tabPanel("Box plots",
                                              value = "tab_box", br(),
                                              column(
                                                width = 12, TsBoxPlotModuleUI(id="tsBoxPlot")
                                              ), # column close
                                     ), # tabPanel 4 end

                                     ### DE, All, CDFs ----
                                     tabPanel("CDFs",
                                              value = "tab_CDF", br(),
                                              column(
                                                width = 12, TsCDFPlotModuleUI(id="tsCDFPlot")
                                              ), # column close
                                              br(),
                                     ), # tabPanel 5 end

                                     ### DE, All, Raster Graphs ----
                                     tabPanel("Raster graphs",
                                              value = "tab_raster", br(),
                                              column(
                                                width = 12, TsRasterPlotModuleUI(id="tsRasterPlot")
                                              ), # column close
                                     ) # tabPanel 6 end Raste
                                   ) # inner tabsetPanel end
                          ), # tabPanel end


                          # DE, Temperature ----
                          tabPanel("Temperature",
                                   value = "temp_tab",br(),
                                   tabsetPanel(
                                     id = "temp_subtabs",
                                     ### DE, Temp, Thermal Stats----
                                     tabPanel("Thermal statistics",
                                              value = "sb1", br(),
                                              column(
                                                width = 12,ThermalStatsModuleUI("thermalStats")
                                              ), # column close
                                     ),
                                     ### DE, Temp, Air v Water ----
                                     tabPanel("Air vs. water",
                                              value = "sb2", br(),
                                              column(
                                                width = 12, AirVsWaterModuleUI("airVsWater")
                                              ) # column close
                                     ), # AW end

                                     ### DE, Temp, GDD ----
                                     tabPanel("Growing degree days",
                                              value = "sb3", br(),
                                              GrowingDegreeModuleUI("growingDegree")
                                     ), # GDD, end
                                     ### DE, Temp, Therm Class ----
                                     tabPanel("Thermal classification",
                                              value = "sb4", br(),
                                              br(),
                                              column(
                                                width = 12,ThermalClassificationModuleUI(id="thermalClassification")
                                              ) # column close
                                     ), # Termal class, end
                                     tabPanel("Temperature not to exceed",
                                              value = "sb5", br(),
                                              br(),
                                              column(
                                                width = 12,TempNotToExceedUI(id="tempNTE")
                                              ) # column close
                                     ) # Temp nte class, end
                                   )
                          ), # outer tabPanel end temperature

                          ## DE, Hydrology ----
                          tabPanel("Hydrology",
                                   value = "hydro_tab",br(),
                                   tabsetPanel(
                                     id = "hydro_subtabs",
                                     ### DE, Hydro, IHA----
                                     tabPanel("Download USGS daily flow", br(),
                                              value = "USGSDaily_tab", br(),
                                              column(
                                                width = 12, USGSDailyModuleUI("USGSDailyTab")
                                              )),
                                     tabPanel("IHA",
                                              value = "IHA_tab", br(),
                                              column(
                                                width = 12, IHAModuleUI("IHATab")
                                              ) # column close
                                     ), # tabpanel, end, IHA

                                     ### DE, Hydro, Flashiness ----
                                     tabPanel("Flashiness",
                                              value = "Flashiness_tab", br(),
                                              br(),
                                              column(
                                                width = 12, FlashinessModuleUI("flashinessTab")
                                              ) # column close
                                     ) # tab panel Hydro flash end
                                   )
                          ) # Hydro, end
                        ) # tabsetPanel end
                      ) # fluidRow close
                    ) # fluidPage close
                  )# tabPanel end Data exploration
      ) # tabsetPanel close
    ),
    fluidRow(column(width=12))
  ),

  # Page Footer
  page_footer(),

  # Site Footer
  custom_footer(),
))
