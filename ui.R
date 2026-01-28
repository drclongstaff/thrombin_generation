library(shiny)
library(dplyr)
library(tibble)
library(purrr)
library(janitor)
library(readxl)
library(vroom)

# library(shinythemes)# Load the library
# library(thematic)

source("./Functions/Session_info.R")
source("./Functions/Load_file.R")
source("./Functions/Polynomials.R")

Thisversion <- "1.001dx4"
thematic::thematic_shiny()

fluidPage( # Setup the UI page
  theme = shinythemes::shinytheme("lumen"),
  tags$script(
    src = "https://cdn.jsdelivr.net/gh/Appsilon/shiny.tictoc@v0.2.0/shiny-tic-toc.min.js"
  ),
  # Add a title
  titlePanel("Thrombin Generation Assays"),
  sidebarLayout(
    sidebarPanel(
      style = "background: #FFF6FB",
      tags$h3("Calibrator Data"),
      fluidRow(
        column(8, fileInput("data0", label = "calibrator csv, txt or xlsx")),
        column(4, numericInput("sheetc", label = "Excel sheet", value = 1, step = 1))
      ),
      fluidRow(
        column(8, textInput("select_cols", label = "Select columns", "2,3")),
        column(4, numericInput("skipc",
          label = "skip rows", value = 0
        ))
      ),
      fluidRow(
        column(5, uiOutput("whatCal")),
        column(7, numericInput("limitD", label = "points for fitting", value = 65, step = 5))
      ),
      fluidRow(
        column(5, numericInput("CalibT", label = "calibrator conc", value = 110, step = 10)),
        column(7, numericInput("calSlope", label = "calibrator max rate", value = 73, step = 10)) # ,
      ),
      tags$h3("Sample Data"),
      fluidRow(
        column(8, fileInput("data1", label = "sample csv, txt, xlsx")),
        column(4, numericInput("sheetd", label = "Excel sheet", value = 1, step = 1))
      ),
      fluidRow(
        column(4, numericInput("truncpoints", label = "truncate", value = 0)),
        column(4, numericInput("skipd",
          label = "skip rows", value = 0
        )),
        column(4, numericInput("numrows",
          label = "plot nrows", value = 4
        ))
      ),
      fluidRow(
        column(8, textInput("select_samples", label = "Select columns", "2,3,4,5,6,7,8,9")),
        column(4, radioButtons(
          inputId = "Transf", label = "Correction method",
          choices = c("none", "Polynomial"), selected = "none"
        ))
      ),
      tags$h3("Transformed Curves"),
      fluidRow(
        column(4, numericInput("lagChange", label = "% change for start", value = 10, step = 1)),
        column(3, numericInput("smtail", label = "smooth tail", value = 5)),
        column(5, radioButtons(inputId = "a2Mcor", label = "Display", choices = c("F", "Thrombin", "Smooth.tail", "Smooth.all", "-T-alpha-2M"), selected = "Thrombin"))
      )
    ),
    mainPanel(
      tabsetPanel( # type="tab",
        tabPanel(
          "Calibrator", # value = 1,

          fluidRow(
            column(6, plotOutput("myCalib")),
            column(6, plotOutput("mytest"))
          ),
          plotOutput("myplotsF"),
        ),
        tabPanel("Curve",
          tags$h4("Individual curves"),
          uiOutput("what"),
          plotOutput("myplot"),
          tableOutput("curveTable"),
          align = "center"
        ),
        tabPanel("Plots",
          plotOutput(outputId = "myplotAll"),
          radioButtons(
            inputId = "tabRes", label = h4("Select parameter", align = "center"),
            choices = c(
              "Column names" = 1, "Initial reading" = 2, "Lag time " = 3, "Area under the curve" = 4,
              "Peak " = 5, "ttPeak" = 6, "ttTail" = 7, "Lag reading" = 8
            ),
            selected = 6, inline = TRUE, width = "100%"
          ),
          tags$h4("Results from plots"),
          tableOutput("plotsTable"),
          align = "center"
        ),
        tabPanel("Results",
          tags$h4("Table of all results"),
          tableOutput("resultsTable"),
          align = "center"
        ),
        tabPanel("Raw Data",
          helpText("Raw fluorescence or plotted data"),
          radioButtons("whatRaw",
            label = NULL,
            choices = c("Raw fluorescence", "Analysed"), selected = "Raw fluorescence", inline = TRUE, width = "100%"
          ),
          tableOutput("rawData"),
          align = "center"
        ),
        tabPanel("Calibrator Data",
          helpText("Raw or selected calibrator data"),
          radioButtons("whatCal", NULL,
            choices = c("Raw Calibrator", "Selected"), selected = "Raw Calibrator", inline = TRUE, width = "100%"
          ),
          tableOutput("calData"),
          align = "center"
        ),
        tabPanel("Settings",
          helpText(h5(paste("Simple thrombin generation app version number", Thisversion))),
          helpText(h5(format(Sys.Date(), "%d %b %Y"), ",", format(Sys.time(), "%X"))),
          fluidRow(
            column(6, tags$h5("Calibrator file"), uiOutput("calset")),
            column(6, tags$h5("Data file"), uiOutput("fileset"))
          ),
          tableOutput("settings"),
          tags$h4("Packages and versions"),
          textOutput("text4"),
          fluidRow(
            column(3, tags$h5("Base packages:")),
            column(9, tags$h5(textOutput("text5")))
          ),
          tags$h5("Other packages"),
          tableOutput("session"),
          #align = "center"
        ),
        tabPanel(
          "Help",
          shinythemes::themeSelector(),
          tags$h3("help notes"),
          tags$blockquote(h5(
            "►Calibrator tab",
            tags$br(),
            tags$br(),
            "►Load calibrator data and select a well: csv, txt or xlsx file types are accepted",
            tags$br(),
            "►Columns of calibrators or data can be selected over a range from 'start' to 'end' or on different Excel sheets",
            tags$br(),
            "►A curve (magenta -) is fitted to the calibrator fluorescence data (blue +) and intial rate calculated (red O)",
            tags$br(),
            "►The number of points selected for fitting should be chosen to cover the fluorescence range in the sample data",
            tags$br(),
            "►It is advisable to limit the number of points to be fitted to avoid noisy, unused data late in the time course",
            tags$br(),
            "►A polynomial correction algorithm is applied to correct the data for substrate depletion and inner filter effect (green X)",
            tags$br(),
            "►The quality of the fit is shown in the graphs to the right",
            tags$br(),
            "►Sample data are loaded using the dialogue box as csv, txt or xlsx",
            tags$br(),
            "►Figures of fluorescence versus time are shown and can be arranged by number of rows",
            tags$br(),
            "►'No correction' or 'polynomial correction' can be applied using the radio buttons",
            tags$br(),
            tags$br(),
            "►Plots and Curve tabs",
            tags$br(),
            tags$br(),
            "►Transformed data plots showing thrombin concentration are shown in these tabs",
            tags$br(),
            "►Results tables are also shown for a selected parameter ('Plots') or all parameters for a single curves ('Curve')",
            tags$br(),
            "►The end of the lag phase is signalled by a change of 10% in trombin concentration but this can be adjusted",
            tags$br(),
            "►Tail smoothing is used to select a steady rate to calculate the final Thrombin-alpha-2M contribution (gold curves)",
            tags$br(),
            "►Smooth the curves after the signal has returned to baseline and smoothing can be seen using the radio button selection",
            tags$br(),
            "►Graphs can display fluorescence or thrombin concentration with or without subtraction of the Thrombin-alpha-2M activity",
            tags$br(),
            "►Lag time and ttTail are the points where the curve reaches or returns to 10% (or selected value) of max",
            tags$br(),
            "►These points are also used as boundaries to calculate the area under the curve",
            tags$br(),
            tags$br(),
            "►Other tabs",
            tags$br(),
            tags$br(),
            "►'Results' presents a table of results",
            tags$br(),
            "►'Raw data' shows all the raw data and fluorescence. Thrombin or corrected thrombin values (with or without alpha-2-M-thrombin) can be selected",
            tags$br(),
            "►'Calibrator data' shows raw calibrator data columns as selected ",
            tags$br(),
            "►'Settings' summarises the setting used to generate the analysis as an aid to reproducibility",
            tags$br(),
            "►'Help' includes a theme selector drop down to permit control of fonts, colour and design ",
            tags$br(),
            tags$br(),
            tags$i("Please cite this reference on reproducible analysis in publications"),
            helpText(h5(
              "Longstaff C, Development of a Shiny app tool to simplify and standardize the analysis
                                    of hemostasis assay data: communication from the SSC of the ISTH, J Thromb Haemost, 15: 1044-6, 2017",
              tags$a(href = "https://onlinelibrary.wiley.com/doi/10.1111/jth.13656", "DOI 10.1111/jth.13656")
            )),
            tags$i("A publication is available where the app has been tested on difficult data"),
            helpText(h5(
              "Jackson, J et al, Sources of bias and limitations of thrombinography: inner filter effect
                                               and substrate depletion at the edge of failure algorithm, Thrombosis Journal, 21: 104, 2023",
              tags$a(
                href = "https://thrombosisjournal.biomedcentral.com/articles/10.1186/s12959-023-00549-5",
                "doi.org/10.1186/s12959-023-00549-5"
              )
            )),
            tags$i("Please contact me with problems or suggestions relating to:"),
            helpText(h5(
              "Thrombin generation assays app", Thisversion, " last accessed", Sys.Date(), "at",
              tags$a(href = "mailto: drclongstaff@gmail.com", "drclongstaff@gmail.com")
            )),
            tags$br(),
            "More detailed ", tags$a(href = "https://github.com/drclongstaff/Thrombin_Generation_Assays/blob/master/Docs/TGA_help_notes_ver_9.pdf", target = "_blank", "help notes"),
            "and the code can be found on my github site:",
            tags$a(href = "https://github.com/drclongstaff/thrombin_generation", "here"),
            tags$br(),
            "Other apps and links for reproducible analysis in haemostasis assays are available",
            tags$a(href = "https://drclongstaff.github.io/shiny-clots/", "here"), "and please check out the",
            tags$a(href = "https://www.youtube.com/@colinlongstaff7270", "Youtube channel of help videos")
          ))
        )
      )
    )
  )
)

# id = "tabselected"
