library(shinyBS)
library(shiny)
library(shinyjs)
##################TOP##################################################################
shinyUI(
  fluidPage(theme = "bootstrap.min.css", 
      tags$head(tags$script(src = "popOver.js")),
      HTML("<span style='font-size:35px'><span style ='color:orange'>T</span><span style ='color:#B2B200'>A</span><span style ='color:orange'>B</span><span style ='color:#B2B200'>S</span><span style ='color:orange'>I</span><span style='color:#B2B200'>E</span></span> <span style='font-size:15px'> <span style ='color:orange'>   Table </span><span style ='color:#B2B200'>Analyzer, </span><span style ='color:orange'>Browser,</span><span style ='color:#B2B200'> and Summarizer </span><span style ='color:orange'> for Informatics</span><span style ='color:#B2B200'> Exploration</span></span>
           <p>Clinical Informatics Research Division (CIRD), University of Texas Health Science Center at San Antonio (UTHSCSA)</h5></p>"),
      shinyjs::hidden(div(id= "TABSIEApp",
      uiOutput("TitleString"),
      uiOutput("Statement"),
      bsAlert("systemError"),
      tabsetPanel(
        tabPanel("Graphs",  #tabpanel for the bargraph
          sidebarLayout(
            sidebarPanel(
  ######################SideBar Graph#######################################################
  ######### BASIC PANEL- Sidebar Graph #####################################################################
            tabsetPanel(
              tabPanel("Basic",
                       verticalLayout(
                         shinyjs::useShinyjs(),
                         shinyjs::hidden(div(id= "filterFlagDiv",
                             selectInput("filter", "Filter", c("No Filter"), "No Filter")
                         )),
                         uiOutput("xy"),
                         shinyjs::hidden(div(id= "xOmitDiv",
                             checkboxInput("xOmit", "Omit blanks in X?", value = TRUE))),
                         shinyjs::hidden(div(id= "barPlotDiv",
                             checkboxInput("yOmit", "Omit blanks in Y?", value = FALSE),
                             radioButtons("barProportion", dictBarProportions, choices = c("Compare proportions", "Show actual values"), selected = "Show actual values"))),
                         shinyjs::hidden(div(id="FNDiv",
                             radioButtons("boxViolin", dictFN, c("Box plot", "Violin", "Points"), selected = "Box plot"))),
                         shinyjs::hidden(div(id= "pointDiv",
                             shinyjs::hidden(div(id= "jitterDiv",
                                  sliderInput('widthSlide', dictJitterWidth, min = 0, max = 1, value = 0.3, step = .1, round = FALSE)
                                )),
                             sliderInput('sizeSlide', "Point Size", min = 0, max = 5, value = 1, step = .5, round = FALSE),
                             sliderInput('alphaSlide', dictPointOpacity, min = 0, max = 1, value = 0.2, step = .1, round = FALSE),
                             checkboxInput('pointJitter', dicJitter))),
                         checkboxInput("coordFlop","Rotate Graph")
                       )
              ),#end basic tab
  ######### ADVANCED PANEL- Sidebar Graph #####################################################################
              tabPanel("Advanced",
                       verticalLayout(
                         shinyjs::useShinyjs(),
                         a(id="toggleTheme", "Theme: Show/hide options", href ="#"),
                         shinyjs::hidden(div(id="themeDiv",
                                             textInput("titleField", "Graph Title", value = "", placeholder = "Enter the graph's title."),
                                             textInput("xLab", "X-Axis Label", value = ""),
                                             textInput("yLab", "Y-Axis Label", value = ""),
                                             sliderInput("textSize", "Text Size", min = 5, max = 30,step = 1, value = 15),
                                             sliderInput("xLabRotation", "X Label Text Rotation", min = 0, max = 90, step = 5, value = 0),
                                             sliderInput("xLabHeight", "X Label Location", min = -1, max = 1, step = .1, value = 0),
                                             actionButton("clearTheme", "Reset Theme")
                         )),#end div "theme
                         a(id="toggleBox", "Box plot: Show/hide options", href ="#"),
                         shinyjs::hidden(div(id="boxDiv",
                                             p("Note: These options will only have an effect on box plots."),
                                             checkboxInput("boxColor", "Add Color?", value = TRUE)
                         )),#end div box options
                         a(id="toggleViolin", "Violin: Show/hide options", href ="#"),
                         shinyjs::hidden(div(id="violinDiv",
                                             p("Note: These options will only have an effect on violin graphs."),
                                             checkboxInput("violinColor", "Add Color?", value = TRUE),
                                             checkboxInput("violinBoxOpt", "Add internal boxplot?", value = TRUE),
                                             checkboxInput("violinTrim", "Trim Edges?", value = TRUE)
                         )),#end div Violin options
                         a(id="togglePoint", "Point: Show/hide options", href ="#"),
                         shinyjs::hidden(div(id="pointAdvDiv",
                                             p("Note: These options will only have an effect on point graphs."),
                                             selectInput("pointColor", "Color Value", c("No color"), selected = "No color"),
                                             selectInput("pointShape", "Shape Value", c("No shape"), selected = "No shape")
                                             
                         ))#end div point options
                       )
              )#end advanced tab
            )#end tab panel
            ), #end graph sidebar panel
  ######### MAIN PANEL- Graph #####################################################################
            mainPanel(
              bsAlert("graphError"),
              plotOutput("visPlot"),
              uiOutput("summaryRegion")#this makes it easier to adaptively display the summary
            )#end bargraph mainPanel
          )),#end sidebarLayout/TabPanel BARGRAPH
  ######### CONSTELLATION #########################################################################
        tabPanel("Constellation",sidebarLayout(
          sidebarPanel(fluidRow( # CONSTELLATIONS
              shinyjs::hidden(div(id= "filterFlagDivCon",
                                selectInput("filterCon", "Filter", c("No Filter"), "No Filter")
              )),
              div(style="display:inline-block",
              checkboxInput("focusedPCA", "Focused PCA Plot?")),
              div(style="display:inline-block",
              HTML("<a href='#' id='btn-2' class='btn btn-primary btn-xs' data-toggle='popover' data-placement= 'auto bottom' title='Focused PCA Plot' data-content='A <b>focused</b> PCA allows you to create a constelation map that revolves around a specific point. The closer a point is to the center, the more closely the column it 
              represents is correlated with the response variable in the center.'>?</a>")),
              shinyjs::hidden(div(id="focusedPCADiv",
                                  hr(),
                                  p("The closer a point is to the center, the more closely the column it 
                                      represents is correlated with the response variable in the center. Green 
                                      indicates positive correlations and yellow, inverse correlations."),
                                  hr(),
                                  uiOutput("PCAVariable"))),
              div(id="constellationDiv",
                                hr(),
                                p("Each point is a column from the dataset. The closer they are together the 
                                    stronger their positive correlation, and the closer they are to 180 degrees,
                                    the stronger their negative correlation. Variables 90 degrees to each other are 
                                    uncorrelated (independent).
                                    White filled circles are on the surface of the sphere that is away from the observer and the red ones are currently facing the observer."),
                                hr(),
                                p("Use these sliders to rotate the points until they become easy to see. Note: The rotation play buttons might not perform well when running TABSIE from CD or on a slower computer."),
                                sliderInput('constVSlider', "Y-Axis", min = 0, max = 360, value = 1, step = 5, round = 0, animate = TRUE),
                                sliderInput('constHSlider', "X-Axis", min = 0, max = 360, value = 1, step = 5, round = 0, animate = TRUE),
                                sliderInput('constFSlider', "Z-Axis", min = 0, max = 360, value = 1, step = 5, round = 0, animate = TRUE))
          )),#end constellation sideBar
          mainPanel(
            plotOutput("constellationPlot",height="800px")
          )#end constellation mainpanel
        ))#End sidbarLayout/TabPanel CONSTELLATIONS
      
      ),#end tabsetPanel
    HTML('<footer><center> <hr>TABSIE v1.2.0, GPL v2 2016.</br> Authors: Laura Manuel, Alex Bokov, and the CIRD team.<br/>For more information, please contact <a href="mailto:informatics@uthscsa.edu">informatics@uthscsa.edu</a></center></footer>')
    )#end fluidPage
  ),#end main app div: id= "TABSIEApp"
  div(id= "AuthPage", 
      verticalLayout(
        bsAlert("authError"),
        isolate(
          passwordInput(inputId = "authPassword", label = "Enter Pin")
          ),
         actionButton(inputId = "authButton", label = "Authenticate") 
   ))#end auth page
  )#end layout
)
