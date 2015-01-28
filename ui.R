# UI for net site

library(shiny)
library(shiny)

# Define UI for slider demo application
shinyUI(fluidPage(
  
  #  Application title
  #titlePanel("Design your plankton net"),
  
  # Sidebar with sliders that demonstrate various available
  # options
    sidebarLayout(
    sidebarPanel(
      helpText(h4("Instructions")),
      helpText("1) Set your sampling plan parameters below."),
      helpText("2) Set your desired mesh size and mouth diameter to determine the minimum length of your net."),
      helpText("3) Voila! You can then adjust the parameters to explore your options."),
      
      
      #helpText(h3("Design your plankton net")),

      br(),
      helpText(h4("Sampling Plan")),
      # Green vs Blue
      selectInput("area", h6("Categorize study area:"),choices = c("Green waters", "Blue waters", "Both")),

      # Tow Type
      selectInput("tow", h6("What kind of tow?"),choices = c("Vertical", "Oblique"),selected=1),
      
      # Assumed IFE
      sliderInput("ife", h6("Initital Filtration Efficiency (%):"),min = 60, max = 130, value = 95, step= 1),

      # Tow Speed
      sliderInput("spd", h6("Tow speed (m/s):"),min = 0.1, max = 2.5, value = 1, step= .01),
      
      # TOW VOLUME
      conditionalPanel(
        condition = "input.tow == 'Vertical'",
        sliderInput("vol", h6("Tow Volume (cubic m.):"),min = 20, max = 1000, value = 75, step = 1,
                    animate=FALSE)
      ),
      conditionalPanel(
        condition = "input.tow == 'Oblique'",
        sliderInput("vol", h6("Tow Volume (cubic m.):"),min = 20, max = 5000, value = 75, step = 1,
                    animate=FALSE)
      ),
      br(),
      br(),
      helpText(h6("Developed by Eric Keen (2015) SIO-NOAA-SWFSC")),
      helpText("Keen, EM. 2015. Net savvy: a practical guide to zooplankton sampler design. NOAA-TM-SWFSC-####. 31pp."),
      br(),
      imageOutput("collab",width="5%",height="5%",inline=TRUE)

      
    ),
     

    # Show a table summarizing the values entered
    mainPanel(
      fluidPage(
        tabsetPanel(type="tabs",
      tabPanel(h3(" Design Your Plankton Net "),
      
      fluidRow(
        column(4,

               sliderInput("mesh", h5("Maximum mesh size (u):"),min = 30, max = 999, value =333, step= 3,
                           animate=FALSE)
               ),
      
        column(4,
               sliderInput("diam", h5("Mouth diameter (m):"),min=0.10, max=3.00, step = 0.01, value=.7,
                           animate=FALSE)
        ),
        
        column(4,htmlOutput("moar"))
                     
     ),
     
     br(),
     
     fluidRow(plotOutput(outputId = "main_plot"))
     ,
     
     fluidRow(
       column(6,plotOutput("por")),
       column(6,plotOutput("durdist"))
     )
               
    ),
    
    tabPanel(h5("About"),
             htmlOutput("about"),
             imageOutput("netpic",height=100)
    )
             
    )
      )
    )
    )
    )
    )

#helpText("Conventional IFE values:"),
#helpText("Simple conical net: 85%"),
#helpText("WP2 cyl-cone net: 100%"),
#helpText("Reducing cone net: 110%"),
#helpText("Non-porous encasements: 60-70%"),