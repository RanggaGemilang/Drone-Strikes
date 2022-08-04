#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

header <- dashboardHeader(
  title = "Drone Strikes"
)

sidebar <- dashboardSidebar(
  collapsed = F,
  sidebarMenu(
    menuItem(
      text = "Overview",
      tabName = "Overview",
      icon = icon("plane")
    ),
    menuItem(
      text = "Casualties",
      tabName = "Casualties",
      icon = icon("person")
    ),
    menuItem(
      text = "Data",
      tabName = "Data",
      icon = icon("book")
    ),
    menuItem(
      text = "Source", 
      icon = icon("file-code-o"),
      href = "https://github.com/RanggaGemilang/Drone-Strikes"
    )))

body <- dashboardBody(
  
  # using custom CSS (disable dashboard skins)

  tags$head(tags$style(HTML('
                                 /* logo */
                                 .skin-blue .main-header .logo {
                                 background-color: #E3D4CD;
                                 color: black;
                                 font-family: "Georgia";
                                 font-style: bold;
                                 }
  
                                 /* logo when hovered */
                                 .skin-blue .main-header .logo:hover {
                                 background-color: #E3D4CD;
                                 color: black;
                                 font-family: "Georgia";
                                 font-style: bold;
                                 }
  
                                 /* navbar (rest of the header) */
                                 .skin-blue .main-header .navbar {
                                 background-color: #E3D4CD;
                                 }
  
                                 /* main sidebar */
                                 .skin-blue .main-sidebar {
                                 background-color: #E3D4CD;
                                 font-family: "Gautami";
                                 }
  
                                 /* active selected tab in the sidebarmenu */
                                 .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                 background-color: #E8E1DB;
                                 color: black;
                                 font-family: "Georgia";
                                 font-style: italic;
                                 }
  
                                 /* other links in the sidebarmenu */
                                 .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                 background-color: #E8E1DB;
                                 color: black;
                                 font-family: "Georgia";
                                 font-style: italic;
                                 }
  
                                 /* other links in the sidebarmenu when hovered */
                                 .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                 background-color: black;
                                 color: white;
                                 font-family: "Georgia";
                                 font-style: italic;
                                 }
                                 /* toggle button when hovered  */
                                 .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                 background-color: black;
                                 }
  
                                 /* body */
                                 .content-wrapper, .right-side {
                                 background-color: #E8E1DB;
                                 font-family: "Georgia";

                                 }
                                 
                                 .box.box-solid.box-primary>.box-header {

                                 }

                                 .box.box-solid.box-primary{
                                 border-bottom-color:#E8E1DB;
                                 border-left-color:#E8E1DB;
                                 border-right-color:#E8E1DB;
                                 border-top-color:#E8E1DB;
                                 background:#E8E1DB
                                 }
                                 
                                 .small-box.bg-navy { 
                                 background-image: url("bush.jpeg") !important;
                                 background-size: 300px 210px !important;
                                 background-attachment: fixed !important;
                                 color: white !important;
                                 
                                 }
                                 
                                 .small-box.bg-red { 
                                 background-image: url("obama.jpg") !important;
                                 background-size: 300px 210px !important;
                                 background-attachment: fixed !important;
                                 color: white !important; 
                                 
                                 }
                                 
                                 .small-box.bg-yellow { 
                                 background-image: url("trump.jpg") !important;
                                 background-size: 300px 210px !important;
                                 background-attachment: fixed !important;
                                 color: white !important; 
                                 
                                 }
                                 
                                 .small-box.bg-teal { 
                                 background-color: #A6655F !important;
                                 color: white !important; 
                                 
                                 }
  
                               '))),
  
  tabItems(
    
    # TAB 1  
    
    tabItem(
      tabName = "Overview",
      fluidRow(
        fluidRow(column(12,align ="center",
                        div(img(src="DroneBW.png", height=200, width=400))))),
      fluidPage(
        div(style = "text-align:justify", 
            p("Among the evolving challenges to global peace and security are 
              the growing incidents of terrorism in Middle East.",
              "With cases in Afghanistan, Pakistan, Yemen, Somalia and among others, 
              the continent is fast earning a moniker as a major frontier in the US-led Global War on Terrorism.",
              "A key counterterrorism measure in this regard has been the controversial reliance on Drones.",
              ),
            p("Drones generally fall into two categories: those that are used for reconnaissance and surveillance purposes, 
            and those that are armed with weapons for military purposes.",
              "The use of drones has grown enormously in recent years, in part, 
              because unlike manned aircraft they can fly long missions, are less costly, and have no (immediate) military casualties."
              ),
            p("There are a number of debates surrounding the use of drones, the most contentious of which have been as to whether 
              governments have legal authorization to do so, and of how combatant status is defined under current international law.",
              "The most salient issue today regarding the deployment of drones has always been the 'culture of secrecy', which doesn't cover up
              any collateral damage that has been inflicted by such unmanned strikes."
              )
        )
      ),
      br(),
      fluidPage(
        valueBox(tags$p(bush.strikes$Total, style = "font-size: 100%; color: #873047;"), 
                 tags$p("Bush Reported Strikes", style = "font-size: 150%; color: white;"),
                 color = "navy",
                 width = 4),
        valueBox(tags$p(comma(obama.strikes$Total), style = "font-size: 100%; color: #873047;"), 
                 tags$p("Obama Reported Strikes", style = "font-size: 150%; color: white;"),
                 color = "red",
                 width = 4),
        valueBox(tags$p(comma(trump.strikes$Total), style = "font-size: 100%; color: #873047;"), 
                 tags$p("Trump Reported Strikes", style = "font-size: 150%; color: white;"),
                 color = "yellow",
                 width = 4)
      )
      ),
    
    # TAB 2
    tabItem(
      tabName = "Casualties",
      fluidPage(
        box(width = 3,
            status = "primary", 
            solidHeader = TRUE,
            align = "center",
            selectInput(inputId = "Country",
                        label = h4(tags$b("Select Country:")),
                        choices = unique(drone$Country))
          
        ),
        valueBoxOutput(width = 3,
                       "Strikebox"),
        valueBoxOutput(width = 3,
                       "Deathbox"),
        valueBoxOutput(width = 3,
                       "Deathbox1"),
        box(width = 12,
            status = "primary", 
            solidHeader = TRUE,
            plotlyOutput(outputId = "plotly_1")),
        box(width = 7,
            status = "primary", 
            solidHeader = TRUE,
            plotlyOutput(outputId = "plotly_2")),
        box(width = 5,
            status = "primary", 
            solidHeader = TRUE,
            plotlyOutput(outputId = "plotly_3"))
      )
    ),
    
    # TAB 3
    tabItem(
      tabName = "Data",
      h2(tags$b("The Dataset"),align = "center",
         style = 'font-family: "Georgia"; font-style: italic;'),
      DT::dataTableOutput("data1")
    )
    )
  )

dashboardPage(
  header = header,
  body = body,
  sidebar = sidebar
)