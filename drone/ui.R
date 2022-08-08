library(shiny)
library(echarts4r)
library(countrycode)
# Header

header <- dashboardHeader(
  title = "U.S Drone Strikes"
)

# Sidedbar

sidebar <- dashboardSidebar(
  collapsed = F,
  sidebarMenu(
    menuItem(
      text = "Overview",
      tabName = "Overview",
      icon = icon("magnifying-glass")
    ),
    menuItem(
      text = "Casualties",
      tabName = "Casualties",
      icon = icon("arrows-down-to-people")
    ),
    menuItem(
      text = "Location",
      tabName = "Location",
      icon = icon("location-crosshairs")
    ),
    menuItem(
      text = "Data",
      tabName = "Data",
      icon = icon("book")
    ),
    menuItem(
      text = "Source", 
      icon = icon("code"),
      href = "https://github.com/RanggaGemilang/Drone-Strikes"
    )))

# Body

body <- dashboardBody(
  
  # using custom CSS 

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
                                 background-image: url("usa.png") !important;
                                 background-size: cover !important;
                                 border-bottom-color:#E8E1DB;
                                 border-left-color:#E8E1DB;
                                 border-right-color:#E8E1DB;
                                 border-top-color:#E8E1DB;
                                 color: white;
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
                                 background-color: #E3D4CD;
                                 border-left-color:#E3D4CD;
                                 color: #5A5981;
                                 font-family: "Georgia";
                                 font-style: italic;
                                 }
  
                                 /* other links in the sidebarmenu */
                                 .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                 background-color: #E8E1DB;
                                 border-left-color:#E8E1DB;
                                 color: black;
                                 font-family: "Georgia";
                                 font-style: italic;
                                 }
  
                                 /* other links in the sidebarmenu when hovered */
                                 .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                 background-color: #E3D4CD;
                                 border-left-color:#E3D4CD;
                                 color: #A6655F;
                                 font-family: "Georgia";
                                 font-style: italic;
                                 }
                                 /* toggle button when hovered  */
                                 .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                 background-color: black;
                                 border-left-color:#E8E1DB;
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
                                 background:#E8E1DB;
                                 box-shadow: none;
                                 }
                                 
                                 .box.box-solid.box-danger{
                                 background-image: url("president.png") !important;
                                 background-size: cover !important;
                                 border-bottom-color:#E8E1DB;
                                 border-left-color:#E8E1DB;
                                 border-right-color:#E8E1DB;
                                 border-top-color:#E8E1DB;
                                 color: white;
                                 box-shadow: none;
                                 }
                                 
                                 .box.box-solid.box-success{
                                 background-image: url("mapper.png") !important;
                                 background-size: cover !important;
                                 border-bottom-color:#E8E1DB;
                                 border-left-color:#E8E1DB;
                                 border-right-color:#E8E1DB;
                                 border-top-color:#E8E1DB;
                                 color: #A6655F;
                                 box-shadow: none;
                                 }
                                 
                                 .small-box.bg-black { 
                                 background-color: #5A5981 !important;
                                 color: white !important; 
                                 
                                 }
                                 
                                 .small-box.bg-navy { 
                                 background-image: url("bush.jpeg") !important;
                                 background-size: cover !important;
                                 color: white !important;
                                 
                                 }
                                 
                                 .small-box.bg-red { 
                                 background-image: url("obama.jpg") !important;
                                 background-size: cover !important;
                                 color: white !important; 
                                 
                                 }
                                 
                                 .small-box.bg-yellow { 
                                 background-image: url("trump.jpg") !important;
                                 background-size: cover !important;
                                 color: white !important; 
                                 
                                 }
                                 
                                 .small-box.bg-green { 
                                 background-color: #A6655F !important;
                                 color: white !important;
                                 
                                 }
                                 
                                 .small-box.bg-teal { 
                                 background-color: #A6655F !important;
                                 color: white !important; 
                                 
                                 }
                                 
                                 .nav-tabs {
                                 background: #E8E1DB;
                                 border-bottom-color:#E8E1DB;
                                 border-left-color:#E8E1DB;
                                 border-right-color:#E8E1DB;
                                 border-top-color:#E8E1DB;
                                 color: white;
                                 }
                                 
                                 .nav-tabs-custom .nav-tabs li.active:hover a,.nav-tabs-custom .nav-tabs li.active a {
                                 background-color: transparent;
                                 border-color: transparent;
                                 color: #A6655F;
                                 
                                 }
                                 
                                 .nav-tabs-custom .nav-tabs li.active {
                                 background-color: white;
                                 border-top-color: #A6655F;
                                 border-bottom-color:white;
                                 color:#A6655F}
                                 
                                 
  
                               '))),
  
  tabItems(
    
    # TAB 1  
    
    tabItem(
      tabName = "Overview",
      fluidRow(
        fluidPage(
          valueBox(tags$p(bush.strikes$Total, style = "font-size: 100%; color: #A6655F;"), 
                   tags$p("Bush Reported Strikes", style = "font-size: 150%; color: white;"),
                   color = "navy",
                   width = 4),
          valueBox(tags$p(comma(obama.strikes$Total), style = "font-size: 100%; color: #A6655F;"), 
                   tags$p("Obama Reported Strikes", style = "font-size: 150%; color: white;"),
                   color = "red",
                   width = 4),
          valueBox(tags$p(comma(trump.strikes$Total), style = "font-size: 100%; color: #A6655F;"), 
                   tags$p("Trump Reported Strikes", style = "font-size: 150%; color: white;"),
                   color = "yellow",
                   width = 4),
          valueBox(tags$p(paste(round(bush.rate,3)*100,"%"), style = "font-size: 100%; color: white;"), 
                   tags$p("Civilian Casualties Rate", style = "font-size: 150%; color: white;"),
                   color = "green",
                   width = 4,
                   icon = icon("skull-crossbones")),
          valueBox(tags$p(paste(round(obama.rate,3)*100,"%"), style = "font-size: 100%; color: white;"), 
                   tags$p("Civilian Casualties Rate", style = "font-size: 150%; color: white;"),
                   color = "green",
                   width = 4,
                   icon = icon("skull-crossbones")),
          valueBox(tags$p(paste(round(trump.rate,3)*100,"%"), style = "font-size: 100%; color: white;"), 
                   tags$p("Civilian Casualties Rate", style = "font-size: 150%; color: white;"),
                   color = "green",
                   width = 4,
                   icon = icon("skull-crossbones"))
        )
        ),
      fluidPage(
        div(style = "text-align:justify", 
            p("Among the evolving challenges to global peace and security are the growing incidents of terrorism in Middle East.", 
              "Even after 9/11 incident, the continent is becoming the major frontier of U.S-led counterterrorism campaign.",
              "Amidst of those campaigns, there was a new key pattern built by U.S military to combat those conflicts.",
              "A key of measure in this regard has been the reliance on 'Drones' or in military terms, an 'Unmanned Aerial Vehices' (UAV)."
              ),
            p("Drones generally fall into two categories: those that are used for 'reconnaissance and surveillance purposes' (which is their initial purpose), 
              and those that are 'armed with weapons for military purposes'. 
              The new pattern has grown enormously in the Middle East campaign, because, 
              unlike manned aircraft they can fly long missions, are less costly, and have no (immediate) military casualties.
              "
              ),
            p("With the ascension of Drone usage in the act of counterterrorism, there also rising number of debates surrounding its operation.
              When it became known that U.S used Drones for targetted killings even outside of the 'official warzones', 
              the body literature on Drone campaign quickly began to grow exponentially. Since the characteristic of Drone operation are heavy
              on 'immediate action', this has resulted in less restrained act of warfare. Questions about legality, accountability and most importantly, 
              transparency, become the center of everything related to Drone issue."
              ),
            p("In this Data Visualization Project, I want to show you what kind of transparency that we have along with its discrepancies. Here are the examples:")
        )
      ),
      br(),
      fluidRow(
        box(width = 6,
            status = "primary", 
            solidHeader = TRUE,
            echarts4rOutput(outputId = "totalstrikes")
        ),
        box(
          width = 6,
          status = "primary", 
          solidHeader = TRUE,
          echarts4rOutput(outputId = "confirmedstrikes")
        )
      ),
      fluidPage(
        div(style = "text-align:justify", 
            p("You might see some
              inconsistencies within the presented report and the dataset, this because the Data itself is not completely 'official'. It was also collected
              by a team of Bureau journalist from news reports, statements, documents, press releases, and local leaders with some integration to the released official data from
              the U.S Air Force. Please be noted that, the official 'Drones Casualties Estimates Report' was first initiated in 2016 by Obama's director of national intelligence 
              while the first drone strike reported by press (based on our dataset) was in Yemen from 2002."
            ),
            p(
              "This might be the solely reason why there are very few reports within Bush Era of Presidency along with the non-existent U.S Confirmed Strikes in Pakistan,
              thus I hope that these visualizations and analysis could help us to bring more transparency to light."
            ))
        ),
      fluidRow(
        fluidRow(column(12,align ="center",
                        div(img(src="Drone.png", height=200, width=400)))))
      ),
    
    # TAB 2
    tabItem(
      tabName = "Casualties",
      fluidPage(
        box(width = 6,
            height = 150,
            status = "success", 
            solidHeader = TRUE,
            align = "center",
            selectInput(inputId = "Country",
                        label = h4(tags$b(" ")),
                        choices = unique(drone$Country))
            ),
        box(width = 6,
            height = 150,
            status = "danger", 
            solidHeader = TRUE,
            align = "center",
            checkboxGroupInput(inputId = "Era",
                               label = h4(tags$b(" ")),
                               choices = unique(drone$Presidency),
                               selected = levels(drone$Presidency))
                               ),
        box(width = 12,
            status = "primary", 
            solidHeader = TRUE,
            echarts4rOutput(outputId = "plot_1")),
        valueBoxOutput(width = 4,
                       "Strikebox"),
        valueBoxOutput(width = 4,
                       "Deathbox"),
        valueBoxOutput(width = 4,
                       "Deathbox1"),
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
      tabName = "Location",
      fluidPage(
        div(style = "text-align:center", 
            p("This map is based on Strikes Intensity between each Country"))
            ),
      fluidPage(
        tabBox(width = 12,
               title = tags$b(" "),
               id = "tabset1",
               side = "left",
               tabPanel(tags$b("Afghanistan"), 
                        leafletOutput("leaflet_a", height = 560)
               ),
               tabPanel(tags$b("Pakistan"), 
                        leafletOutput("leaflet_p", height = 560)
               ),
               tabPanel(tags$b("Somalia"), 
                        leafletOutput("leaflet_s", height = 560)
               ),
               tabPanel(tags$b("Yemen"), 
                        leafletOutput("leaflet_y", height = 560)
               )
        )
      )),
    
    # TAB 4
    tabItem(
      tabName = "Data",
      h2(tags$b("The Dataset"),align = "center",
         style = 'font-family: "Georgia"; font-style: italic;'),
      fluidPage(
        div(style = "text-align:justify", 
            p("The original dataset consists of 4 different datasets from each Country available,",
              "and the dataset shown below is the one that already been tidied up into 1 dataset for easier read and Pre-processing.",
              "For the original dataset and tidying up process can be seen in the 'Source' page."
            ),
        )
      ),
      DT::dataTableOutput("data1")
    )
    )
  )

#Assembly

ui <- 
dashboardPage(
  header = header,
  body = body,
  sidebar = sidebar
)
