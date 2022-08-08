server <- function(input, output) {
  
  # Subsetting for CheckGroupInput
  EraSubset <-reactive({
    if (is.null(input$Era) == TRUE) {
      return(drone)
    } else {
      return(filter(drone, Presidency %in% input$Era))
    }
  })
  
  # Total Strikes Pie
  output$totalstrikes <- renderEcharts4r({
    e_common(font_family = "georgia")
    
    strikes.pie <- 
      drone %>% 
      select(Country,`Maximum Strikes`) %>% 
      group_by(Country) %>% 
      summarize(Strikes = sum(`Maximum Strikes`)) %>% 
      e_charts(x = Country) %>% 
      e_pie(Strikes, legend = FALSE) %>% 
      e_tooltip() %>% 
      e_title("Total Strikes Reported", "in Each Country") %>% 
      e_theme_custom("www/rg_theme.json")
    
    strikes.pie
  })
  
  # Total Confirmed Strikes
  
  output$confirmedstrikes <- renderEcharts4r({
    e_common(font_family = "georgia")
    
    confirmed.pie <- 
      drone %>% 
      filter(`US Confirmed` %in% "Confirmed") %>% 
      select(Country,`Maximum Strikes`) %>% 
      group_by(Country) %>% 
      summarize(Strikes = sum(`Maximum Strikes`)) %>%  
      e_charts(x = Country) %>% 
      e_pie(Strikes, legend = FALSE, name = "Strikes") %>% 
      e_tooltip() %>% 
      e_title("Strikes Confirmed by U.S", "in Each Country") %>% 
      e_theme_custom("www/rg_theme.json")
    
    confirmed.pie
  })
  
  # Strikes Timeline Plot
  
  output$plot_1 <- renderEcharts4r({
    line_data <- 
      EraSubset() %>% 
      filter(Country == input$Country) %>% 
      select(Country, Date,`Maximum Strikes`) %>% 
      mutate(Date = floor_date(Date, "quarter")) %>% 
      group_by(Date, Country) %>% 
      summarise(Strikes = sum(`Maximum Strikes`),.groups = 'drop') %>%
      mutate(Quart = case_when(
        month(Date) == "1" ~ "Q1",
        month(Date) == "4" ~ "Q2",
        month(Date) == "7" ~ "Q3",
        month(Date) == "10" ~ "Q4"
      )) %>% 
      mutate(Quart = paste(year(Date), Quart),
             Date = as.Date(Date, format = "%d-%m-%Y"))
    
    style <- list(
      itemStyle = list(
        color = "#A6655F",
        opacity = 0.4
      )
    )
    
    line_base <- line_data %>% 
      group_by(Country) %>% 
      e_charts(x = Quart) %>% 
      e_datazoom(
        type = "slider", 
        toolbox = FALSE
      ) %>% 
      e_tooltip() %>% 
      e_title("Strikes Timeline per Quartal") %>% 
      e_x_axis(Quart, axisPointer = list(show = TRUE)) %>% 
      e_theme_custom("www/rg_theme.json")
    
    line_plot <- line_base %>% 
      e_line(serie=Strikes, name = "Strikes", legend = FALSE, emphasis=style)
    line_plot
  })
  
  # Death Plot
  
  output$plotly_2 <- renderPlotly({
    death.plot <- 
      EraSubset() %>% 
      filter(Country == input$Country) %>% 
      select(`Minimum Total Killed`, `Maximum Total Killed`, `Minimum Civilians Killed`, `Maximum Civilians Killed`, 
             `Minimum Children Killed`, `Maximum Children Killed`) %>% 
      pivot_longer(cols = c("Minimum Total Killed","Maximum Total Killed",
                            "Minimum Civilians Killed", "Maximum Civilians Killed", 
                            "Minimum Children Killed", "Maximum Children Killed"), 
                   names_to = "Name", values_to = "Value") %>% 
      mutate( Type = case_when(
        Name == "Minimum Total Killed" | Name == "Maximum Total Killed" ~ "Total",
        Name == "Minimum Civilians Killed" | Name == "Maximum Civilians Killed" ~ "Civilians",
        Name == "Minimum Children Killed" | Name == "Maximum Children Killed" ~ "Children")) %>% 
      mutate ( Name = case_when(
        Name == "Minimum Total Killed" | Name == "Minimum Civilians Killed" | Name == "Minimum Children Killed" ~ "Minimum",
        Name == "Maximum Total Killed" | Name == "Maximum Civilians Killed" | Name == "Maximum Children Killed" ~ "Maximum")) %>% 
      group_by(Type, Name) %>% 
      summarize(Value = sum(Value), .groups = 'drop') %>% 
      ungroup() %>%
      mutate(label = glue("{Type}
                      Deaths: {comma(Value)}")) %>% 
      ggplot(aes(Value, Type, text = label)) +
      geom_col(aes(fill = factor(Name, levels=c("Maximum", "Minimum"))),
               position = "stack", width = 0.7) +
      scale_fill_manual(values = alpha(c("#A6655F", "#F5DB9E"),0.9)) +
      scale_x_continuous(breaks = seq(0,14000,2000)) +
      labs(title = "Death",
           x = NULL,
           y = NULL,
           fill = NULL) +
      theme(legend.position = "right",
            legend.direction = "vertical",
            legend.background = element_rect(fill="#E8E1DB", color = "#E8E1DB"),
            legend.key = element_rect(fill="#E8E1DB", color = "#E8E1DB"),
            legend.title = element_text(colour = "Black", face ="bold", size = 9, family = "Georgia"),
            legend.text = element_text(color="Black", face ="italic", family = "Georgia"),
            plot.background = element_rect(fill = "#E8E1DB", color = "#E8E1DB"),
            panel.background = element_rect(fill = "#E8E1DB"),
            panel.grid = element_line(alpha(colour = "#C3C3C3",alpha = 0.4)),
            axis.title.x = element_text(colour = "Black", family = "Georgia",face = "bold"),
            axis.text.x = element_text(angle = 15, color = "Black", face = "bold", family = "Georgia"),
            axis.text.y = element_text(color="Black", face="italic", family = "Georgia"),
            plot.title = element_text(face = "bold", family = "Georgia"))
    
    ggplotly(death.plot, tooltip = "text")
  })
  
  # Injured Plot
  
  output$plotly_3 <- renderPlotly({
    injured.plot <- 
      EraSubset() %>% 
      filter(Country == input$Country) %>% 
      select(`Minimum Injured`, `Maximum Injured`) %>% 
      rename(Minimum = "Minimum Injured",
             Maximum = "Maximum Injured") %>% 
      pivot_longer(cols = c("Minimum", "Maximum"),
                   names_to = "Injured",
                   values_to = "Value") %>% 
      group_by(Injured) %>% 
      summarize(Total = sum(Value)) %>% 
      mutate(label = glue("Injured: {comma(Total)}")) %>% 
      ggplot(aes(Total, reorder(Injured, Total), text = label, fill = Injured )) +
      scale_x_continuous(breaks = seq(0,1750,250)) +
      geom_col(width = 0.6) +
      scale_fill_manual(values = alpha(c("#5A5981", "#F5DB9E"),0.9)) +
      labs(title = "Injured",
           x = NULL,
           y = NULL,
           fill = NULL) +
      theme(legend.position = "none",
            plot.background = element_rect(fill = "#E8E1DB", color = "#E8E1DB"),
            panel.background = element_rect(fill = "#E8E1DB"),
            panel.grid = element_line(alpha(colour = "#C3C3C3",alpha = 0.4)),
            axis.title.x = element_text(colour = "Black", family = "Georgia",face = "bold"),
            axis.text.x = element_text(angle = 15, color = "Black", face = "bold", family = "Georgia"),
            axis.text.y = element_text(color="Black", face="italic", family = "Georgia"),
            plot.title = element_text(face = "bold", family = "Georgia"))
    
    ggplotly(injured.plot, tooltip = "text")
  })
  
  # Total Strike Info Box
  
  output$Strikebox <- renderValueBox({
    
    total.strike <- EraSubset() %>% 
      filter(Country == input$Country) %>% 
      select(`Maximum Strikes`,`Minimum Strikes`) %>% 
      summarize(`Strikes Reported` = sum(`Maximum Strikes`),
                `Minimum Strikes` = sum(`Minimum Strikes`))
    
    valueBox(tags$p(paste(comma(total.strike$`Minimum Strikes`),"-",comma(total.strike$`Strikes Reported`)), 
                    style = "font-size: 100%; color: white;"),
             subtitle = "Total Strikes",
             color = "teal", 
             icon = icon("explosion"))
  })
  
  # Innocent Death Info Box
  
  output$Deathbox <- renderValueBox({
    
    inno.death <- EraSubset() %>% 
      filter(Country == input$Country) %>% 
      select(`Maximum Civilians Killed`,`Maximum Children Killed`,`Minimum Civilians Killed`,`Minimum Children Killed`) %>% 
      summarize(maxCivilians = sum(`Maximum Civilians Killed`),
                maxChildren = sum(`Maximum Civilians Killed`),
                minCivilians = sum(`Minimum Civilians Killed`),
                minChildren = sum(`Minimum Children Killed`)) %>% 
      mutate(DeathMax = maxCivilians + maxChildren,
             DeathMin = minCivilians + minChildren)
    
    valueBox(tags$p(paste(comma(inno.death$DeathMin),"-",comma(inno.death$DeathMax)), 
                    style = "font-size: 100%; color: white;"),
             subtitle = "Civilians Death",
             color = "black", 
             icon = icon("person-falling-burst"))
  })
  
  # Total Death Info Box
  
  output$Deathbox1 <- renderValueBox({
    
    total.death <- EraSubset() %>% 
      filter(Country == input$Country) %>% 
      select(`Maximum Total Killed`,`Minimum Total Killed`) %>% 
      summarize(DeathMax = sum(`Maximum Total Killed`),
                DeathMin = sum(`Minimum Total Killed`))
    
    valueBox(tags$p(paste(comma(total.death$DeathMin),"-",comma(total.death$DeathMax)), 
                    style = "font-size: 100%; color: white;"),
             subtitle = "Total Death",
             color = "teal", 
             icon = icon("hospital"))
  })
  
  # Afghanistan Leaflet Map
  
  output$leaflet_a <- renderLeaflet({
    m.a <- leaflet(afghan_sf) %>% 
      addProviderTiles(providers$Stamen.Terrain) %>% 
      addPolygons(fillColor = ~factpal.a(Intensity),
                  weight = 1,
                  opacity = 1,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 0.9,
                  label = afghan_sf$NAME_1,
                  popup = popup.cont.a) %>% 
      addLegend("bottomright", 
                values = ~Intensity,
                colors =c("#A6655F", "#FABF78", "#F5DB9E", "#C3B299","#5A5981"), 
                labels= c("Very High", "High", "Medium", "Low", "None"),
                title = "Strikes Intensity:",
                labFormat = labelFormat(digits = 2),
                opacity = 1)
    
    m.a
  })
  
  # Pakistan Leaflet Map
  
  output$leaflet_p <- renderLeaflet({
    m.p <- leaflet(pakistan_sf)
    pakistan_map <- m.p %>% 
      addProviderTiles(providers$Stamen.Terrain) %>% 
      addPolygons(fillColor = ~factpal.p(Intensity),
                  weight = 1,
                  opacity = 1,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 0.9,
                  label = pakistan_sf$NAME_1,
                  popup = popup.cont.p) %>% 
      addLegend("bottomright", 
                values = ~Intensity,
                colors =c("#A6655F", "#FABF78", "#F5DB9E", "#C3B299","#5A5981"), 
                labels= c("Very High", "High", "Medium", "Low", "None"),
                title = "Strikes Intensity:",
                labFormat = labelFormat(digits = 2),
                opacity = 1)
    
    pakistan_map
  })
  
  # Somalia Leaflet Map
  
  output$leaflet_s <- renderLeaflet({
    m.s <- leaflet(somalia_sf)
    somalia_map <- m.s %>% 
      addProviderTiles(providers$Stamen.Terrain) %>% 
      addPolygons(fillColor = ~factpal.s(Intensity),
                  weight = 1,
                  opacity = 1,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 0.9,
                  label = somalia_sf$NAME_1,
                  popup = popup.cont.s) %>% 
      addLegend("bottomright", 
                values = ~Intensity,
                colors =c("#A6655F", "#FABF78", "#F5DB9E", "#C3B299","#5A5981"), 
                labels= c("Very High", "High", "Medium", "Low", "None"),
                title = "Strikes Intensity:",
                labFormat = labelFormat(digits = 2),
                opacity = 1)
    
    somalia_map
  })
  
  # Yemen Leaflet Map
  
  output$leaflet_y <- renderLeaflet({
    m.y <- leaflet(yemen_sf)
    yemen_map <- m.y %>% 
      addProviderTiles(providers$Stamen.Terrain) %>% 
      addPolygons(fillColor = ~factpal.y(Intensity),
                  weight = 1,
                  opacity = 1,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 0.9,
                  label = yemen_sf$NAME_1,
                  popup = popup.cont.y) %>% 
      addLegend("bottomright", 
                values = ~Intensity,
                colors =c("#A6655F", "#FABF78", "#F5DB9E", "#C3B299","#5A5981"), 
                labels= c("Very High", "High", "Medium", "Low", "None"),
                title = "Strikes Intensity:",
                labFormat = labelFormat(digits = 2),
                opacity = 1)
    
    yemen_map
  })
  
  # Dataset
  
  output$data1 <- DT::renderDataTable(drone, options = list(scrollX = T))
}