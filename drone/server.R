server <- function(input, output) {
  output$plotly_1 <- renderPlotly({
    strike.plot <- 
      drone %>% 
      filter(Country == input$Country) %>% 
      select(Date,`Maximum Strikes`) %>% 
      mutate(Date = floor_date(Date, "quarter")) %>% 
      group_by(Date) %>% 
      summarise(Strikes = sum(`Maximum Strikes`)) %>%
      mutate(Quart = case_when(
        month(Date) == "1" ~ "Q1",
        month(Date) == "4" ~ "Q2",
        month(Date) == "7" ~ "Q3",
        month(Date) == "10" ~ "Q4"
      )) %>% 
      mutate(Quart = paste(year(Date), Quart),
             Label = glue("{Quart}
                        Strikes: {comma(Strikes)}"),
             Date = as.Date(Date, format = "%d-%m-%Y")) %>%  
      ggplot(aes(Date, Strikes, text = Label)) +
      geom_line(aes(group=1),color = "#5A5981", size = 1) +
      geom_point(col = "#A6655F", size = 2, alpha = 0.5) +
      scale_x_date(breaks = "3 months", date_labels = "%b %Y") +
      labs(title = "Total Strikes by Quartal",
           x = NULL,
           y = NULL) +
      theme(plot.background = element_rect(fill = "#E8E1DB", color = "#E8E1DB"),
            panel.background = element_rect(fill = "#E8E1DB"),
            panel.grid = element_line(alpha(colour = "#C3C3C3",alpha = 0.4)),
            axis.text.x = element_text(angle = 45, hjust = 5, color = "Black", face = "italic", family = "Georgia"),
            axis.text.y = element_text(face = "bold", family = "Georgia"),
            plot.title = element_text(face = "bold", family = "Georgia"))
    
    ggplotly(strike.plot, tooltip = "text")
  })
  
  output$plotly_2 <- renderPlotly({
    death.plot <- 
      drone %>% 
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
      labs(title = "Deaths",
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
  
  output$plotly_3 <- renderPlotly({
    injured.plot <- 
      drone %>% 
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
    
    ggplotly(injured.plot, tooltip = "text")
  })
  
  output$Strikebox <- renderValueBox({
    
    total.strike <- drone %>% 
      filter(Country == input$Country) %>% 
      select(`Maximum Strikes`) %>% 
      summarize(`Strikes Reported` = sum(`Maximum Strikes`))
    
    valueBox(tags$p(comma(total.strike$`Strikes Reported`), style = "font-size: 100%; color: white;"),
             subtitle = "Total Strikes",
             color = "teal", 
             icon = icon("rocket"))
  })
  
  output$Deathbox <- renderValueBox({
    
    inno.death <- drone %>% 
      filter(Country == input$Country) %>% 
      select(`Maximum Civilians Killed`,`Maximum Children Killed`) %>% 
      summarize(Civilians = sum(`Maximum Civilians Killed`),
                Children = sum(`Maximum Civilians Killed`)) %>% 
      mutate(`Deaths Reported` = Civilians + Children)
    
    valueBox(tags$p(comma(inno.death$`Deaths Reported`), style = "font-size: 100%; color: white;"),
             subtitle = "Innocent Death",
             color = "teal", 
             icon = icon("skull"))
  })
  
  output$Deathbox1 <- renderValueBox({
    
    total.death <- drone %>% 
      filter(Country == input$Country) %>% 
      select(`Maximum Total Killed`) %>% 
      summarize(`Deaths Reported` = sum(`Maximum Total Killed`))
    
    valueBox(tags$p(comma(total.death$`Deaths Reported`), style = "font-size: 100%; color: white;"),
             subtitle = "Total Death",
             color = "teal", 
             icon = icon("hospital"))
  })
  
  output$data1 <- DT::renderDataTable(drone, options = list(scrollX = T))
}