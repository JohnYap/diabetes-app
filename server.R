library(dplyr)
library(DT)
library(shiny)
library(googleVis)
library(ggplot2)
library(leaflet)
library(maps)
library(scales)
library(gtable)
library(ggpubr)

##########  REORDER LEVELS ###########

df$sponsor_type <-factor(df$sponsor_type, 
                         levels = c("US Fed", "NIH", "Industry", "Other"))

df$condition_type <-factor(df$condition_type, 
                           levels = c("Type 1", "Type 2", "Diabetes", "Other"))

df$status_new <-factor(df$status_new, 
                       levels = c("Not yet recruiting", "Recruiting", "Active, not recruiting", "Completed", "Other"))

df$enrollment_n <-factor(df$enrollment_n, 
                         levels = c("<=50", "50-100", "100-1,000", "1,000-5,000", "5,000-10,000", ">10,000"))

df$phase <-factor(df$phase, 
                  levels = c("Early Phase 1", "Phase 1", "Phase 1/Phase 2", "Phase 2",
                             "Phase 2/Phase 3", "Phase 3", "Phase 4", "N/A"))

df$duration_n <-factor(df$duration_n, 
                         levels = c("<= 1 year", "1-2 years", "2-5 years", "5-10 years", "10+ years"))

##########  LAST-MINUTE FILTERS ###########

#study type
df_interventional = df %>% filter(., study_type == "Clinical Trial")
df_interventional_not_NA = df_interventional %>% filter(., intervention_type != 'NA')
df_observational = df %>% filter(., study_type == "Observational Study")
df_intervention_type_not_NA = df %>% filter(., intervention_type != 'NA')

#sponsors
df1 = df %>% filter(., sponsor_type == "US Fed")
df2 = df %>% filter(., sponsor_type == "NIH")
df3 = df %>% filter(., sponsor_type == "Industry")
df4 = df %>% filter(., sponsor_type == "Other")

df_enrollment_not_NA = df %>% filter(., enrollment_n != 'NA')

df_duration_not_NA = df %>% filter(., duration_n != 'NA')

#data table
df_by_study = df %>% group_by(., study_type) %>% summarise(., n())
names(df_by_study) = c("study_type", "n")

################################

shinyServer(function(input, output){

  ### STUDY TYPES ###
  output$studyinfo <- renderPlot(

    if (input$chosen == "Sponsor Type") {
      
      num_trials = df %>% 
        group_by(., study_type, sponsor_type) %>%
        summarise(., num_trials = n())
      df_sponsor = inner_join(df, num_trials, c("study_type","sponsor_type"))
        
      g <- ggplot(data = df_sponsor, aes(x = sponsor_type)) 
      g + geom_bar(aes(fill = sponsor_type)) + 
        geom_text(stat = 'count', aes(label = num_trials), size=3.5, vjust=-.5) +
        labs(
          #title = "Sponsor Type by Study Type", 
          x = "Sponsor Type by Study Type", 
          y = "Number of Studies", 
          fill = "Sponsor Type") +
        theme(
          #plot.title = element_text(color="black", size=20, face="bold.italic"),
          axis.text = element_text(size = 13),
          axis.title.x = element_text(color="black", size=16),# face="bold"),
          axis.title.y = element_text(color="black", size=16),# face="bold"),
          strip.text.x = element_text(size=16, color="black") #, face="bold")
          
          # panel.background = element_rect(fill = "lightgrey",
          #                                 colour = "lightblue",
          #                                 size = 2.0, linetype = "solid"),
          # panel.grid.major = element_line(size = 0.5, linetype = 'solid',
          #                                 colour = "white"),
          # panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
          #                                 colour = "white")
          
        ) +
        facet_wrap( ~ study_type) 
           
    } else if (input$chosen == "Condition Type") {
      
      num_trials = df %>% 
        group_by(., study_type, condition_type) %>%
        summarise(., num_trials = n())
      df_condition = inner_join(df, num_trials, c("study_type","condition_type"))      
      
      g <- ggplot(data = df_condition, aes(x = condition_type)) 
      g + geom_bar(aes(fill = condition_type)) +
        #geom_text(stat = 'count', aes(label = num_trials), size=3.5, vjust=-.5) +
        geom_text(stat = 'count', aes(label = num_trials), size=3.5, vjust=-.5) +
        labs(
          #title = "Sponsor Type by Study Type", 
          x = "Condition Type by Study Type", 
          y = "Number of Studies", 
          fill = "Condition Type") +
        theme(
          #plot.title = element_text(color="black", size=20, face="bold.italic"),
          axis.text = element_text(size = 13),
          axis.title.x = element_text(color="black", size=16), 
          axis.title.y = element_text(color="black", size=16), 
          strip.text.x = element_text(size=16, color="black")
        ) +        
        facet_wrap( ~ study_type)
      
    } else if (input$chosen == "Intervention Type") {
      
      num_trials = df_intervention_type_not_NA %>% 
        group_by(., study_type, intervention_type) %>%
        summarise(., num_trials = n())
      df_intervention = inner_join(df, num_trials, c("study_type","intervention_type"))      
      
      g <-  ggplot(data = df_intervention, aes(x = intervention_type)) 
      g + geom_bar(aes(fill = intervention_type)) +
        geom_text(stat = 'count', aes(label = num_trials), size=3.5, vjust=-.5) +
        labs(
          #title = "Sponsor Type by Study Type", 
          x = "Intervention Type by Study Type", 
          y = "Number of Studies", 
          fill = "Intervention Type") +
        theme(
          #plot.title = element_text(color="black", size=20, face="bold.italic"),
          axis.text = element_text(size = 13),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_text(color="black", size=16), 
          axis.title.y = element_text(color="black", size=16), 
          strip.text.x = element_text(size=16, color="black")
        ) +        
        facet_wrap( ~ study_type)
      
    } else if (input$chosen == "Status") {

      num_trials = df %>% 
        group_by(., study_type, status_new) %>%
        summarise(., num_trials = n())
      df_status = inner_join(df, num_trials, c("study_type","status_new"))          
            
      g <-  ggplot(data = df_status, aes(x = status_new)) 
      g + geom_bar(aes(fill = status_new)) + 
        geom_text(stat = 'count', aes(label = num_trials), size=3.5, vjust=-.5) +
        labs(
          #title = "Sponsor Type by Study Type", 
          x = "Status by Study Type", 
          y = "Number of Studies", 
          fill = "Status") +
        theme(
          #plot.title = element_text(color="black", size=20, face="bold.italic"),
          axis.text = element_text(size = 13),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_text(color="black", size=16), 
          axis.title.y = element_text(color="black", size=16), 
          strip.text.x = element_text(size=16, color="black")
        ) +   
        facet_wrap( ~ study_type)
      
    } else if (input$chosen == "Enrollment") {
      
      num_trials = df_enrollment_not_NA %>% 
        group_by(., study_type, enrollment_n) %>%
        summarise(., num_trials = n())
      df_enrollment = inner_join(df, num_trials, c("study_type","enrollment_n"))       
      
      g <-  ggplot(data = df_enrollment, aes(x = enrollment_n))
      g + geom_bar(aes(fill = enrollment_n)) + 
        geom_text(stat = 'count', aes(label = num_trials), size=3.5, vjust=-.5) +
        labs(
          #title = "Sponsor Type by Study Type", 
          x = "Enrollment by Study Type", 
          y = "Number of Studies", 
          fill = "Enrollment") +
        theme(
          #plot.title = element_text(color="black", size=20, face="bold.italic"),
          axis.text = element_text(size = 13),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_text(color="black", size=16), 
          axis.title.y = element_text(color="black", size=16), 
          strip.text.x = element_text(size=16, color="black")
        ) +    
        facet_wrap( ~ study_type)
      
    } else if (input$chosen == "Phase (Clinical Trial)") {
      
      num_trials = df_interventional_not_NA %>% 
        group_by(., study_type, phase) %>%
        summarise(., num_trials = n())
      df_phase = inner_join(df, num_trials, c("study_type","phase"))             
      
      g <-  ggplot(data = df_phase, aes(x = phase)) 
      g + geom_bar(aes(fill = phase)) + 
        geom_text(stat = 'count', aes(label = num_trials), size=3.5, vjust=-.5) +
        labs(
          #title = "Sponsor Type by Study Type", 
          x = "Phase (Clinical Trial)", 
          y = "Number of Studies", 
          fill = "Phase") +
        theme(
          #plot.title = element_text(color="black", size=20, face="bold.italic"),
          axis.text = element_text(size = 13),
          #axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_text(color="black", size=16), 
          axis.title.y = element_text(color="black", size=16), 
          strip.text.x = element_text(size=16, color="black")
        ) +    
        facet_wrap( ~ study_type)
      
    } else {

      num_trials = df_duration_not_NA %>% 
        group_by(., study_type, duration_n) %>%
        summarise(., num_trials = n())
      df_duration = inner_join(df, num_trials, c("study_type","duration_n"))              
            
      g <- ggplot(data = df_duration, aes(x = duration_n))
      g + geom_bar(aes(fill = duration_n)) + 
        geom_text(stat = 'count', aes(label = num_trials), size=3.5, vjust=-.5) +
        labs(
          #title = "Sponsor Type by Study Type", 
          x = "Duration by Study Type", 
          y = "Number of Studies", 
          fill = "Duration") +
        theme(
          #plot.title = element_text(color="black", size=20, face="bold.italic"),
          axis.text = element_text(size = 13),
          #axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_text(color="black", size=16), 
          axis.title.y = element_text(color="black", size=16), 
          strip.text.x = element_text(size=16, color="black")
        ) +   
        facet_wrap( ~ study_type)
      
    }, height = 650)

  ### TIME ###
  output$time <- renderPlot({

    if (input$sel == "Studies by Start Year") {
      
      #sponsor
      num_trials_sponsor = df %>% filter(., start_date_yr > 1990 & completion_date_yr < 2030) %>%
        group_by(., start_date_yr, sponsor_type) %>%
        summarise(., num_trials = n()) 
      
      p1 <- ggplot(data=num_trials_sponsor, aes(x=start_date_yr, y=num_trials, group=sponsor_type)) + 
        geom_line(aes(linetype = sponsor_type, color = sponsor_type)) +
        scale_linetype_manual(values=rep("solid", 4)) +
        geom_point() +
        labs(
          title = "Number of Studies by Start Year and by Sponsor Type", 
          x = "Start Year", 
          y = "Number of Studies", 
          group = "Sponsor Type") +
        theme(
          plot.title = element_text(color="black", size=16, face="bold"),
          axis.text = element_text(size = 13),
          axis.title.x = element_text(color="black", size=16), 
          axis.title.y = element_text(color="black", size=16),
          legend.position = c(0.15, 0.6)
        ) 
      
      #condition
      num_trials_intervention = df_intervention_type_not_NA %>% filter(., start_date_yr > 1990 & completion_date_yr < 2030) %>%
        group_by(., start_date_yr, intervention_type) %>%
        summarise(., num_trials = n()) 
      
      p2 <- ggplot(data=num_trials_intervention, aes(x=start_date_yr, y=num_trials, group=intervention_type)) + 
        geom_line(aes(linetype = intervention_type, color = intervention_type)) + 
        scale_linetype_manual(values=rep("solid", 7)) +        
        geom_point() +
        labs(
          title = "Number of Studies by Start Year and by Intervention Type", 
          x = "Start Year", 
          y = "Number of Studies", 
          group = "Intervention Type") +
        theme(
          plot.title = element_text(color="black", size=16, face="bold"),
          axis.text = element_text(size = 13),
          axis.title.x = element_text(color="black", size=16), 
          axis.title.y = element_text(color="black", size=16),
          legend.position = c(0.15, 0.6)
        ) 

      ggarrange(p1, p2, ncol = 2, nrow = 1)

    } else {

      #sponsor
      num_trials_sponsor = df %>% filter(., start_date_yr > 1990 & completion_date_yr < 2030) %>%
        group_by(., completion_date_yr, sponsor_type) %>%
        summarise(., num_trials = n()) 
      
      p1 <- ggplot(data=num_trials_sponsor, aes(x=completion_date_yr, y=num_trials, group=sponsor_type)) + 
        geom_line(aes(linetype = sponsor_type, color = sponsor_type)) +
        scale_linetype_manual(values=rep("solid", 4)) +
        geom_point() +
        labs(
          title = "Number of Studies by Completion Year and by Sponsor Type", 
          x = "Completion Year", 
          y = "Number of Studies", 
          group = "Sponsor Type") +
        theme(
          plot.title = element_text(color="black", size=16, face="bold"),
          axis.text = element_text(size = 13),
          axis.title.x = element_text(color="black", size=16), 
          axis.title.y = element_text(color="black", size=16),
          legend.position = c(0.85, 0.5)
        ) 
      
      #condition
      num_trials_intervention = df_intervention_type_not_NA %>% filter(., start_date_yr > 1990 & completion_date_yr < 2030) %>%
        group_by(., completion_date_yr, intervention_type) %>%
        summarise(., num_trials = n()) 
      
      p2 <- ggplot(data=num_trials_intervention, aes(x=completion_date_yr, y=num_trials, group=intervention_type)) + 
        geom_line(aes(linetype = intervention_type, color = intervention_type)) + 
        scale_linetype_manual(values=rep("solid", 7)) +        
        geom_point() +
        labs(
          title = "Number of Studies by Completion Year and by Intervention Type", 
          x = "Completion Year", 
          y = "Number of Studies", 
          group = "Intervention Type") +
        theme(
          plot.title = element_text(color="black", size=16, face="bold"),
          axis.text = element_text(size = 13),
          axis.title.x = element_text(color="black", size=16), 
          axis.title.y = element_text(color="black", size=16),
          legend.position = c(0.85, 0.5)
        ) 
      
      ggarrange(p1, p2, ncol = 2, nrow = 1)
      
    }
  }, height = 650)
  
  ### SPONSOR ###
  # output$sponsor <- renderPlot(
  #   
  #   if (input$selected == "sponsor_type") {
  #     ggplot(data = df, aes(x = sponsor_type)) + geom_bar(aes(fill = study_type), position = "dodge")
  #   } else if (input$selected == "sponsor") {
  #     ggplot(data = df, aes(x = sponsor_type)) + geom_bar(aes(fill = study_type), position = "dodge")
  #   } else {
  #     ggplot(data = df, aes(x = sponsor_num)) + geom_bar(aes(fill = study_type), position = "dodge")
  #   }
  #   
  #   )
  
  # show data using DataTable
  output$table <- DT::renderDataTable({
    datatable(df_sponsor) #%>% 
      #formatStyle(input$selected, background="skyblue", fontWeight='bold')
  })
  
  ### MAPS ###
  output$mymap <- renderLeaflet({
    
    if (input$choice == "All") {
      leaflet() %>% 
        addTiles() %>%
        addMarkers(data = df %>% filter(., study_type == "Clinical Trial"), group = "Clinical Trial", clusterOptions = markerClusterOptions()) %>%
        addMarkers(data = df %>% filter(., study_type == "Observational Study"), group = "Observational Study", clusterOptions = markerClusterOptions()) %>%
        addLayersControl(
          baseGroups = c("Clinical Trial", "Observational Study"),
          options = layersControlOptions(collapsed = FALSE),
          position = c("topleft") 
        ) 
    } 
    
    else if (input$choice == "US Fed") {
      leaflet() %>%
        addTiles() %>%
        addMarkers(data = df1 %>% filter(., study_type == "Clinical Trial"), group = "Clinical Trial", clusterOptions = markerClusterOptions()) %>%
        addMarkers(data = df1 %>% filter(., study_type == "Observational Study"), group = "Observational Study", clusterOptions = markerClusterOptions()) %>%
        addLayersControl(
          baseGroups = c("Clinical Trial", "Observational Study"),
          options = layersControlOptions(collapsed = FALSE),
          position = c("topleft")
        )        
      
    } else if (input$choice == "NIH") {
      leaflet() %>%
        addTiles() %>%
        addMarkers(data = df2 %>% filter(., study_type == "Clinical Trial"), group = "Clinical Trial", clusterOptions = markerClusterOptions()) %>%
        addMarkers(data = df2 %>% filter(., study_type == "Observational Study"), group = "Observational Study", clusterOptions = markerClusterOptions()) %>%
        addLayersControl(
          baseGroups = c("Clinical Trial", "Observational Study"),
          options = layersControlOptions(collapsed = FALSE),
          position = c("topleft")
        )
      
    } else if (input$choice == "Industry") {
      leaflet() %>%
        addTiles() %>%
        addMarkers(data = df3 %>% filter(., study_type == "Clinical Trial"), group = "Clinical Trial", clusterOptions = markerClusterOptions()) %>%
        addMarkers(data = df3 %>% filter(., study_type == "Observational Study"), group = "Observational Study", clusterOptions = markerClusterOptions()) %>%
        addLayersControl(
          baseGroups = c("Clinical Trial", "Observational Study"),
          options = layersControlOptions(collapsed = FALSE),
          position = c("topleft")
        )        
      
    } else {
      leaflet() %>%
        addTiles() %>%
        addMarkers(data = df4 %>% filter(., study_type == "Clinical Trial"), group = "Clinical Trial", clusterOptions = markerClusterOptions()) %>%
        addMarkers(data = df4 %>% filter(., study_type == "Observational Study"), group = "Observational Study", clusterOptions = markerClusterOptions()) %>%
        addLayersControl(
          baseGroups = c("Clinical Trial", "Observational Study"),
          options = layersControlOptions(collapsed = FALSE),
          position = c("topleft")
        )
    }
    
  })  
  
})
