library(shiny)

shinyServer(function(input, output) {
  
  tabla_1 <- reactive({
    #Positivos=Estado_V() %>% mutate(contar=1) %>% 
    #  group_by(Estado=PCR1) %>% summarise(PCR=sum(contar))
  })
  
  #Infracciones----
  
  output$pie_plot <- renderAmCharts({
    
    cons_pie=infracciones %>% filter(Responsable==input$area) %>% group_by(label=Valor) %>% 
      summarise(value=as.numeric(length(Codigo))) %>% data.frame() %>% mutate(label=as.character(label))
    
    amPie(data = cons_pie, legend = TRUE, legendPosition = "left",depth = 20, export = TRUE)
    
  })
  
  output$infr_1 <- renderDataTable({
    
    datatable(cons1)
    
  })
  
  output$infr_2 <- renderDataTable({
    
    cons2=infracciones %>% filter(Responsable==input$area) %>% group_by(Responsable,Codigo_M) %>% 
      summarise(Conteo=length(Codigo))
    cons2 <- cons2[with(cons2, order(-cons2$Conteo)), ] # Orden inverso
    datatable(cons2)
    
  })
  
  #data_viz----
  output$serie_1 <- renderAmCharts({
    
    cons11=data_viz %>% filter(Servicio==input$servicio) %>% group_by(FechaDeIdent) %>% 
      summarise(Multas=as.numeric(length(DateKey)))
    
    plot(cons11$FechaDeIdent,cons11$Multas,type="line")
    
    cons11$FechaDeIdent=as.POSIXct(cons11$FechaDeIdent)
    
    cons11$Multas_low <- cons11$Multas-2.5
    cons11$Multas_up <- cons11$Multas+2.5
    
    color_t=ifelse(input$servicio=="Troncal","red","blue")
    
    amTimeSeries(cons11, "FechaDeIdent", list(c("Multas_low", "Multas", "Multas_up")), 
                 color = color_t, bullet = c("round"), export = TRUE)
    
  })
  
  output$vizu_1 <- renderDataTable({
    
    cons8=data_viz %>% filter(Servicio==input$servicio) %>% group_by(Area,mes) %>% 
      summarise(conteo=as.numeric(length(DateKey))) %>% spread(Area,conteo)
    
    datatable(cons8)
    
  })
  
  output$vizu_2 <- renderDataTable({
    
    cons10=data_viz %>% filter(Servicio==input$servicio) %>% group_by(Etapa,Area) %>% 
      summarise(conteo=as.numeric(length(DateKey))) %>% spread(Area,conteo)
    
    datatable(cons10)
  })
  
  output$ranking_1 <- renderAmCharts({
    
    cons4=data_viz %>% filter(Servicio==input$servicio) %>% group_by(Ruta) %>% 
      summarise(conteo=as.numeric(length(DateKey)))
    cons4 <- cons4[with(cons4, order(-cons4$conteo)), ] # Orden inverso
    
    amBarplot(x = "Ruta", y = "conteo", data = cons4[1:5,], depth = 15, labelRotation = -90,
              show_values = TRUE, export = TRUE)
    #datatable(head(cons4))
  })
  
  output$ranking_2 <- renderAmCharts({
    
    cons6=data_viz %>% filter(Servicio=="Zonal") %>% group_by(Infraccion) %>% 
      summarise(conteo=as.numeric(length(DateKey)))
    cons6 <- cons6[with(cons6, order(-cons6$conteo)), ] # Orden inverso
    
    amBarplot(x = "Infraccion", y = "conteo", data = cons6[1:5,], depth = 15, labelRotation = -90,
              show_values = TRUE, export = TRUE)
    #datatable(head(cons4))
  })
  
})
