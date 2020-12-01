library(shiny)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  dashboardHeader(title = "Gmovil"),
  dashboardSidebar(
    #sliderInput("rateThreshold", "Warn when rate exceeds",
    #            min = 0, max = 50, value = 3, step = 0.1
    #),
    sidebarMenu(
      menuItem("Bienvenidos", tabName = "dashboard", icon = icon("home", lib = "glyphicon")),
      menuItem("Infracciones 1", tabName = "rawdata",icon = icon("chart-pie")),
      menuItem("Infracciones 2", tabName = "rawdata2",icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                box(tags$h2(em("Bienvenidos al portal de datos de Gmovil")),width = 12,
                    img(src="quedate en casa.png", height=300, width="100%"))
              )
      ),
      tabItem("rawdata",
              fluidRow(
                column(width = 12,
                selectInput("area", "Seleccione área",levels(as.factor(infracciones$Responsable)),
                            selected = "Mantenimiento",width = "100%")),
                box(amChartsOutput("pie_plot"),width = 12),
                box(DTOutput("infr_1"),width = 6),
                box(DTOutput("infr_2"),width = 6)
              )
      ),
      tabItem("rawdata2",
              fluidRow(
                column(width = 12,
                       selectInput("servicio", "Seleccione servicio",levels(as.factor(data_viz$Servicio)),
                                   selected = "Troncal",width = "100%")),
                box(amChartsOutput("serie_1"),width = 12),
                box(DTOutput("vizu_1"),width = 6),
                box(DTOutput("vizu_2"),width = 6),
                box(title="Las 6 rutas con más multas",
                    amChartsOutput("ranking_1"),width = 6),
                box(title="Las 6 multas mas frecuentes",
                    amChartsOutput("ranking_2"),width = 6)
              )
      )
    )
  )
))
