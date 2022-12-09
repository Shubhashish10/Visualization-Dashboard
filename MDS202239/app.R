
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(plotly)

ui = dashboardPage(skin="black",
                   dashboardHeader(title = "Dashboard", titleWidth =250),
                   dashboardSidebar(
                     width=300,
                     sidebarMenu(
                       menuItem(h4("Introduction"),tabName = "menu1"),
                       menuItem(h4("Plots"),tabName = "menu2"),
                       menuItem(h4("Summary"),tabName = "menu3")
                     )
                   ),
                   
                   dashboardBody(
                     tabItems(
                       tabItem("menu1",
                               mainPanel(
                                 h1("Amsterdam Housing Price Data",align = 'center',style = "color:blue"),
                                 h4("The following report contains the graphical analysis of data containing Amsterdam house prices"),
                                 img(src = 'datadesciption.jpg',height = "390", width="1222",align = 'center')
                               )
                       ),
                       tabItem("menu2",
                               tabsetPanel(
                                 tabPanel(h3("Price vs Area on Log scale"),
                                          fluidPage(
                                            fluidRow(
                                              
                                              plotOutput("plot1"),
                                            )
                                          )
                                 ),
                                 
                                 
                                 tabPanel(h3("Location of the houses compared to their Price per unit Area"),
                                          fluidPage(
                                            fluidRow(
                                              
                                              plotlyOutput("plot2"),
                                              h3("hover cursor over datapoints"))
                                          )),tabPanel(h3("Location of the houses compared to their Price per unit Area"),
                                                      fluidPage(
                                                        fluidRow(
                                                          
                                                          plotlyOutput("plot3"),
                                                          h3("hover cursor over datapoints"))
                                                      )),
                                 
                                 
                               )),
                       
                       
                       tabItem("menu3", 
                               fluidPage(
                                 fluidRow(
                                   
                                   
                                   h3("Observations:"),h3("1. We can observe that price and area of a house are highly correlated."),h3("2. We can see the houses in the central region are more expensive per unit area."),h3("3. Average Price per unit area of houses with 10, 13 and 14 rooms is highest, which suggests
that these are luxury houses."))
                               )
                       )
                       
                       
                       
                       
                       
                     )
                     
                     
                   )
)                   


server = function(input, output) {
  
  ahspd = as.data.frame(na.omit(hspd))
  
  plott2 = ggplot(ahspd, aes(x=Lon, y=Lat,colour=Price/Area, text=paste("Price/Area->",Price/Area)))+geom_point()+theme_classic()+ theme(panel.background = element_rect(fill = "black"))+ scale_colour_gradient(low="blue", high="yellow")+xlab(label = 'Longitude')+ylab(label = 'Latitude')
  
  room_stat = ahspd%>%
    group_by(Room)%>%
    summarise(mean_price_per_unit_area=sum(Price)/sum(Area))
  plott3 = ggplot(data =  room_stat, aes(x=factor(Room), y=mean_price_per_unit_area, text=paste("Rooms->",Room,", Mean Price/Area->",mean_price_per_unit_area))) + geom_col() + xlab(label = "Number of Rooms")+ ylab(label = "Average Price per unit Area")+theme_classic()
  
  
  
  output$plot1 = renderPlot( ggplot(ahspd, aes(x=Area, y=Price))+geom_point()+scale_x_continuous(trans='log2') +
                               scale_y_continuous(trans='log2') +theme_classic() + geom_smooth(method = 'lm')
                             
                             
  )
  
  output$plot2 = renderPlotly( ggplotly(plott2, tooltip="text")
                               
                               
  )
  
  output$plot3 = renderPlotly( ggplotly(plott3, tooltip="text")
                               
                               
  )
  
  
  
  
}



shinyApp(ui = ui, server = server)


