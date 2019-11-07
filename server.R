library(ggplot2)

FI <- read.csv("Food_Inspections.csv",stringsAsFactors = FALSE)
FI$Year = format(as.Date(FI[,11],"%m/%d/%Y"),"%Y")

server <- function(input, output) {
  output$Figure1 <- renderPlot({
    # FIPass <- subset(FI,FI$Results=='Pass')
    filtered <-
      FI %>%
      filter(Risk== input$riskInput,
             Year== input$YearInput,
             Results== input$ResultInput)

    k = 10
    Top_k_Facilities = names(sort(summary(as.factor(filtered$Facility.Type)),decreasing = T)[1:k])
    print(Top_k_Facilities)

    ggplot(data=filtered,aes(Results))+geom_bar(aes(Facility.Type))+ggtitle("Figure 1: Frequency Distribution of Facility Types")+
      theme(plot.title = element_text(size=20,face='bold'))
  })
  
  output$Table1 <- DT::renderDataTable({
    
    filtered <-
      FI %>%
      filter(Risk== input$riskInput,
             Year== input$YearInput,
             Results== input$ResultInput)
    k = 10
    Top_k_Facilities = (sort(summary(as.factor(filtered$Facility.Type)),decreasing = T)[1:k])
    
    DT::datatable(as.data.frame(Top_k_Facilities))
  })

  output$Figure2 <- renderPlot({
    
    filtered <-
      FI %>%
      filter(Year== input$YearInput,
             Results== input$ResultInput,
             Facility.Type== input$FacilityInput)

    ggplot(data=filtered,aes(Results))+geom_bar(aes(fill=Risk))+ggtitle("Figure 2: Risk Proportions Facility Types")+
      theme(plot.title = element_text(size=20,face='bold'))
  })
  
  output$Table2 <- DT::renderDataTable({
    
    filtered <-
      FI %>%
      filter(Year== input$YearInput,
             Results== input$ResultInput,
             Facility.Type== input$FacilityInput)
    
    nrow_filtered = nrow(filtered)
    nrow_Risk1 = nrow(subset(filtered, filtered$Risk=='Risk 1 (High)'))
    nrow_Risk2 = nrow(subset(filtered, filtered$Risk=='Risk 2 (Medium)'))
    nrow_Risk3 = nrow(subset(filtered, filtered$Risk=='Risk 3 (Low)'))
    
    Percent_Risk1 = round(100*nrow_Risk1/nrow_filtered,2)
    Percent_Risk2 = round(100*nrow_Risk2/nrow_filtered,2)
    Percent_Risk3 = round(100*nrow_Risk3/nrow_filtered,2)
    
    Fraction <- list(Risk = c('Risk 1 (High)','Risk 2 (Medium)','Risk 3 (Low)'), 
                     Percentage=c(Percent_Risk1,Percent_Risk2,Percent_Risk3))
    
    DT::datatable(as.data.frame(Fraction))
  })  
  
  # output$Figure3 <- renderPlot({
  #   filtered <-
  #     FI %>%
  #     filter(Inspection.ID>= input$InspectionIDInput[1],
  #            Inspection.ID<= input$InspectionIDInput[2],
  #            Risk== input$riskInput,
  #            Year== input$YearInput,
  #            Facility.Type== input$FacilityInput,
  #            Results==input$ResultInput)
  # 
  #   chicago <- get_map(location = 'chicago', zoom = 11)
  #   ggmap(chicago)+geom_point(data=filtered,aes(x=Longitude,y=Latitude,color='blue'),
  #                             color="blue",size=0.8,alpha=0.1)+xlim(-87.93,-87.55)
  # })
  
  output$Figure4 <- renderPlot({
    filtered <-
      FI %>%
      filter(Year==input$YearInput,
             Results==input$ResultInput,
             Risk==input$riskInput,
             Facility.Type== input$FacilityInput)
    ggplot(data=filtered,aes(x=Latitude))+geom_histogram(data=filtered,alpha=0.8)+
      ggtitle("Figure 3: Frequency Distribution of Latitude")+
      theme(plot.title = element_text(size=20,face='bold'))

  })
  
  output$Figure5 <- renderPlot({
    filtered <-
      FI %>%
      filter(Year==input$YearInput,
             Results==input$ResultInput,
             Risk==input$riskInput,
             Facility.Type== input$FacilityInput)
    ggplot(data=filtered,aes(x=Longitude))+geom_histogram(data=filtered,alpha=0.8)+
      ggtitle("Figure 4: Frequency Distribution of Longitude")+
      theme(plot.title = element_text(size=20,face='bold'))
    
  })
  
}