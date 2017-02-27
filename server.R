library(shiny)
library(DT)
library(plotly)
library(ggplot2)

MyData <- read.csv(file="statsMadridTemp1995_2015.csv", header=TRUE, sep=";")
MyData$year_num = MyData$year
MyData$year = as.character(MyData$year)
MyData$month_abb = month.abb[MyData$month]

function(input, output) {

  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- MyData[,c(1,7,3,4,5)]
    colnames(data) <- c("Year", "Month","Avg. daily temp.","Avg. min. daily temp.","Avg. max. daily temp.")
    if (input$year != "All") {
      data <- data[data$Year %in% input$year,]
    }
    if (input$month != "All") {
      data <- data[data$Month %in% input$month,]
    }
    data
  }))

  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    data <- MyData
    if (input$year != "All") {
      data <- data[data$year %in% input$year,]
    }
    if (input$month != "All") {
      data <- data[data$month_abb %in% input$month,]
    }
  })

  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3","#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    plot(selectedData()[,c(2,3)], col = selectedData()[,1], pch = 20, cex = 2) })

  output$plot1 <- renderPlot({
    hist(selectedData()[,c(3)],breaks=12,col='darkgray',border='white')})

  plot_scale_y <- list(
    dtick = 5,
    range = c(0,45)
  )

  plot_scale_x <- list(
    dtick = 1
  )

  #Plot avg
  output$plot1 <- renderPlotly({
    plot <- plot_ly(selectedData(), x = ~month, y = ~avgTempMinMonthly, color = ~year, type = "scatter", mode = "lines", fill='tonexty')
    plot <- layout(plot, yaxis=list(title="Celsius temperature"))
    plot <- layout(plot, xaxis=list(title="Month"))
    plot <- layout(plot,title='Average monthly minimun temperature')
    plot <- layout(plot, xaxis = plot_scale_x, yaxis = plot_scale_y)
    plot
  })

  #Plot min
  output$plot2 <- renderPlotly({
    plot <- plot_ly(selectedData(), x = ~month, y = ~avgTempMonthly, color = ~year, type = "scatter", mode = "lines", fill='tonexty')
    plot <- layout(plot, yaxis=list(title="Celsius temperature"))
    plot <- layout(plot, xaxis=list(title="Month"))
    plot <- layout(plot,title='Average monthly temperature')
    plot <- layout(plot, xaxis = plot_scale_x, yaxis = plot_scale_y)
    plot
  })

  #Plot max
  output$plot3 <- renderPlotly({
    plot <- plot_ly(selectedData(), x = ~month, y = ~avgTempMaxMonthly, color = ~year, type = "scatter", mode = "lines", fill='tonexty')
    plot <- layout(plot, yaxis=list(title="Celsius temperature"))
    plot <- layout(plot, xaxis=list(title="Month"))
    plot <- layout(plot,title='Average monthly maximum temperature')
    plot <- layout(plot, xaxis = plot_scale_x, yaxis = plot_scale_y)
    plot
  })

  # Combine the selected variables into a new data frame
  selectedData2 <- reactive({
    data <- MyData
    if (input$year2 != "All") {
      data <- data[data$year_num %in% input$year2,]
    }
    if (input$month2 != "All") {
      data <- data[data$month_abb %in% input$month2,]
    }
  })

  #Plot tendency
  output$plot4 <- renderPlot({
    p <- ggplot(selectedData2(), aes(year_num)) +
      labs(x="Year",y="Celsius temperature") +
      ggtitle("Minimum, average and maximum monthly temperature through years") +
      theme(plot.title = element_text(size=18), axis.text=element_text(size=12,face="bold"),
            axis.title=element_text(size=16)) +
      geom_line(aes(y = avgTempMinMonthly),colour="steelblue") +
      geom_line(aes(y = avgTempMonthly), colour='aquamarine4') +
      geom_line(aes(y = avgTempMaxMonthly), colour = "firebrick")
    coef_min = coef(lm(avgTempMinMonthly ~ year_num, data = selectedData2() ))
    coef_avg = coef(lm(avgTempMonthly ~ year_num, data = selectedData2() ))
    coef_max = coef(lm(avgTempMaxMonthly ~ year_num, data = selectedData2() ))
    p = p + geom_abline(intercept=coef_min[1], slope=coef_min[2],colour="steelblue",linetype = 2)
    p = p + geom_abline(intercept=coef_avg[1], slope=coef_avg[2],colour="aquamarine4",linetype = 2)
    p = p + geom_abline(intercept=coef_max[1], slope=coef_max[2],colour="firebrick",linetype = 2)
    p
    })

  output$slopeMin <- renderText({
    coef_min = coef(lm(avgTempMinMonthly ~ year_num, data = selectedData2()))
    paste ("Minimun temperature slope:", format(coef_min[2], digits=3) )
  })
  output$slopeAvg <- renderText({
    coef_avg = coef(lm(avgTempMonthly ~ year_num, data = selectedData2()))
    paste ("Maximum average slope:", format(coef_avg[2], digits=3) )
  })
  output$slopeMax <- renderText({
    coef_max = coef(lm(avgTempMaxMonthly ~ year_num, data = selectedData2()))
    paste ("Maximum temperature slope:", format(coef_max[2], digits=3) )
  })

}



