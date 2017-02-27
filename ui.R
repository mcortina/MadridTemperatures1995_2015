library(DT)
library(plotly)
library(ggplot2)

# Dataset estructure:
#   year month avgTempMonthly avgTempMinMonthly avgTempMaxMonthly
# 1 1988     1            7.8               5.0              10.6
# 2 1988     2            7.6               3.8              11.4
# 3 1988     3           11.7               6.0              17.4
# 4 1988     4           12.5               8.1              16.9
# 5 1988     5           15.5              11.0              20.0
# 6 1988     6           18.2              13.3              23.1
# ...

#setwd("~/Documents/Curso Data Science Hopkins Coursera/09 Developing Data Products/week 4/submission")
MyData <- read.csv(file="statsMadridTemp1995_2015.csv", header=TRUE, sep=";")
MyData$year_num = MyData$year
MyData$year = as.character(MyData$year)
MyData$month_abb = month.abb[MyData$month]
month_all = month.abb[c(1:12)]

#----------------------------------------------------
# 1.
#----------------------------------------------------
fluidPage(
  headerPanel('Average monthly temperatures in Madrid, Spain (1995-2015).'),
  fluidRow(column(12,hr())),
  column(12,
         h2("1. Let's explore and play with the dataset"),
         p("The dataset is about the average monthly temperatures in Madrid, Spain."),
         p("You can add or remove years or months simply using the controls provided below. Table with the data is automatically refeshed, also the 3 plots below for each temperature."),
         p("The three temperatures are the monthly averages of its own daily average, minimum and maximun.")
         ),
  fluidRow(
    column(12,offset = 0,
           column(1, selectInput('year', 'Year', unique(as.character(MyData$year)), selected = c(1995, 2005, 2015),multiple=TRUE, selectize=TRUE)),
           column(1, checkboxGroupInput('month', 'Month', unique(as.character(MyData$month_abb)), selected = month_all)),
           column(6, DT::dataTableOutput("table")))
  ),

  fluidRow(column(12,hr())),
  column(12,
         p("Here you can see here the three temperature plots with the data above selected.")),
  fluidRow(
    column(12),
    column(4,offset = 0,headerPanel('Plots'),plotlyOutput("plot1")),
    column(4,offset = 0,headerPanel('Plots'),plotlyOutput("plot2")),
    column(4,offset = 0,headerPanel('Plots'),plotlyOutput("plot3"))
  ),

#----------------------------------------------------
# 2
#----------------------------------------------------
  fluidRow(column(12,hr())),
  column(12,
         h2("2. Monthy analisys"),
         p("Now let's have a look over a monthly comparison through years. Feel free to play with the selected month and the years. Also automated tendency calculation is provided. Choose winter months, you will get a surprise don't expected.")),
  fluidRow(
    column(12,offset = 0,
           column(1, selectInput('month2', 'Month', unique(as.character(MyData$month_abb)), selected = "Aug")),
           column(1, checkboxGroupInput('year2', 'Year', unique(as.character(MyData$year)), selected = unique(MyData$year))),
           column(8, plotOutput("plot4", "auto", height="550")),
           fluidRow(
              column(2,  hr()),
              column(2,  textOutput("slopeMax")),
              column(2,  hr()),
              column(2,  textOutput("slopeAvg")),
              column(2,  hr()),
              column(2,  textOutput("slopeMin"))
              )
           )
  )
)









