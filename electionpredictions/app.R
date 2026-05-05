library(shiny)
library(dplyr)
library(rvest)
library(lmtest)
library(ggplot2)
library(ggrepel)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Improved Methods of the U.S. Presidential Election Predictions"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        h4("Predictive Model Results"),
        checkboxGroupInput("year", 
                           "Election Year",
                           choices = c(1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020, 2024),
                           selected = 2024),
        radioButtons("add_line", 
                     "Add Standard Line? ", 
                     choices = c("No" = "noline",
                                 "Yes" = "line"),
                     selected = "noline")),
      wellPanel(
        h4("Simulation Using Model"),
        numericInput("key1_input",  
                     "Net House of Representative Votes for Republican Party (Incumbent)",
                     value = 0),
        numericInput("key4_input",  
                     "Approval Rating of Current President (as a decimal)",
                     value = 0),
        numericInput("key8_oldgdp",  
                     paste0("GDP per Capita on ", Sys.Date() - 365),
                     value = 0),
        numericInput("key8_currentgdp",  
                     paste0("GDP per Capita on ", Sys.Date()),
                     value = 0)
      )),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        selected = "Instructions",
        tabPanel("Instructions",
                 h1("Instructions"),
                 h2("Overview"),
                 p(strong("Overarching Question:"), "How accurate are various models aimed at predicting the U.S. presidential election and popular vote winner?"),
                 p("The goal of this Shiny App is to analyze models for predicting the U.S. presidential election winner. This app explores three distinct models, one created by historian Allan Lichtman, and two numerical models I developed. One of my models, along with Lichtman’s, seek to predict the Electoral College margin of victory, while my other numerical model seeks to predict the Popular Vote margin of victory. For all models, the case is a given U.S. presidential election year starting in 1984 to 2024 and the scope of inference is limited to just United States presidential elections. In the various tabs of this app, you can review how my models faired in presidential elections spanning from 1984 to 2024, compare the predictions of Lichtman’s model and my own, and simulate an election outcome by inputting your own data into my models. Have fun exploring!"),
                 h2("Definitions"),
                 tags$ul(
                   tags$li(strong("Incumbent party: "), "the political party of the current U.S. president; currently Republican"),
                   tags$li(strong("Challenger party: "), "the other major political party not belonging to the current U.S. president; currently Democratic"),
                   tags$li(strong("Midterm election: "), "the election held in between presidential elections for the House of Representatives and some Senate positions")),
                 p("Note that for these purposes, only the Republican and Democratic parties are considered as the political parties in the model. Also note that in both my Electoral College and popular vote models, when referring to the margin of victory, I am referring to the amount by which the incumbent party wins/loses with respect to the challenger party. For example, for the upcoming 2028 presidential election, a positive margin of victory would indicate the Republican (incumbent party) candidate is projected to beat the Democratic (challenger party) candidate."),
                 h2("Graphs"),
                 p("There are two different graphs that you can interact with. The first one, featured on the ", em("Graph of Model vs. Actual Results (Electoral College)"), " page, allows you to select (an) election year(s) to see what my Electoral College model predicted the margin of victory in the Electoral College to be. You can choose from the list of options on the left side of the app in the “Predictive Model Results” section. On the x-axis is my model’s prediction with anything left of the y-axis indicating the challenger party to win and anything to the right of the y-axis indicating the incumbent party to win. On the y-axis is the actual Electoral College margin of victory for the given election(s). Anything above the x-axis indicates the incumbent party won and anything below the x-axis indicates the challenger party won."),
                 p("You can choose the option to add a standard line which will add the y=x line to the graph. Any points on this line would indicate that my model perfectly predicted the results. From this line, you can see just how far off my model was to a perfect prediction."),
                 p("The second graph, featured on the ", em("Graph of Model vs. Actual Results (Popular Vote)"), " page, allows you to conduct the same process except for the model I created to predict the popular vote margin of victory. Once again, the location of a point indicates what my model predicted the winner to be and what the actual results ended up being. Choosing the option to add a standard line acts in the same way as before to show just how close my model predictions were to the actual outcome."),
                 h2("Table"),
                 p("On the tab titled ", em("Table of Lichtman vs. Vagnini Predictions,"),"you will find an interactive table spanning two pages that shows the results of all three models across the past 11 presidential elections. For a given election, the table shows the candidates, the Electoral College and popular vote winners, the predictions from all three models, the actual election results, and columns indicating whether the models predicted the correct outcome. Feel free to sort by margin of victory sizes and election years. If you are looking for a specific name or value, feel free to use the “Search” bar in the upper right-hand corner of the table."),
                 h2("Simulation"),
                 p("On the tab titled ", em("Simulated Graph,"), "you can use my two models to predict the election outcome. On the left-hand side of the app in the “Simulation Using Model” section, you’ll notice 4 places to insert values:"),
                 tags$ol(
                   tags$li(strong("Net House of Representative Votes for Republican Party (Incumbent):"), " Input the net number of House of Representative seats that the Republican Party receives after the midterm elections. Since this app is meant to simulate the next presidential election (2028), we are using the Republican Party as the incumbent party since the current president (Donald Trump) is a member of the Republican Party. If the Republican party loses seats, input the value as a negative. If the Republican Party gains seats, input the value as a positive."),
                   tags$li(strong("Approval Rating of Current President (as a decimal):"), " Input the most recent approval rating received by the current president (President Trump) as a decimal value. Feel free to use whichever polling platform you prefer, but here is a ", a("link", href = "https://news.gallup.com/interactives/507569/presidential-job-approval-center.aspx", target = "_blank"), " to Gallup’s approval rating site for your convenience."),
                   tags$li(strong("GDP per Capita on [Date 1 Year Ago]:"), " Input the U.S. GDP per capita on or around the date listed (it should be about 1 year ago to date). Feel free to use whichever source you’d like, but here is a ", a("link", href = "https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?locations=US", target = "_blank"), " to the World Bank’s data."),
                   tags$li(strong("GDP per Capita on [Date Today]:"), " Input the U.S. GDP per capita as of today. Once again, feel free to use whichever source you’d like, but here is a ", a("link", href = "https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?locations=US", target = "_blank"), " to the World Bank’s data.")),
                 p("Once you input your values, a graph will appear on the page with a prediction for the Electoral College and popular vote margin of victories. Next to the data point will be the predicted values. Note that the x-axis shows the predicted Electoral College margin of victory and the y-axis shows the predicted popular vote margin of victory. Anything to the left of the y-axis indicates that the challenger party is predicted to win the Electoral College and anything to the right of the y-axis indicates that the incumbent party is predicted to win the Electoral College. Anything above the x-axis indicates the incumbent party is predicted to win the popular vote and anything below the x-axis indicates the challenger party is predicted to win the popular vote. Feel free to use real data or data you make up. Just be sure to input data that is sensible as certain inputs may result in a prediction that is out of bounds. Also, be sure to input a value greater than 0 for the GDP per capita values to avoid an error."),
                 h2("Takeaways"),
                 p("Once you’re done exploring, feel free to take a look at the ", em("Takeaways"), " tab to see a list of important insights from the various models."),
                 h2("Sources"),
                 p(em("1984 | The American Presidency Project."), tags$a(href = "https://www.presidency.ucsb.edu/statistics/elections/1984", target = "_blank", "https://www.presidency.ucsb.edu/statistics/elections/1984.")),
                 p(em("1988 | The American Presidency Project."), tags$a(href = "https://www.presidency.ucsb.edu/statistics/elections/1988", target = "_blank", "https://www.presidency.ucsb.edu/statistics/elections/1988.")), 
                 p(em("1992 | The American Presidency Project."), tags$a(href = "https://www.presidency.ucsb.edu/statistics/elections/1992", target = "_blank", "https://www.presidency.ucsb.edu/statistics/elections/1992.")),
                 p(em("1996 | The American Presidency Project."), tags$a(href = "https://www.presidency.ucsb.edu/statistics/elections/1996", target = "_blank", "https://www.presidency.ucsb.edu/statistics/elections/1996.")),
                 p(em("2000 | The American Presidency Project."), tags$a(href = "https://www.presidency.ucsb.edu/statistics/elections/2000", target = "_blank", "https://www.presidency.ucsb.edu/statistics/elections/2000.")),
                 p(em("2004 | The American Presidency Project."), tags$a(href = "https://www.presidency.ucsb.edu/statistics/elections/2004", target = "_blank", "https://www.presidency.ucsb.edu/statistics/elections/2004.")),
                 p(em("2008 | The American Presidency Project."), tags$a(href = "https://www.presidency.ucsb.edu/statistics/elections/2008", target = "_blank", "https://www.presidency.ucsb.edu/statistics/elections/2008.")),
                 p(em("2012 | The American Presidency Project."), tags$a(href = "https://www.presidency.ucsb.edu/statistics/elections/2012", target = "_blank", "https://www.presidency.ucsb.edu/statistics/elections/2012.")),
                 p(em("2016 | The American Presidency Project."), tags$a(href = "https://www.presidency.ucsb.edu/statistics/elections/2016", target = "_blank", "https://www.presidency.ucsb.edu/statistics/elections/2016.")),
                 p(em("2020 | The American Presidency Project."), tags$a(href = "https://www.presidency.ucsb.edu/statistics/elections/2020", target = "_blank", "https://www.presidency.ucsb.edu/statistics/elections/2020.")),
                 p(em("2024 | The American Presidency Project."), tags$a(href = "https://www.presidency.ucsb.edu/statistics/elections/2024", target = "_blank", "https://www.presidency.ucsb.edu/statistics/elections/2024.")),
                 p("“GDP per Capita (Current US$).”", em("World Bank Open Data,"), tags$a(href = "https://data.worldbank.org/indicator/NY.GDP.PCAP.CD", target = "_blank", "https://data.worldbank.org/indicator/NY.GDP.PCAP.CD."), "Accessed 3 Dec. 2025."),
                 p("Lichtman, Allan J., and Ken DeCell.", em("The Thirteen Keys to the Presidency."), "1990."),
                 p(em("Party Divisions of the House of Representatives, 1789 to Present | US House of Representatives: History, Art and Archives."), tags$a(href = "https://history.house.gov/Institution/Party-Divisions/Party-Divisions/.", target = "_blank", "https://history.house.gov/Institution/Party-Divisions/Party-Divisions/.")),
                 p("“Presidential Approval Ratings -- Gallup Historical Statistics and Trends.”", em("Gallup.Com,"), "Gallup, 11 Nov. 2025,", tags$a(href = "https://news.gallup.com/poll/116677/presidential-approval-ratings-gallup-historical-statistics-trends.aspx.", target = "_blank", "https://news.gallup.com/poll/116677/presidential-approval-ratings-gallup-historical-statistics-trends.aspx.")),
                 p("Wikipedia contributors. “The Keys to the White House.”", em("Wikipedia,"), "2 May 2026,", tags$a(href = "https://en.wikipedia.org/wiki/The_Keys_to_the_White_House#Lichtman's_prediction_record_(1984%E2%80%93present).", target = "_blank", "https://en.wikipedia.org/wiki/The_Keys_to_the_White_House#Lichtman's_prediction_record_(1984%E2%80%93present)."))
                 ),
        tabPanel("Graph of Model vs. Actual Results (Electoral College)",
                 plotOutput("modvsactPlotEC")),
        tabPanel("Graph of Model vs. Actual Results (Popular Vote)",
                 plotOutput("modvsactPlotMOV")),
        tabPanel("Table of Lichtman vs. Vagnini Predictions",
                 br(),
                 p(em("**Note that ", strong("EC"), " stands for Electoral College and ", strong("MOV"), " stands for margin of victory.**")),
                 br(),
                 dataTableOutput("data_table")),
        tabPanel("Simulated Graph",
                 plotOutput("simPlot")),
        tabPanel("Takeaways",
                 h1("Takeaways"),
                 p("Here are some key insights that you may have noticed while exploring:"),
                 tags$ul(
                   tags$li("While Lichtman’s model to predict the Electoral College winner (and thus the presidency) was accurate for 9 out of the last elections, mine was accurate for 10 of the past 11 elections"),
                   tags$li("The only election where my model incorrectly predicted the Electoral College winner was the Al Gore vs. George W. Bush election in 2000 where Bush won by 5 Electoral College votes"),
                   tags$li("Lichtman’s model incorrectly predicted the 2000 and 2024 presidential elections"),
                   tags$li("My model accurately predicts all 11 of the past 11 presidential election popular vote winners"),
                   tags$li("My model did a fairly good job at predicting the exact margins of victory as shown when adding the standard line to the graphs"),
                   tags$li("Small changes in inputs can significantly affect both of my models’ predictions, especially in regard to the GDP per capita. When inputting smaller values into the GDP per capita slots, the percent change grows quite high and thus, you may often see “out of bounds” errors"),
                   tags$li("Although my models haven’t best used on an unseen U.S. presidential election yet, I look forward to seeing it’s accuracy in 2028!")),
                 p(em("Thank you for taking the time to explore my page! I hope you enjoyed."))
                 )
        
        
      )
    )
  )
)

# Read in dataset and make models/data tables
full_data <- readRDS("full_data.rds")
model_ec <- lm(ec_incumbent ~ incumbent_approval_rating + incumbent_party_net_votes + percent_change_gdp_per_cap_short, data = full_data)
model_mov <- lm(mov_incumbent ~ incumbent_approval_rating + incumbent_party_net_votes + percent_change_gdp_per_cap_short, data = full_data)
lichtvsrobdata <- full_data |> select(year, candidate_incumbent, candidate_challenger, win_ec, pred_ec_inc, ec_incumbent, lichtman_ec_correct, rob_ec_correct, win_mov, pred_mov_inc, mov_incumbent, rob_mov_correct) |> 
  mutate(pred_ec_inc = round(pred_ec_inc, 2),
         pred_mov_inc = round(pred_mov_inc, 2),
         mov_incumbent = round(mov_incumbent, 2))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Create model vs. actual plot for EC
  output$modvsactPlotEC <- renderPlot({
    
    modvsactdataEC <- full_data |> filter(year %in% input$year) |> select(year, rob_ec_correct, ec_incumbent, incumbent_party_net_votes, incumbent_approval_rating, percent_change_gdp_per_cap_short)
    modvsactPredictionsEC <- data.frame(pred = predict(model_ec, newdata = modvsactdataEC),
                                        actual = modvsactdataEC$ec_incumbent,
                                        year = modvsactdataEC$year,
                                        rob_ec_correct = modvsactdataEC$rob_ec_correct)
    
    ggplot(data = modvsactPredictionsEC, aes(x = pred, y = actual)) +
      geom_point(aes(color = rob_ec_correct), size = 3.5) + 
      {if (input$add_line == "line") geom_abline(slope = 1, intercept = 0, color = "purple")} + 
      geom_text_repel(aes(label = year), family = "Times New Roman", fontface = "bold") +
      annotate("text", x = 350, y = 350, label = "PREDICT: Incumbent Wins \nACTUAL: Incumbent Wins", fontface = "bold", size = 4, family = "Times New Roman", color = "darkgreen") +
      annotate("text", x = 350, y = -350, label = "PREDICT: Incumbent Wins \nACTUAL: Challenger Wins", fontface = "bold", size = 4, family = "Times New Roman", color = "red") +
      annotate("text", x = -350, y = 350, label = "PREDICT: Challenger Wins \nACTUAL: Incumbent Wins", fontface = "bold", size = 4, family = "Times New Roman", color = "red") +
      annotate("text", x = -350, y = -350, label = "PREDICT: Challenger Wins \nACTUAL: Challenger Wins", fontface = "bold", size = 4, family = "Times New Roman", color = "darkgreen") +
      geom_hline(yintercept = 0, linetype = "dashed") + 
      geom_vline(xintercept = 0, linetype = "dashed") + 
      coord_cartesian(
        xlim = c(-538, 538),
        ylim = c(-538, 538)) +
      scale_color_manual(values = c(
        "Correct" = "darkgreen",
        "Incorrect" = "red")) + 
      labs(x = "Vagnini Model Prediction",
           y = "Actual Results",
           title = "Predicted vs. Actual Electoral College Margin of Victories",
           subtitle = "Electoral College Margin of Victories Given for Incumbent Party",
           color = "Vagnini Model Prediction Results") +
      theme(plot.title = element_text(family = "Times New Roman", hjust = 0.5),
            plot.subtitle = element_text(family = "Times New Roman", face = "italic", hjust = 0.5),
            legend.title = element_text(family = "Times New Roman"),
            legend.text = element_text(family = "Times New Roman"),
            axis.text = element_text(family = "Times New Roman"),
            axis.title = element_text(family = "Times New Roman"))
  })
  
  
  # Create model vs. actual plot for popular vote
  output$modvsactPlotMOV <- renderPlot({
    
    modvsactdataMOV <- full_data |> filter(year %in% input$year) |> select(year, rob_mov_correct, mov_incumbent, incumbent_party_net_votes, incumbent_approval_rating, percent_change_gdp_per_cap_short)
    modvsactPredictionsMOV <- data.frame(pred = predict(model_mov, newdata = modvsactdataMOV),
                                         actual = modvsactdataMOV$mov_incumbent,
                                         year = modvsactdataMOV$year,
                                         rob_mov_correct = modvsactdataMOV$rob_mov_correct)
    
    ggplot(data = modvsactPredictionsMOV, aes(x = pred, y = actual)) +
      geom_point(aes(color = rob_mov_correct), size = 3.5) + 
      annotate("text", x = 12.5, y = 22.5, label = "PREDICT: Incumbent Wins \nACTUAL: Incumbent Wins", fontface = "bold", size = 4, family = "Times New Roman", color = "darkgreen") +
      annotate("text", x = 12.5, y = -22.5, label = "PREDICT: Incumbent Wins \nACTUAL: Challenger Wins", fontface = "bold", size = 4, family = "Times New Roman", color = "red") +
      annotate("text", x = -12.5, y = 22.5, label = "PREDICT: Challenger Wins \nACTUAL: Incumbent Wins", fontface = "bold", size = 4, family = "Times New Roman", color = "red") +
      annotate("text", x = -12.5, y = -22.5, label = "PREDICT: Challenger Wins \nACTUAL: Challenger Wins", fontface = "bold", size = 4, family = "Times New Roman", color = "darkgreen") +
      {if (input$add_line == "line") geom_abline(slope = 1, intercept = 0, color = "purple")} + 
      geom_text_repel(aes(label = year), family = "Times New Roman", fontface = "bold") +
      geom_hline(yintercept = 0, linetype = "dashed") + 
      geom_vline(xintercept = 0, linetype = "dashed") + 
      coord_cartesian(
        xlim = c(-25, 25),
        ylim = c(-25, 25)) +
      scale_color_manual(values = c(
        "Correct" = "darkgreen",
        "Incorrect" = "red")) + 
      labs(x = "Vagnini Model Prediction",
           y = "Actual Results",
           title = "Predicted vs. Actual Popular Vote Margin of Victories",
           subtitle = "Popular Vote Margin of Victories Given for Incumbent Party",
           color = "Vagnini Model Prediction Results") +
      theme(plot.title = element_text(family = "Times New Roman", hjust = 0.5),
            plot.subtitle = element_text(family = "Times New Roman", face = "italic", hjust = 0.5),
            legend.title = element_text(family = "Times New Roman"),
            legend.text = element_text(family = "Times New Roman"),
            axis.text = element_text(family = "Times New Roman"),
            axis.title = element_text(family = "Times New Roman"))
  })
  
  
  # Create data table
  output$data_table <- renderDT({
    datatable(
      lichtvsrobdata,
      rownames = F,
      colnames = c("Year", "Incumbent Candidate", "Challenger Candidate", "EC Winner", "Vagnini Predicted EC MOV", "Actual EC MOV", "Lichtman EC: Correct/Incorrect?", "Vagnini EC: Correct/Incorrect?", "Popular Vote Winner", "Vagnini Predicted Popular Vote MOV", "Actual Popular Vote MOV", "Vagnini Popular Vote: Correct/Incorrect?")
    )
  })
  
  
  
  # Create simulation
  output$simPlot <- renderPlot({
    
    req(input$key1_input, input$key4_input, 
        input$key8_currentgdp, input$key8_oldgdp)
    
    validate(need(input$key8_oldgdp != 0, "Old GDP can't be 0"))
    
    simuserdata <- data.frame(incumbent_party_net_votes = input$key1_input,
                              incumbent_approval_rating = input$key4_input,
                              percent_change_gdp_per_cap_short = (input$key8_currentgdp - input$key8_oldgdp) / input$key8_oldgdp)
    predictions <- data.frame(EC = predict(model_ec, newdata = simuserdata),
                              MOV = predict(model_mov, newdata = simuserdata))
    validate(need((predictions$EC <= 538 & predictions$EC >= -538) & (predictions$MOV <= 100 & predictions$MOV >= -100), "Out of bounds"))
    
    ggplot(data = predictions, aes(x = EC, y = MOV)) +
      geom_point(color = "purple", size = 5) + 
      geom_hline(yintercept = 0, linetype = "dashed") + 
      geom_vline(xintercept = 0, linetype = "dashed") + 
      geom_text_repel(label = paste0("EC: ", round(predictions$EC,2), " \nPopular: ", round(predictions$MOV,2), "%"), family = "Times New Roman", fontface = "bold.italic") +
      {if (predictions$EC > 0 & predictions$MOV > 0) {annotate("text", x = 270, y = 75, label = "Republican Wins EC, Republican Wins Popular Vote", fontface = "bold", size = 4, family = "Times New Roman", color = "darkgreen")} 
        else if (predictions$EC > 0 & predictions$MOV < 0) {annotate("text", x = 270, y = -75, label = "Republican Wins EC, Democrat Wins Popular Vote", fontface = "bold", size = 4, family = "Times New Roman", color = "darkgreen")}
        else if (predictions$EC < 0 & predictions$MOV > 0) {annotate("text", x = -270, y = 75, label = "Democrat Wins EC, Republican Wins Popular Vote", fontface = "bold", size = 4, family = "Times New Roman", color = "darkgreen")}
        else {annotate("text", x = -270, y = -75, label = "Democrat Wins EC, Democrat Wins Popular Vote", fontface = "bold", size = 4, family = "Times New Roman", color = "darkgreen")}} +
      coord_cartesian(
        xlim = c(-538, 538),
        ylim = c(-100, 100)) +
      labs(x = "Electoral College Margin of Victory",
           y = "Popular Vote Margin of Victory (as a %)",
           title = "Simulated Margin of Victories Using Vagnini Model",
           subtitle = "Electoral College and Popular Vote Margin of Victories Given for Incumbent Party") +
      theme(plot.title = element_text(family = "Times New Roman", hjust = 0.5),
            plot.subtitle = element_text(family = "Times New Roman", face = "italic", hjust = 0.5),
            axis.text = element_text(family = "Times New Roman"),
            axis.title = element_text(family = "Times New Roman"))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


