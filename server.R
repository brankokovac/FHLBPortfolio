# server.R

shinyServer(function(input, output) {
  
  output$state <- renderText({input$state})
  
  output$loantotal <- renderText({
    
    loanSize <- filter(portfolio, STATENAME == input$state)
    paste0("Loan Portfolio: $",prettyNum(sum(loanSize$Amount), big.mark = ","))
    
  })
  
  output$wgtRate <- renderText({
    loanSize <- filter(portfolio, STATENAME == input$state)
    paste0("Weigthed Rate: ",formatC(sum(loanSize$annInt)/sum(loanSize$Amount) * 100, digits = 4),"%")
    
  })
  
  mapChoice <- reactive({switch(input$mapType,
                                "Total Mortgage Value" = mortByState$TotalMortgage,
                                "Weighted Interest Rate" = mortByState$WgtRate)
  })
  
  output$usmortmap <- renderPlotly({
    l <- list(color = toRGB("white"), width = 2)
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    plot_ly(mortByState, z = mapChoice(), text = hover, locations = State, type = 'choropleth',
            locationmode = 'USA-states', color = mapChoice(), colors = 'Purples',
            marker = list(line = l), colorbar = list(title = input$mapType),
            filename="r-docs/usa-choropleth") %>%
      layout(title = 'FHLB Mortgage Portfolio by State<br>(Hover for breakdown)', geo = g)
    
  })
  
  mapChoice <- reactive({switch(input$mapType,
                                "Total Mortgage Value" = mortByState$TotalMortgage,
                                "Weighted Interest Rate" = mortByState$WgtRate)
  })
  
  dataForPlot <- reactive({
    
    y <- switch(input$graphChoice,
                "Year" = portfolio$Year,
                "FHLB Bank" = portfolio$FHLBankID,
                "Purpose" = portfolio$Purpose,
                "First Time Home Buyer" = portfolio$First,
                "Race" = portfolio$BoRace,
                "Gender" = portfolio$BoGender,
                "Age" = portfolio$BoAge,
                "Credit Score" = portfolio$Borrower.Credit.Score)
    
    return(y)
    
  })
  
  titlePlot <- reactive({
    paste("Mortgage Portfolio by", input$graphChoice)
  })
  
  output$chart <- renderPlot({
    
    plot <- switch(input$graphChoice,
                   "Year" = ggplot(portfolio, aes(x = Year, y = Amount)) +
                     geom_bar(stat = "identity") + theme_tufte() + ggtitle(titlePlot()) +
                     xlab(input$graphChoice) + scale_y_continuous(labels = comma),
                   
                   "FHLB Bank" = ggplot(portfolio, aes(x = FHLBankID, y = Amount)) +
                     geom_bar(stat = "identity") + theme_tufte() + ggtitle(titlePlot()) +
                     xlab(input$graphChoice) + scale_y_continuous(labels = comma),
                   
                   "Purpose" = ggplot(portfolio, aes(x = Purpose, y = Amount)) +
                     geom_bar(stat = "identity") + theme_tufte() + ggtitle(titlePlot()) +
                     xlab(input$graphChoice) + scale_y_continuous(labels = comma),
                   
                   "First Time Home Buyer" = ggplot(portfolio, aes(x = First, y = Amount)) +
                     geom_bar(stat = "identity") + theme_tufte() + ggtitle(titlePlot()) +
                     xlab(input$graphChoice) + scale_y_continuous(labels = comma),
                   
                   "Race" = ggplot(portfolio, aes(x = BoRace, y = Amount)) +
                     geom_bar(stat = "identity") + theme_tufte() + ggtitle(titlePlot()) +
                     xlab(input$graphChoice) + scale_y_continuous(labels = comma),
                   
                   "Gender" = ggplot(portfolio, aes(x = BoGender, y = Amount)) +
                     geom_bar(stat = "identity") + theme_tufte() + ggtitle(titlePlot()) +
                     xlab(input$graphChoice) + scale_y_continuous(labels = comma),
                   
                   "Age" = ggplot(portfolio, aes(x = BoAge, y = Amount)) +
                     geom_bar(stat = "identity") + theme_tufte() + ggtitle(titlePlot()) +
                     xlab(input$graphChoice) + scale_y_continuous(labels = comma),
                   
                   "Credit Score" = ggplot(portfolio, aes(x = Borrower.Credit.Score, y = Amount)) +
                     geom_bar(stat = "identity") + theme_tufte() + ggtitle(titlePlot()) +
                     xlab(input$graphChoice) + scale_y_continuous(labels = comma))
    
    print(plot)
    
  })
  
  
  output$table <- renderDataTable({
    
    portfolio
    
  })
  
  
  output$stateCounty <- renderPlot({
    
    map <- county_choropleth(countySum, state_zoom = tolower(input$state))
    print(map)
    
  })
  
  output$genderTable <- renderTable({
    
    temp <- filter(portfolio, STATENAME == input$state)
    temp <- group_by(temp, BoGender)
    temp <- summarise(temp, Amount = sum(Amount))
    temp$Amount <- prettyNum(temp$Amount, big.mark = ",")
    names(temp) <- c("Gender", "Amount")
    temp
    
  })
  
  output$raceTable <- renderTable({
    
    temp <- filter(portfolio, STATENAME == input$state)
    temp <- group_by(temp, BoRace)
    temp <- summarise(temp, Amount = sum(Amount))
    temp$Amount <- prettyNum(temp$Amount, big.mark = ",")
    names(temp) <- c("Race", "Amount")
    temp
    
  })
  
  output$csTable <- renderTable({
    
    temp <- filter(portfolio, STATENAME == input$state)
    temp <- group_by(temp, Borrower.Credit.Score)
    temp <- summarise(temp, Amount = sum(Amount))
    temp$Amount <- prettyNum(temp$Amount, big.mark = ",")
    names(temp) <- c("Credit Rate", "Amount")
    temp
    
  })
  
})

