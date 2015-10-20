library(shiny)

shinyUI(
  
  fluidPage(theme = "bootstrap.min.css",
            
            # Aplication Title
            titlePanel(img(src = "fhlb.png"),
                       "Mortgage Portfolio"),
            
            #Sidebar with user inputs
            tabsetPanel(
              tabPanel(title = "About",
                       
                       # Output panel
                       mainPanel(
                         
                         tags$br(),
                         
                         "The Federal Home Loan Banks are 11 U.S. government-sponsored banks that provide stable, on-demand, low-cost funding to American financial institutions for home mortgage loans, small business, rural, agricultural, and economic development lending. With their members, the FHLBanks represents the largest collective source of home mortgage and community credit in the United States.",
                         tags$br(),
                         tags$br(),
                         
                         em("The purpose of this app is to examine the distribution of mortgages that make up the FHLB portfolio in order to analyze the FHLB System's effectiveness."),
                         
                         tags$br(),
                         h3("Map of the Federal Home Loan Bank System"),
                         tags$br(),
                         
                         img(src = "map.png"),
                         tags$br(),
                         em("Source: Wikipedia")
                         
                       )
              ),
              
              tabPanel(title = "Summary",
                       
                       # Output panel
                       mainPanel(
                         tags$br(),
                         sidebarPanel(
                           
                           tags$br(),
                           paste0("FHLB Mortgages Held (AUM): $",
                                  prettyNum(sum(portfolio$Amount, rm.na = TRUE), big.mark = ",", scientific = FALSE)),
                           tags$br(),
                           tags$br(),
                           paste0("Weighted Average Interest Rate of Portfolio: ",
                                  formatC(sum(portfolio$Amount * portfolio$Rate) / sum(portfolio$Amount) * 100, digits = 4),
                                  "%"),
                           tags$br(),
                           tags$br(),
                           paste0("Number of Mortgages Held: ",
                                  prettyNum(nrow(portfolio), big.mark = ",")),
                           tags$br(),
                           tags$br(),
                           paste0("Average Mortgage Value: $",
                                  prettyNum(sum(portfolio$Amount, rm.na = TRUE) / nrow(portfolio), big.mark = ",")),
                           tags$br(),
                           tags$br(),
                           
                           selectInput(inputId = "mapType",
                                       label = "Choose Metric to Map",
                                       choices = c("Total Mortgage Value", "Weighted Interest Rate")
                           )
                           
                         ),
                         
                         mainPanel(
                           tags$br(),
                           plotlyOutput("usmortmap")
                           
                         )
                         
                       )
              ),
              tabPanel(title = "Graphs",
                       mainPanel(
                         tags$br(),
                         
                         sidebarPanel(
                           
                           selectInput(inputId = "graphChoice",
                                       label = "Choose Feature to Plot", 
                                       choices = c("Year", "FHLB Bank", "Purpose", "First Time Home Buyer", "Race", "Gender", "Age", "Credit Score"))
                         ),
                         
                         # Output panel
                         mainPanel(
                           
                           plotOutput("chart")
                           
                         )
                       )
                       
              ),
              tabPanel(title = "State Details",
                       mainPanel(
                         tags$br(),
                         sidebarPanel(
                           
                           selectInput(inputId = "state",
                                       label = "Choose State", 
                                       choices = sort(unique(portfolio$STATENAME))
                           )
                         ),
                         
                         # Output panel
                         mainPanel(
                           
                           h2(textOutput("state")),
                           textOutput("loantotal"),
                           textOutput("wgtRate"),
                           tags$br(),
                           h3("Mortgage Portfolio by County"),
                           tags$br(),
                           plotOutput("stateCounty"),
                           tags$br(),
                           h4("Total by Gender"),
                           tableOutput("genderTable"),
                           tags$br(),
                           h4("Total by Race"),
                           tableOutput("raceTable"),
                           tags$br(),
                           h4("Total by Credit Score"),
                           tableOutput("csTable")
                           
                         )
                       )
              ),
              
              tabPanel(title = "Loan Details",
                       
                       mainPanel(
                         tags$br(),
                         dataTableOutput("table")
                         
                         
                       )
              )
            )
  )
)

