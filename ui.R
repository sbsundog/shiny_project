library(shiny)

# Define UI for application 
ui <- fluidPage(
   
   # Application title
   titlePanel("SoCal Edison Monthly Solar Savings Calculator"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("kw",
                     "KiloWatts Used:",
                     min = 1,
                     max = 2000,
                     value = 1017),
         numericInput("tier1rate", "Tier 1 Rate  ", value = 0.16),
         numericInput("tier1", "Tier 1 Ceiling", value = 282),
         numericInput("tier2rate", "Tier 2 Rate  ", value = 0.23),
         numericInput("tier2", "Tier 2 Ceiling", value = 282),
         numericInput("tier3rate", "Tier 3 Rate  ", value = 0.29),
        
         numericInput("fixed", "Basic Charge  ", value = 6),
         numericInput("bondrate", "Bond Rate", value = 0.00539),
         numericInput("energycredit", "Energy Credit Rate  ", value = -0.00022),
         numericInput("citytax", "City Tax Rate    ", value = 0.06),
         numericInput("cityfee", "City Frachise Fee Rate", value = 0.01),
         numericInput("statetax", "State Tax Rate  ", value = 0.00029),
         numericInput("solarbill", "Fixed Solar Charge", value = 11.26),
         numericInput("systemcost", "Solar System Cost", value = 25300),
         numericInput("capitalcost", "Interest Rate", value = 0.025)
       ),
      
      # Show a plot of the generated distribution
      mainPanel(
         textOutput("cost"),
         textOutput("fixed"),
         textOutput("bondrate"),
         textOutput("energycredit"),
         textOutput("citytax"),
         textOutput("cityfee"),
         textOutput("statetax"),
         textOutput("totalbill"),
         textOutput("solarbill"),
         textOutput("netsavings"),
         textOutput("carryingcost"),
         textOutput("realsavings")
      )
   )
)
