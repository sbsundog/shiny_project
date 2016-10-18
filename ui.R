#
# This is a Shiny web application for calculating SoCal Edison Solar Savings. You can run the application by clicking
# the 'Run App' button above. Default data s current as of September, 2016, and for a scenario where consumption equals production.
# When production is much higher than consumption the solar fee approaches zero and the other numbers and computation 
# hold. When consumption is much higher than production, the savings are simply tier 3 savings and associated taxes and fee reductions.
# This calculator would understate the savings in default mode  because it amrotizes costs across multiple cost usage tiers.
# 

library(shiny)

# Define UI for application    
navbarPage("SoCal Edison Monthly Solar Savings Calculator",   #1   
                  
   
   tabPanel("Calc",        #2
   
   # slider input for number of kilowatts 
   sidebarLayout(         #3
      sidebarPanel(        #4
         sliderInput("kw",
                     "KiloWatts Used:",
                     min = 1,
                     max = 2000,
                     value = 1017),
         numericInput("tier1rate", "Tier 1 Rate  ", value = 0.16, min=0, max=.30,step=.01),
         numericInput("tier1", "Tier 1 Ceiling", value = 282, min=0, max=500,step=1),
         numericInput("tier2rate", "Tier 2 Rate  ", value = 0.23,min=0, max=.60,step=.01),
         numericInput("tier2", "Tier 2 Ceiling", value = 282,min=0, max = 282, step=1),
         numericInput("tier3rate", "Tier 3 Rate  ", value = 0.29,min=0,max=.99,step=.01),
        
         numericInput("fixed", "Basic Charge  ", value = 6,min=0,max=99,step=1),
         numericInput("bondrate", "Bond Rate", value = 0.00539,min=0,max=0.10,step=.0005),
         numericInput("energycredit", "Energy Credit Rate  ", value = -0.00022,min=-0.00050,max=-0.00001,step=0.00001),
         numericInput("citytax", "City Tax Rate    ", value = 0.06,min=.0, max=.15, step=.01),
         numericInput("cityfee", "City Frachise Fee Rate", value = 0.01,min=0,max=.05, step=.005),
         numericInput("statetax", "State Tax Rate  ", value = 0.00029,min=0, max=.001,step=.0001),
         numericInput("solarbill", "Fixed Solar Charg1e", value = 11.26,min=0, max=35, step=.01),
         numericInput("systemcost", "Solar System Cost", value = 25300,min=1000,max=100000,step=1000),
         numericInput("capitalcost", "Interest Rate", value = 0.025,min=.005,max=.10,step=.001)
        ), #sidebar panel
      
      mainPanel(
         p("Enter kilowatts Used for a month. Other items, usually unchanging from month to month, can also be adjusted as needed."),
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
     ) #main panel
   ) # layout panel
), #tabbar
    navbarMenu("About Inputs", 
           tabPanel("KiloWatt Hours Used",
            column(6,
             p(" Kilowatt hours Used: Number of kilowatts of eletricity used during billing period"),
             p("Select Calc to return to calculator or About Inputs or Outputs for more information")
            )
           ), 
           tabPanel("Tier 1, 2, 3 Rates",
                    column(6,
                           p("Tier 1, 2, 3 Rates: Cost per kilowatt used in the named tier"),
                           p("Select Calc to return to calculator or About Inputs or Outputs for more information")
                    )
           ),  
           tabPanel("Tier 1, 2, Ceiling",
                    column(6,
                           p("Tier 1, 2, Ceiling: Maximum Number of Kilowatts allowed before subsequent tier cost struture is used"),
                           p("Select Calc to return to calculator or About Inputs or Outputs for more information")
                    )
           ),  
           tabPanel("Basic Charge",
                    column(6,
                           p("Basic Charge:Fixed charge amount, independent of usage"),
                           p("Select Calc to return to calculator or About Inputs or Outputs for more information")
                    )
           ),      
           tabPanel("Bond Rate",
                    column(6,
                           p("Bond Rate: Rate charged for Department of Water Resources costs for energy"),
                           p("Select Calc to return to calculator or About Inputs or Outputs for more information")
                    )
           ),      
           tabPanel("City Tax",
                    column(6,
                           p("City Tax: Tax rate on usage at city  tax jurisdictions applied against billed amount"),
                           p("Select Calc to return to calculator or About Inputs or Outputs for more information")
                    )
           ),      
           tabPanel("State Tax",
                   column(6,
                          p("State Tax: Tax rate on at state tax jurisdiction applied against kilowatt hours used"),
                          p("Select Calc to return to calculator or About Inputs or Outputs for more information")
                   )
           ), 
           tabPanel("City Fee",
                    column(6,
                           p("City Fee: Local fixed charge rate on billed amount less 2 dollars"),
                           p("Select Calc to return to calculator or About Inputs or Outputs for more information")
                    )
           ), 
           tabPanel("Energy Credit",
                   column(6,
                          p("Energy Credit: A discount rate, quite miniscule, applied against kilowatt hours used"),
                          p("Select Calc to return to calculator or About Inputs or Outputs for more information")
                   )
           ), 
           
           tabPanel("Solar Bill",
                   column(6,
                          p("Solar Bill: Fixed monthly charge for being connected to grid and using SoCal Edison for solar system shortfalls"),
                          p("Select Calc to return to calculator or About Inputs or Outputs for more information")
                   )
           ),      
           tabPanel("Solar System Cost",
                    column(6,
                           p("Solar System Cost: Cost of the solar system, including material costs, labor, taxes and fees"),
                           p("Select Calc to return to calculator or About Inputs or Outputs for more information")
                    )
           ),     
           tabPanel("Interest Rate",
                    column(6,
                           p("Interest Rate: Cost of capital, 25 year t-bill rate (2.5%) or 5 year cd (< 1%)"),
                           p("Select Calc to return to calculator or About Inputs or Outputs for more information")
                    )
           )      
        
),
  navbarMenu("About Outputs", 
           tabPanel("Net Savings",
                    column(6,
                           p("Net Savings: Cost difference between a projected non Solar system usage bill and a Solar Bill"),
                           p("Select Calc to return to calculator or About Inputs or Outputs for more information")
                    )
           ),
           tabPanel("Carrying Cost",
                    column(6,
                           p("Carrying Cost: Imputed alternative usage dollars foregone in interest/investment earnings"),
                           p("Select Calc to return to calculator or About Inputs or Outputs for more information")
                    )
           ),
           tabPanel("Real Savings",
                    column(6,
                           p("Real Savings: Cost difference between Net savings and Carrying Cost"),
                           p("Select Calc to return to calculator or About Inputs or Outputs for more information")
                    )
           )
  )
)
