# Define server logic required to draw a histogram
server <- function(input, output) {   
   compcost <- reactive({ 
     input$kw
     input$tier1   
     input$tier2  
     input$tier1rate   
     input$tier2rate
     input$tier3rate
     input$fixed
     input$energycredit
     input$citytax
     input$cityfee
     input$statetax
     input$solarbill
     input$systemcost
     input$capitalcost
     
         })
  
   output$cost <- renderText({
      compcost()
      kw.used.1 <- input$kw 
      kw.used.2 <- input$kw - input$tier1
      kw.used.3 <- input$kw - input$tier1 - input$tier2
      if (input$kw < input$tier1 + 1) {
         cost.tier <- kw.used.1 * input$tier1rate
      } else if (input$kw < input$tier1 + input$tier2 + 1 ) {
        cost.tier <- input$tier1 * input$tier1rate + kw.used.2 * input$tier2rate
      } else {cost.tier <- input$tier1 * input$tier1rate + input$tier2 *input$tier2rate + kw.used.3 * input$tier3rate
      }
      
      paste0("Kilowatt Usage Cost", ".........", sprintf("%.2f", cost.tier))
      
   })   
   
   output$fixed <- renderText({  
      compcost()
      paste0("Basic Charge", "........................", sprintf("%.2f", input$fixed)) 
        })     
    
   output$bondrate <- renderText({ 
     compcost()
     kw.used <-   input$kw
     bond.charge <- kw.used * input$bondrate
      paste0("Bond Rate", ".............................", sprintf("%.2f", bond.charge))
   }) 
   
   output$energycredit <- renderText({ 
     compcost()
     kw.used <-   input$kw
     energy.credit <- kw.used * input$energycredit
      paste0("Energy Credit", ".......................", sprintf("%.2f", energy.credit))
   })
   
   output$citytax <- renderText({ 
     compcost()
     kw.used <-   input$kw
     kw.used.1 <- input$kw 
     kw.used.2 <- input$kw - input$tier1
     kw.used.3 <- input$kw - input$tier1 - input$tier2
     if (input$kw < input$tier1 + 1) {
       cost.tier <- kw.used.1 * input$tier1rate
     } else if (input$kw < input$tier1 + input$tier2 + 1 ) {
       cost.tier <- input$tier1 * input$tier1rate + kw.used.2 * input$tier2rate
     } else {cost.tier <- input$tier1 * input$tier1rate + input$tier2 *input$tier2rate + kw.used.3 * input$tier3rate
     }
     bond.charge <- kw.used * input$bondrate
     energy.credit <- kw.used * input$energycredit
     subtotal <- input$fixed  + cost.tier + bond.charge + energy.credit 
     city.tax <- subtotal * input$citytax 
     paste0("City Tax",  "................................", sprintf("%.2f", city.tax))
   })   
   output$cityfee <- renderText({ 
     compcost()
     kw.used <-   input$kw
     kw.used.1 <- input$kw 
     kw.used.2 <- input$kw - input$tier1
     kw.used.3 <- input$kw - input$tier1 - input$tier2
     if (input$kw < input$tier1 + 1) {
       cost.tier <- kw.used.1 * input$tier1rate
     } else if (input$kw < input$tier1 + input$tier2 + 1 ) {
       cost.tier <- input$tier1 * input$tier1rate + kw.used.2 * input$tier2rate
     } else {cost.tier <- input$tier1 * input$tier1rate + input$tier2 *input$tier2rate + kw.used.3 * input$tier3rate
     }  
     bond.charge <- kw.used * input$bondrate
     energy.credit <- kw.used * input$energycredit
     subtotal <- input$fixed  + cost.tier + bond.charge + energy.credit 
     city.fee <- (subtotal - 2) * input$cityfee
     paste0("City Fee", ".................................", sprintf("%.2f", city.fee))
   }) 
   output$statetax <- renderText({ 
     compcost()
     kw.used <-   input$kw
     
     state.tax <- kw.used * input$statetax
     paste0("State Tax", "...............................", sprintf("%.2f", state.tax))
   }) 
   output$totalbill <- renderText({ 
     compcost()
     kw.used <-   input$kw
     kw.used.1 <- input$kw 
     kw.used.2 <- input$kw - input$tier1
     kw.used.3 <- input$kw - input$tier1 - input$tier2
     if (input$kw < input$tier1 + 1) {
       cost.tier <- kw.used.1 * input$tier1rate
     } else if (input$kw < input$tier1 + input$tier2 + 1 ) {
       cost.tier <- input$tier1 * input$tier1rate + kw.used.2 * input$tier2rate
     } else {cost.tier <- input$tier1 * input$tier1rate + input$tier2 *input$tier2rate + kw.used.3 * input$tier3rate
     } 
     bond.charge <- kw.used * input$bondrate
     energy.credit <- kw.used * input$energycredit
     subtotal <- input$fixed  + cost.tier + bond.charge + energy.credit 
     city.fee <- (subtotal - 2) * input$cityfee
     city.tax <- subtotal * input$citytax 
     state.tax <- kw.used * input$statetax
     total.bill <- subtotal + city.tax + city.fee + state.tax
     paste0("Total Costs",  "........................", sprintf("%.2f", total.bill))
   }) 
   output$solarbill <- renderText({ 
     compcost()
     paste0("Solar Charge", ".......................", sprintf("%.2f", input$solarbill))
   }) 
   output$netsavings <- renderText({  
     compcost()
     kw.used <-   input$kw
     kw.used.1 <- input$kw 
     kw.used.2 <- input$kw - input$tier1
     kw.used.3 <- input$kw - input$tier1 - input$tier2
     if (input$kw < input$tier1 + 1) {
       cost.tier <- kw.used.1 * input$tier1rate
     } else if (input$kw < input$tier1 + input$tier2 + 1 ) {
       cost.tier <- input$tier1 * input$tier1rate + kw.used.2 * input$tier2rate
     } else {cost.tier <- input$tier1 * input$tier1rate + input$tier2 *input$tier2rate + kw.used.3 * input$tier3rate
     }  
     bond.charge <- kw.used * input$bondrate
     energy.credit <- kw.used * input$energycredit
     subtotal <- input$fixed  + cost.tier + bond.charge + energy.credit 
     city.fee <- (subtotal - 2) * input$cityfee
     city.tax <- subtotal * input$citytax 
     state.tax <- kw.used * input$statetax
     total.bill <- subtotal + city.tax + city.fee + state.tax
     net.savings <- total.bill - input$solarbill
     paste0("Net Savings", ".......................", sprintf("%.2f", net.savings ))
   })  
   
   output$carryingcost <- renderText({
     compcost()
     carrying.cost <- input$systemcost * (input$capitalcost / 12)
     paste0("Carrying Cost", "......................", sprintf("%.2f", carrying.cost))
   })
    
   output$realsavings <- renderText({ 
     compcost()
     kw.used <-   input$kw
     kw.used.1 <- input$kw 
     kw.used.2 <- input$kw - input$tier1
     kw.used.3 <- input$kw - input$tier1 - input$tier2
     if (input$kw < input$tier1 + 1) {
       cost.tier <- kw.used.1 * input$tier1rate
     } else if (input$kw < input$tier1 + input$tier2 + 1 ) {
       cost.tier <- input$tier1 * input$tier1rate + kw.used.2 * input$tier2rate
     } else {cost.tier <- input$tier1 * input$tier1rate + input$tier2 *input$tier2rate + kw.used.3 * input$tier3rate
     } 
     bond.charge <- kw.used * input$bondrate
     energy.credit <- kw.used * input$energycredit
     subtotal <- input$fixed  + cost.tier + bond.charge + energy.credit 
     city.fee <- (subtotal - 2) * input$cityfee
     city.tax <- subtotal * input$citytax 
     state.tax <- kw.used * input$statetax
     total.bill <- subtotal + city.tax + city.fee + state.tax
     net.savings <- total.bill - input$solarbill
     carrying.cost <- input$systemcost * (input$capitalcost / 12)
     real.savings <- net.savings - carrying.cost
     paste0("Real Savings", ".....................", sprintf("%.2f", real.savings))
   }) 
 
} 
