#
#

library(shiny)


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
      y <- cost.tier
      if (y > 99.99){
      x <- "Kilowatt Usage Cost..."
            # 123456789012345678901234567
        paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y))
      } else if (y > 9.99) {
        x <- "Kilowatt Usage Cost....."
              # 12345678901234567890123456789
        paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
      } else if (y > 0) {
        x <- "Kilowatt Usage Cost......."
              # 1234567890123456789012345678901
        paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
      }
   })    
   
   output$fixed <-  renderText({ 
     compcost()
     x <- "Basic Charge........................"
     y <- input$fixed
     if (y > 99.99){
       x <- "Basic Charge..............."
             # 12345678901234567890123456789
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y))
     } else if (y > 9.99) {
       x <- "Basic Charge................."
             # 12345678901234567890123456789
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     } else if (y > 0) {
       x <- "Basic Charge..................."
             # 1234567890123456789012345678901
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     }

   })   
    
   output$bondrate <- renderText({ 
     compcost()
     kw.used <-   input$kw
     bond.charge <- kw.used * input$bondrate
     y <- bond.charge
     if (y > 99.99){
       x <- "Bond Rate..................." 
             1234567890123456789012345678 
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y))
     } else if (y > 9.99) {
       x <- "Bond Rate......................" 
           # 12345678901234567890123456789
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     } else if (y > 0) {
       x <- "Bond Rate........................"  
           # 1234567890123456789012345678901 
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     }
    
   }) 
   
   output$energycredit <- renderText({ 
     compcost()
     kw.used <-   input$kw
     energy.credit <- kw.used * input$energycredit
     y <- energy.credit
     if (y > 99.99){
       x <- "Energy Credit............." 
             # 1234567890123456789012345678901 
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y))
     } else if (y > 9.99) {
       x <- "Energy Credit................" 
             # 12345678901234567890123456789
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     } else if (y > 0) {
       x <- "Energy Credit.................."  
             # 1234567890123456789012345678901 
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     }
     
     
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
     y <- city.tax
     if (y > 99.99){
       x <- "City Tax........................" 
           # 12345678901234567890123456789012
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y))
     } else if (y > 9.99) {
       x <- "City Tax..........................." 
           # 12345678901234567890123456789012346
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     } else if (y > 0) {
       x <- "City Tax............................."  
           # 1234567890123456789012345678901234567
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     }
     
    
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
     y <- city.fee
     if (y > 99.99){
       x <- "City Fee..................." 
       # 123456789012345678901234567
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y))
     } else if (y > 9.99) {
       x <- "City Fee.........................." 
       # 12345678901234567890123456789
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     } else if (y > 0) {
       x <- "City Fee............................"  
             # 1234567890123456789012345678901 
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     }
     
   
   }) 
   output$statetax <- renderText({ 
     compcost()
     kw.used <-   input$kw
     
     state.tax <- kw.used * input$statetax
     y <- state.tax
     if (y > 99.99){
       x <- "State Tax.................." 
           # 123456789012345678901234567
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y))
     } else if (y > 9.99) {
       x <- "State Tax.........................." 
             # 12345678901234567890123456789
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     } else if (y > 0) {
       x <- "State Tax.........................."  
             # 1234567890123456789012345678901 
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     }
     
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
     y <- total.bill
     if (y > 99.99){
       x <- "Total Bill........................" 
           # 123456789012345678901234567
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y))
     } else if (y > 9.99) {
       x <- "Total Bill.........................." 
             # 12345678901234567890123456789
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     } else if (y > 0) {
       x <- "Total Bill............................"  
           # 1234567890123456789012345678901 
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     }
     
  }) 
   
   output$solarbill <- renderText({ 
     compcost()
     y <- input$solarbill
     if (y > 99.99){
       x <- "Solar Charge.............." 
            # 123456789012345678901234567
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y))
     } else if (y > 9.99) {
       x <- "Solar Charge................." 
             # 12345678901234567890123456789
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     } else if (y > 0) {
       x <- "Solar Charge..................."  
             # 1234567890123456789012345678901 
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     }
    
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
     y <- net.savings
     if (y > 99.99){
       x <- "Net Savings................" 
             # 123456789012345678901234567
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y))
     } else if (y > 9.99) {
       x <- "Net Savings.................." 
             # 12345678901234567890123456789
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     } else if (y > 0) {
       x <- "Net Savings...................."  
             # 1234567890123456789012345678901 
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     }
     else  {
       x <- "Net Savings......................"  
       # 1234567890123456789012345678901 
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     }
   })  
   
   output$carryingcost <- renderText({
     compcost()
     carrying.cost <- input$systemcost * (input$capitalcost / 12)
     y <- carrying.cost
     if (y > 99.99){
       x <- "Carrying Cost.............." 
             # 123456789012345678901234567
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y))
     } else if (y > 9.99) {
       x <- "Carrying Cost................" 
             # 12345678901234567890123456789
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     } else if (y > 0) {
       x <- "Carrying Cost.................."  
             # 1234567890123456789012345678901 
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     }
     
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
     y <- real.savings
     if (y > 99.99){
       x <- "Real Savings..............." 
       # 123456789012345678901234567
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y))
     } else if (y > 9.99) {
       x <- "Real Savings................." 
       # 12345678901234567890123456789
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     } else if (y > 0) {
       x <- "Real Savings..................."  
             # 1234567890123456789012345678901 
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     } else {
       x <- "Real Savings.................."  
       # 1234567890123456789012345678901 
       paste0(format(x, justify = "left", width=30), sprintf("$ %20.2f", y)) 
     }
   
   }) 
 
} 


