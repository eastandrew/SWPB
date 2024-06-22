#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)


# Define UI for random distribution app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("SWPB, StageWise ProBit Dose Response Model and Dose Selection"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            h2("Enter Data, Hit Run"),
            p("Make sure number of doses, responses, and total animals are equal."),
            p("Model is fit to entered data and stop point checked."),
            p("If stop point not met, model is used to pick treatments at important response levels."),
            p("After each testing stage, add entire dataset and re-run."),
            
            textInput("dose", label = h3("Dose Input"), value = "0,1,2.718,7.388,20.083,0,1,2.718,7.388,20.083"),
            textInput("response", label=h3("Response Input"), value="0,1,5,9,10, 0,1,4,8,9"),
            textInput("total", label=h3("Total Animals Input"), value="10,10,10,10,10,10,10,10,10,10"),
            textInput("numtreats", label=h3("Number of Treats to Plan"), value="5"),
            p("Check number of treatments and animals!"),
            actionButton("run",label=h3("Run")),
            br(),
            br(),
            br(),
            p("Written by A. East"),
            p(a("GitHub",href="https://github.com/eastandrew/SWPB"))
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Data Table", tableOutput("table")),
                        tabPanel("Plot", plotOutput("plot")),
                        tabPanel("Summary", verbatimTextOutput("summary")),
                        tabPanel("LD Values", tableOutput("lc50")),
                        tabPanel("Stop Point Test", tableOutput("stoptest")),
                        tabPanel("New Dose Chooser", tableOutput("predictions"))
            )
            
        )
    )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
    
    
    # Reactive expression to generate the requested distribution ----
    # This is called whenever the inputs change. The output functions
    # defined below then use the value computed from this expression
    d <- eventReactive(input$run, {
        
        dose <- as.numeric(unlist(strsplit(input$dose,",")))
        response <- as.numeric(unlist(strsplit(input$response,",")))
        total <- as.numeric(unlist(strsplit(input$total,",")))
        df <- data.frame(dose=dose, response=response, total=total)
        drm(response/total~dose, data=df,fct=LN.2(names=c("Slope","LC50")), type="binomial", weights=total)

    })
    
    e <- eventReactive(input$run, {
        dose <- as.numeric(unlist(strsplit(input$dose,",")))
        response <- as.numeric(unlist(strsplit(input$response,",")))
        total <- as.numeric(unlist(strsplit(input$total,",")))
        df <- data.frame(dose=dose, response=response, total=total)
        df
    })
    
    f <- eventReactive(input$run, {
        
        up <- ED(d(), 0.5, type="absolute", interval="delta", display=F)[,4]
        down <- ED(d(), 0.5, type="absolute", interval="delta", display=F)[,3]
        ld50 <- ED(d(), 0.5, type="absolute", interval="delta", display=F)[,1]


        df <- data.frame(Intra_CI_Range=up-down, Twice_LD50=ld50*2)
        df$Stop_Ratio_Value <- df$Intra_CI_Range/df$Twice_LD50
        df$'<=0.4?' <- ifelse(df$Stop_Ratio_Value<=0.4, "Yes, Stop","No, Repeat")
        df
        
    })
    
    library(drc)
    # Generate a plot of the data ----
    # Also uses the inputs to build the plot label. Note that the
    # dependencies on the inputs and the data reactive expression are
    # both tracked, and all expressions are called in the sequence
    # implied by the dependency graph.
    
    # Generate an HTML table view of the data ----
    output$table <- renderTable({
        #dose <- as.numeric(unlist(strsplit(input$dose,",")))
        #response <- as.numeric(unlist(strsplit(input$response,",")))
        #total <- as.numeric(unlist(strsplit(input$total,",")))
        #df <- data.frame(dose=dose, response=response, total=total)
        e()
    })
    
    
    output$plot <- renderPlot({
        plot(d(), type="all", pch=16, lwd=2, bty="l", xlab="Dose", ylab="Response/Total", cex=2)#with(d(), plot)
    }, height=500, width=600, units="px")
    
    # Generate a summary of the data ----
    output$summary <- renderPrint({
       summary(d())
    })
    
    # Generate an HTML table view of the data ----
    output$lc50 <- renderTable({
        #ED(d(), c(0.01,0.05,0.5,0.95,0.99), type="absolute", interval="delta")
        
        resplist <- c(0.01,0.1,0.25,0.5,0.75,0.9,0.99)
        EDlist <- c(ED(d(),resplist, type="absolute", display=F)[,1])
        EDlistlower <- c(ED(d(),resplist, type="absolute", interval="delta", display=F)[,3])
        EDlistupper <- c(ED(d(),resplist, type="absolute", interval="delta", display=F)[,4])
        dfpred <- data.frame(LD_value=resplist, Estimated_Dose=EDlist, ED_lower=EDlistlower, ED_upper=EDlistupper)
    })
    
    output$stoptest <- renderTable({
        f()
    })
    
    output$predictions <- renderTable({
        
        EDlist <- c(ED(d(),c(0.1,0.9), type="absolute", display=F)[,1])
        vect <- c()
        numbertreat <- as.numeric(unlist(strsplit(input$numtreats,",")))
        vect[1] <- min(EDlist)
        multiplier <- (exp((log(max(EDlist))-log(min(EDlist)))/(numbertreat-1)))
        for (i in 2:(numbertreat)){
            vect[i] <- vect[i-1]*multiplier
        }
        
        
        dfpred <- data.frame(New_Planning_Doses=vect, Estimated_Mortality=predict(d(),newdata=data.frame(vect)))
    })
    
}

# Create Shiny app ----
shinyApp(ui, server)


