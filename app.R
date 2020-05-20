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
    titlePanel("Dose Response Model and Dose Selection"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
           
            textInput("dose", label = h3("Dose Input"), value = "0,1,2.718,7.388,20.083"),
            textInput("response", label=h3("Response Input"), value="0,1,5,9,10"),
            textInput("total", label=h3("Total Animals Input"), value="10,10,10,10,10"),
            textInput("numtreats", label=h3("Number of Treats to Plan"), value="5")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Data Table", tableOutput("table")),
                        tabPanel("Plot", plotOutput("plot")),
                        tabPanel("Summary", verbatimTextOutput("summary")),
                        tabPanel("LC Values", tableOutput("lc50")),
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
    d <- reactive({
        library(drc)
        dose <- as.numeric(unlist(strsplit(input$dose,",")))
        response <- as.numeric(unlist(strsplit(input$response,",")))
        total <- as.numeric(unlist(strsplit(input$total,",")))
        df <- data.frame(dose=dose, response=response, total=total)
        drm(response/total~dose, data=df,fct=LN.2(names=c("Slope","LC50")), type="binomial", weights=total)
    })
    
    # Generate a plot of the data ----
    # Also uses the inputs to build the plot label. Note that the
    # dependencies on the inputs and the data reactive expression are
    # both tracked, and all expressions are called in the sequence
    # implied by the dependency graph.
    
    # Generate an HTML table view of the data ----
    output$table <- renderTable({
        dose <- as.numeric(unlist(strsplit(input$dose,",")))
        response <- as.numeric(unlist(strsplit(input$response,",")))
        total <- as.numeric(unlist(strsplit(input$total,",")))
        df <- data.frame(dose=dose, response=response, total=total)
    })
    
    
    output$plot <- renderPlot({
        plot(d())
    })
    
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
        dfpred <- data.frame(LC_value=resplist, Estimated_Dose=EDlist, ED_lower=EDlistlower, ED_upper=EDlistupper)
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
        
        
        dfpred <- data.frame(New_Doses=vect, Estimated_Mortality=predict(d(),newdata=data.frame(vect)))
    })
    
}

# Create Shiny app ----
shinyApp(ui, server)

