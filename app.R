library(shiny)

# Define UI for application that reads file
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            numericInput("points", "Output curve points", 1024, min=2, max=65535),
            fileInput("curve", "Load input curve...")
        ),
        mainPanel(
            tableOutput("contents")
        )
    )
)

# Define server logic required to load file
server <- function(input, output) {
    output$contents <- renderTable({
        file <- input$curve
        ext <- tools::file_ext(file$datapath)
        nsample <- input$points
        
        # Curve inversion
        invert.curve=function(x, nsample=256) {
            x=as.matrix(x)  # in case x is a vector
            ROWS=dim(x)[1]
            if (dim(x)[2]==1) x=cbind(seq(0,1,length.out=ROWS), x)  # x is vector
            x=x[,2:1]  # swap in/out
            spl=spline(x, n=nsample, method="natural")
            return(cbind(spl$x, spl$y))       
        }
        
        req(file)
        x=as.matrix(read.csv(file$datapath, header=FALSE, sep=' '))
        m=invert.curve(x, nsample=nsample)
        # write.table(m, paste0("C:/InvertCurves/INV_", nsample, ".", ext),
        write.table(m, "INVIERNO.txt",
                        sep=' ', row.names=FALSE, col.names=FALSE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


# To share the shiny app via https://www.shinyapps.io/
# install.packages('rsconnect')
# library(rsconnect)
# ruta=paste0(getwd(), "/CurveInvert")
# rsconnect::deployApp(ruta)
# Application successfully deployed to https://guillermoluijk.shinyapps.io/curveinvert/