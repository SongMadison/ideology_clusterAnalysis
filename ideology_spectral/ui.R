shinyUI(fluidPage(
    headerPanel("Cluseter Analysis on Ideology"),
    sidebarPanel(
        selectInput("data set", "Select the dataset",
                    selected = "Extened_independent-1010",
                    choices  = c("Extened_independent-1010")),
                    #"allPartisans-2066","strongPartisans-535","Partisans-1509",          
                    #"leans-527", "independent-483" ,"fulldata-2549")),
        selectInput("transform", "Select the transformation",
                    selected = 'standardization',
                    choices  = c("column standardization")),
        selectInput("method", "Select the clustering algorithm",
                    selected = 'spectral-clustering',
                    choices  = c('spectral-clustering')),
        numericInput('clusters', 'Cluster count', value =  4, 
                     min = 2, max = 20),
        downloadButton('downloadData', 'membership.csv')
    ),
    mainPanel(
        tabsetPanel(
            tabPanel ("correlation plot", plotOutput('correlation') ),
            tabPanel("before standardization", plotOutput('boxplot1')),
            tabPanel("after standardization", plotOutput('boxplot2')),
            tabPanel( "Comments",  verbatimTextOutput("comments") )
       )
    ),
    
    # mainPanel("a lot of positive coefficients, like libertarian & authoritarian, gvttrust & conspiracy"),
    # mainPanel(plotOutput('correlation')),
    # 
    # mainPanel(plotOutput('boxplot1')),
    # mainPanel(plotOutput('boxplot2')),
    # 
    mainPanel(plotOutput('scree_plot') ), 
    mainPanel(tableOutput('clust_size') ),
    mainPanel(plotOutput('centers_plot1') ),
    mainPanel(plotOutput('centers_plot2') ),
    #mainPanel(plotOutput('centers_plot3') ),
    
    mainPanel(plotOutput('mds_plot1') ),
    mainPanel(plotOutput('mds_plot2') ),
    mainPanel(plotOutput('mds_plot3') )
    
    )
)