shinyUI(fluidPage(
    headerPanel("Cluseter Analysis on Ideology"),
    sidebarPanel(
        selectInput("transform", "Select the transformation",
                    selected = 'standardization',
                    choices  = c("standardization", "standardization+PCA")),
        numericInput('clusters', 'Cluster count', value =  4, 
                     min = 2, max = 30),
        selectInput("method", "Select the clustering algorithm",
                    selected = 'kmeans',
                    choices  = c("kmeans", "hierachical", "GMM")),
        selectInput("link_method", "linkeage method, if you use Hierachical",
                    selected = 'complete',
                    choices  = c("complete","single", "average")),
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
    
    
    mainPanel(tableOutput('clust_size')),
    mainPanel(plotOutput('centers_plot') ),
    mainPanel(plotOutput('centers_plot2') ),

    mainPanel(plotOutput('mds_plot1') ),
    mainPanel(plotOutput('mds_plot2') ),
    mainPanel(plotOutput('mds_plot3') ),
    mainPanel(plotOutput('h_tree_plot') )
   )
)