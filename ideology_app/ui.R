shinyUI(fluidPage(
    headerPanel("Cluseter Analysis on Ideology"),
    sidebarPanel(
        selectInput("data set", "Select the dataset",
                    selected = "strongPartisans-535",
                    choices  = c("allPartisans-2066","strongPartisans-535","Partisans-1509",          
                                 "leans-527", "independent-483" ,"Extened_independent-1010","fulldata-2561")),
        selectInput("transform", "Select the transformation",
                    selected = 'standardization',
                    choices  = c("standardization", "standardization+PCA")),
        selectInput("method", "Select the clustering algorithm",
                    selected = 'kmeans',
                    choices  = c("kmeans", "hierachical", "GMM")),
        selectInput("link_method", "choose linkeage method to use Hierachical",
                    selected = 'complete',
                    choices  = c("complete","single", "average")),
        numericInput('clusters', 'Cluster count', value =  4, 
                     min = 2, max = 30),
        
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
    mainPanel(plotOutput('centers_plot1') ),
    mainPanel(plotOutput('centers_plot2') ),
    #mainPanel(plotOutput('centers_plot3') ),
    
    mainPanel(plotOutput('mds_plot1') ),
    mainPanel(plotOutput('mds_plot2') ),
    mainPanel(plotOutput('mds_plot3') ),
    mainPanel(plotOutput('h_tree_plot') )
   )
)