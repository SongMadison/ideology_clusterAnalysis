shinyUI(
    
    
    
    fluidPage(
        
        
        titlePanel("Cluseter Analysis on Ideology"),
        
        sidebarPanel(
            selectInput("data set", "Select the dataset",
                        selected = "Extened_independent-1010",
                        choices  = c("allPartisans-2066","strongPartisans-535","Partisans-1509",          
                                     "leans-527", "independent-483" ,"Extened_independent-1010",
                                     "fulldata-2549")),
            selectInput("transform", "Select the transformation",
                        selected = 'standardization',
                        choices  = c("row and column standardization")),
            selectInput("method", "Select the clustering algorithm",
                        #selected = 'kmeans',
                        choices  = c("kmeans", "hierachical", "GMM")),
            conditionalPanel(
                condition = "input.method == 'hierachical'",
                selectInput("link_method", "choose linkeage method",
                            c("complete","single", "average"))
            ),
            numericInput('clusters', 'Cluster count', value =  4, 
                         min = 2, max = 30),
            #numericInput('otheriables', 'NULL', value =  4, 
            #min = 2, max = 30),
            
            downloadButton('membership_csv', 'membership.csv'),
            
            checkboxInput("partial", label = "ONLY Points Close to the Centers"),
            conditionalPanel(
                condition = "input.partial == true",
                selectInput("percentDownload", "Percentage close to centers",
                            selected  = '100%',
                            choices = c("1%", "5%", '10%', '20%','50%','100%')),
                downloadButton('partMembership_csv', 'partMembership.csv')            
            )
        ),
        
        # mainPanel(
        #     tabsetPanel(
        #         tabPanel ("correlation plot", plotOutput('correlation') ),
        #         tabPanel("before standardization", plotOutput('boxplot1')),
        #         tabPanel("after standardization", plotOutput('boxplot2')),
        #         tabPanel( "Comments",  verbatimTextOutput("comments") )
        #     )
        # ),
        
        # mainPanel("a lot of positive coefficients, like libertarian & authoritarian, gvttrust & conspiracy"),
        # mainPanel(plotOutput('correlation')),
        # 
        # mainPanel(plotOutput('boxplot1')),
        # mainPanel(plotOutput('boxplot2')),
        # 
        
        mainPanel(
            tableOutput('clust_size')),
        mainPanel(
            h4("Plots for centers calculated on the normalized ideology scores"),
            plotOutput('centers_plot1') ),
        mainPanel(plotOutput('centers_plot2') ),
        #mainPanel(plotOutput('centers_plot3') ),
        
        mainPanel(plotOutput('mds_plot1') ),
        mainPanel(plotOutput('mds_plot2') ),
        mainPanel(plotOutput('mds_plot3') ),
        mainPanel(plotOutput('h_tree_plot') )
    )
)