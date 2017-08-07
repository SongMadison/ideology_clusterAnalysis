shinyUI(
    
fluidPage(
    
    navbarPage("Cluseter Analysis on Ideology 2016 Election",
    
    tabPanel("Main Results",
    
    sidebarPanel(
        selectInput("data set", "Select the dataset",
                    selected = "fulldata-2549",
                    choices  = c("Democrats-1024","Republicans-485","allPartisans-2066","strongPartisans-535",
                                 "Partisans-1509",          
                                 "leans-527", "independent-483" ,"Extened_independent-1010",
                                 "fulldata-2549")),
        selectInput("transform", "Select the transformation",
                    selected = 'standardization',
                    choices  = c("column standardization only")),
        selectInput("method", "Select the clustering algorithm",
                    #selected = 'kmeans',
                    choices  = c("kmeans", "hierachical", "GMM","LCA")),
        conditionalPanel(
            condition = "input.method == 'hierachical'",
            selectInput("link_method", "choose linkeage method",
                        c("complete","single", "average"))
        ),
        numericInput('clusters', 'Cluster count', value =  6, 
                     min = 2, max = 30),
        #numericInput('otheriables', 'NULL', value =  4, 
        #min = 2, max = 30),
        
        downloadButton('membership_csv', 'membership.csv'),
        
        #downloadButton('centers_csv', 'Centers.csv'))  ,
        
        checkboxInput("partial", label = "Highlight Points Close to the Centers"),
        conditionalPanel(
            condition = "input.partial == true",
            selectInput("percentDownload", "Percentage close to centers",
                        selected  = '50%',
                        choices = c("1%", "5%", '10%', '20%','50%','100%')),
            downloadButton('partMembership_csv', 'partMembership.csv')
            #downloadButton('partcenters_csv', 'partCenters.csv')
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
            fluidRow(
                column(6,
                       tableOutput('clust_size')),
                column(6,
                       tableOutput('clust_size_p'))
            ),
            
            h4("Line Plots for cluster centers (the normalized ideology scores)"),
            plotOutput('centers_plot1') ,
            plotOutput('centers_plot12'),
            
            h4("engagement vs dieology plot"),
            plotOutput('diagnosisPlot'),
            plotOutput('diagnosisPlot2'),
       
            h4("Balloonplot for cluster centers (the normalized ideology scores)"),
            plotOutput('centers_plot2'),
        
            h4("Low dimension representation of the points"),
            p("'o' are the points far from the centers, points labeled by number are the points within selected percentage close to the centers"),
            plotOutput('mds_plot1') ,
            plotOutput('mds_plot2') ,
            plotOutput('mds_plot3') ,
            plotOutput('h_tree_plot') 
            
            
        )
    ),
        
        
    # tabPanel( "Some diagnosis plots",
    #           mainPanel(
    #               h4("for 6 clusters, k-means"),
    #               a("report_kmeans_0626.html")
    #           )      
    #           
    #     ),
    tabPanel( "Others",
              mainPanel(
                  h4("correlation among all variables"),
                  plotOutput('correlation') ,
                  
                  h4("variables before normalization"),
                  plotOutput('boxplot1'),
                  
                  h4("variables after normalization"),
                  plotOutput('boxplot2'),
                  
                  h4("some more comments"),
                  verbatimTextOutput("comments") 
                  
              )      
              
    )
    )
)
)