if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(wordcloud)) install.packages("wordcloud", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(bslib)) install.packages("bslib", repos = "http://cran.us.r-project.org")
if(!require(formattable)) install.packages("formattable", repos = "http://cran.us.r-project.org")
if(!require(gtable)) install.packages("gtable", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org")
if(!require(stopwords)) install.packages("stopwords", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.us.r-project.org")
if(!require(zoo)) install.packages("zoo", repos = "http://cran.us.r-project.org")


# set mapping colour for each outbreak
text_high<- read.csv("text_high.csv")
text_low <- read.csv("text_low.csv")
highai_projection <- read.csv("highai_projection.csv")
lowai_projection <- read.csv("lowai_projection.csv")
start_data_collection = as.Date("2021-04-01")
end_data_collection = as.Date("2023-03-31")
start_date_forecast = as.Date("2023-04-01")
end_date_forecast = as.Date("2025-03-31")
ghost_highai_table<- read.csv("ghost_highai.csv")
ghost_lowai_table<- read.csv("ghost_lowai.csv")
highai_skillset <- read.csv("highai_skillset.csv")
lowai_skillset <- read.csv("lowai_skillset.csv")
stprojections <- read_xlsx("stprojections_2325.xlsx", sheet = "stprojections_2325") %>%
  filter(areaname == "Wisconsin" |areaname == "Missouri"  ,
    code %in% c("13-1161", "43-3031", "43-3071", "15-1211", "15-1232", "15-1244", "15-1252", "43-4171", "17-2112", "17-2141",
                "37-2012", "37-2011", "37-3011", "35-2021", "35-2014", "35-3023", "35-3031", "47-2061", "51-4121", "47-2031")
  )
stprojections$areaname <- ifelse(stprojections$areaname =="Wisconsin","WI", "MO")
current_postings<- read.csv("current_postings.csv")

customGreen0 = "#DeF7E9"

customGreen = "#71CA97"

customRed = "#ff7f7f"

summary_table_high <- data.table(
  Occupation = c("Market Research Analysts & Marketing Specialists",
                 "Bookkeeping, Accounting, and Auditing Clerks",
                 "Tellers",
                 "Computer Systems Analysts",
                 "Computer User Support Specialists",
                 "Network and Computer Systems Administrators",
                 "Software Developers",
                 "Receptionists and Information Clerks",
                 "Industrial Engineers",
                 "Mechanical Engineers"),
  "Employment 2022 WI" = c(14510, 38900, 10110, 11460, 12830, 8560, 19270,
                           28980, 11780, 9350),
  "Projected 2032 WI"  =c(17080, 38190, 8820, 13110, 14000, 8980, 25280,
                          29950, 13830, 10960),
  "Employment 2022 MO" =c(14503, 22861, 7374, 9225, 14751, 6966, 19531,
                          17805, 4925, 3080),
  "Projected 2032 MO" =c(16789, 22048, 6389, 10169, 15820, 7289, 24693,
                         17847, 5748, 3415),
  "AI Exposure" = c(1.46,1.32,1.09,1.04,1.04,1.04,1.04,0.92,0.84,0.84)
)

summary_table_high<-formattable(summary_table_high, 
                                caption = htmltools::tags$caption(
                                  style = 'caption-side: bottom; text-align: center;',
                                  'Table: ', htmltools::em('Projections for Most AI Impacted Occupations, Data from State Projections.')),
                                  align =c("l","c","c","c","c","r"), list(
  `Occupation` = formatter("span",
                           style = ~ style(color = "gray")), 
  `Projected 2032 WI`= formatter("span", style = ~ style(color = ifelse(`Projected 2032 WI`> `Employment 2022 WI`, "green", "red")),
                                 ~ icontext(ifelse(`Projected 2032 WI` > `Employment 2022 WI`,"arrow-up", "arrow-down"), `Projected 2032 WI`)),
  `Projected 2032 MO`= formatter("span", style = ~ style(color = ifelse(`Projected 2032 MO` > `Employment 2022 MO`, "green", "red")),
                                 ~ icontext(ifelse(`Projected 2032 MO` > `Employment 2022 MO`,"arrow-up", "arrow-down"), `Projected 2032 MO`))
  
))

   
summary_table_low <- data.table(
  Occupation = c( "Maids and Housekeeping Cleaners",
                  "Janitors and Cleaners, Except Maids and Housekeeping Cleaners",
                  "Landscaping and Groundskeeping Workers",
                  "Food Preparation Workers",
                  "Cooks, Restaurant",
                  "Fast Food and Counter Workers",
                  "Waiters and Waitresses",
                  "Construction Laborers",
                  "Welders, Cutters, Solderers, and Brazers",
                  "Carpenters"),
  "Employment 2022 WI" = c(20410, 43010, 22140, 21030, 24790, 56080, 35640,
                           23620, 18930, 18630),
  "Projected 2032 WI" = c(22130, 45830, 24580, 20740, 32190, 60930, 37100,
                          27590, 20690, 21040),
  "Employment 2022 MO" =c(17808, 45332, 21359, 15572, 31981, 29736, 41628,
                          22895, 10316, 20471),
  "Projected 2032 MO" =c(18050, 47062, 22378, 15330, 39312, 30878, 41142,
                         24657, 10450, 21623),
  "AI Exposure"  = c(-1.40,-1.24,-1.23,-1.04,-0.96,-0.74,-0.73,-0.72,-0.68,-0.67)
)
summary_table_low <-formattable(summary_table_low, 
                                caption = htmltools::tags$caption(
                                  style = 'caption-side: bottom; text-align: center;',
                                  'Table: ', htmltools::em('Projections for Least AI Impacted Occupations, Data from State Projections.')),
                                align =c("l","c","c","c","c","r"), list(
  `Occupation` = formatter("span",
                           style = ~ style(color = "gray")), 
  `Projected 2032 WI`= formatter("span", style = ~ style(color = ifelse(`Projected 2032 WI` >`Employment 2022 WI`, "green", "red")),
                                 ~ icontext(ifelse(`Projected 2032 WI` >`Employment 2022 WI`,"arrow-up", "arrow-down"), `Projected 2032 WI`)),
  `Projected 2032 MO`= formatter("span", style = ~ style(color = ifelse(`Projected 2032 MO` >`Employment 2022 MO`, "green", "red")),
                                 ~ icontext(ifelse(`Projected 2032 MO` >`Employment 2022 MO`,"arrow-up", "arrow-down"), `Projected 2032 MO`))
  
))


low_skills<- c("Operation","Mopping","Housekeeping","Safety","Sanitation","Furniture", "Cleaning",
               "Cooking","Preparation","Mathematics","Handling","Linens","Register",
               "Disinfecting","Landscaping","Carpentry","Construction","Grilling","Procedures",
               "Merchandising","Tools","Mowing","Sale","Welding","Selling","Machinery","Standards",
               "Infection","Control","Removal","Complaint","Stocks","Inventory","Warehousing",
               "Balancing","Ledger","Billing","Grinding","Painting","Upholstery","Commercial",
               "Hazard","Irrigation","Forklift","Communication","Cleanliness","Lifting",
               "Management","Detail","Operations","Positivity","Teamwork", "English","Sales",
               "Multitasking","Interpersonal","Communications","Leadership","Timing","Ethic",
               "Writing","Solving","Organizational","Punctuality","Arithmetic","Loading",
               "Unloading","Packaging","Labeling","Driving","Motivation","Coordinating",
               "Computer","Literacy", "Learning","Scheduling","Tactfulness","Proactivity",
               "Quality","Honesty","Planning","Accountability","Tape","Measure","Security", 
               "Coaching","Order","Outlook","Operating","Systems","Office","Microsoft","Excel",         
               "Google","Workspace","Valor","Epic","App","Web","Browsers","Framer","Spreadsheets",
               "VEE","Word","PowerPoint","STL","System","LAMP","Bundle","SAP","Protractor",
               "Disassembler","Truss","Bluetooth","IOS","Safari","Firefox","Edge","WindowBlinds","Geographic",
               "Access","HotSOS","Tracking","Primavera","Project","Database",
               "Epicor","Prophet","Distribution", "Expo","AutoCAD","Hosting","Business","Zoom","Ruby",
               "API","Maps","Rexx","Kronos")  

top_skills<- c("Science","Project","Management","Agile","Methodology","SQL","Marketing",
               "Software","Engineering","Development","Accounting","Automation","Java","JavaScript",     
               "Invoicing","Workflow","Python","Application","API","Auditing","Support","Payable","Amazon","Web",            
               "Services","Process","Improvement","Operating","Continuous","C","Azure","Business","Mechanical","Debugging",      
               "Finance","DevOps","Data","Analysis","Accounts","Receivable","Git","Scrum","Scalability","CI","CD",          
               "Entry","Linux","Information","Angular","Scripting", "Relationship","HTML","RESTful", "CSS",          
               "Microservices", "Communication","Service","Problem","Solving","Troubleshooting", "Detail",
               "Operations","Leadership","Writing","Sales","Office","Research","Excel",          
               "Planning","Interpersonal","Verbal","Technology","Multitasking", "Organizational",
               "Innovation","Prioritization","Presentations","Mathematics","Coordinating",
               "Motivation","Professionalism", "Mentorship", "Outlook","Literacy","PowerPoint",
               "Teamwork","Analytical", "Confidentiality","Decision","Scheduling","Starter","Influencing",    
               "Filing","English","Critical","Clerical","Ability","Deadlines","Consulting","Quality",
               "Assurance","Word","Administrative","Functions","Willingness","Coaching","Lifting",        
               "Kubernetes","JIRA","Docker","Jenkins", "Salesforce","AutoCAD","SAP","Databases","CAD",            
               "NET","Dashboard","Spreadsheets","XML","Servers","BI","Boot","SharePoint",     
               "JSON","PowerShell" )  

############# Add theme

custom_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#000000",
  primary = "#0199F8",
  secondary = "#FF374B",
  base_font = "Maven Pro"
)


ui <- fluidPage(
  theme = custom_theme,
  headerPanel("AI Impact on Selected Occupations in Missouri and Wisconsin"),
 
  sidebarLayout(
    sidebarPanel(
   
  
  # Sidebar panel for inputs ----
   radioButtons("state", "Choose the State:",
                 c("Wisconsin" = "WI",
                   "Missouri" = "MO")),
 # br() element to introduce extra vertical spacing ----
  
  br(),
 
 selectInput( "category", "Choose the AI Impact Category:",
   choices = c("Most Impacted Occupations" = "high_ai",
               "Least Impacted Occupations" = "low_ai")),
 br(),
 
 conditionalPanel(condition = "input.category =='high_ai'",
                  pickerInput("highai", "Select an occupation from the following list:",   
                  choices = c("Market Research Analysts & Marketing Specialists",
                         "Bookkeeping, Accounting, and Auditing Clerks",
                         "Tellers",
                         "Computer Systems Analysts",
                         "Computer User Support Specialists",
                         "Network and Computer Systems Administrators",
                         "Software Developers",
                         "Receptionists and Information Clerks",
                         "Industrial Engineers",
                         "Mechanical Engineers"), 
             selected = c("Tellers"),
             
             multiple = FALSE) ),

 conditionalPanel(condition = "input.category =='low_ai'",
               pickerInput("lowai", "Select an occupation from the following list:",   
               choices = c( "Maids and Housekeeping Cleaners",
                          "Janitors and Cleaners, Except Maids and Housekeeping Cleaners",
                          "Landscaping and Groundskeeping Workers",
                          "Food Preparation Workers",
                          "Cooks, Restaurant",
                          "Fast Food and Counter Workers",
                          "Waiters and Waitresses",
                          "Construction Laborers",
                          "Welders, Cutters, Solderers, and Brazers",
                          "Carpenters"), 
            selected = "Maids and Housekeeping Cleaners",
             multiple = FALSE)
 )
 
  ),
  


mainPanel(
  tabsetPanel(
    tabPanel("Summary",style = 'margin-left: 5%; margin-right: 5%;',
             formattableOutput("summary")),
             
    # tabPanel("Trend", 
    #          fluidRow( style = 'margin-left: 5%; margin-right: 5%;',
    #           div(plotOutput("job_openings"),
    #             style ='display: inline-block; vertical-align: middle;')),
    #          fluidRow(  style = 'margin-left: 5%; margin-right: 5%;',
    #            div(plotOutput("job_changes"),
    #             style ='display: inline-block; vertical-align: middle;'))),
    tabPanel("Trend", 
             fluidRow(  style = 'margin-left: 5%; margin-right: 5%;',
                        div(plotOutput("combo",height = 600),
                            style ='display: inline-block; vertical-align: middle;'))),
    tabPanel("Skills Analysis",
            fluidRow( plotOutput("wordcloud")),
            fluidRow( plotlyOutput("skill_set", height = 600))),
    tabPanel("Ghost job Analysis",
             fluidRow( plotOutput("ghost_job"))),
            
    tabPanel("Projection",
            fluidRow( style = 'margin-top:5%;',
                       plotOutput("comparison",width = "100%", height = "300")),
            fluidRow( style = 'margin-top:5%;',
                      plotlyOutput("projection",width = "100%", height = "300"))
  )
)
)
)
)




# Define server logic for random distribution app ----
server <- function(input, output) {
  
  ######## Tab 1
  
  
  output$summary <- renderFormattable({

 
    if (input$category =="high_ai"){
      summary_table_high 
    } else {
      summary_table_low
    }
  })
 

   #######  Tab 2 Trend
  output$job_openings <- renderPlot({
    
    if (input$category =="high_ai") {
      
      openings<- highai_projection %>% filter(state == input$state & job_title == input$highai)%>% 
        select(job_id, date_compiled,state, job_title)
     
      openings$date_compiled <- as.Date(openings$date_compiled)
      openings$year<- year(openings$date_compiled)
      openings$month<- month(openings$date_compiled)
      openings%>% group_by(year,month, job_title)%>% summarise(Postings= n())%>%mutate(Date = as.Date(paste(year, month, "01", sep = "-")))%>% 
        ggplot()+geom_line(aes(x=Date,y=Postings, color=job_title))+
        theme_linedraw()+
        scale_x_date(date_labels = "%b %Y")+ 
        theme(
          plot.title = element_text(color="#0199F8", size=18, hjust = 0.5,face="bold.italic"),
          axis.title.x = element_text(color="#993333", size=12, face="bold"),
          axis.title.y = element_text(color="#993333", size=12, face="bold"),
          legend.position = "none"
        )+ggtitle(paste0(input$highai," ","Job Postings Trend"))
        
      
    } else {
      
      openings<- lowai_projection %>% filter(state == input$state & job_title == input$lowai)%>% 
        select(job_id, date_compiled,state, job_title)
      
      openings$date_compiled <- as.Date(openings$date_compiled)
      openings$year<- year(openings$date_compiled)
      openings$month<- month(openings$date_compiled)
      openings%>% group_by(year,month, job_title)%>% summarise(Postings = n())%>%mutate(Date = as.Date(paste(year, month, "01", sep = "-")))%>% 
        ggplot()+geom_line(aes(x=Date,y=Postings, color=job_title))+
        scale_x_date(date_labels = "%b %Y")+ 
        theme_linedraw()+
        ggtitle(paste0(input$lowai," ","Job Postings Trend"))+
        theme(
          plot.title = element_text(color="#0199F8", size=18, ,hjust = 0.5,face="bold.italic"),
          axis.title.x = element_text(color="#993333", size=12, face="bold"),
          axis.title.y = element_text(color="#993333", size=12, face="bold"),
          legend.position = "none"
        )
    }
  })
  
  
  output$job_changes <- renderPlot({
    if (input$category =="high_ai") {
      
      highai_projection %>% filter(state == input$state & job_title == input$highai)%>% 
        select(job_id, date_compiled,state, job_title)%>% mutate(YearMonth = format(as.Date(date_compiled), "%Y-%m"))%>%
        group_by(YearMonth) %>% summarize(Postings = n()) %>%
        arrange(YearMonth)%>% mutate(Monthly_Changes = (Postings - lag(Postings)) / lag(Postings),
          Yearly_changes = (Postings - lag(Postings, 12)) / lag(Postings, 12))%>%
           ggplot(aes(x=YearMonth, y=Monthly_Changes, fill=Postings)) + 
        geom_col(position="dodge") +
        scale_y_continuous(labels = scales::percent)+
        theme_linedraw()+
        guides(fill = FALSE) +
        theme(
          plot.title = element_text(color="#0199F8", size=18, hjust = 0.5,face="bold.italic"),
          axis.title.x = element_text(color="#993333", size=12, face="bold"),
          axis.title.y = element_text(color="#993333", size=12, face="bold"),
          legend.position = "none",
          axis.text.x = element_blank()
        )+ggtitle(paste0(input$highai," ","Job Postings Percent Change By Month"))
      
      
    } else {
      
      lowai_projection %>% filter(state == input$state & job_title == input$lowai)%>% 
        select(job_id, date_compiled,state, job_title)%>% mutate(YearMonth = format(as.Date(date_compiled), "%Y-%m"))%>%
        group_by(YearMonth) %>% summarize(Postings = n()) %>%
        arrange(YearMonth)%>% mutate(Monthly_Changes = (Postings - lag(Postings)) / lag(Postings),
                                     Yearly_changes = (Postings - lag(Postings, 12)) / lag(Postings, 12))%>%
        ggplot(aes(x=YearMonth, y=Monthly_Changes, fill=Postings)) + 
        geom_col(position="dodge") +
        scale_y_continuous(labels = scales::percent)+
        guides(fill = FALSE)+
        theme_linedraw()+
        ggtitle(paste0(input$lowai," ","Job Postings Percent Changes By Month"))+
        theme(
          plot.title = element_text(color="#0199F8", size=18, ,hjust = 0.5,face="bold.italic"),
          axis.title.x = element_text(color="#993333", size=12, face="bold"),
          axis.title.y = element_text(color="#993333", size=12, face="bold"),
          axis.text.x = element_blank(),
          legend.position = "none"
        )
    }
    
   
  
  })
  
  
  output$combo <- renderPlot({
    
    if (input$category =="high_ai") {
      
      openings<- highai_projection %>% filter(state == input$state & job_title == input$highai)%>% 
        select(job_id, date_compiled,state, job_title)
      
      openings$date_compiled <- as.Date(openings$date_compiled)
      openings$year<- year(openings$date_compiled)
      openings$month<- month(openings$date_compiled)
      highai_openings<- openings%>% group_by(year,month, job_title)%>% summarise(Postings = n(), .groups = 'drop')%>%
        mutate(Date = as.Date(paste(year, month, "01", sep = "-")))%>% 
        ggplot()+geom_line(aes(x=Date,y=Postings, color=job_title))+
        theme_linedraw()+
        labs(x="")+
        scale_y_log10() +
        scale_x_date(date_labels = "%b %Y")+ 
        theme(
          plot.title = element_text(color="#0199F8", size=18, hjust = 0.5,face="bold.italic"),
          axis.title.x = element_text(color="#993333", size=12, face="bold"),
          axis.title.y = element_text(color="#993333", size=12, face="bold"),
          legend.position = "none"
        )+ggtitle(paste0(input$highai," ","Job Postings Trend"))
      
      
     highai_changes<-  highai_projection %>% filter(state == input$state & job_title == input$highai)%>% 
        select(job_id, date_compiled,state, job_title)%>% mutate(YearMonth = format(as.Date(date_compiled), "%Y-%m"))%>%
        group_by(YearMonth) %>% summarize(Postings = n()) %>%
        arrange(YearMonth)%>% mutate(Monthly_Changes = (Postings - lag(Postings)) / lag(Postings),
                                     Yearly_changes = (Postings - lag(Postings, 12)) / lag(Postings, 12))%>%
        ggplot(aes(x=YearMonth, y=Monthly_Changes, fill=Postings)) + 
        geom_col(position="dodge") +
       scale_x_discrete(breaks=c("2021-01","2022-01","2023-01","2024-01"),labels=c("2021-01" = "Jan 2021", "2022-01" = "Jan 2022","2023-01" = "Jan 2023",
                                 "2024-01" = "Jan 2024"))+
        scale_y_continuous(labels = scales::percent)+
        theme_linedraw()+
        guides(fill = FALSE) +
        theme(
          plot.title = element_text(color="#0199F8", size=18, hjust = 0.5,face="bold.italic"),
          axis.title.x = element_text(color="#993333", size=12, face="bold"),
          axis.title.y = element_text(color="#993333", size=12, face="bold"),
          legend.position = "none",
          axis.text.x = element_text(size = 8)
        )+ggtitle(paste0(input$highai," ","Job Posting Percent Change By Month"))+
       labs( y="Percent Change", x="")
     
     g1 <- ggplotGrob(highai_openings)
     g2 <- ggplotGrob(highai_changes)
     g <- rbind(g1, g2, size = "first")
 #    g$widths <- unit.pmax(g1$widths, g1$widths)
     grid.newpage()
     grid.draw(g) 
      
    } else {
      
      openings<- lowai_projection %>% filter(state == input$state & job_title == input$lowai)%>% 
        select(job_id, date_compiled,state, job_title)
      
      openings$date_compiled <- as.Date(openings$date_compiled)
      openings$year<- year(openings$date_compiled)
      openings$month<- month(openings$date_compiled)
      lowai_openings<- openings%>% group_by(year,month, job_title)%>% summarise(Postings = n(), .groups = 'drop')%>%
        mutate(Date = as.Date(paste(year, month, "01", sep = "-")))%>% 
        ggplot()+geom_line(aes(x=Date,y=Postings, color=job_title))+
        scale_x_date(date_labels = "%b %Y")+ 
        scale_y_log10() +
        theme_linedraw()+
        labs(x="")+
        ggtitle(paste0(input$lowai," ","Job Postings Trend"))+
        theme(
          plot.title = element_text(color="#0199F8", size=18, ,hjust = 0.5,face="bold.italic"),
          axis.title.x = element_text(color="#993333", size=12, face="bold"),
          axis.title.y = element_text(color="#993333", size=12, face="bold"),
          legend.position = "none"
        )
    
    
    
    lowai_changes<- lowai_projection %>% filter(state == input$state & job_title == input$lowai)%>% 
      select(job_id, date_compiled,state, job_title)%>% mutate(YearMonth = format(as.Date(date_compiled), "%Y-%m"))%>%
      group_by(YearMonth) %>% summarize(Postings = n()) %>%
      arrange(YearMonth)%>% mutate(Monthly_Changes = (Postings - lag(Postings)) / lag(Postings),
                                   Yearly_changes = (Postings - lag(Postings, 12)) / lag(Postings, 12))%>%
      ggplot(aes(x=YearMonth, y=Monthly_Changes, fill=Postings)) + 
      geom_col(position="dodge") +
      scale_y_continuous(labels = scales::percent)+
      scale_x_discrete(breaks=c("2021-01","2022-01","2023-01","2024-01"),labels=c("2021-01" = "Jan 2021", "2022-01" = "Jan 2022","2023-01" = "Jan 2023",
                                                                                  "2024-01" = "Jan 2024"))+
      guides(fill = FALSE)+
      theme_linedraw()+
      labs(y= "Percent Change", x="")+
      ggtitle(paste0(input$lowai," ","Job Postings Percent Change By Month"))+
      theme(
        plot.title = element_text(color="#0199F8", size=18, ,hjust = 0.5,face="bold.italic"),
        axis.title.x = element_text(color="#993333", size=12, face="bold"),
        axis.title.y = element_text(color="#993333", size=12, face="bold"),
        axis.text.x = element_text(size = 8),
        legend.position = "none"
      )
    
    g1 <- ggplotGrob(lowai_openings)
    g2 <- ggplotGrob(lowai_changes)
    g <- rbind(g1, g2, size = "first")
 #   g$widths <- unit.pmax(g1$widths, g1$widths)
    grid.newpage()
    grid.draw(g) 
    
  }  

    
  })
  
  
  
  ######### Tab 3
  
  ##Wordcloud
  
  output$wordcloud <- renderPlot({
    
        if (input$category =="high_ai") {
      cloud<- text_high %>% filter( (State == input$state) & (job_title == input$highai) & !(word %in% stopwords::stopwords(source = "smart")))%>% select(word, freq)
      
      wordcloud(words=cloud$word, freq = cloud$freq,max.words=100, random.order = FALSE,
                rot.per=0.40,colors=brewer.pal(8, "Dark2"))
      
      title(paste0(input$highai," ", "Most Frequent Words Analysis"))
      
    } else {
      
      cloud<- text_low%>% filter((State == input$state) & (job_title == input$lowai) & !(word %in% stopwords::stopwords(source = "smart")))%>% select(word, freq)
      
      wordcloud(words=cloud$word, freq = cloud$freq,max.words=100, random.order = FALSE, 
                rot.per=0.40,colors=brewer.pal(8, "Dark2"))
      title(paste0(input$lowai," ", "Most Frequent Words Analysis"))
          }
  })

    
     ## Skill set
     
    
  output$skill_set <- renderPlotly({
    
    
    if (input$category =="high_ai") {
      
      # skill_high<- text_high %>% filter(State == input$state & job_title == input$highai)%>% select(word, freq)
      # skill_word<- skill_high%>% arrange(-freq) %>% filter(word %in% tolower(top_skills))%>% head(10)
      # high_skill_set<- highai_skill %>% filter(state == input$state & job_title == input$highai)
      # 
      # 
      # dict <- skill_word$word
      # 
      # word_matches <- data.frame()
      # 
      # for (i in dict) { 
      #   if (nchar(i)==1){
      #     xx<- sapply(strsplit(high_skill_set$skills, " "), function(words) which(nchar(words) == 1))
      #     word_tot<-str_count(xx, "^[i]")
      #     word_matches <- rbind(word_matches,word_tot)
      #   }
      #   else{
      #     word_tot<-str_count(high_skill_set$skills, i)
      #     word_matches <- rbind(word_matches,word_tot)
      #   }
      #   
      # }
      # word_matches<- word_matches
      # colnames(word_matches) <- paste( 1:ncol(word_matches))
      # 
      # word_matches<- t(word_matches)
      # colnames(word_matches)<- dict
      # date_compiled<- as.data.frame(as.Date(high_skill_set$date_compiled))
      # skill_top10 <- cbind(date_compiled, word_matches)
      #   colnames(skill_top10)[1]<- "Date_compiled"
      #   skill_top10$year<- year(skill_top10$Date_compiled)
      #   skill_top10$month<- month(skill_top10$Date_compiled)
      #   
      #   
      #   
      #   skill_top10<- na.omit(skill_top10)
      #   skill_top10<- skill_top10%>% group_by(year, month) %>%
      #     summarise_at( .vars = colnames(.)[2:11] , sum)%>%mutate(Date = as.Date(paste(year, month, "01", sep = "-")))%>%
      #     arrange(Date)
        
        
        
      #  g<-skill_top10 %>% pivot_longer(cols = -c(year,month), names_to = "Word_group", values_to = "Frequency")%>%
      #     group_by(year,month,Word_group) %>%
      #     summarise(Frequency=sum(Frequency)) %>% mutate(Date = as.Date(paste(year, month, "01", sep = "-")))%>% 
      #     ggplot()+geom_line(aes(x=Date,y=Frequency, color=as.factor(str_to_title(Word_group))))+
      #     scale_x_date(date_labels = "%b %Y")+ 
      #    # scale_y_log10() +
      #    theme_light()+
      #    guides(color = guide_legend(title = "Top  10 Skills"))+
      #    theme(
      #      plot.title = element_text(color="#0199F8", size=18, hjust = 0.5,face="bold.italic"),
      #      axis.title.x = element_text(color="#993333", size=12, face="bold"),
      #      axis.title.y = element_text(color="#993333", size=12, face="bold")
      #      )+ggtitle(paste0(input$highai," ","Top 10 Requested Skills"))
      # 
      # ggplotly(g)
      #   
      # fig<- plot_ly(skill_top10) %>%
      #   add_trace(x = ~skill_top10$Date, y = ~(skill_top10 %>% pull(3)), type = 'scatter', mode = 'lines+markers',
      #             name = colnames(skill_top10[,3]),visible = "legendonly") %>%
      #   add_trace(x = ~skill_top10$Date, y = ~(skill_top10 %>% pull(4)), type = 'scatter', mode = 'lines+markers',
      #             name = colnames(skill_top10[,4]), visible = TRUE) %>%
      #   add_trace(x = ~skill_top10$Date, y = ~(skill_top10 %>% pull(5)), type = 'scatter', mode = 'lines+markers',
      #             name = colnames(skill_top10[,5]),visible = "legendonly") %>%
      #   add_trace(x = ~skill_top10$Date, y = ~(skill_top10 %>% pull(6)), type = 'scatter', mode = 'lines+markers',
      #             name = colnames(skill_top10[,6]),visible = "legendonly") %>%
      #   add_trace(x = ~skill_top10$Date, y = ~(skill_top10 %>% pull(7)), type = 'scatter', mode = 'lines+markers',
      #             name = colnames(skill_top10[,7]), visible = "legendonly") %>%
      #   add_trace(x = ~skill_top10$Date, y = ~(skill_top10 %>% pull(8)), type = 'scatter', mode = 'lines+markers',
      #             name = colnames(skill_top10[,8]), visible = "legendonly") %>%
      #   add_trace(x = ~skill_top10$Date, y = ~(skill_top10 %>% pull(9)), type = 'scatter', mode = 'lines+markers',
      #             name = colnames(skill_top10[,9]), visible = "legendonly") %>%
      #   add_trace(x = ~skill_top10$Date, y = ~(skill_top10 %>% pull(10)), type = 'scatter', mode = 'lines+markers',
      #             name = colnames(skill_top10[,10]), visible = "legendonly") %>%
      #   add_trace(x = ~skill_top10$Date, y = ~(skill_top10 %>% pull(11)), type = 'scatter', mode = 'lines+markers',
      #             name = colnames(skill_top10[,11]), visible = TRUE) %>%
      #   add_trace(x = ~skill_top10$Date, y = ~(skill_top10 %>% pull(12)), type = 'scatter', mode = 'lines+markers',
      #             name = colnames(skill_top10[,12]), visible = "legendonly") %>%
      #   layout(title = paste0(input$lowai," ","Top 10 Requested Skills"), font =list(size=12),
      #          xaxis = list(title = 'Date', 
      #                       ticktext =list ("Jan 2021","Jan 2022", "Jan 2023", "Jan 2024", "Jan 2025"), 
      #                       tickvals=list("2021-01-01","2022-01-01", "2023-01-01", "2024-01-01", "2025-01-01"),
      #                       tickmode = "array",
      #                       ticks="inside",
      #                       showexponent = "all"),
      #          yaxis = list(title = 'Frequency'))
      skill_top10 <- highai_skillset%>% filter(state == input$state & job_title == input$highai)
      
      vis<- levels(as.factor(skill_top10$skills))
      fig<- plot_ly() %>%
        add_trace( data = (skill_top10%>% filter(skills ==vis[1])),x = ~Date, y = ~vals, type = 'scatter', mode = 'lines+markers',
                  name = ~ skills,visible = "legendonly") %>%
        add_trace(data = (skill_top10%>% filter(skills ==vis[2])),x = ~Date, y = ~vals, type = 'scatter', mode = 'lines+markers',
                  name = ~skills, visible = TRUE) %>%
        add_trace(data = (skill_top10%>% filter(skills ==vis[3])),x = ~Date, y = ~vals, type = 'scatter', mode = 'lines+markers',
                  name = ~skills,visible = "legendonly") %>%
        add_trace(data = (skill_top10%>% filter(skills ==vis[4])),x = ~Date, y = ~vals, type = 'scatter', mode = 'lines+markers',
                  name = ~skills,visible = "legendonly") %>%
        add_trace(data = (skill_top10%>% filter(skills ==vis[5])),x = ~Date, y = ~vals, type = 'scatter', mode = 'lines+markers',
                  name = ~skills, visible = "legendonly") %>%
        add_trace(data = (skill_top10%>% filter(skills ==vis[6])),x = ~Date, y = ~vals, type = 'scatter', mode = 'lines+markers',
                  name = ~skills, visible = "legendonly") %>%
        add_trace(data = (skill_top10%>% filter(skills ==vis[7])),x = ~Date, y = ~vals, type = 'scatter', mode = 'lines+markers',
                  name = ~skills, visible = "legendonly") %>%
        add_trace(data = (skill_top10%>% filter(skills ==vis[8])),x = ~Date, y = ~vals, type = 'scatter', mode = 'lines+markers',
                  name = ~skills, visible = "legendonly") %>%
        add_trace(data = (skill_top10%>% filter(skills ==vis[9])),x = ~Date, y = ~vals, type = 'scatter', mode = 'lines+markers',
                  name = ~skills, visible = TRUE) %>%
        add_trace(data = (skill_top10%>% filter(skills ==vis[10])),x = ~Date, y = ~vals, type = 'scatter', mode = 'lines+markers',
                  name = ~skills, visible = "legendonly") %>%
        layout(title = paste0(input$highai," ","Top 10 Requested Skills"), font =list(size=12),
               xaxis = list(title = 'Date', 
                            ticktext =list ("Jan 2021","Jan 2022", "Jan 2023", "Jan 2024", "Jan 2025"), 
                            tickvals=list("2021-01-01","2022-01-01", "2023-01-01", "2024-01-01", "2025-01-01"),
                            tickmode = "array",
                            ticks="inside",
                            showexponent = "all"),
               yaxis = list(title = 'Frequency'))
      fig
      
    } else {
      
      # skill_low<- text_low %>% filter(State == input$state & job_title == input$lowai)%>% select(word, freq)
      # skill_word<- skill_low%>% arrange(-freq) %>% filter(word %in% tolower(low_skills))%>% head(10)
      # low_skill_set<- lowai_skill %>% filter(state == input$state & job_title == input$lowai)
      # 
      # 
      # dict <- skill_word$word
      # 
      # word_matches <- data.frame()
      # 
      # for (i in dict) { 
      #   if (nchar(i)==1){
      #     xx<- sapply(strsplit(low_skill_set$skills, " "), function(words) which(nchar(words) == 1))
      #     word_tot<-str_count(xx, "^[i]")
      #     word_matches <- rbind(word_matches,word_tot)
      #   }
      #   else{
      #     word_tot<-str_count(low_skill_set$skills, i)
      #     word_matches <- rbind(word_matches,word_tot)
      #   }
      # }
      # word_matches<- word_matches
      # colnames(word_matches) <- paste( 1:ncol(word_matches))
      # 
      # word_matches<- t(word_matches)
      # colnames(word_matches)<- dict
      # low_skill_set$date_compiled<- as.data.frame(as.Date(low_skill_set$date_compiled))
      # skill_top10 <- cbind(low_skill_set$date_compiled, word_matches)
      # colnames(skill_top10)[1]<- "Date_compiled"
      # skill_top10$year<- year(skill_top10$Date_compiled)
      # skill_top10$month<- month(skill_top10$Date_compiled)
      # skill_top10<- na.omit(skill_top10)
      # skill_top10<- skill_top10%>% group_by(year, month) %>%
      #   summarise_at( .vars = colnames(.)[2:11] , sum)%>%
      #   mutate(Date = as.Date(paste(year, month, "01", sep = "-")))%>%
      #   arrange(Date)
      
      
    # g<- skill_top10 %>% pivot_longer(cols = -c(year,month), names_to = "Word_group", values_to = "Frequency")%>%
    #     group_by(year,month,Word_group) %>%
    #     summarise(Frequency=sum(Frequency)) %>% mutate(Date = as.Date(paste(year, month, "01", sep = "-")))%>% 
    #     ggplot()+geom_line(aes(x=Date,y=Frequency, color=as.factor(str_to_title(Word_group))))+
    #     scale_x_date(date_labels = "%b %Y")+ 
    #   # scale_y_log10() +
    #   theme_light()+
    #   guides(color = guide_legend(title = "Top 10 Skills"))+
    #   theme(
    #     plot.title = element_text(color="#0199F8", size=18, hjust = 0.5,face="bold.italic"),
    #     axis.title.x = element_text(color="#993333", size=12, face="bold"),
    #     axis.title.y = element_text(color="#993333", size=12, face="bold")
    #   
    #     )+ggtitle(paste0(input$lowai," ","Top 10 Requested Skills"))
    #   
    # ggplotly(g)
      
      skill_top10<- lowai_skillset%>% filter(state == input$state & job_title == input$lowai)
   
    vis<- levels(as.factor(skill_top10$skills))
    fig<- plot_ly() %>%
      add_trace( data = (skill_top10%>% filter(skills ==vis[1])),x = ~Date, y = ~vals, type = 'scatter', mode = 'lines+markers',
                 name = ~ skills,visible = "legendonly") %>%
      add_trace(data = (skill_top10%>% filter(skills ==vis[2])),x = ~Date, y = ~vals, type = 'scatter', mode = 'lines+markers',
                name = ~skills, visible = TRUE) %>%
      add_trace(data = (skill_top10%>% filter(skills ==vis[3])),x = ~Date, y = ~vals, type = 'scatter', mode = 'lines+markers',
                name = ~skills,visible = "legendonly") %>%
      add_trace(data = (skill_top10%>% filter(skills ==vis[4])),x = ~Date, y = ~vals, type = 'scatter', mode = 'lines+markers',
                name = ~skills,visible = "legendonly") %>%
      add_trace(data = (skill_top10%>% filter(skills ==vis[5])),x = ~Date, y = ~vals, type = 'scatter', mode = 'lines+markers',
                name = ~skills, visible = "legendonly") %>%
      add_trace(data = (skill_top10%>% filter(skills ==vis[6])),x = ~Date, y = ~vals, type = 'scatter', mode = 'lines+markers',
                name = ~skills, visible = "legendonly") %>%
      add_trace(data = (skill_top10%>% filter(skills ==vis[7])),x = ~Date, y = ~vals, type = 'scatter', mode = 'lines+markers',
                name = ~skills, visible = "legendonly") %>%
      add_trace(data = (skill_top10%>% filter(skills ==vis[8])),x = ~Date, y = ~vals, type = 'scatter', mode = 'lines+markers',
                name = ~skills, visible = "legendonly") %>%
      add_trace(data = (skill_top10%>% filter(skills ==vis[9])),x = ~Date, y = ~vals, type = 'scatter', mode = 'lines+markers',
                name = ~skills, visible = TRUE) %>%
      add_trace(data = (skill_top10%>% filter(skills ==vis[10])),x = ~Date, y = ~vals, type = 'scatter', mode = 'lines+markers',
                name = ~skills, visible = "legendonly") %>%
      layout(title = paste0(input$lowai," ","Top 10 Requested Skills"), font =list(size=12),
             xaxis = list(title = 'Date', 
                          ticktext =list ("Jan 2021","Jan 2022", "Jan 2023", "Jan 2024", "Jan 2025"), 
                          tickvals=list("2021-01-01","2022-01-01", "2023-01-01", "2024-01-01", "2025-01-01"),
                          tickmode = "array",
                          ticks="inside",
                          showexponent = "all"),
             yaxis = list(title = 'Frequency'))
                fig
      
    }
    
  })
  ######## Tab 4 Ghost job
  
  output$ghost_job<- renderPlot({
    
    
    if (input$category =="high_ai") {
      
      ghost_table <- ghost_highai_table %>% filter(state == input$state) %>% select(-state)
      
      tot_postings <- ghost_table %>% 
        group_by(onet_title) %>% 
        summarize(total_openings = sum(n_jobs_postings))
      
      ghost_jobs <- ghost_table %>%
        filter(ghostjob == "t") %>% 
        group_by(onet_title) %>% 
        summarize(n_ghost_jobs = sum(n_jobs_postings))
      
      ghost_summary <- full_join(tot_postings, ghost_jobs, by=c("onet_title"))
     # ghost_summary <- ghost_summary[, -3]
      
      ghost_summary$percent_ghost <- (ghost_summary$n_ghost_jobs / ghost_summary$total_openings)*100
      ghost_summary[is.na(ghost_summary)]<-0
      all_occ_ghost_share <- (100 * (sum(ghost_summary$n_ghost_jobs) / sum(ghost_summary$total_openings)))
      
      ggplot(ghost_summary, aes(x = onet_title, y = percent_ghost)) +
        geom_col(aes(fill= onet_title), show.legend = FALSE) +
        geom_hline(yintercept = all_occ_ghost_share, color = "red", linetype = "dashed") +
        coord_flip() +
        labs(
          title = "Over 30% of All Postings Among High AI Occupations are \nGhost Jobs",
          x = "",
          y = paste0("Share of Postings in" ," ", input$state, " " ,"that are Ghost Jobs (09-01-2021 - 09-01-2023)"))+
        theme_bw()
      
      
    }
    else{
      ghost_table <- ghost_lowai_table %>% filter(state == input$state)%>% select(-state)
      
      tot_postings <- ghost_table %>% 
        group_by(onet_title) %>% 
        summarize(total_openings = sum(n_jobs_postings))
      
      ghost_jobs <- ghost_table %>%
        filter(ghostjob == "t") %>% 
        group_by(onet_title) %>% 
        summarize(n_ghost_jobs = sum(n_jobs_postings))
      
      ghost_summary <- full_join(tot_postings, ghost_jobs, by=c("onet_title"))
      # ghost_summary <- ghost_summary[, -3]
      
      ghost_summary$percent_ghost <- (ghost_summary$n_ghost_jobs / ghost_summary$total_openings)*100
      
      ghost_summary[is.na(ghost_summary)]<-0
      all_occ_ghost_share <- (100 * (sum(ghost_summary$n_ghost_jobs) / sum(ghost_summary$total_openings)))
      
      ggplot(ghost_summary, aes(x = onet_title, y = percent_ghost)) +
        geom_col(aes(fill= onet_title), show.legend = FALSE) +
        geom_hline(yintercept = all_occ_ghost_share, color = "red", linetype = "dashed") +
        coord_flip() +
        labs(
          title = "Over 30% of All Postings Among High AI Occupations are \nGhost Jobs",
          x = "",
          y = paste0("Share of Postings in" ," ", input$state, " " ,"that are Ghost Jobs (09-01-2021 - 09-01-2023)")) +
        theme_bw()
      
      
    }
    
    
  })
  ######## Tab 5 Projection   
  #Graph 1
  output$comparison <- renderPlot({
    if (input$category =="high_ai") {
      
     compare_model<- highai_projection %>% filter(state ==input$state)
     compare_model$year<- year(compare_model$date_compiled)
     compare_model$month<- month(compare_model$date_compiled)
      
      input_data_wide <- compare_model %>%
        mutate(date = as.yearmon(paste(year, month), "%Y %m"))%>%
        group_by(year,month, job_title)%>% summarise(Postings = n())%>%
        arrange(year, month)%>%
        pivot_wider(names_from = job_title, values_from = Postings) %>%
        arrange(year, month)%>% mutate(across(everything(), ~ replace_na(.x, 0)))
      
      
   
      # convert to time series
      input_data_ts <- ts(data = input_data_wide[, 3:12], 
                          start = c(2021, 4), frequency = 12)
      
      
      
      arima_projections <-  data.frame(
        # generate each month within start and end dates of the forecast
        date = ym(format(seq(start_date_forecast, end_date_forecast, by="month"), "%Y%m")),
        baac = as.numeric(forecast(auto.arima(input_data_ts[,1], stepwise=FALSE, approximation=FALSE, seasonal=FALSE), h=24)$mean),
        csa = as.numeric(forecast(auto.arima(input_data_ts[,2], stepwise=FALSE, approximation=FALSE, seasonal=FALSE), h=24)$mean),
        cuss = as.numeric(forecast(auto.arima(input_data_ts[,3], stepwise=FALSE, approximation=FALSE, seasonal=FALSE), h=24)$mean),
        ie = as.numeric(forecast(auto.arima(input_data_ts[,4], stepwise=FALSE, approximation=FALSE, seasonal=FALSE), h=24)$mean),
        mrams = as.numeric(forecast(auto.arima(input_data_ts[,5], stepwise=FALSE, approximation=FALSE, seasonal=FALSE), h=24)$mean),
        me = as.numeric(forecast(auto.arima(input_data_ts[,6], stepwise=FALSE, approximation=FALSE, seasonal=FALSE), h=24)$mean),
        ncsa = as.numeric(forecast(auto.arima(input_data_ts[,7], stepwise=FALSE, approximation=FALSE, seasonal=FALSE), h=24)$mean),
        ric = as.numeric(forecast(auto.arima(input_data_ts[,8], stepwise=FALSE, approximation=FALSE, seasonal=FALSE), h=24)$mean),
        sd = as.numeric(forecast(auto.arima(input_data_ts[,9], stepwise=FALSE, approximation=FALSE, seasonal=FALSE), h=24)$mean),
        tell = as.numeric(forecast(auto.arima(input_data_ts[,10], stepwise=FALSE, approximation=FALSE, seasonal=FALSE), h=24)$mean)
      )%>%pivot_longer(!date, names_to = "job_title", values_to = "projection") %>%
        group_by(job_title) %>%
        summarize(projections = round(sum(projection)))
      
      
      
      # Bookkeeping, Accounting, and Auditing Clerks (baac) commented out because of non-stationary error
      sarima_projections <- data.frame(
        date = ym(format(seq(start_date_forecast, end_date_forecast, by="month"), "%Y%m")),
        baac = as.numeric(forecast(arima(input_data_ts[,1], order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),h = 24)$mean),
        csa = as.numeric(forecast(arima(input_data_ts[,2], order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),h = 24)$mean),
        cuss = as.numeric(forecast(arima(input_data_ts[,3], order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),h = 24)$mean),
        ie = as.numeric(forecast(arima(input_data_ts[,4], order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),h = 24)$mean),
        mrams = as.numeric(forecast(arima(input_data_ts[,5], order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),h = 24)$mean),
        me = as.numeric(forecast(arima(input_data_ts[,6], order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),h = 24)$mean),
        ncsa = as.numeric(forecast(arima(input_data_ts[,7], order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),h = 24)$mean),
        ric = as.numeric(forecast(arima(input_data_ts[,8], order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),h = 24)$mean),
        sd = as.numeric(forecast(arima(input_data_ts[,9], order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),h = 24)$mean),
        tell = as.numeric(forecast(arima(input_data_ts[,10], order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),h = 24)$mean)
      ) %>% pivot_longer(!date, names_to = "job_title", values_to = "projection") %>% 
        mutate(projection = ifelse(projection < 0, 0, projection)) %>%
        group_by(job_title) %>%
        summarize(projections = round(sum(projection)))
      
      
      hw_add_projections <-  data.frame(
        date = ym(format(seq(start_date_forecast, end_date_forecast, by="month"), "%Y%m")),
        baac = as.numeric(forecast(HoltWinters(input_data_ts[,1], seasonal="additive", alpha=.2, beta=.2, gamma= .2))$mean),
        csa = as.numeric(forecast(HoltWinters(input_data_ts[,2], seasonal="additive", alpha=.2, beta=.2, gamma= .2))$mean),
        cuss = as.numeric(forecast(HoltWinters(input_data_ts[,3], seasonal="additive", alpha=.2, beta=.2, gamma= .2))$mean),
        ie = as.numeric(forecast(HoltWinters(input_data_ts[,4], seasonal="additive", alpha=.2, beta=.2, gamma= .2))$mean),
        mrams = as.numeric(forecast(HoltWinters(input_data_ts[,5], seasonal="additive", alpha=.2, beta=.2, gamma= .2))$mean),
        me = as.numeric(forecast(HoltWinters(input_data_ts[,6], seasonal="additive", alpha=.2, beta=.2, gamma= .2))$mean),
        ncsa = as.numeric(forecast(HoltWinters(input_data_ts[,7], seasonal="additive", alpha=.2, beta=.2, gamma= .2))$mean),
        ric = as.numeric(forecast(HoltWinters(input_data_ts[,8], seasonal="additive", alpha=.2, beta=.2, gamma= .2))$mean),
        sd = as.numeric(forecast(HoltWinters(input_data_ts[,9], seasonal="additive", alpha=.2, beta=.2, gamma= .2))$mean),
        tell = as.numeric(forecast(HoltWinters(input_data_ts[,10], seasonal="additive", alpha=.2, beta=.2, gamma= .2))$mean)
      ) %>% pivot_longer(!date, names_to = "job_title", values_to = "projection") %>% 
        mutate(projection = ifelse(projection< 0, 0, projection)) %>%
        group_by(job_title) %>%
        summarize(projections = round(sum(projection)))
      
      
      hw_mul_projections <-  data.frame(
        date = ym(format(seq(start_date_forecast, end_date_forecast, by="month"), "%Y%m")),
        baac = as.numeric(forecast(HoltWinters(input_data_ts[,1]+0.01, seasonal="multiplicative", alpha=.2, beta=.2, gamma= .2))$mean),
        csa = as.numeric(forecast(HoltWinters(input_data_ts[,2]+0.01, seasonal="multiplicative", alpha=.2, beta=.2, gamma= .2))$mean),
        cuss = as.numeric(forecast(HoltWinters(input_data_ts[,3]+0.01, seasonal="multiplicative", alpha=.2, beta=.2, gamma= .2))$mean),
        ie = as.numeric(forecast(HoltWinters(input_data_ts[,4]+0.01, seasonal="multiplicative", alpha=.2, beta=.2, gamma= .2))$mean),
        mrams = as.numeric(forecast(HoltWinters(input_data_ts[,5]+0.01, seasonal="multiplicative", alpha=.2, beta=.2, gamma= .2))$mean),
        me = as.numeric(forecast(HoltWinters(input_data_ts[,6]+0.01, seasonal="multiplicative", alpha=.2, beta=.2, gamma= .2))$mean),
        ncsa = as.numeric(forecast(HoltWinters(input_data_ts[,7]+0.01, seasonal="multiplicative", alpha=.2, beta=.2, gamma= .2))$mean),
        ric = as.numeric(forecast(HoltWinters(input_data_ts[,8]+0.01, seasonal="multiplicative", alpha=.2, beta=.2, gamma= .2))$mean),
        sd = as.numeric(forecast(HoltWinters(input_data_ts[,9]+0.01, seasonal="multiplicative", alpha=.2, beta=.2, gamma= .2))$mean),
        tell = as.numeric(forecast(HoltWinters(input_data_ts[,10]+0.01, seasonal="multiplicative", alpha=.2, beta=.2, gamma= .2))$mean)
      ) %>% pivot_longer(!date, names_to = "job_title", values_to = "projection") %>% 
        mutate(projection = ifelse(projection < 0, 0, projection)) %>%
        group_by(job_title) %>%
        summarize(projections = round(sum(projection)))
      
      
      stpro <- stprojections %>% filter( areaname == input$state,
                                         code %in% c("13-1161", "43-3031", "43-3071", "15-1211", "15-1232", 
                                                     "15-1244", "15-1252", "43-4171", "17-2112", "17-2141")) %>%
        mutate(
          name = case_when(
            name == "Market Research Analysts and Marketing Specialists" ~ "mrams",
            name == "Computer Systems Analysts (SOC 2018)" ~ "csa",
            name == "Computer User Support Specialists (SOC 2018)" ~ "cuss",
            name == "Network and Computer Systems Administrators (SOC 2018)" ~ "ncsa",
            name == "Software Developers" ~ "sd",
            name == "Industrial Engineers" ~ "ie",
            name == "Mechanical Engineers" ~ "me",
            name == "Bookkeeping, Accounting, and Auditing Clerks" ~ "baac",
            name == "Tellers" ~ "tell",
            name == "Receptionists and Information Clerks" ~ "ric",
            TRUE ~ "other"
          ),
          openings = avgannualopenings * 2
        ) %>% summarise( job_title = name, postings = openings)
      
      current<- current_postings %>% filter(state == input$state & (job_title %in% stpro$job_title))
      
      arima_projections %>%
        mutate(model="arima") %>%
        add_row(sarima_projections %>% mutate(model="sarima")) %>%
        add_row(hw_add_projections %>% mutate(model="Holt-Winters Additive")) %>%
        add_row(hw_mul_projections %>% mutate(model = "Holt-Winters Multiplicative")) %>%
        left_join(current,by = "job_title") %>%
        mutate( updated_projections = as.numeric(projections) + as.numeric(postings))%>% 
        select(job_title, model, updated_projections)%>%
        pivot_wider(names_from = model, values_from = updated_projections) %>%
        left_join(stprojections, by = c("job_title" = "name")) %>%
        select("job_title","arima","sarima","Holt-Winters Additive","Holt-Winters Multiplicative")%>%
        pivot_longer(!job_title, names_to = "model", values_to = "projections") %>%
        ggplot(aes(x = job_title, y = projections, fill=model)) +
        geom_bar(stat="identity", position = position_dodge()) +
        labs(
          title = "Projected Postings (NLx) vs. Projected Openings (State Employment Projections)",
          subtitle = paste0(ifelse(input$state == "MO","Missouri","Wisconsin"), 
                            " ", "Most Impacted Occupations")
        ) +
        theme( plot.title = element_text(color="#0199F8", size=16, hjust = 0.5,face="bold.italic"),
               plot.subtitle = element_text(color="#0199F8", size=12, hjust = 0.5,face="bold.italic"))
      
      
      
    }else{
      
      
      compare_model<- lowai_projection %>% filter(state ==input$state)
      compare_model$year<- year(compare_model$date_compiled)
      compare_model$month<- month(compare_model$date_compiled)
      
      input_data_wide <- compare_model %>%
        mutate(date = as.yearmon(paste(year, month), "%Y %m"))%>%
        group_by(year,month, job_title)%>% summarise(Postings = n())%>%
        arrange(year, month)%>%
        pivot_wider(names_from = job_title, values_from = Postings) %>%
        arrange(year, month)%>% mutate(across(everything(), ~ replace_na(.x, 0)))
      

      # convert to time series
      input_data_ts <- ts(data = input_data_wide[, 3:12], 
                          start = c(2021, 4), frequency = 12)
      
      
      
      arima_projections <-  data.frame(
        # generate each month within start and end dates of the forecast
        date = ym(format(seq(start_date_forecast, end_date_forecast, by="month"), "%Y%m")),
        carp = as.numeric(forecast(auto.arima(input_data_ts[,1], stepwise=FALSE, approximation=FALSE, seasonal=FALSE), h=24)$mean),
        cl = as.numeric(forecast(auto.arima(input_data_ts[,2], stepwise=FALSE, approximation=FALSE, seasonal=FALSE), h=24)$mean),
        cr = as.numeric(forecast(auto.arima(input_data_ts[,3], stepwise=FALSE, approximation=FALSE, seasonal=FALSE), h=24)$mean),
        ffcw = as.numeric(forecast(auto.arima(input_data_ts[,4], stepwise=FALSE, approximation=FALSE, seasonal=FALSE), h=24)$mean),
        fpw = as.numeric(forecast(auto.arima(input_data_ts[,5], stepwise=FALSE, approximation=FALSE, seasonal=FALSE), h=24)$mean),
        jchc = as.numeric(forecast(auto.arima(input_data_ts[,6], stepwise=FALSE, approximation=FALSE, seasonal=FALSE), h=24)$mean),
        lgw = as.numeric(forecast(auto.arima(input_data_ts[,7], stepwise=FALSE, approximation=FALSE, seasonal=FALSE), h=24)$mean),
        mhc = as.numeric(forecast(auto.arima(input_data_ts[,8], stepwise=FALSE, approximation=FALSE, seasonal=FALSE), h=24)$mean),
        ww = as.numeric(forecast(auto.arima(input_data_ts[,9], stepwise=FALSE, approximation=FALSE, seasonal=FALSE), h=24)$mean),
        wcsb = as.numeric(forecast(auto.arima(input_data_ts[,10], stepwise=FALSE, approximation=FALSE, seasonal=FALSE), h=24)$mean)
      )%>%pivot_longer(!date, names_to = "job_title", values_to = "projection") %>%
        group_by(job_title) %>%
        summarize(projections = round(sum(projection)))
      
      
      
      # Bookkeeping, Accounting, and Auditing Clerks (baac) commented out because of non-stationary error
      sarima_projections <- data.frame(
        date = ym(format(seq(start_date_forecast, end_date_forecast, by="month"), "%Y%m")),
        carp = as.numeric(forecast(arima(input_data_ts[,1], order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),h = 24)$mean),
        cl = as.numeric(forecast(arima(input_data_ts[,2], order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),h = 24)$mean),
        cr = as.numeric(forecast(arima(input_data_ts[,3], order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),h = 24)$mean),
        ffcw = as.numeric(forecast(arima(input_data_ts[,4], order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),h = 24)$mean),
        fpw = as.numeric(forecast(arima(input_data_ts[,5], order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),h = 24)$mean),
        jchc = as.numeric(forecast(arima(input_data_ts[,6], order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),h = 24)$mean),
        lgw = as.numeric(forecast(arima(input_data_ts[,7], order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),h = 24)$mean),
        mhc = as.numeric(forecast(arima(input_data_ts[,8], order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),h = 24)$mean),
        ww = as.numeric(forecast(arima(input_data_ts[,9], order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),h = 24)$mean),
        wcsb = as.numeric(forecast(arima(input_data_ts[,10], order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),h = 24)$mean)
      ) %>% pivot_longer(!date, names_to = "job_title", values_to = "projection") %>% 
        mutate(projection = ifelse(projection < 0, 0, projection)) %>%
        group_by(job_title) %>%
        summarize(projections = round(sum(projection)))
      
    
      hw_add_projections <-  data.frame(
        date = ym(format(seq(start_date_forecast, end_date_forecast, by="month"), "%Y%m")),
        carp = as.numeric(forecast(HoltWinters(input_data_ts[,1], seasonal="additive", alpha=.2, beta=.2, gamma= .2))$mean),
        cl = as.numeric(forecast(HoltWinters(input_data_ts[,2], seasonal="additive", alpha=.2, beta=.2, gamma= .2))$mean),
        cr = as.numeric(forecast(HoltWinters(input_data_ts[,3], seasonal="additive", alpha=.2, beta=.2, gamma= .2))$mean),
        ffcw = as.numeric(forecast(HoltWinters(input_data_ts[,4], seasonal="additive", alpha=.2, beta=.2, gamma= .2))$mean),
        fpw = as.numeric(forecast(HoltWinters(input_data_ts[,5], seasonal="additive", alpha=.2, beta=.2, gamma= .2))$mean),
        jchc = as.numeric(forecast(HoltWinters(input_data_ts[,6], seasonal="additive", alpha=.2, beta=.2, gamma= .2))$mean),
        lgw = as.numeric(forecast(HoltWinters(input_data_ts[,7], seasonal="additive", alpha=.2, beta=.2, gamma= .2))$mean),
        mhc = as.numeric(forecast(HoltWinters(input_data_ts[,8], seasonal="additive", alpha=.2, beta=.2, gamma= .2))$mean),
        ww = as.numeric(forecast(HoltWinters(input_data_ts[,9], seasonal="additive", alpha=.2, beta=.2, gamma= .2))$mean),
        wcsb = as.numeric(forecast(HoltWinters(input_data_ts[,10], seasonal="additive", alpha=.2, beta=.2, gamma= .2))$mean)
      ) %>% pivot_longer(!date, names_to = "job_title", values_to = "projection") %>% 
        mutate(projection = ifelse(projection< 0, 0, projection)) %>%
        group_by(job_title) %>%
        summarize(projections = round(sum(projection)))
      
     
      hw_mul_projections <-  data.frame(
        date = ym(format(seq(start_date_forecast, end_date_forecast, by="month"), "%Y%m")),
        carp = as.numeric(forecast(HoltWinters(input_data_ts[,1]+0.01, seasonal="multiplicative", alpha=.2, beta=.2, gamma= .2))$mean),
        cl = as.numeric(forecast(HoltWinters(input_data_ts[,2]+0.01, seasonal="multiplicative", alpha=.2, beta=.2, gamma= .2))$mean),
        cr = as.numeric(forecast(HoltWinters(input_data_ts[,3]+0.01, seasonal="multiplicative", alpha=.2, beta=.2, gamma= .2))$mean),
        ffcw = as.numeric(forecast(HoltWinters(input_data_ts[,4]+0.01, seasonal="multiplicative", alpha=.2, beta=.2, gamma= .2))$mean),
        fpw = as.numeric(forecast(HoltWinters(input_data_ts[,5]+0.01, seasonal="multiplicative", alpha=.2, beta=.2, gamma= .2))$mean),
        jchc = as.numeric(forecast(HoltWinters(input_data_ts[,6]+0.01, seasonal="multiplicative", alpha=.2, beta=.2, gamma= .2))$mean),
        lgw = as.numeric(forecast(HoltWinters(input_data_ts[,7]+0.01, seasonal="multiplicative", alpha=.2, beta=.2, gamma= .2))$mean),
        mhc = as.numeric(forecast(HoltWinters(input_data_ts[,8]+0.01, seasonal="multiplicative", alpha=.2, beta=.2, gamma= .2))$mean),
        ww = as.numeric(forecast(HoltWinters(input_data_ts[,9]+0.01, seasonal="multiplicative", alpha=.2, beta=.2, gamma= .2))$mean),
        wcsb = as.numeric(forecast(HoltWinters(input_data_ts[,10]+0.01, seasonal="multiplicative", alpha=.2, beta=.2, gamma= .2))$mean)
      ) %>% pivot_longer(!date, names_to = "job_title", values_to = "projection") %>% 
        mutate(projection = ifelse(projection < 0, 0, projection)) %>%
        group_by(job_title) %>%
        summarize(projections = round(sum(projection)))
     
      
      stpro <- stprojections %>% filter( areaname == input$state,
          code %in% c("37-2012","37-2011","37-3011","35-2021","35-2014",
                      "35-3023","35-3031","47-2061","51-4121","47-2031")) %>%
        mutate(
          name = case_when(
            name == "Carpenters" ~ "carp",
            name == "Construction Laborers" ~ "cl",
            name == "Cooks, Restaurant" ~ "cr",
            name == "Fast Food and Counter Workers" ~ "ffcw",
            name == "Food Preparation Workers" ~ "fpw",
            name == "Janitors and Cleaners, Except Maids and Housekeeping Cleaners" ~ "jchc",
            name == "Waiters and Waitresses" ~ "ww",
            name == "Landscaping and Groundskeeping Workers" ~ "lgw",
            name == "Maids and Housekeeping Cleaners" ~ "mhc",
            name == "Welders, Cutters, Solderers, and Brazers" ~ "wcsb",
            TRUE ~ "other"
          ),
          openings = avgannualopenings * 2
        ) %>% summarise( job_title = name, postings = openings)
      
        current<- current_postings %>% filter(state == input$state & (job_title %in% stpro$job_title))
       
         arima_projections %>%
        mutate(model="arima") %>%
        add_row(sarima_projections %>% mutate(model="sarima")) %>%
        add_row(hw_add_projections %>% mutate(model="Holt-Winters Additive")) %>%
        add_row(hw_mul_projections %>% mutate(model = "Holt-Winters Multiplicative")) %>%
        left_join(current,by = "job_title") %>%
        mutate( updated_projections = as.numeric(projections) + as.numeric(postings))%>% 
        select(job_title, model, updated_projections)%>%
        pivot_wider(names_from = model, values_from = updated_projections) %>%
        left_join(stprojections, by = c("job_title" = "name")) %>%
        select("job_title","arima","sarima","Holt-Winters Additive","Holt-Winters Multiplicative")%>%
        pivot_longer(!job_title, names_to = "model", values_to = "projections") %>%
        ggplot(aes(x = job_title, y = projections, fill=model)) +
        geom_bar(stat="identity", position = position_dodge()) +
        labs(
          title = "Projected Postings (NLx) vs. Projected Openings (State Employment Projections)",
          subtitle = paste0(ifelse(input$state == "MO","Missouri","Wisconsin"), 
                            " ", "Least Impacted Occupations")
        ) +
           theme( plot.title = element_text(color="#0199F8", size=16, hjust = 0.5,face="bold.italic"),
                  plot.subtitle = element_text(color="#0199F8", size=12, hjust = 0.5,face="bold.italic"))
      
      
      
    }
    
    
  })
  
  #Graph 2     
    output$projection <- renderPlotly({
      

    if (input$category =="high_ai") {

        openings<- highai_projection %>% filter(state == input$state & job_title == input$highai &
           as.Date(date_compiled) <= as.Date("2023-03-31") )%>%
          select(job_id, date_compiled,state, job_title)

        openings$date_compiled <- as.Date(openings$date_compiled)
        openings$year<- year(openings$date_compiled)
        openings$month<- month(openings$date_compiled)
        highai_openings<- openings%>% group_by(year,month, job_title)%>% summarise(Postings = n())%>%
          arrange(year, month)

        highai_openings_ts <- highai_openings%>% select(-job_title)%>% ts(
          start = c(2021, 1),
          # number of observations per year
          frequency = 4
        )

        ###########
        arima<- forecast(object = auto.arima(y =highai_openings_ts[,3], seasonal=TRUE, stepwise = FALSE,
                                             stationary = FALSE, approximation = FALSE),h = 24)$mean
        ###############
        sarima<- forecast(arima(highai_openings_ts[,3], order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),h = 24)$mean
        ###############
        hw_add<- forecast(HoltWinters(highai_openings_ts[,3], seasonal="additive", alpha=.2, beta=.2, gamma= .2))$mean
        ###############
        hw_mul<- forecast(HoltWinters(highai_openings_ts[,3], seasonal="multiplicative", alpha=.2, beta=.1, gamma= .1))$mean

        models<- data.frame(
          Date = format(seq(start_date_forecast, end_date_forecast, by="month"), "%b %Y"),
          arima = as.numeric(arima),
          sarima = as.numeric(sarima),
          hw_add = as.numeric(hw_add),
          hw_mul= as.numeric(hw_mul)
        ) %>% arrange(Date)
          # pivot_longer( cols = c(arima, sarima, hw_add, hw_mul),
          #                   names_to = "Models",
          #                   values_to = "Projection")

        real<- highai_projection %>% filter(state == input$state & job_title == input$highai &
                  (as.Date(date_compiled) >= start_date_forecast & as.Date(date_compiled) <= end_date_forecast ))%>%
          mutate(year = year(as.Date(date_compiled)), month = month(as.Date(date_compiled)))%>%
          group_by(year, month, job_title)%>% summarise(Postings=n())%>% mutate(Date = as.yearmon(paste(year, month), "%Y %m"))%>%
          select(Date, Postings)

               # g<-  ggplot(models)+
        #         geom_line( aes(x = Date, y = Projection, color = Models, group = Models,linetype = Models))+
        #         scale_x_discrete(breaks=c("Apr 2023","Oct 2024"),
        #                           labels=c("Apr 2023" , "Oct 2024" ))+
        #         scale_colour_manual(values = c("arima" = "chocolate2", "sarima" = "firebrick2",
        #                                        "hw_add" = "mediumslateblue", "hw_mul" = "steelblue"))+
        #         scale_linetype_manual(values = c("arima" = 1, "sarima" = 2,"hw_add" = 3, "hw_mul" = 4))+
        #         theme_bw()+
        #         theme(axis.title.y = element_blank())+
        #         ggtitle("Model Comparison")
        #
        #  ggplotly(g)
        plotly<- merge(models, real, by ="Date")
        
        fig<- plot_ly(plotly) %>%
          add_trace(x = ~Date, y = ~arima, type = 'scatter', mode = 'lines+markers',
                    name = 'arima',visible = "legendonly",width = 2) %>%
          add_trace(x = ~Date, y = ~sarima, type = 'scatter', mode = 'lines+markers',
                    name = 'sarima',visible = TRUE,width = 2) %>%
          add_trace(x = ~Date, y = ~hw_add, type = 'scatter', mode = 'lines+markers',
                    name = 'hw_add',visible = "legendonly",width = 2) %>%
          add_trace(x = ~Date, y = ~hw_mul, type = 'scatter', mode = 'lines+markers',
                    name = 'hw_mul',visible = "legendonly",width = 2) %>%
          add_trace(x= ~Date, y= ~Postings, type='scatter', mode='lines+markers', 
                    name = 'Historical Postings', visible = TRUE, width = 4,
                    line = list( width = 4, dash = 'dash'))%>%
          layout(title =list( text = paste0(input$highai," ","Time Series Model Projection"), 
                              font =list(size=16,color='#5875D5')),
                 xaxis = list(title = 'Date', 
                              ticktext =list ("Apr 2023","Oct 2024"), 
                              tickvals=list("Apr 2023","Oct 2024")),
                 yaxis = list(title = 'Projection'))
        
        fig
        
        

      } else{


        openings<- lowai_projection %>% filter(state == input$state & job_title == input$lowai &
                                                 as.Date(date_compiled) <= as.Date("2023-03-31") )%>%
          select(job_id, date_compiled,state, job_title)

        openings$date_compiled <- as.Date(openings$date_compiled)
        openings$year<- year(openings$date_compiled)
        openings$month<- month(openings$date_compiled)
        lowai_openings<- openings%>% group_by(year,month, job_title)%>% summarise(Postings = n())%>%
          arrange(year, month)

        lowai_openings_ts <- lowai_openings%>% select(-job_title)%>% ts(
          start = c(2021, 1),
          # number of observations per year
          frequency = 4)

        ###########
        arima<- forecast(object = auto.arima(y =lowai_openings_ts[,3], seasonal=FALSE, stepwise = FALSE,
                                             stationary = FALSE, approximation = FALSE), h = 24)$mean
        ###############
        sarima<- forecast(arima(lowai_openings_ts[,3], order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),h = 24)$mean
        ###############
        hw_add<- forecast(HoltWinters(lowai_openings_ts[,3], seasonal="additive", alpha=.2, beta=.2, gamma= .2))$mean
        ###############
        hw_mul<- forecast(HoltWinters(lowai_openings_ts[,3], seasonal="multiplicative", alpha=.2, beta=.1, gamma= .1))$mean

        models<- data.frame(
          Date = format(seq(start_date_forecast, end_date_forecast, by="month"), "%b %Y"),
          arima = as.numeric(arima),
          sarima = as.numeric(sarima),
          hw_add = as.numeric(hw_add),
          hw_mul= as.numeric(hw_mul)
        ) %>% arrange(Date)
        
          # pivot_longer( cols = c(arima, sarima, hw_add, hw_mul),
          #                   names_to = "Models",
          #                   values_to = "Projection")

        real<- lowai_projection %>% filter(state == input$state & job_title == input$lowai &
                                        (as.Date(date_compiled)>=as.Date("2023-04-01") & as.Date(date_compiled) <= as.Date("2025-03-31")))%>%
          mutate(year = year(as.Date(date_compiled)), month = month(as.Date(date_compiled)))%>%
          group_by(year, month, job_title)%>% summarise(Postings=n())%>% mutate(Date = as.yearmon(paste(year, month), "%Y %m"))%>%
          select(Date, Postings) %>% arrange(Date)
        #
        # g<-  ggplot(models)+
        #   geom_line( aes(x = Date, y = Projection, color = Models, group = Models,linetype = Models))+
        #   scale_x_discrete(breaks=c("Apr 2023","Oct 2024"),
        #                    labels=c("Apr 2023" , "Oct 2024" ))+
        #   scale_colour_manual(values = c("arima" = "chocolate2", "sarima" = "firebrick2",
        #                                  "hw_add" = "mediumslateblue", "hw_mul" = "steelblue"))+
        #   scale_linetype_manual(values = c("arima" = 1, "sarima" = 2,"hw_add" = 3, "hw_mul" = 4))+
        #   theme_bw()+
        #   theme(axis.title.y = element_blank())+
        #   ggtitle("Model Comparison")
        #
        # ggplotly(g)
        plotly<- merge(models, real, by ="Date")

        fig<- plot_ly(plotly) %>%
               add_trace(x = ~Date, y = ~arima, type = 'scatter', mode = 'lines+markers',
                                         name = 'arima',visible = "legendonly",width = 2) %>%
               add_trace(x = ~Date, y = ~sarima, type = 'scatter', mode = 'lines+markers',
                                         name = 'sarima',visible = TRUE,width = 2) %>%
               add_trace(x = ~Date, y = ~hw_add, type = 'scatter', mode = 'lines+markers',
                                         name = 'hw_add',visible = "legendonly",width = 2) %>%
               add_trace(x = ~Date, y = ~hw_mul, type = 'scatter', mode = 'lines+markers',
                                         name = 'hw_mul',visible = "legendonly",width = 2) %>%
              add_trace(x= ~Date, y= ~Postings, type='scatter', mode='lines+markers', 
                     name = 'Historical Postings', visible = TRUE, width = 4,
                     line = list( width = 4, dash = 'dash'))%>%
               layout(title =list( text = paste0(input$lowai," ","Time Series Model Projection"), 
                      font =list(size=16,color='#5875D5')),
                      xaxis = list(title = 'Date', 
                                   ticktext =list ("Apr 2023","Oct 2024"), 
                                   tickvals=list("Apr 2023","Oct 2024")),
                      yaxis = list(title = 'Projection'))
        
        fig


      }



    })

  

}

# Run the application 
shinyApp(ui = ui, server = server)

 
