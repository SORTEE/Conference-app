library(shiny)
library(shinythemes)
library(shinyjs)
library(dplyr)
library(lubridate) # for the dates
library(DT)

# Deploy app

## Don't forget to set working directory to the folder where the app.R is
## Always the following lines as comments when running the app, otherwise the app won't run
## Run two lines of code below to deploy the app
# library(rsconnect)
# deployApp()

# Load data 

## The following lines upload the program data from a Google Sheets write it as a csv file on your computer
## After doing this, leave the lines as comments otherwise the app will take ages to run
## When it is the first time, run $gs4_auth()$ to authenticate with your Google account
# library(googlesheets4)
# library(lubridate)
# program <- read_sheet('https://docs.google.com/spreadsheets/d/1QfbxJR_vAe26vJuUXMUx7B7TSixt0Ea2xDCXTHKFV4k/edit?gid=510110456#gid=510110456',
#                      skip = 1) # skip the first line
# program[is.na(program)] <- "" # replace NA by ""
# program$`End Time`  <- round_date(program$`End Time` , unit ="minute") # bug with some times set at XX:49:59 instead of XX:50:00
# program$`Start Time` <- as.character(format(program$`Start Time`)) ## to solve the problem with write.csv and the entry "2025-10-15 00:00:00"
# program$`End Time` <- as.character(format(program$`End Time`))
# write.csv(program, file = "Program.csv", row.names = FALSE)

## Once you have the program data on your computer, you can just read it directly from your computer
## The program data must store the start and end times as UTC format as "10/15/2024  7:00:00 AM" for example
program <- read.csv("Program.csv")
dat <- program


# Get time zone

## The following lines extract the time zones of different parts of the world on your meeting date
meeting <- ymd_hms("2025-10-15 07:00:00", tz = "UTC") # add your meeting date here, use UTC
zones <- data.frame(time_zone = OlsonNames()) %>%
  mutate(local = force_tz(meeting, time_zone),
         diff = as.numeric(meeting - local)/(60*60), # difference in hours
         plusminus = ifelse(diff<0, '-', ifelse(diff==0,'', '+')),
         hours = floor(abs(diff)),
         minutes = round((abs(diff) - hours) * 60),
         utc = sprintf("%s%02d:%02d",plusminus, hours,minutes),
         time_zone_plus = paste(time_zone, ' (', utc, ')', sep='' )) %>%
  dplyr::select(-plusminus, -hours, -minutes)


# ui

ui <- fluidPage(
  useShinyjs(),
  # theme + logo + title
  theme = shinytheme("united"),
  tags$head( 
    tags$style(HTML("
      a {   
        color: inherit;  <!-- adjust link colour -->
      }
      
    
      a:hover {
        color: #0A5400; 
      }
      
      div.modal-footer a {
                text-decoration: underline !important;

      }
      
      #big-heading { 
        color: #0A5400; <!--SORTEE 2025 conference heading -->
      }
      .modal-title { 
        color: #0A5400;  <!-- title of the popup cell -->
        padding: 5px;
        background-image: url('background.jpg');
        background-size: 100% auto;

      }
      
      .background { 
        background-image: url('background.jpg');
        background-size: 100% auto;
        display: flex;
        align-items: center;
        justify-content: start;
        flex-wrap: wrap;
        margin: 0 15px 10px 15px; <!-- top,left, bottom and right margin-->
        padding: 0px;
      }
      table.dataTable tbody td.selected {
        background-color: #6c9866 !important; <!--change colour of selected cell -->
      }
      .container-fluid {
      padding: 0px; <!-- remove left and right margins -->
      }
      .container {
      padding: 10px 0 10px 10px;
      margin: 0px;
      max-width: 680px;
      }

      
      @media (min-width: 768px) {
    .container {
        width: auto;  <-- I had to remove this automatic setting -->
    }}
      
    "))
  ),
  div(class = "background",
      div(
        tags$img(src = "circle_green.png",  height="auto", width="150px", style = "margin: 10px;")), # logo stored in the www folder
      div(class = "container",
          h1(id="big-heading","SORTEE 2025 conference program "),
          p("Welcome to the program app for the 2025 SORTEE Conference!", br()),
          p(strong("Click on any session")," on the table to access its information."),
          p(strong("Type and select your country below"), "to have the program in your local time.", br(),
            "You can then export the program in csv, excel or pdf format."),
          selectizeInput(inputId = "UTC",
                         label = NULL,
                         choices = c("",zones$time_zone_plus),
                         options = list(placeholder = 'Type or select your country name')),
          p("If anything goes wrong, refer to the conference information pack, section 'Where to go for help'.", br(),
            "The available technical assistants are listed on the last column in the below table. Contact them via the Slack channel #conf2025-technicalassistance, or by sending them a direct message on Slack or Zoom.")
      )),
  
  #DT table output which shows the schedule
  column(DT::dataTableOutput("table"),style = "margin-bottom: 15px;", width=12))

# server

server <- function(input, output, session)
{
  
  # DT table output
  output$table <- DT::renderDataTable(DT::datatable({
    
    # Convert the start time and end time depending on the time zone selected
    ## extract offset from the input
    if(input$UTC==""){ 
      offset <- "+00:00" # if no country is selected then use UTC
    }else{
      offset <- gsub("^.*\\((.*)\\)$", '\\1' , input$UTC)
    }
    
    ## change to numeric
    (UTC_hour <- as.numeric(substr(offset,2,3)))
    (UTC_minutes <- as.numeric(substr(offset,5,6)))
    
    if(substr(offset,1,1)=="+"){
      offset_minutes <- UTC_hour*60 + UTC_minutes
    } else {
      offset_minutes <- -UTC_hour*60 - UTC_minutes
    }
    
    ## adjust the date
    dat$convert_start <- ymd_hms(dat$Start.Time) + lubridate::minutes(offset_minutes)
    dat$convert_end <- ymd_hms(dat$End.Time) + lubridate::minutes(offset_minutes)
    
    # Create the columns of the table
    dat$Day <- wday(dat$convert_start, label = TRUE, week_start = 1,# week_start = 1 means Monday is the first week day
                    locale = "C") # locale = "C" ensures date are displayed in English
    dat$`Date` <- paste0("Oct ", day(dat$convert_start)) # Adjust the month 
    dat$`Start Time` <- format.Date(dat$convert_start, "%H:%M")
    dat$`End Time` <- format.Date(dat$convert_end, "%H:%M")
    
    # Adjust column names
    names(dat)[which(names(dat)=="Technical.assistance")] <- "Technical assistance"
    
    # Extract the final columns to be displayed in the program table on the web app
    dat[,c("Day",
           "Date",
           "Start Time",
           "End Time",
           "Program",
           "Technical assistance")]
    
  },
  #DT table objects
  rownames = FALSE,
  extensions = 'Buttons', 
  options = list(scrollX=TRUE, lengthMenu = 97,
                 rowGroup = list(dataSrc = 1), extensions = 'RowGroup',
                 paging = FALSE, searching = TRUE,
                 fixedColumns = TRUE, autoWidth = FALSE,
                 ordering = TRUE, dom = 'Bftsp',
                 columnDefs = list(list(className = 'dt-center', targets = 0:5)), # adjust targets depending on the number of columns. In this case 6 columns (0:5)
                 buttons = list('csv', 'excel',list(extend = 'pdf', # options to export the program table
                                                    pageSize = 'A4',
                                                    orientation = 'landscape'))),
  
  class = c('cell-border', "hover"),
  selection = list(mode="single", target="cell")
  
  ) %>% 
    # format size of table columns
    formatStyle(c("Date", "Day", "Start Time", "End Time","Technical assistance"), width='5px') %>%
    formatStyle(c("Program"), width='450px')   %>%
    formatStyle(c("Technical assistance"), width='150px')
  %>%
    formatStyle("Program",backgroundColor = styleRow(c(1,11,25,43,54), "#e0e0e0")) 
  %>% 
    formatStyle("Program",
                backgroundColor = styleRow(c(2,5:6,12:15,20:21,26:28,35:36,39:40,47:48,52:53), "#eaf9ea")) 
  %>% 
    formatStyle("Program",backgroundColor = styleRow(c(3:4,7:10,16:17,22:24,31:34,37:38,41:42,49:51), "#ccebce")) 
  %>% 
    formatStyle("Program",backgroundColor = styleRow(c(18:19,29:30), "#ebebeb")) 
  %>%
    # format specific start and end times in bold
    formatStyle("End Time",
                fontWeight = styleRow(c(1,53), "bold"))
  %>%
    formatStyle("Start Time",
                fontWeight = styleRow(c(2,54), "bold"))
  )
  
  # observe when a cell is clicked.
  shiny::observeEvent(input$table_cell_clicked, {
    req(length(input$table_cell_clicked) > 0)
    cell.data <- (input$table_cell_clicked)
    
    # how input$table_cell_clicked looks like if you need to edit it
    #str(input$table_cell_clicked)
    #List of 3
    #$ row  : int 2 # row number of clicked cell
    #$ col  : int 4 # column number of clicked cell
    #$ value: chr "Plenary 1: Chuan-Peng Hu - Promoting Open Science via grassroots network in a developing country"
    
    # create a popup for an event based on where the cell was clicked, what row etc.
    if(cell.data[["col"]] == 4 & cell.data[["value"]]!=""){ # col where the program is. Only create a popup for cells in the program column
      if (!grepl("https://osf.io", program$OSF.link[cell.data[["row"]]])) { # there is a special pop-up below for sessions with OSF link
      row <- cell.data[["row"]]
      showModal(modalDialog(
        title = div(class = "background",# title of the popup
                    style = "flex-wrap: nowrap; margin: 0px;",
                    tags$img(src = "circle_green.png", height = "50px", style = "margin: 3px;"),
                    dat$Program[row]
        ), 
        # content of the popup
        HTML(paste("<u>Facilitators</u><br>", program$Facilitators[row]),
             "<br>",
             "<br>",
             paste("<u>Topic</u><br>", program$Topic[row]),
             "<br>",
             "<br>",
             paste("<u>Outline</u><br>", program$Outline[row]),
             "<br>",
             "<br>",
             paste("<u>Audience</u><br>", program$Audience[row]),
             "<br>",
             "<br>"),
        footer = div(
          style = "text-align: left;",
          HTML(paste("Slack channel name =", program$Slack.channel.name[row])),
          tags$br(),
          HTML(sprintf('Slack channel link = <a href="%s">%s</a>',
                       program$Slack.channel.link[row],
                       program$Slack.channel.link[row]))
        ),
        easyClose = TRUE,
        size = "l"
      ))
      } else
      {
        row <- cell.data[["row"]]
        showModal(modalDialog(
          title = div(class = "background",# title of the popup
                      style = "flex-wrap: nowrap; margin: 0px;",
                      tags$img(src = "circle_green.png", height = "50px", style = "margin: 3px;"),
                      dat$Program[row]
          ), 
          # content of the popup
          HTML(paste("<u>Facilitators</u><br>", program$Facilitators[row]),
               "<br>",
               "<br>",
               paste("<u>Topic</u><br>", program$Topic[row]),
               "<br>",
               "<br>",
               paste("<u>Outline</u><br>", program$Outline[row]),
               "<br>",
               "<br>",
               paste("<u>Audience</u><br>", program$Audience[row]),
               "<br>",
               "<br>"),
          footer = div(
            style = "text-align: left;",
            HTML(sprintf('OSF document repository = <a href="%s">%s</a>',
                         program$OSF.link[row],
                         program$OSF.link[row])),
            tags$br(),
            HTML(paste("Slack channel name =", program$Slack.channel.name[row])),
            tags$br(),
            HTML(sprintf('Slack channel link = <a href="%s">%s</a>',
                         program$Slack.channel.link[row],
                         program$Slack.channel.link[row]))
          ),
          easyClose = TRUE,
          size = "l"
        ))
      }
    }
  }
  )
}


# Run the application 
shinyApp(ui = ui, server = server)
