# import potrebnih library-ia
library(shiny)
library(httr)
library(jsonlite)
library(shinyFiles)

# definiranje elemenata grafickog sucelja
ui <- fluidPage(
  titlePanel("Data Input Form"),
  
  # definiranje formi za ulazne vrijednosti
  sidebarLayout(
    sidebarPanel(
      textInput("airport_of_departure", "Airport of Departure"),
      dateInput("time_period_from", "Time Period (From)"),
      textInput("hour_from", "From hour"),
      dateInput("time_period_to", "Time Period (To)"),
      textInput("hour_to", "To hour"),
      textInput("input_dir", "Path to the existing directory"),
      textInput("output_filename", "Output CSV Filename"),
      
      # gumb za pokretanje dohvata i obrade podataka
      actionButton("download_button", "Download Observations")
    ),
    
    # definiranje poruka sa uputama i informacijama 
    mainPanel(
      verbatimTextOutput("airportInfo"),
      verbatimTextOutput("timeInfo"),
      verbatimTextOutput("folderPathInfo"),
      verbatimTextOutput("filenameInfo"),
      verbatimTextOutput("output_message")
    )
  )
)

# server logika
server <- function(input, output) {
  
  # render (prikaz) uputa i informacija
  
  output$airportInfo <- renderText({
    "Primjeri kodova zračnih luka:\n 
    Hamburg Fuhlsbuettel Airport - EDDH\n
    Lyon–Saint-Exupéry Airport - LFLL\n
    Paris-Orly Airport - LFPO\n
    Napoli International Airport - LIRN\n
    Venice Marco Polo Airport - LIPZ\n
    Zürich International Airport - LSZH"
  })
  
  output$folderPathInfo <- renderText({
    "Primjer formata zapisa puta do direktorija (windows): C:\\Users\\ddrokan\\Documents\\Diplomski\\Radio\\Projekt\\"
  })
  
  output$filenameInfo <- renderText({
    "Primjer formata zapisa naziva datoteke (mora sadrzavati sufix .csv): podaci.csv"
  })
  output$timeInfo <- renderText({
    "Maksimalni vremenski interval je 7 dana"
  })
  
  
  
  # event handler za događaj pritiska na Download dugme
  observeEvent(input$download_button, {
    
    
    # spremanje vrijednosti iz ulaznih formi u varijable
    airport_of_departure <- input$airport_of_departure
    time_period_from <- input$time_period_from
    time_period_to <- input$time_period_to
    hour_to <- input$hour_to
    hour_from <- input$hour_from
    folder_path <- input$input_dir
    output_filename <- input$output_filename
    
    
    print(folder_path)
    print(output_filename)
    
    # konstrukcija cijele putanje do nove datoteke u kojoj će biti spremljeni dohvaćeni podaci
    folder_path = paste(folder_path, output_filename, sep="")
    print(folder_path)

    
    # Konstrukcija pocetnog timestamp zapisa iz ulaznih vrijednsoti
    timestampFrom = paste(time_period_from, " ", hour_from, ":00:00", sep="")
    timestampFrom = as.POSIXct(timestampFrom)
    print(timestampFrom)
    timestampFrom = as.numeric(timestampFrom)
    print(timestampFrom)
    
    # Konstrukcija zavrsnog timestamp zapisa iz ulaznih vrijednsoti
    timestampTo = paste(time_period_to, " ", hour_to, ":00:00", sep="")
    timestampTo = as.POSIXct(timestampTo)
    print(timestampTo)
    timestampTo = as.numeric(timestampTo)
    print(timestampTo)
    
    # konstrukcija REST API poziva
    baseURL = "https://opensky-network.org/api/flights/departure?airport="
    URL = paste(baseURL, airport_of_departure, "&begin=", timestampFrom, "&", "end=", timestampTo,sep="")
    print(URL)
    
    # REST API poziv
    res = GET(URL)
    
    # ako je poziv bio uspjesan
    if (status_code(res) == 200){
      print("Success")
      
      # konstrukcija JSON podatka iz raw podataka
      data = fromJSON(rawToChar(res$content))
      
      # zapis podataka u csv datoteku
      write.csv(data, folder_path, row.names = FALSE)
      
      # Prikaz uspjesne poruke
      output$output_message <- renderText({
        "Obzervacije skinute i spremljene u CSV datoteku"
      })
      
    }
    # ako poziv nije bio uspjesan
    else {
      # Prikaz neuspjesne poruke
      output$output_message <- renderText({
        paste("Greska: Nije moguce dohvatiti podatke. Status code:", status_code(res))
      })
    }
    
    
    
    
  })
}

# Pokretanje cijele aplikacije
shinyApp(ui = ui, server = server)






