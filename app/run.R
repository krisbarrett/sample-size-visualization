host <- '0.0.0.0'
port <- Sys.getenv("PORT")

options(shiny.port = as.integer(port), shiny.host = host)

shiny::runApp('.')
