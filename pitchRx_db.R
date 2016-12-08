library(pitchRx)
library(DBI)
library(RPostgreSQL)

pg <- dbDriver("PostgreSQL")

db <- dbConnect(pg, 
                user = .rs.askForPassword("Enter username:"), 
                password = .rs.askForPassword("Enter password:"),
                host = 'localhost', 
                port = 5432,
                dbname = .rs.askForPassword("Enter database:"))

update_db(db, end = Sys.Date() - 1)