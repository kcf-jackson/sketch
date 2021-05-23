library(plumber)

# 'app.R' is the location of the file containing your endpoint functions
r <- plumb("app.R")

# get port number from environment variable
port <- strtoi(Sys.getenv("PORT"))
r$run(port = port, host = "0.0.0.0")
