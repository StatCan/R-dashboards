# the body of the server is identical in the two versions.
# the server logic is defined in en/server_body.R
# and sourced here to avoid duplication.

server <- function(input, output, session) {
  source("../en/server_body.R", local=TRUE)
}
