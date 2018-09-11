send_query <- function(query, ...) {

  on.exit({
    if (exists("con")) {
      DBI::dbDisconnect(con)
    }
  })

  query <- get_query(query)
  query <- as.character(templates::tmpl(query, ...))
  suppressWarnings(DBI::dbGetQuery(con <- DB(), query))

}

get_query <- function(query) {
  if (grepl(";$", query)) query
  else paste(collapse = "\n", readLines(system.file(
    sprintf("sql/%s.sql", query),
    package = "my-package",
    mustWork = TRUE
  )))
}

DB <- function() {
  DBI::dbConnect(
    RMySQL::MySQL(),
    username = settings$username,
    password = settings$password,
    dbname = settings$dbname,
    host = settings$host,
    port = settings$port
  )
}

settings <- list2env(list(
  home_dir = "~/.wmiscs/",
  config = "~/.wmiscs/config.R"
))

.onLoad <- function(libname, pkgname) {
  try({
    source(settings$config, settings)
  })
  invisible(NULL)
}
