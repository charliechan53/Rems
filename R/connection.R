
# Credentials live here, keyed per connection, rather than inside the connection
# object, so they don't leak through print(), str(), saveRDS() or saved workspaces.
.credentials <- new.env(parent = emptyenv())

#' Create EMS Connection Object
#'
#' \code{connect} Creates a connection object to an EMS server for use in FltQuery objects.
#'
#' This function creates a connection object, including an authentication token.
#' These can be passed into a \code{fltquery} or \code{tsquery} function to start the process of retrieving data.
#' These functions take unmasked usernames and passwords - it is recommended that you either store your credentials
#' securely, or wrap this function in something like \code{rstudioapi::askForPassword()} to avoid disclosing
#' your credentials.
#'
#' The password is held in memory for the current R session only (so the token can be refreshed when it
#' expires); it is not stored in the returned object. A connection object restored from disk in a new
#' session therefore cannot reconnect - call \code{connect} again.
#'
#' TLS certificates are always verified. If you are behind a proxy that re-signs traffic with a corporate
#' certificate, pass that CA bundle via \code{cainfo} (or set the \code{CURL_CA_BUNDLE} environment variable).
#'
#' @param usr A string, FOQA username (like firstname.lastname)
#' @param pwd A string, FOQA password
#' @param proxies A list containing the system proxy information, with attributes "url", "port", "usr", "pwd"
#' @param server A string. Lets you choose an API server. Should be one of 'prod', 'cluster', 'stable', 'beta', 'nightly', or 'ctc'.
#' @param server_url An optional string. If this is passed, it will be used instead of the internal server definitions matched by the 'server' argument.
#' @param cainfo An optional path to a CA certificate bundle (PEM) used to verify the server certificate.
#' @param stall_timeout Seconds a request may go without receiving any data before it is aborted
#'   (and retried, for GET/DELETE). Without this a dropped connection can hang forever.
#' @return a Connection object to be used in FltQuery.
#'
#' @examples
#' # connect in a standard way, using no proxy and connecting to prod by default.
#' \dontrun{
#' con <- connect("joe.bloggs", "mypassword")
#' qry <- flt_query(conn = con, ems_name = "my-ems", data_file = "file.db")
#' }
#'# connect using the beta build, and a proxy
#'\dontrun{
#'con <- connect("joe.bloggs", "mypassword",
#'               proxies = list(url = "http://myproxy.com", port = 8080,
#'               usr = "joebloggs", pwd = "mypassword"),
#'               server = 'beta')
#'}
#'
#' @export
connect <- function(usr, pwd, proxies = NULL, server = c('prod', 'cluster', 'stable', 'beta', 'nightly', 'ctc'), server_url = NULL, cainfo = NULL, stall_timeout = 300)
  {
    server <- match.arg(server, c('prod', 'cluster', 'stable', 'beta', 'nightly', 'ctc'), several.ok = FALSE)
    sel_uri_root <- if (is.null(server_url)) uri_root[[server]] else server_url

    cred_key <- basename(tempfile("conn"))
    assign(cred_key, list(usr = usr, pwd = pwd, proxies = proxies), envir = .credentials)

    conn <- list(
      proxies   = proxies[setdiff(names(proxies), c("usr", "pwd"))],
      uri_root  = sel_uri_root,
      cainfo    = cainfo,
      stall_timeout = stall_timeout,
      cred_key  = cred_key,
      # The token sits in an environment so a refresh is seen by every copy of
      # the connection (each query object holds its own copy of this list).
      auth      = new.env(parent = emptyenv())
    )
    class(conn) <- "EmsConnection"
    authenticate(conn)
    conn
  }

authenticate <-
  function(conn)
  {
    cred <- get_credentials(conn)
    header <- c("Content-Type" = "application/x-www-form-urlencoded", "User-Agent" = user_agent)
    body <- list(grant_type = "password",
                 username   = cred$usr,
                 password   = cred$pwd)

    r <- httr::RETRY("POST", paste(conn$uri_root, uris$sys$auth, sep = ""),
                     conn_config(conn),
                     httr::add_headers(.headers = header),
                     body = body,
                     encode = "form",
                     terminate_on = setdiff(400:499, 429),
                     quiet = TRUE)
    res <- parse_json(r)
    if ( !is.null(res$message) ) {
      print(paste("Message:", res$message))
    }
    if ( httr::http_error(r) ) {
      stop(paste("Message:", res$error_description))
    }
    conn$auth$token      <- res$access_token
    conn$auth$token_type <- res$token_type
    invisible(conn)
  }

get_credentials <-
  function(conn)
  {
    if (is.null(conn$cred_key) || !exists(conn$cred_key, envir = .credentials, inherits = FALSE)) {
      stop("Credentials for this connection are not available in this R session. Please call connect() again.")
    }
    get(conn$cred_key, envir = .credentials, inherits = FALSE)
  }

# Proxy settings including the proxy password, which is only kept in the credential store.
conn_proxies <-
  function(conn)
  {
    if (!is.null(conn$cred_key) && exists(conn$cred_key, envir = .credentials, inherits = FALSE)) {
      return(get(conn$cred_key, envir = .credentials, inherits = FALSE)$proxies)
    }
    conn$proxies
  }

# httr config (timeouts, proxy, CA bundle) shared by every request on this connection.
conn_config <-
  function(conn)
  {
    # Abort when nothing arrives for stall_timeout seconds, rather than a fixed
    # total timeout, so long but healthy downloads are not cut off.
    stall <- if (is.null(conn$stall_timeout)) 300 else conn$stall_timeout
    cfg <- httr::config(connecttimeout = 60, low_speed_limit = 1, low_speed_time = stall)
    prx <- conn_proxies(conn)
    if (length(prx) > 0) {
      cfg <- c(cfg, httr::use_proxy(prx$url, port = prx$port, username = prx$usr, password = prx$pwd))
    }
    if (!is.null(conn$cainfo)) {
      cfg <- c(cfg, httr::config(cainfo = conn$cainfo))
    }
    cfg
  }

auth_headers <-
  function(conn)
  {
    c(Authorization = paste(conn$auth$token_type, conn$auth$token),
      'User-Agent'  = user_agent)
  }

#' @export
print.EmsConnection <-
  function(x, ...)
  {
    cat("<EMS connection>\n")
    cat("  Server: ", x$uri_root, "\n", sep = "")
    if (length(x$proxies) > 0) cat("  Proxy:  ", x$proxies$url, "\n", sep = "")
    cat("  Token:  ", if (is.null(x$auth$token)) "<none>" else "<hidden>", "\n", sep = "")
    invisible(x)
  }

#' Refresh Connection
#'
#' Refresh the connection object in case auth token expires.
#'
#' This function will re-use the connection credentials held for this R session to regenerate the authentication token.
#' Requests made through this package already do this automatically when the token expires.
#'
#' @param conn A connection object created by the \code{connect} function.
#' @return A refreshed connection object like the one returned by \code{connect}
#'
#' @examples
#' \dontrun{
#' con <- connect("joe.bloggs", "mypassword")
#' sys.Sleep(3600)
#' con <- reconnect(con)
#' }
#'
#' @export
reconnect <-
  function(conn)
  {
    authenticate(conn)
  }

build_uri <-
  function(conn, uri_keys, uri_args = NULL)
  {
    if (is.null(uri_keys)) {
      stop("uri_keys must be given.")
    }
    uri <- paste(conn$uri_root, uris[[uri_keys[1]]][[uri_keys[2]]], sep = "")
    if (!is.null(uri_args)) {
      # percent encode the args
      uri_args <- sapply(uri_args, function(x) if (is.na(suppressWarnings(as.numeric(x)))) URLencode(x, reserved = TRUE) else x)
      uri      <- do.call(sprintf, as.list(c(uri, uri_args)))
    }
    uri
  }

# Parse a JSON response the same way httr::content() does (lists of lists), or with
# simplify = TRUE turn arrays into vectors/matrices, which is much faster for row data.
parse_json <-
  function(r, simplify = FALSE)
  {
    txt <- if (inherits(r, "response")) httr::content(r, as = "text", encoding = "UTF-8") else r
    if (is.null(txt) || !nzchar(txt)) {
      return(NULL)
    }
    if (simplify) {
      jsonlite::fromJSON(txt, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = TRUE)
    } else {
      jsonlite::fromJSON(txt, simplifyVector = FALSE)
    }
  }

#' Arbitrary Request
#'
#' Perform an arbitrary GET, POST or DELETE function using a connection  object.
#'
#' For advanced users only, this function allows you to perform an arbitrary GET, POST or DELETE
#' request on the low-level API. Allows for full configurability of the request, including
#' setting the keys, arguments, headers, and JSON payload.
#'
#' GET and DELETE requests are retried on network errors, 429 and 5xx responses. If the
#' auth token has expired (401), it is refreshed and the request is sent once more.
#'
#' @param conn A connection object generated by \code{connect}
#' @param rtype A string indicating the kind of HTTP request to be performed - accepts GET, POST and DELETE
#' @param uri_keys A two-element string vector indicating which URI to use - see common.R
#' @param uri_args A list of arguments to be passed to the URI
#' @param headers A list of headers to be used by the GET/POST/DELETE request, per the \code{.headers} argument
#' @param body A string, indicating the body to be passed to the request. Do not pass this and jsondata simultaneously.
#' @param jsondata A string, indicating the JSON body to be passed. See above.
#'
#' @return Returns the raw GET/POST/DELETE output from the request.
#'
#' @export
request <-
  function(conn, rtype = c("GET","POST","DELETE"), uri_keys = NULL, uri_args = NULL,
           headers = NULL, body = NULL, jsondata = NULL
           )
  {
    rtype <- match.arg(rtype, c("GET","POST","DELETE"), several.ok = FALSE)
    uri   <- build_uri(conn, uri_keys, uri_args)

    # Default encoding is "application/x-www-form-urlencoded"
    encoding <- "form"
    if (!is.null(jsondata)) {
      body <- jsondata
      encoding <- "json"
    }

    send <- function() {
      hdrs <- if (is.null(headers)) c(auth_headers(conn), 'Accept-Encoding' = 'gzip') else headers
      if (rtype == "POST") {
        # POSTs are not retried automatically: some (e.g. database inserts) are not idempotent.
        httr::POST(uri, conn_config(conn), body = body, httr::add_headers(.headers = hdrs), encode = encoding)
      } else if (rtype == "GET") {
        httr::RETRY("GET", uri, conn_config(conn), query = body, httr::add_headers(.headers = hdrs),
                    encode = encoding, terminate_on = setdiff(400:499, 429), quiet = TRUE)
      } else {
        httr::RETRY("DELETE", uri, conn_config(conn), body = body, httr::add_headers(.headers = hdrs),
                    encode = encoding, terminate_on = setdiff(400:499, 429), quiet = TRUE)
      }
    }

    r <- send()
    if (httr::status_code(r) == 401 && is.null(headers)) {
      cat("Auth token expired. Reconnecting to EMS...\n")
      reconnect(conn)
      r <- send()
    }
    r
  }

# Send several requests concurrently. Each element of `reqs` is a list with the
# arguments of request(): rtype, uri_keys, uri_args, and optionally body (sent as a
# query string) or jsondata. Returns, in order, a list of list(status, text) where a
# network failure has status NA and the error message as text. Requests rejected
# with 401 are sent once more after refreshing the token.
request_parallel <-
  function(conn, reqs, max_concurrent = 4, on_done = NULL)
  {
    out     <- vector("list", length(reqs))
    pending <- seq_along(reqs)
    for (attempt in 1:2) {
      pool <- curl::new_pool(total_con = max_concurrent, host_con = max_concurrent)
      opts <- c(conn_config(conn)$options, list(accept_encoding = "gzip"))
      hdrs <- as.list(auth_headers(conn))
      for (i in pending) {
        local({
          idx <- i
          rq  <- reqs[[idx]]
          url <- build_uri(conn, rq$uri_keys, rq$uri_args)
          h   <- curl::new_handle()
          curl::handle_setopt(h, .list = opts)
          req_hdrs <- hdrs
          if (!is.null(rq$jsondata)) {
            # Same JSON encoding httr uses for encode = "json"
            payload <- Filter(function(x) length(x) > 0, rq$jsondata)
            curl::handle_setopt(h, customrequest = rq$rtype,
                                postfields = as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, digits = 22)))
            req_hdrs[["Content-Type"]] <- "application/json"
          } else {
            if (!is.null(rq$body)) url <- httr::modify_url(url, query = rq$body)
            if (!is.null(rq$rtype) && rq$rtype != "GET") curl::handle_setopt(h, customrequest = rq$rtype)
          }
          curl::handle_setheaders(h, .list = req_hdrs)
          curl::curl_fetch_multi(url, handle = h, pool = pool,
                                 done = function(res) {
                                   out[[idx]] <<- list(status = res$status_code,
                                                       text   = rawToChar(res$content))
                                   if (!is.null(on_done) && res$status_code != 401) on_done(idx)
                                 },
                                 fail = function(msg) {
                                   out[[idx]] <<- list(status = NA_integer_, text = msg)
                                 })
        })
      }
      curl::multi_run(pool = pool)

      pending <- pending[vapply(out[pending], function(x) identical(x$status, 401L), logical(1))]
      if (length(pending) == 0 || attempt == 2) break
      cat("Auth token expired. Reconnecting to EMS...\n")
      reconnect(conn)
    }
    out
  }
