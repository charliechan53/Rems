#' @export
run <- function(qry, ...) UseMethod("run", qry)

#' @export
run.FltQuery <-
  function(qry, n_row = 25000, max_concurrent = 1)
  {
    if (is.null(qry$queryset$top)) {
      Nout <- NULL
    } else {
      Nout <- qry$queryset$top
    }
    if ((!is.null(Nout)) && (Nout <= 25000)) {
      return(simple_run(qry, output = "dataframe"))
    }
    return(async_run(qry, n_row = n_row, max_concurrent = max_concurrent))
  }

#' @export
simple_run <- function(qry, ...) UseMethod("simple_run", qry)

#' @export
simple_run.FltQuery <-
  function(qry, output = c("dataframe", "raw"))
  {

    output <- match.arg(output)
    cat("Sending a regular query to EMS ...")
    r <- request(qry$connection, rtype = "POST",
                 uri_keys = c('database', 'query'),
                 uri_args = c(qry$ems_id, qry$flight$db_id),
                 jsondata = qry$queryset)
    cat("Done.\n")
    if ( output == "raw" ) {
      return(httr::content(r))
    }
    else if ( output == "dataframe" ) {
      return(to_dataframe(qry, parse_json(r, simplify = TRUE)))
    }
  }

#' @export
async_run <- function(qry, ...) UseMethod("async_run", qry)

# max_concurrent > 1 reads that many pages of the async query at once. Leave it at 1
# unless your EMS server is known to accept concurrent reads of the same query.
#' @export
async_run.FltQuery <-
  function(qry, n_row = 25000, max_concurrent = 1)
  {
    open_async_query <- function() {
      cat('Sending and opening an async-query to EMS ...\n')
      r <- request(qry$connection, rtype = "POST",
                   uri_keys = c('database', 'open_asyncq'),
                   uri_args = c(qry$ems_id, qry$flight$db_id),
                   jsondata = qry$queryset)
      res <- parse_json(r)
      if (is.null(res$id)) {
        print(httr::headers(r))
        print(res)
        stop("Opening Async query did not return the query Id.")
      }
      cat('Done.\n')
      res
    }

    page_request <- function(ctr) {
      list(rtype    = "GET",
           uri_keys = c('database', 'get_asyncq'),
           uri_args = c(qry$ems_id,
                        qry$flight$db_id,
                        async_q$id,
                        formatC(n_row*(ctr-1), format="d"),
                        formatC(n_row*ctr-1, format="d")))
    }

    read_pages <- function(ctrs) {
      if (length(ctrs) == 1) {
        rq <- page_request(ctrs)
        r  <- request(qry$connection, rtype = rq$rtype, uri_keys = rq$uri_keys, uri_args = rq$uri_args)
        return(list(parse_json(r, simplify = TRUE)))
      }
      resps <- request_parallel(qry$connection, lapply(ctrs, page_request), max_concurrent)
      lapply(resps, function(x) {
        if (is.na(x$status)) stop(x$text)
        parse_json(x$text, simplify = TRUE)
      })
    }

    # Open async-query
    async_q <- open_async_query()

    ctr   <- 1
    pages <- list()
    n_received <- 0
    tryCatch({
      repeat {
        ctrs <- ctr:(ctr + max_concurrent - 1)
        cat(sprintf("=== Async call: %s === \n", paste(unique(c(min(ctrs), max(ctrs))), collapse = "-")))
        # Mini batch async call. Sometimes the issued query ID gets expired.
        # In that case, try up to three times until getting a new query ID.
        for (i in 1:3) {
          resps <- read_pages(ctrs)
          if (all(vapply(resps, function(x) !is.null(x$rows), logical(1)))) break
          # Reopen the query if not returning data
          async_q <- open_async_query()
        }
        finished <- FALSE
        for (resp in resps) {
          nr <- NROW(resp$rows)
          if (nr > 0) {
            pages[[length(pages) + 1]] <- resp$rows
            n_received <- n_received + nr
          }
          if (nr < n_row) {
            finished <- TRUE
            break
          }
        }
        cat(sprintf("Received up to  %d rows.\n", n_received))
        if (finished) break
        ctr <- ctr + max_concurrent
      }
    }, error = function(e) {
      warning(sprintf("Async query stopped early (%s). The result only has the %d rows received so far.",
                      conditionMessage(e), n_received), call. = FALSE)
    })
    tryCatch({
      r <- request(qry$connection, rtype = "DELETE",
                   uri_keys = c('database', 'close_asyncq'),
                   uri_args = c(qry$ems_id, qry$flight$db_id, async_q$id))
      cat(sprintf("Async query connection (query ID: %s) deleted.\n", async_q$id))
    }, error = function(e) {
      cat(sprintf("Couldn't delete the async query (query ID: %s). Probably it was already expired.\n", async_q$id))
    })

    if (length(pages) == 0) {
      cat("Done.\n")
      return(data.frame(stringsAsFactors = F))
    }
    # Combine all pages once and convert types once, instead of per page.
    df <- to_dataframe(qry, list(header = async_q$header, rows = do.call(rbind, pages)))
    cat("Done.\n")
    df
  }
