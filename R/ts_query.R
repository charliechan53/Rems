#' Time Series Query
#'
#' Instantiate a time series query object
#'
#' This function creates a time series query object that can be used to query
#' time series (i.e. parameter level) data from EMS. If you need physical
#' parameter data, you will need to set a specific flight record to search
#' to return those parameters.
#'
#' @param conn A connect object created by the connect function.
#' @param ems_name A string, the name of the EMS instance being connected to.
#' @param data_file A string, the name of the db file to store metadata to.
#' @param flight_record_searched optional numeric flight record to search params
#'
#' @export
tseries_query <-
  function(conn, ems_name, data_file = NULL, flight_record_searched = NULL)
  {
    obj <- list()
    class(obj) <- 'TsQuery'

    # Instantiating other objects
    obj$connection <- conn
    obj$ems        <- ems(conn)
    obj$ems_id     <- get_id(obj$ems, ems_name)
    obj$analytic   <- analytic(conn, obj$ems_id, data_file)
    obj$fr         <- flight_record_searched

    # object data
    obj$queryset <- list()
    obj$columns  <- list()
    obj <- reset(obj)

    return(obj)
  }

#' @export
reset.TsQuery <-
  function(qry)
  {
    qry$queryset <- list(select = list())
    qry$columns <- list()
    qry
  }

#' @export
#' @importFrom dplyr bind_rows
select.TsQuery <-
  function(qry, ...)
  {
    keywords <- list(...)

    new_params <- data.frame()
    for ( kw in keywords ) {
      # Get the param from the param table
      prm <- get_param(qry$analytic, kw)
      if ( prm$id=="" ) {

        # If param's not found, search from EMS API
        res <- search_param(qry$analytic, kw, flight_record = qry$fr)

        #Drop both the 'Path' and 'displayPath' as they are list elements.
        #I don't believe they get used down-stream so easier to drop.
        #This has to happen before bind_rows, which otherwise expands each
        #param into one row per path element (or zero rows for an empty path).
        res_flat <- lapply(res, function(x) x[setdiff(names(x), c("path", "displayPath"))])
        df <- dplyr::bind_rows( res_flat )

        # Only keep params the local table doesn't have yet
        df <- df[!duplicated(df$id) & !(df$id %in% qry$analytic$param_table$id), , drop = FALSE]
        if ( nrow(qry$analytic$param_table) == 0 ) {
          # The empty table has untyped (logical) columns that bind_rows won't combine
          qry$analytic$param_table <- as.data.frame(df)
        } else {
          qry$analytic$param_table <- dplyr::bind_rows(qry$analytic$param_table, df)
        }
        new_params <- dplyr::bind_rows(new_params, df)
        prm <- res[[1]]
      }
      # Add the selected param into the query set
      n_sel <- length(qry$columns)
      qry$queryset$select[[n_sel+1]] <- list(analyticId = prm$id)
      # Just in case you need information of the selected params
      qry$columns[[n_sel+1]] <- prm
    }
    if ( nrow(new_params) > 0 ) {
      save_paramtable(qry$analytic, new_params)
    }
    return(qry)
  }

#' @export
range <-
  function(qry, tstart = NULL, tend = NULL)
  {
    # if ( !is.numeric(c(tstart, tend)) ) {
    #   stop(sprintf("The values for time range should be numeric. Given values are from %s to %s.", tstart, tend))
    # }
    qry$queryset[["start"]] <- tstart
    qry$queryset[["end"]]   <- tend
    return(qry)
  }

timepoint <-
  function(qry, tpoint)
  {
    if (!is.vector(tpoint)) {
      stop("Timepoint should be given as a vector type.")
    }
    qry$queryset$offsets <- tpoint
    return(qry)
  }

# flight_duration <-
#   function(qry, flight, unit = "second")
#   {
#     prm <- get_param(qry$analytic, "hours of data (hours)")
#     q <- list(select = list(analyticId = prm$id),
#               size = 1)
#     r <- request(qry$connection, rtype="POST",
#                  uri_keys = c("analytic", "query"),
#                  uri_args = c(qry$ems_id, flight),
#                  jsondata = q)
#     res <- httr::content(r)
#     if ( !is.null(res$message) ) {
#       stop(sprintf('API query for flight = %s, parameter = "%s" was unsuccessful.\nHere is the massage from API: %s',
#                    flight, prm$name, res$message))
#     }
#     fl_len <- res$results[[1]][['values']][[1]]
#
#     if (unit=="second") {
#       y <- fl_len * 60 * 60
#     } else if (unit=="minute") {
#       y <- fl_len * 60
#     } else if (unit=="hour") {
#       y <- fl_len
#     } else {
#       stop(sprintf('Unrecognizable time unit (%s).', unit))
#     }
#     return(y)
#   }

#' @export
run.TsQuery <-
  function(qry,
           flight,
           start = NULL,
           end = NULL,
           timestep = NULL,
           timepoint = NULL,
           sample_mode = c("leaveBlank", "uniquePreviousSample",
                           "stairStep", "linearInterpolation",
                           "parameterDefault", "previousSample"),
           offset_type = c("sampledValues", "fixedRate"),
           samplingRate = NULL
  )

  {
    qry <- prepare_ts_queryset(qry, start, end, timestep, timepoint,
                               sample_mode, offset_type, samplingRate)

    r <- request(qry$connection, rtype="POST",
                 uri_keys = c("analytic", "query"),
                 uri_args = c(qry$ems_id, flight),
                 jsondata = qry$queryset)
    ts_result_to_df(qry, parse_json(r, simplify = TRUE), flight)
  }

prepare_ts_queryset <-
  function(qry, start = NULL, end = NULL, timestep = NULL, timepoint = NULL,
           sample_mode = c("leaveBlank", "uniquePreviousSample",
                           "stairStep", "linearInterpolation",
                           "parameterDefault", "previousSample"),
           offset_type = c("sampledValues", "fixedRate"),
           samplingRate = NULL)
  {
    sample_mode <- match.arg(sample_mode,
                             c("leaveBlank", "uniquePreviousSample",
                               "stairStep", "linearInterpolation",
                               "parameterDefault", "previousSample")
    )

    offset_type <- match.arg(offset_type, c("sampledValues", "fixedRate"))

    qry$queryset[["unsampledDataMode"]] <- sample_mode

    if (offset_type == "fixedRate" & !is.null(samplingRate)) {
      qry$queryset$offsetType[["type"]] <- offset_type
      qry$queryset$offsetType[["samplingRate"]] <- samplingRate
    }

    if (!is.null(timepoint)) {
      qry <- timepoint(qry, timepoint)
    } else if (!is.null(timestep)) {
      start <- if (is.null(start)) 0.0 else start
      if (is.null(end)) {
        stop("End timepoint should be given along with timestep input.")
      }
      qry <- timepoint(qry, seq(start, end, by = timestep))
    } else {
      qry <- range(qry, start, end)
    }
    qry
  }

# res is a response parsed with parse_json(..., simplify = TRUE), so offsets and
# each parameter's values are already vectors with nulls as NA.
ts_result_to_df <-
  function(qry, res, flight)
  {
    if ( !is.null(res$message) ) {
      stop(sprintf('API query for flight = %s was unsuccessful.\nHere is the massage from API: %s',
                   flight, res$message))
    }

    vals <- lapply(seq_along(qry$columns), function(i) {
      v <- res$results[[i]]$values
      if (is.list(v)) unlist(lapply(v, function(x) if (is.null(x)) NA else x)) else v
    })
    df <- data.frame(c(list(unlist(res$offsets)), vals), stringsAsFactors = F)
    names(df) <- c("Time (sec)", vapply(qry$columns, function(c) c$name, character(1)))
    df
  }

# Flights are queried concurrently, up to max_concurrent requests at a time.
#' @export
run_multiflts <-
  function(qry, flight, start = NULL, end = NULL, timestep = NULL, timepoint = NULL, max_concurrent = 4)
  {

    # input argument "flight" as multi-column data
    res <- list()

    attr_flag <- F
    if ( is.data.frame(flight) ) {
      FR <- flight[ , "Flight Record"]
      attr_flag <- T
    } else {
      FR <- flight
    }

    if (!is.null(timepoint)) {
      cat("Time points are not yet supported. The given time points will be ignored.\n")
      timepoint = NULL
    }

    cat(sprintf("=== Start running time series data querying for %d flights ===\n", length(FR)))
    reqs <- lapply(seq_along(FR), function(i) {
      q <- prepare_ts_queryset(qry, start = start[i], end = end[i], timestep = timestep[i])
      list(rtype    = "POST",
           uri_keys = c("analytic", "query"),
           uri_args = c(qry$ems_id, FR[i]),
           jsondata = q$queryset)
    })
    n_done <- 0
    resps <- request_parallel(qry$connection, reqs, max_concurrent,
                              on_done = function(i) {
                                n_done <<- n_done + 1
                                cat(sprintf("%d / %d: FR %d\n", n_done, length(FR), FR[i]))
                              })

    for (i in seq_along(FR)) {
      if (is.na(resps[[i]]$status)) {
        stop(sprintf("API query for flight = %s failed: %s", FR[i], resps[[i]]$text))
      }
      res[[i]] <- list()
      if ( attr_flag ) {
        res[[i]]$flt_data <- as.list(flight[i, ])
      } else {
        res[[i]]$flt_data <- list("Flight Record" = FR[i])
      }
      res[[i]]$ts_data <- ts_result_to_df(qry, parse_json(resps[[i]]$text, simplify = TRUE), FR[i])
    }
    return(res)
  }





