split_expr <-
  function(expr)
  {
    # for ( pattern in c( names(sp_ops), "[=!<>]=?" ) ) {
    #   l <- regexpr(pattern, expr)
    #   if (l[1]!=-1) break
    # }
    l <- regexpr(" in | not in |[=!<>]=?", expr)
    op <- substring(expr, l[1], l[1] + attr(l, "match.length") - 1)
    lr <- strsplit(expr, op)[[1]]
    return(list(
      field = eval(parse(text=lr[1])),
      op = op,
      value = eval(parse(text=lr[2]))
    ))
  }


translate_expr <-
  function(flt, spl_expr)
  {
    expr <- spl_expr

    # Field information
    fld <- search_fields(flt, expr$field)[[1]]
    fld_info <- list(type = 'field', value = fld$id)
    fld_type <- fld$type

    # Condition value
    val_info <- lapply(expr$value, function(v) list(type = 'constant', value = v))


    # Translate differently for different field types
    if ( fld_type == "boolean" ) {
      fltr <- boolean_filter(expr$op, fld_info, val_info)
    } else if ( fld_type == "discrete" ) {
      fltr <- discrete_filter(expr$op, fld_info, val_info, flt)
    } else if ( fld_type == "number" ) {
      fltr <- number_filter(expr$op, fld_info, val_info)
    } else if ( fld_type == "string" ) {
      fltr <- string_filter(expr$op, fld_info, val_info)
    } else if ( fld_type == "dateTime" ) {
      fltr <- datetime_filter(expr$op, fld_info, val_info)
    } else {
      stop(sprintf("%s has an unknown field type %s.", fld$name, fld_type))
    }
    return(fltr)
  }

# readable_output <-
#   function(qry, r=TRUE)
#   {
#     if (r) {
#       y <- "display"
#     } else {
#       y <- "none"
#     }
#     qry$queryset$format <- y
#     return(qry)
#   }

to_dataframe <-
  function(qry, raw_out)
  {
    cat("Raw JSON output to R dataframe...\n")
    colname <- sapply(raw_out$header, function(h) h$name)
    coltype <- sapply(qry$columns, function(c) c$type)
    col_id  <- sapply(qry$columns, function(c) c$id)

    # raw_out$rows is the row matrix produced by parse_json(..., simplify = TRUE),
    # with nulls as NA. Each column will need to be converted to a right type.
    df <- data.frame(raw_out$rows, stringsAsFactors = F)
    names(df) <- colname

    if ( qry$queryset$format == 'display') {
      cat('Done.\n')
      return(df)
    }
    # Do the dirty work of casting a right type for each column of the data
    for ( i in seq_along(coltype) ) {
      if ( coltype[i] == 'number' ) {
        df[ , i] <- as.numeric(df[ , i])
      } else if ( coltype[i] == "dateTime" ) {
        df[ , i] <- as.POSIXct(df[, i], tz="GMT",
                               format = "%Y-%m-%dT%H:%M:%S")
      } else if ( coltype[i] == "discrete" ) {
        k_map <- list_allvalues(qry$flight, field_id = col_id[i], in_vec = T)
        # Keys are integers; going through as.integer avoids "1e+05"-style strings.
        keys <- as.character(suppressWarnings(as.integer(df[ , i])))
        df[ , i] <- unname(k_map[keys])
      } else if ( coltype[i] == "boolean") {
        df[ , i] <- as_boolean(df[ , i])
      }
    }

    cat("Done.\n")
    return(df)
  }

# Booleans may arrive as 0/1 or as "TRUE"/"FALSE" depending on how the page parsed.
as_boolean <-
  function(x)
  {
    if (!is.character(x)) {
      return(as.logical(x))
    }
    out <- as.logical(x)
    num <- suppressWarnings(as.integer(x))
    out[!is.na(num)] <- as.logical(num[!is.na(num)])
    out
  }

get_rwy_id <-
  function(qry, ci)
  {
    # ==== Deprecated =====
    # Special routine for retreiving the runway IDs
    cat("\n --Running a special routine for querying runway IDs. This will make the querying twice longer.\n")
    qry$queryset$format <- 'display'
    res <- run(qry)
    res[ , ci]
  }

