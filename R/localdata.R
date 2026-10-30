localdata <-
  function(dbfile = NULL)
  {
    obj <- list()
    class(obj) <- 'LocalData'
    obj$table_info <- list(fieldtree = c('ems_id','db_id','id','nodetype','type','name','parent_id'),
                           dbtree    = c('ems_id','id','nodetype','name','parent_id'),
                           kvmaps    = c('ems_id','id','key','value'),
                           params    = c('ems_id','id','name','description','units'))

    if (is.null(dbfile)) {
      obj$dbfile <- default_metadata_file()
    } else {
      obj$dbfile <- file.path(dbfile)
    }
    obj <- connect.LocalData(obj)
    obj
  }

# Default cache location in the user's cache directory, not the package install folder.
default_metadata_file <-
  function()
  {
    cache_dir <- tools::R_user_dir("Rems", which = "cache")
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    file.path(cache_dir, "emsMetaData.db")
  }

#' @importFrom DBI dbConnect
connect.LocalData <-
  function(ldat)
  {
    ldat$conn <- DBI::dbConnect(SQLite(), dbname = ldat$dbfile)
    ldat
  }

check_colnames <-
  function(ldat, table_name, dat)
  {
    colnames <- ldat$table_info[[table_name]]
    missing  <- setdiff(colnames, names(dat))
    if (length(missing) > 0) {
      stop(sprintf("Data misses the following columns that are required: %s", paste(missing, collapse = ", ")))
    }
  }
#' @importFrom DBI dbDisconnect
close.LocalData <-
  function(ldat)
  {
    DBI::dbDisconnect(ldat$conn)
  }

#' @importFrom DBI dbWriteTable
append_data <-
  function(ldat, table_name, dat)
  {
    check_colnames(ldat, table_name, dat)
    DBI::dbWriteTable(ldat$conn, table_name, dat, append = T, row.names = F)
  }
# condition is a WHERE clause using ? placeholders, bound to the values in params.
#' @importFrom DBI dbExistsTable
#' @importFrom DBI dbGetQuery
get_data <-
  function(ldat, table_name, condition = NULL, params = NULL)
  {
    if (DBI::dbExistsTable(ldat$conn, table_name)) {
      q <- paste("SELECT * FROM", table_name)
      if (!is.null(condition)) {
        q <- paste(q, "WHERE", condition)
      }
      dat <- DBI::dbGetQuery(ldat$conn, q, params = params)
      return (dat)
    }
    dat <- data.frame(matrix(NA,nrow=0, ncol=length(ldat$table_info[[table_name]])), stringsAsFactors = F)
    names(dat) <- ldat$table_info[[table_name]]
    return (dat)
  }

#' @importFrom DBI dbExecute
delete_data <-
  function(ldat, table_name, condition = NULL, params = NULL)
  {
    if (DBI::dbExistsTable(ldat$conn, table_name)) {
      if (is.null(condition)) {
        DBI::dbExecute(ldat$conn, paste("DROP TABLE", table_name))
      } else {
        DBI::dbExecute(ldat$conn, paste("DELETE FROM", table_name, "WHERE", condition), params = params)
      }
    }
  }


delete_all_tables <-
  function(ldat)
  {
    for (table_name in names(ldat$table_info)) {
      delete_data(ldat, table_name)
    }
  }

file_loc <-
  function(ldat)
  {
    ldat$dbfile
  }

