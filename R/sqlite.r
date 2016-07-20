#------------------------------------------------------------------------------#
#-------------------------------- yg::sqlite.r --------------------------------#
#------------------------- author: gyang274@gmail.com -------------------------#
#------------------------------------------------------------------------------#

#--------+---------+---------+---------+---------+---------+---------+---------#
#234567890123456789012345678901234567890123456789012345678901234567890123456789#

#------------------------------------------------------------------------------#
#------------------------------------ init ------------------------------------#
#------------------------------------------------------------------------------#
#' `%|%`: set x default to y when x is null or na
`%|%` <- function(x, y) { if ( is.null(x) || is.na(x) ) y else x }

#' `%+%`: concatenate strings
#' @note `%+%` is also defined in ggplot2 and when loaded ggplot2 after yg
#' using ggplot2::`%+%` in connecting string will cause R stop and crashes
#' in such case it would be the best to use commented version as integrate
#' `%+%` <- function(x, y) {
#'
#' if ( is.environment(x) && is.environment(y) ) {
#'
#'   return( ggplot2::`%+%`(x, y) )
#'
#' } else {
#'
#'   return( paste0(x, y) )
#'
#' }
#' }
`%+%` <- function(stringX, stringY) { return( paste0(stringX, stringY) ) }
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#--------------------------------- data.table ---------------------------------#
#------------------------------------------------------------------------------#
#' removeWS
#' replace whitespace "\t", "\n" and "\\s+" with single whitespace ' '.
removeWS <- function(x) {
  x %>%
    gsub('\t'  , ' ', .) %>%
    gsub('\n'  , ' ', .) %>%
    gsub('\\s+', ' ', .)
}

#' CJ.dt
#' CJ on data.table
CJ.dt <- function(X, Y) {

  stopifnot(is.data.table(X), is.data.table(Y))

  k = NULL

  X = X[, c(k = 1, .SD)]
  setkey(X, k)

  Y = Y[, c(k = 1, .SD)]
  setkey(Y, NULL)

  return( X[Y, allow.cartesian=TRUE][, k := NULL][] )

}

#' dtFillNA
dtFillNA <- function(dt, fillNA = 0, fillNA_I = 0L, fillNA_C = "",
                     jcolidx = NULL, jcolnms = NULL) {

  # fill by name:
  # for ( j in names(dt) ) {
  #
  #   set(dt, which(is.na(dt[[j]])), j, fillNA)
  #
  # }

  # fill by number:
  # slightly faster
  if ( ! is.null(jcolidx) ) {

    message("fillNA on column index: ", jcolidx, " ...\n")

    if ( all(jcolidx < dim(dt)[2]) ) {

      # jcolidx <- jcolidx

    } else {

      stop("fillNA on column index: ", paste(jcolidx, sep = ", "), " not all in dt column index: ", paste(dim(dt)[2]  , sep = ", "), "...\n")

    }

  } else if ( ! is.null(jcolnms) ) {

    message("fillNA on column names: ", paste(jcolnms, sep = ", "), " ...\n")

    if ( all(jcolnms %in% colnames(dt)) ) {

      jcolidx <- match(jcolnms, colnames(dt))

    } else {

      stop("fillNA on column names: ", paste(jcolnms, sep = ", "), " not all in dt column names: ", paste(colnames(dt), sep = ", "), " ...\n")

    }

  } else {

    # message("fillNA on all columns ...\n")

    jcolidx <- seq_len(ncol(dt))

  }

  for ( j in jcolidx ) {

    if ( class(dt[[j]]) == "integer" ) {

      set(dt, which(is.na(dt[[j]])), j, fillNA_I)

    } else if ( class(dt[[j]]) == "numeric" ) {

      set(dt, which(is.na(dt[[j]])), j, fillNA)

    } else if ( class(dt[[j]]) == "character" ) {

      set(dt, which(is.na(dt[[j]])), j, fillNA_C)

    } else {

      message("warning: column ", j, " is not integer, numeric or character.\n")

      set(dt, which(is.na(dt[[j]])), j, fillNA)

    }

  }

  return(dt)

}

#' dtRfrhTb
#' update dt with tb based on id on columns in vd (all none id column in tb)
#' nomatched dt id will be value specified in nomatch (kept intact as in dt)
#' nofounded tb id will be added into dt with value in tb when nofound = TRUE
dtRfrhTb <- function(dt, tb, id, vd = NULL, nomatch = NULL, nofound = FALSE,
                     id_unique = TRUE, in_situ = TRUE) {

  if ( in_situ ) {

    dt <- dt %>% `class<-`(c("data.table", "data.frame"))

  } else {

    dt <- copy(dt) %>% `class<-`(c("data.table", "data.frame"))

  }

  tb <- tb %>% `class<-`(c("data.table", "data.frame"))


  if ( ! all(c(id, vd) %in% colnames(dt)) ) { stop("dtRfrhTb: c(id, vd) not all in colnames(dt).") }

  if ( ! all(c(id, vd) %in% colnames(tb)) ) { stop("dtRfrhTb: c(id, vd) not all in colnames(tb).") }

  if ( is.null(vd) ) {

    vd <- setdiff(intersect(colnames(dt), colnames(tb)), id)

    if ( length(vd) == 0 ) {

      stop("dtRfrhTb: refresh no column.\n")

    }

  }

  dt_id <- eval(parse(text = "dt %$% paste0(" %+% paste(id, collapse = ", ") %+% ")"))

  if ( length(unique(dt_id)) < nrow(dt) ) {

    if ( id_unique ) {

      stop("dtRfrhTb: none unique id in dt.\n")

    } else {

      message("dtRfrhTb: none unique id in dt.\n")

    }

  }

  tb_id <- eval(parse(text = "tb %$% paste0(" %+% paste(id, collapse = ", ") %+% ")"))

  if ( length(unique(tb_id)) < nrow(tb) ) {

    if ( id_unique ) {

      stop("dtRfrhTb: none unique id in tb.\n")

    } else {

      message("dtRfrhTb: none unique id in tb.\n")

    }

  }

  # inspired by plyr::mapvalues

  mapidx <- match(dt_id, tb_id)

  mapidxNA <- is.na(mapidx)

  # update on matched idx
  eval(parse(text = 'dt[!mapidxNA, c("' %+% paste0(vd, collapse = '" , "') %+% '") := as.list(tb[mapidx[!mapidxNA], vd, with = FALSE])]'))

  # update on nomatch idx
  if ( !is.null(nomatch) ) {

    eval(parse(text = 'dt[mapidxNA, c("' %+% paste0(vd, collapse = '" , "') %+% '") := nomatch]'))

  }

  # insert id in tb nofound in dt
  idx_found_tb <- sort(unique(mapidx))

  if ( length(idx_found_tb) != length(tb_id) && nofound ) {

    message("dtRfrhTb: insert id in tb not in dt into dt.\n")

    idx_nofound_tb <- setdiff(seq_len(length(tb_id)), idx_found_tb)

    dt <- dt %>%
      rbind(tb[idx_nofound_tb, c(id, vd), with = FALSE], use.names = TRUE, fill = TRUE)

  }

  return(dt)
}

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#----------------------- execute windows system command -----------------------#
#------------------------------------------------------------------------------#
#' executesc
#' executesc wrapper on system() to execute system commands
#' @note use shell() on calling and kicking off batch files
executesc <- function(sc, ...) {

  message("execute system command: ", substitute(sc), " ...\n")

  .ptc <- proc.time()

  x <- system(command = sc, ...)

  if ( x != 0 ) {

    message("execute system command return none success info?\n")

  }

  .ptd <- proc.time() - .ptc

  message("execute system command: ", substitute(sc), " consumes ", .ptd[3], " ...\n")

  message("execute system command: ", substitute(sc), " ... done.\n")

  return(x)
}
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#------------------------ bcp azure sql table or query ------------------------#
#------------------------------------------------------------------------------#
#' bcp_azure_table
#' copy tb from db into fn
#' must connect to designated azsql database within azdb
#' because azsql server dsnt allow db change after login
#' @param db: list of srv, usr, pwd, and dbn
bcp_azure_table <- function(tb, db, fn) {

  winAuth <- db[["winAuth"]] %|% FALSE

  if ( winAuth ) {

    message("bcp_azure_table: connect sql server with windows authentication.\n")

    sc <- "bcp " %+% tb %+% " out " %+% fn %+%
      " -S " %+% db[["srv"]] %+%
      " -d " %+% db[["dbn"]] %+%
      " -T -c -r \r\n"

  } else {

    sc <- "bcp " %+% tb %+% " out " %+% fn %+%
      " -S " %+% db[["srv"]] %+%
      " -U " %+% db[["usr"]] %+%
      " -P " %+% db[["pwd"]] %+%
      " -d " %+% db[["dbn"]] %+%
      " -c -r \r\n"

  }

  executesc(sc)
}

#' bcp_azure_query
#' copy qy from db into fn
#' @param db: list of srv, usr, pwd, and dbn
bcp_azure_query <- function(qy, db, fn) {

  winAuth <- db[["winAuth"]] %|% FALSE

  if ( winAuth ) {

    message("bcp_azure_query: connect sql server with windows authentication.\n")

    sc <- "bcp \"" %+% qy %+% "\" queryout " %+% fn %+%
      " -S " %+% db[["srv"]] %+%
      " -d " %+% db[["dbn"]] %+%
      " -T -c -r \r\n"

  } else {

    sc <- "bcp \"" %+% qy %+% "\" queryout " %+% fn %+%
      " -S " %+% db[["srv"]] %+%
      " -U " %+% db[["usr"]] %+%
      " -P " %+% db[["pwd"]] %+%
      " -d " %+% db[["dbn"]] %+%
      " -c -r \r\n"

  }

  executesc(sc)
}

#' bcp_azure_inrdt
#' copy dt from r into db
#' @param db: list of srv, usr, pwd, and dbn
bcp_azure_inrdt <- function(dt, tb, db, qy_fmt = NULL, overwrite = TRUE,
                            id = NULL, id_unique = TRUE) {

  winAuth <- db[["winAuth"]] %|% FALSE

  if ( is.null(qy_fmt) ) {

    qy_fmt <- "create table " %+% tb %+%
      " (" %+%
      paste(paste(colnames(dt), det_col_type(dt), "NULL"), collapse = ", ") %+%
      ");" %>% removeWS

  }

  if ( overwrite ) {

    qy_fmt <- "if object_id('" %+% db[["dbn"]] %+% "." %+% tb %+%"', 'u') is not null " %+%
      "drop table " %+% db[["dbn"]] %+% "." %+% tb %+% ";\n" %+%
      qy_fmt %>% removeWS

  }

  sqodbc_executeqy(qy = qy_fmt, db = db)

  fm_tmpt_file <- getwd() %+% gsub("\\\\", "/", tempfile(pattern = "fm", tmpdir = "", fileext = ".xml"))

  if ( winAuth ) {

    fm <- "bcp " %+% tb %+% " format nul -c -x -f " %+% fm_tmpt_file %+%
      " -S " %+% db[["srv"]] %+%
      " -d " %+% db[["dbn"]] %+%
      " -T "

  } else {

    fm <- "bcp " %+% tb %+% " format nul -c -x -f " %+% fm_tmpt_file %+%
      " -S " %+% db[["srv"]] %+%
      " -U " %+% db[["usr"]] %+%
      " -P " %+% db[["pwd"]] %+%
      " -d " %+% db[["dbn"]]

  }

  xs_fm <- executesc(fm)

  sc_tmpt_file <- getwd() %+% gsub("\\\\", "/", tempfile(pattern = "sc", tmpdir = "", fileext = ".txt"))

  write.table(dt, file = sc_tmpt_file, col.names = FALSE, row.names = FALSE, quote = FALSE, na = "", sep = "\t")

  if ( winAuth ) {

    sc <- "bcp " %+% tb %+% " in " %+% sc_tmpt_file %+% " -f " %+% fm_tmpt_file %+%
      " -S " %+% db[["srv"]] %+%
      " -d " %+% db[["dbn"]] %+%
      " -T "

  } else {

    sc <- "bcp " %+% tb %+% " in " %+% sc_tmpt_file %+% " -f " %+% fm_tmpt_file %+%
      " -S " %+% db[["srv"]] %+%
      " -U " %+% db[["usr"]] %+%
      " -P " %+% db[["pwd"]] %+%
      " -d " %+% db[["dbn"]]

  }

  xs_sc <- executesc(sc)

  if ( ! is.null(id) ) {

    # gsub("[[:punct:]]", "", tb) remove "." and all other special characters in tb
    qy_idx <-'create ' %+% ifelse(id_unique, 'unique ', ' ') %+%
      'index idx_' %+% gsub("[[:punct:]]", "", tb) %+% ' on ' %+% tb %+% '(' %+% paste0(id, collapse = ', ') %+% ');'

    sqodbc_executeqy(qy = qy_idx, db = db)

  }

  if ( file.exists(fm_tmpt_file) ) { file.remove(fm_tmpt_file) }

  if ( file.exists(sc_tmpt_file) ) { file.remove(sc_tmpt_file) }

  return(xs_sc)
}

det_col_type <- function(x) {

  # cc <- data.table(idx = 1:ncol(x), r_type = vapply(x, class, character(1L)))
  #
  # dd <- data.table(r_type = c("character"   , "numeric"  , "integer"),
  #                  s_type = c("varchar(255)", "float(16)", "int"    ))
  #
  # tt <- merge(cc, dd, by = "r_type", all.x = TRUE, all.y = FALSE) %>%
  #   setorder(idx) %>% .[ , s_type, drop = TRUE]

  cc <- data.table(r_type = vapply(x, class, character(1L)))

  dd <- data.table(r_type = c("character"   , "numeric"  , "integer"),
                   s_type = c("varchar(255)", "float(16)", "int"    ))

  tt <- merge(cc, dd, by = "r_type", all.x = TRUE, all.y = FALSE, sort = FALSE) %>% .[ , s_type, drop = TRUE]

  return(tt)

}
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#---------------------- wrapper on rsqlite sql functions ----------------------#
#------------------------------------------------------------------------------#
#' sqlite_executeqs wrapper on sqlite3
#' wrapper on sqlite3 command line shell command
#' chooses no wrapper on the dbGetQuery function
#' because of dbGetQuery can not handle mutliple line query with "\n"
sqlite_executeqs <- function(qs, db) {

  qs_tmpt_file <- getwd() %+% gsub("\\\\", "/", tempfile(pattern = "qs", tmpdir = "", fileext = ".sql"))

  message("write qs into ", qs_tmpt_file, " ...\n")

  writeLines(qs, qs_tmpt_file)

  executesc(sc = 'sqlite3 ' %+% db %+% ' ".read ' %+% qs_tmpt_file %+% '"')

  if ( file.exists(qs_tmpt_file) ) { file.remove(qs_tmpt_file) }

  return(NULL)
}

#' sqlite_executeqy wrapper on dbGetQuery
sqlite_executeqy <- function(qy, db, ...) {

  .sqlite_db <- dbConnect(dbDriver("SQLite"), db)

  xs <- dbGetQuery(.sqlite_db, qy, ...)

  dbDisconnect(.sqlite_db)

  return(xs)
}

#' sqlite_existsqtb wrapper on dbExistsTable
sqlite_existsqtb <- function(tb, db) {

  .sqlite_db <- dbConnect(dbDriver("SQLite"), db)

  xs <- dbExistsTable(.sqlite_db, tb)

  dbDisconnect(.sqlite_db)

  return(xs)
}

#' sqlite_fetchsqtb wrapper on dbReadTable
sqlite_fetchsqtb <- function(tb, db, ...) {

  message("fetch table: ", substitute(tb), " from ", substitute(db), " ...\n")

  .ptc <- proc.time()

  .sqlite_db <- dbConnect(dbDriver("SQLite"), db)

  dt <- dbReadTable(.sqlite_db, tb, ...) %>%
    `class<-`(c("data.table", "data.frame"))

  dbDisconnect(.sqlite_db)

  .ptd <- proc.time() - .ptc

  message("fetch table: ", substitute(tb), " consumes ", .ptd[3], " seconds.\n")

  message("fetch table: ", substitute(tb), " from ", substitute(db), " ... done.\n")

  return(dt)
}

#' sqlite_removestb wrapper on dbRemoveTable
sqlite_removestb <- function(tb, db) {

  .sqlite_db <- dbConnect(dbDriver("SQLite"), db)

  xs <- dbRemoveTable(.sqlite_db, tb)

  dbDisconnect(.sqlite_db)

  return(xs)

}

#' sqlite_uploadrdt wrapper on dbWriteTable
#' id and id_unique - create [unique] index
sqlite_uploadrdt <- function(dt, tb, db, id = NULL, id_unique = TRUE,
                             overwrite = TRUE, append = FALSE, ...) {

  message("sqlite_uploadrdt: load table ", substitute(tb), " into ", substitute(db), " ...\n")

  .ptc <- proc.time()

  .sqlite_db <- dbConnect(dbDriver("SQLite"), db)

  xs <- dbWriteTable(conn = .sqlite_db, name = tb, value = dt,
                     overwrite = overwrite, append = append, ...)

  if ( !is.null(id) ) {

    if ( ! all( id %in% colnames(dt) ) ) { stop(substitute(id),  " not in ", substitute(dt)) }

    dbGetQuery(conn = .sqlite_db, statement = 'create ' %+% ifelse(id_unique, 'unique ', ' ') %+%
                 'index idx_' %+% tb %+% ' on ' %+% tb %+% '(' %+% paste0(id, collapse = ', ') %+% ');')
  }

  dbDisconnect(.sqlite_db)

  .ptd <- proc.time() - .ptc

  message("sqlite_uploadrdt: load table ", substitute(tb), " consumes ", .ptd[3], " seconds.\n")

  message("sqlite_uploadrdt: load table ", substitute(tb), " into ", substitute(db), " ... done.\n")

  return(xs)
}

#' sqlite_refreshtb
#' refreshtb will update value when primary key exist,
#' and also insert new row when primary key not exist.
sqlite_refreshtb <- function(dt, tb, id, db, dt_coltp = NULL, id_unique = TRUE,
                             batch_size = 10000) {

  message("refresh table: ", substitute(tb), " in ", substitute(db), " ...\n")

  .ptc <- proc.time()

  .nn  <- nrow(dt)

  .sqlite_db <- RSQLite::dbConnect(dbDriver("SQLite"), db)

  if ( ! (tb %in% RSQLite::dbListTables(.sqlite_db)) ) {

    message("sqlite_refreshtb: create ", substitute(tb), " not in ", substitute(db), " ... \n")

    # dplyr::copy_to cann't create unqiue index?

    # .src_sqlite_db <- dplyr::src_sqlite(db, create = TRUE)

    # dplyr::copy_to(dest = .src_sqlite_db, df = dt, name = tb, indexes = list(id), temporary = FALSE)

    # rm(.src_sqlite_db)

    # gc()

    dt_coltp <- dt_coltp %|% "text"

    # dbGetQuery cann't handle '\n' - execute first line limit 1
    .qs <- 'create table ' %+% tb %+% ' (' %+%  paste(colnames(dt), dt_coltp, collapse = ', ') %+% ');\n' %+%
      'create ' %+% ifelse(id_unique, 'unique ', ' ') %+%
      'index idx_' %+% tb %+% ' on ' %+% tb %+% '(' %+% paste0(id, collapse = ', ') %+% ');\n'

    sqlite_executeqs(qs = .qs, db = db)

    message("sqlite_refreshtb: create ", substitute(tb), " not in ", substitute(db), " ... done.\n")

  }

  .nm  <- RSQLite::dbListFields(.sqlite_db, tb)

  if ( ! all(colnames(dt) == .nm) ) {

    stop("dt and tb must have same name.\n")

  }

  if ( ! all(id %in% .nm) ) {

    stop("id not in tb\n")

  }

  .nk <- setdiff(.nm, id)

  .idx <- 0

  while (.idx < .nn) {

    .xx <- dt[(.idx + 1):min(.idx + batch_size, .nn), ]

    .qs <- 'begin;\n' %+%
      'insert or replace into ' %+%  tb %+%
      ' (' %+% paste0(.nm, collapse = ", ") %+% ') ' %+%
      'values' %+%
      '("' %+% paste0(apply(.xx, 1, paste0, collapse =  '", "'), collapse = '"), ("') %+% '");\n' %+%
      'commit;\n'

    sqlite_executeqs(qs = .qs, db = db)

    .idx <- min(.idx + batch_size, .nn)
  }

  RSQLite::dbDisconnect(conn = .sqlite_db)

  .ptd <- proc.time() - .ptc

  message("refresh table: ", substitute(tb), " consumes ", .ptd[3], " seconds.\n")

  message("refresh table: ", substitute(tb), " in ", substitute(connectsql), " ... done.\n")

  return(NULL)

}

#' sqlite_subsetidx
#' fetch an subset of index table from sqlite db - inner join
#' @param dt: a data.frame/data.table in R with column of key
#' @param tb: a full reference table in sqlite with key value
#' @return a data.table with value append to the R data.table
#' id is limit to single index - add capability to id multiple index
sqlite_subsetidx <- function(dt, tb, db, id, batch_size = 10000,
                             all.dt = FALSE, all.tb = FALSE) {

  message("subset index from table: ", substitute(tb), " in ", substitute(db), " ...\n")

  .ptc = proc.time()

  dt <- dt %>% `class<-`(c("data.table", "data.frame"))

  # limit id to single index
  # dt_idx <- eval(substitute(dt %>% dplyr::select(.idx), list(.idx = parse(text = id)))) %>% unique
  dt_idx <- eval(substitute(dt %>% dplyr::select(.idx), list(.idx = parse(text = id)))) %>% unique %>% unlist(use.names = FALSE)

  .sqlite_db <- dbConnect(dbDriver("SQLite"), db)

  # .nn <- nrow(dt_idx)
  .nn <- length(dt_idx)

  .nm <- dbListFields(.sqlite_db, tb)

  # limit 1 is potential bug when empty table
  # fixit use dbGetQuery(.sqlite_db, 'PRAGMA table_info("' %+% tb %+% '")')
  # .nc <- dbGetQuery(.sqlite_db, "select * from " %+% tb %+% " limit 1") %>% apply(2, class)

  # dt_rtn <- matrix(NA_character_, .nn, length(.nm)) %>% data.table %>% `colnames<-`(.nm)

  # fix attempt 1 - limit to handle type of int and text
  .nc <- dbGetQuery(.sqlite_db, 'PRAGMA table_info("' %+% tb %+% '")') %>% `[[`("type")

  dt_rtn <- eval(parse(text = "data.table(" %+% paste0(
    paste0(.nm %+% " = rep(" %+% ifelse(.nc == "int", "NA_integer_", "NA_character_") %+% " , " %+% .nn %+% ")" ),
    collapse = ", ") %+% ")"))

  .ik <- 0

  .jk <- 0

  while (.ik < .nn) {

    # limit id to single index
    # .xx <- dt_idx[(.ik + 1):min(.ik + batch_size, .nn), ] %>% unlist(use.names = FALSE)
    .xx <- dt_idx[(.ik + 1):min(.ik + batch_size, .nn)]

    # double to escape ' when query against sqlite sql
    .qy <- 'select * from ' %+% tb %+% ' where ' %+% id %+% ' in ("' %+% paste0(gsub("'", "''", .xx), collapse = '", "') %+% '")'

    dt_rtn_chunk <- RSQLite::dbGetQuery(conn = .sqlite_db, statement = .qy)

    dt_rtn[(.jk + 1):(.jk + nrow(dt_rtn_chunk)), ] <- dt_rtn_chunk

    .ik <- min(.ik + batch_size, .nn)

    .jk <- .jk + nrow(dt_rtn_chunk)

    message("sqlite_subsetidx: bulk-match ", .ik, " rows ...\n")
  }

  dt_rtn <- dt_rtn[1:.jk, ] %>% `class<-`(c("data.table", "data.frame"))

  dtAgmt <- merge(dt, dt_rtn, by = id, all.x = all.dt, all.y = all.tb) %>%
    `class<-`(c("data.table", "data.frame"))

  .ptd = proc.time() - .ptc

  message("sqlite_subsetidx: consumes ", .ptd[3], " seconds ...\n")

  message("subset index from table: ", substitute(tb), " in ", substitute(db), " ... done.\n")

  return(dtAgmt)
}

#' sqlite_selectidx wrapper on dbGetQuery
#' select * from tb in db where id = id_value
sqlite_selectidx <- function(tb, db, id, id_value) {

  .sqlite_db <- dbConnect(dbDriver("SQLite"), db)

  # double to escape ' when query against sqlite sql
  xs <- dbGetQuery(.sqlite_db, "select * from " %+% tb %+% " where " %+% paste(id, "'" %+% gsub("'", "''", id_value) %+% "'", sep = " = ", collapse = " and ") %+% ";") %>%
    `class<-`(c("data.table", "data.frame"))

  dbDisconnect(.sqlite_db)

  return(xs)

}
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#----------------------- wrapper on rodbc sql functions -----------------------#
#------------------------------------------------------------------------------#
#' sqodbc_createcnn wrapper on odbcDriverConnect
#' @param db: list of [dsn] srv usr pwd and dbn
sqodbc_createcnn <- function(db) {

  # require windows dsn configuration #
  # RODBC::odbcConnect(
  #   dsn = db[["dsn"]],
  #   uid = db[["usr"]],
  #   pwd = db[["pwd"]]
  # )

  winAuth <- db[["winAuth"]] %|% FALSE

  if ( winAuth ) {

    xc <- RODBC::odbcDriverConnect(
      connection =
        "driver=ODBC Driver 11 for SQL Server;" %+%
        "server="   %+% db[["srv"]] %+% ";" %+%
        "database=" %+% db[["dbn"]] %+% ";" %+%
        "trusted_connection=true"
    )

  } else {

    xc <- RODBC::odbcDriverConnect(
      connection =
        "driver=ODBC Driver 11 for SQL Server;" %+%
        "server="   %+% db[["srv"]] %+% ";" %+%
        "uid="      %+% db[["usr"]] %+% ";" %+%
        "pwd="      %+% db[["pwd"]] %+% ";" %+%
        "database=" %+% db[["dbn"]]
    )

  }

  return( xc )

}

#' sqodbc_executeqy wrapper on sqlQuery
#' @param db: list of [dsn] srv usr pwd and dbn
sqodbc_executeqy <- function(qy, db, ...) {

  message("execute query: ", substitute(qy), " on ", substitute(db), " ...\n")

  .ptc <- proc.time()

  .sql <- sqodbc_createcnn(db)

  x <- RODBC::sqlQuery(channel = .sql, query = qy, ...)

  odbcClose(.sql)

  if ( ! (is.data.frame(x) || length(x) == 0 || x == "No Data") ) {

    message("query returns none data and none success info?\n")

  }

  .ptd <- proc.time() - .ptc

  message("execute query: ", substitute(qy), " consumes ", .ptd[3], " seconds.\n")

  message("execute query: ", substitute(qy), " on ", substitute(db), " ... done.\n")

  return(x)
}

#' sqodbc_existsqtb
#' @param db: list of [dsn] srv usr pwd and dbn
sqodbc_existsqtb <- function(tb, db, tb_type = c("table", "view"), ...) {

  message("exists table: ", substitute(tb), " on ", substitute(db), " ...\n")

  .ptc <- proc.time()

  tb_list = unlist(strsplit(tb, split = ".", fixed = TRUE))

  if ( length(tb_list) == 1 ) {

    tb_name = tb_list[1]

    tb_schm = "dbo"

  } else if ( length(tb_list) == 2 ) {

    tb_name = tb_list[2]

    tb_schm = tb_list[1]

  } else {

    stop("exists table: tb name must have at most one . and specify db with dbn ...\n")

  }

  .sql <- sqodbc_createcnn(db)

  xs <- sqlTables(.sql) %>% `class<-`(c("data.table", "data.frame"))

  odbcClose(.sql)

  names(xs) <- tolower(names(xs))

  xs <- data.frame(lapply(xs, tolower)) %>% `class<-`(c("data.table", "data.frame"))

  xstb <- xs[table_schem == tb_schm & table_name == tb_name & table_type %in% tb_type]

  if ( dim(xstb)[1] > 0 ) {

    xstb_b = TRUE

  } else {

    xstb_b = FALSE

  }

  .ptd <- proc.time() - .ptc

  message("exists table: ", substitute(tb), " consumes ", .ptd[3], " seconds.\n")

  message("exists table: ", substitute(tb), " on ", substitute(db), " ... done.\n")

  return( xstb_b )

}

#' sqodbc_fetchsqtb wrapper on sqlFetch
sqodbc_fetchsqtb <- function(tb, db, ...) {

  message("fetch table: ", substitute(tb), " from ", substitute(db), " ...\n")

  .ptc <- proc.time()

  .sql <- sqodbc_createcnn(db)

  x <- sqlFetch(channel = .sql, sqtable = tb, ...)

  odbcClose(.sql)

  if ( !is.data.frame(x) && length(x) != 0 ) {

    message("query returns none data and none success info?\n")

  }

  .ptd <- proc.time() - .ptc

  message("fetch table: ", substitute(tb), " consumes ", .ptd[3], " seconds.\n")

  message("fetch table: ", substitute(tb), " from ", substitute(db), " ... done.\n")

  return(x)
}

#' sqodbc_uploadrdt wrapper on sqlSave
#' make sqodbc_uploadrdt wrapper on bcp_azure_inrdt?
sqodbc_uploadrdt <- function(dt, tb, db, id = NULL, id_unique = TRUE,
                             rownames = FALSE, colnames = FALSE,
                             safer = TRUE, fast = TRUE, overwrite = TRUE, ...) {

  message("upload table: ", substitute(tb), " to ", substitute(db), " ...\n")

  .ptc <- proc.time()

  e <- sqodbc_existsqtb(tb, db)

  if ( e && !overwrite ) {

    message("upload table: tb exists on db and not overwrite ...\n")

  } else {

    .sql <- sqodbc_createcnn(db)

    if ( e ) { sqlDrop(channel = .sql, sqtable = tb) }

    x <- sqlSave(channel = .sql, dat = dt, tablename = tb, rownames = rownames,
                 colnames = colnames, safer = safer, fast = fast, ...)

    if ( !is.null(id) ) {

      if ( ! all( id %in% colnames(dt) ) ) { stop(substitute(id),  " not in ", substitute(dt)) }

      RODBC::sqlQuery(channel = .sql, query = 'create ' %+% ifelse(id_unique, 'unique ', ' ') %+%
                   'index idx_' %+% gsub("[[:punct:]]", "", tb) %+% ' on ' %+% tb %+% '(' %+% paste0(id, collapse = ', ') %+% ');')
    }

    odbcClose(.sql)

    if ( x != 1 ) {

      message("upload table: query returns none data and none success info?\n")

    }

  }

  .ptd <- proc.time() - .ptc

  message("upload table: ", substitute(tb), " consumes ", .ptd[3], " seconds.\n")

  message("upload table: ", substitute(tb), " to ", substitute(db), " ... done.\n")

  return(x)
}

#' sqodbc_removetb wrapper on sqlDrop
sqodbc_removestb <- function(tb, db) {

  message("remove table: ", substitute(tb), " on ", substitute(db), " ...\n")

  .ptc <- proc.time()

  if ( yg::sqodbc_existsqtb(tb, db) ) {

    .sql <- sqodbc_createcnn(db)

    x <- RODBC::sqlDrop(channel = .sql, sqtable = tb, error = FALSE)

    odbcClose(.sql)

  } else {

    message("remove table: ", substitute(tb),  " not exists on ", substitute(db), " ...\n")

    x <- NULL

  }

  # if ( ! (is.data.frame(x) || length(x) == 0 || x == "No Data") ) {
  #
  #   message("query returns none data and none success info?\n")
  #
  # }

  if ( !is.null(x) && x == -1 ) {

    message("query returns none data and none success info?\n")

  }

  .ptd <- proc.time() - .ptc

  message("remove table: ", substitute(tb), " consumes ", .ptd[3], " seconds.\n")

  message("remove table: ", substitute(tb), " on ", substitute(db), " ... done.\n")

  return(x)

}

#' sqodbc_updatestb wrapper on sqlUpdate
#' sqlUpdate will update value when primary key exist,
#' but no insert or append when primary key not exist.
sqodbc_updatestb <- function(dt, tb, id, db, ...) {

  message("update table: ", substitute(tb), " in ", substitute(db), " ...\n")

  .ptc <- proc.time()

  .nn  <- nrow(dt)

  .sql <- sqodbc_createcnn(db)

  x <- sqlUpdate(channel = .sql, dat = dt, tablename = tb, index = id, ...)

  odbcClose(.sql)

  if ( x != 1 ) {

    message("query returns none data and none success info?\n")

  }

  .ptd <- proc.time() - .ptc

  message("update table: ", substitute(tb), " consumes ", .ptd[3], " seconds.\n")

  message("update table: ", substitute(tb), " in ", substitute(db), " ... done.\n")

  return(x)

}

#' sqodbc_refreshtb
#' refreshtb mannual insert rows
#' refreshtb will update value when primary key exist,
#' and also insert new row when primary key not exist.
sqodbc_refreshtb <- function(dt, tb, id, db, batch_size = 10000) {

  message("refresh table: ", substitute(tb), " in ", substitute(db), " ...\n")

  .ptc <- proc.time()

  .nn  <- nrow(dt)

  .sql <- sqodbc_createcnn(db)

  .nm  <- sqlColumns(.sql, tb) %>% `[[`("COLUMN_NAME")

  odbcClose(.sql)

  if ( ! all(colnames(dt) == .nm) ) {

    stop("dt and tb must have same name.\n")

  }

  if ( ! (id %in% .nm) ) {

    stop("id not in tb\n")

  }

  .nk <- setdiff(.nm, id)

  .idx <- 0

  while (.idx < .nn) {

    .xx <- dt[(.idx + 1):min(.idx + batch_size, .nn), ]

    .qy <- "insert into " %+%  tb %+%
      " (" %+% paste0(.nm, collapse = ", ") %+% ") " %+%
      "values" %+%
      " ('" %+% paste0(apply(.xx, 1, paste0, collapse =  "', '"), collapse = "'), ('") %+% "') " %+%
      "on duplicate key update " %+% paste0(sprintf("%s = values(%s)", .nk, .nk), collapse = ", ")

    suppressMessages(sqodbc_executeqy(.qy, db))

    .idx <- min(.idx + batch_size, .nn)
  }

  .ptd <- proc.time() - .ptc

  message("refresh table: ", substitute(tb), " consumes ", .ptd[3], " seconds.\n")

  message("refresh table: ", substitute(tb), " in ", substitute(db), " ... done.\n")

  return(NULL)

}

#' sqodbc_subsetidx
#' fetch an subset of index table from sqodbc db - inner join
#' @param dt: a data.frame/data.table in R with column of key
#' @param tb: a full reference table in sqodbc with key value
#' @return a data.table with value append to the R data.table
#' id limit to single index: add capability to id multi index
#' sqodbc_subsetidx is a simplier verison of sqlite_subsetidx
#' without checking on type of each column when define dt_rtn
sqodbc_subsetidx <- function(dt, tb, db, id, batch_size = 10000,
                             all.dt = FALSE, all.tb = FALSE) {

  # TODO: add batch_size to improve efficiency when subset a large set of id

  message("subset index from table: ", substitute(tb), " in ", substitute(db), " ...\n")

  .ptc = proc.time()

  dt <- dt %>% `class<-`(c("data.table", "data.frame"))

  # TODO: id limit to single index: add capability to id multi index
  # dt_idx <- eval(substitute(dt %>% dplyr::select(.idx), list(.idx = parse(text = id)))) %>% unique
  dt_idx <- eval(substitute(dt %>% dplyr::select(.idx), list(.idx = parse(text = id)))) %>% unique %>% unlist(use.names = FALSE)

  .sql <- sqodbc_createcnn(db)

  # TODO: id limit to single index: add capability to id multi index
  # .nn <- nrow(dt_idx)
  .nn <- length(dt_idx)

  .ik <- 0

  .kk <- 0

  while (.ik < .nn) {

    # TODO: id limit to single index: add capability to id multi index
    # .xx <- dt_idx[(.ik + 1):min(.ik + batch_size, .nn), ] %>% unlist(use.names = FALSE)
    .xx <- dt_idx[(.ik + 1):min(.ik + batch_size, .nn)]

    # double to escape ' when query against azure sql
    .qy <- "select * from " %+% tb %+% " where " %+% id %+% " in ('" %+% paste0(gsub("'", "''", .xx), collapse = "', '") %+% "')"

    .vv <- RODBC::sqlQuery(channel = .sql, query = .qy)

    do.call('<-', list(x = paste("dt_rtn_chunk", .kk, sep = "_"), value = .vv))

    .ik <- min(.ik + batch_size, .nn)

    .kk <- .kk + 1

    message("sqlite_subsetidx: bulk-match ", .ik, " rows ...\n")
  }

  eval(parse(text = "dt_rtn <- rbind(" %+% paste("dt_rtn_chunk", 0:(.kk - 1), sep = "_", collapse = ", ") %+% ")"))

  dt_rtn <- dt_rtn %>% `class<-`(c("data.table", "data.frame"))

  dtAgmt <- merge(dt, dt_rtn, by = id, all.x = all.dt, all.y = all.tb) %>%
    `class<-`(c("data.table", "data.frame"))

  .ptd = proc.time() - .ptc

  message("sqodbc_subsetidx: consumes ", .ptd[3], " seconds ...\n")

  message("subset index from table: ", substitute(tb), " in ", substitute(db), " ... done.\n")

  return(dtAgmt)
}

#' sqodbc_selectidx wrapper on sqlQuery
#' select * from tb in db where id = id_value
sqodbc_selectidx <- function(tb, db, id, id_value) {

  .sql <- sqodbc_createcnn(db)

  # double to escape ' when query against azure sql
  x <- RODBC::sqlQuery(channel = .sql, query = "select * from " %+% tb %+% " where " %+% paste(id, "'" %+% gsub("'", "''", id_value) %+% "'", sep = " = ", collapse = " and ") %+% ";") %>%
    `class<-`(c("data.table", "data.frame"))

  odbcClose(.sql)

  return(x)

}
#------------------------------------------------------------------------------#


