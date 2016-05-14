#------------------------------------------------------------------------------#
#-------------------------------- yg::dbfile.r --------------------------------#
#------------------------- author: gyang274@gmail.com -------------------------#
#------------------------------------------------------------------------------#

#--------+---------+---------+---------+---------+---------+---------+---------#
#234567890123456789012345678901234567890123456789012345678901234567890123456789#

#------------------------------------------------------------------------------#
#----------------------- create dbfile and load into db -----------------------#
#------------------------------------------------------------------------------#

#' create_disk_dbfile
#' create dbfile on disk
create_disk_dbfile <- function(qy, db, fn, rfrh) {

  if ( ! file.exists(fn) || rfrh ) {

    if ( ! file.exists(fn) ) {

      message("create_load_dbfile: init dbfile into disk ...\n")

      yg::bcp_azure_query(qy = qy, db = db, fn = fn)

      message("create_load_dbfile: init dbfile into disk ... done.\n")

    } else {

      message("create_load_dbfile: init dbfile into disk ...\n")

      message("create_load_dbfile: write into db_tmpt_file as dbfile exist ...\n")

      db_tmpt_file <- getwd() %+% gsub("\\\\", "/", tempfile(pattern = "db", tmpdir = "", fileext = ""))

      yg::bcp_azure_query(qy = qy, db = db, fn = db_tmpt_file)

      message("create_load_dbfile: copy db_tmpt_file into dbfile ...\n")

      ss0 <- remove_dbfile(fn)

      ss1 <- file.rename(from = db_tmpt_file, to = fn)

      if ( ss0 && ss1 ) {

        message("create_load_dbfile: init dbfile into disk ... done.\n")

      } else {

        message("create_load_dbfile: init dbfile into disk ... fail.\n")

      }

    }

  } else {

    message("create_load_dbfile: init dbfile skipped as file exists and no refresh required ... done.\n")

  }

  return(NULL)

}

#' create_load_dbfile
#' create and load dbfile into memory
create_load_dbfile <- function(qy, db, fn, rfrh, colname, coltype) {

  create_disk_dbfile(qy, db, fn, rfrh)

  message("create_load_dbfile: load dbfile into memory ...\n")

  xs <- fread(input = fn, sep = "\t", header = FALSE,
              skip = 0L, nrows = -1L, stringsAsFactors = FALSE,
              colClasses = coltype, col.names = colname)

  message("create_load_dbfile: load dbfile into memory ... done.\n")

  return(xs)

}

#' create_load_dbfile_disk
#' create and load dbfile into db_disk
create_load_dbfile_disk <- function(qy, db, fn, rfrh, qs, db_disk) {

  create_disk_dbfile(qy, db, fn, rfrh)

  message("create_load_dbfile_disk: load dbfile into db_disk ...\n")

  sqlite_executeqs(qs = qs, db = db_disk)

  message("create_load_dbfile_disk: load dbfile into db_disk ... done.\n")

  return(NULL)

}

#' load_dbfile
#' load dbfile into memory w. check on file.info intact
load_dbfile <- function(fn, colname, coltype, waitime = 10, maxtime = 600) {

  message("load_dbfile: load dbfile into memory ...\n")

  fs0 <- file.info(fn)

  Sys.sleep(waitime)

  fs1 <- file.info(fn)

  a_waitime <- waitime

  while ( ( ! all(fs0 == fs1) ) && ( a_waitime < maxtime ) ) {

    message("load_dbfile: wait as file is upgrading?\n")

    fs0 <- file.info(fn)

    Sys.sleep(waitime)

    fs1 <- file.info(fn)

    a_waitime <- a_waitime + waitime

  }

  if ( all(fs0 == fs1) ) {

    message("load_dbfile: file is ok for loading! ...\n")

    xs <- fread(input = fn, sep = "\t", header = FALSE,
                skip = 0L, nrows = -1L, stringsAsFactors = FALSE,
                colClasses = coltype, col.names = colname)

    message("load_dbfile: load dbfile into memory ... done.\n")

  } else {

    message("load_dbfile: file is not loading ok! ...\n")

    xs <- NULL

    message("load_dbfile: load dbfile into memory ... fail.\n")
  }

  return(xs)
}

#' load_dbfile_sc
#' load dbfile into memory w. selected columns from file
load_dbfile_sc <- function(fn, colname, coltype, sc_colname, id_colname, ...) {

  message("load_dbfile_sc: load data from dbfile ", fn, " ...\n")

  # init
  if ( !(length(colname) == length(coltype)) ) {

    stop("load_dbfile_sc: colname and coltype must in same length!\n")

  }

  if ( is.null(sc_colname) ) {

    message("load_dbfile_sc: load all columns from dbfile ", fn, " ...\n")

  }

  sc_colname <- sc_colname %|% colname

  if ( !is.null(id_colname) && !all(id_colname %in% sc_colname) ) {

    kk_idx <- match(id_colname, sc_colname)

    kk_idxNA <- is.na(kk_idx)

    message("load_dbfile_sc: warn id_colname not found and not set on loaded dbfile ", fn, " : ",
            paste(id_colname[kk_idxNA], collapse = ", "), ".\n")

    id_colname <- id_colname[!kk_idxNA]

  }

  # main
  sc_idx <- match(sc_colname, colname)

  sc_idxNA <- is.na(sc_idx)

  if ( any(is.na(sc_idx)) ) {

    message("load_dbfile_sc: warn sc_colname not found and load from dbfile ", fn, " : ",
            paste(sc_colname[sc_idxNA], collapse = ", "), ".\n")

    message("load_dbfile_sc: all colname can be found and load from dbfile ", fn, " : ",
            paste(colname, collapse = ", "), ".\n")

    sc_colname <- sc_colname[!sc_idxNA]

  }

  a_colname <- colname[sort(sc_idx[!sc_idxNA])]

  a_coltype <- rep("NULL", length(coltype))

  a_coltype[sort(sc_idx[!sc_idxNA])] <- coltype[sort(sc_idx[!sc_idxNA])]

  xs <- yg::load_dbfile(fn = fn, colname = a_colname, coltype = a_coltype, ...)

  if ( !is.null(id_colname) ) {

    message("load_dbfile_sc: setkeyv columns on dbfile ", fn, " ...\n")

    setkeyv(xs, id_colname)

  }

  setcolorder(xs, sc_colname)

  message("load_dbfile_sc: load dat from dbfile ", fn, " ... done.\n")

  return(xs)

}

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#------------------------------- refresh dbfile -------------------------------#
#------------------------------------------------------------------------------#

#' refresh_dbfile
#' rename dbfile into dbfakv and copy dbfrfh into dbfile
refresh_dbfile <- function(dbfile, dbfakv = NULL, dbfrfh = NULL) {

  message("refresh_dbfile: ", dbfile, " ...\n")

  dbfakv <- dbfakv %|%
    gsub(pattern = "^(.*)([.].*)$", replacement = "\\1_archive\\2", x = dbfile)

  dbfrfh <- dbfrfh %|%
    gsub(pattern = "^(.*)([.].*)$", replacement = "\\1_refresh\\2", x = dbfile)

  message("refresh_dbfile: remove ", dbfakv, " ...\n")

  remove_dbfile(dbfakv)

  message("refresh_dbfile: rename ", dbfile, " into ", dbfakv, " ...\n")

  file.rename(from = dbfile, to = dbfakv)

  message("refresh_dbfile: copied ", dbfrfh, " into ", dbfile, " ...\n")

  file.copy(from = dbfrfh, to = dbfile, copy.date = TRUE)

  message("refresh_dbfile: ", dbfile, " ... done.\n")

  return(NULL)

}

#' remove_dbfile
#' remove dbfile if exists
remove_dbfile <- function(dbfile) {

  if ( file.exists(dbfile) ) {

    message("remove_dbfile: remove ", dbfile, " .....\n")

    xs <- file.remove( dbfile )

  } else {

    message("remove_dbfile: ", dbfile, " nonexistence?\n")

    xs <- TRUE

  }

  return(xs)
}

#' copy_dbfile
#' make a copy of dbfile into dbcopy if dbfile exist and no dbcopy
#' dbcopy does not have size and mode and mtime all same as dbfile
copy_dbfile <- function(dbfile, dbcopy, overwrite = TRUE, copy.date = TRUE) {

  if ( file.exists(dbfile) ) {

    if ( ! file.exists(dbcopy) ) {

      message("copy_dbfile: copy dbfile ", dbfile, " into ", dbcopy, " - dbcopy nonexistence.\n")

      xs <- file.copy(from = dbfile, to = dbcopy, overwrite = overwrite, copy.date = copy.date)

    } else if ( ! all( (file.info(dbfile) == file.info(dbcopy))[1:4] ) ) {

      message("copy_dbfile: copy dbfile ", dbfile, " into ", dbcopy, " - dbcopy asynchronize.\n")

      xs <- file.copy(from = dbfile, to = dbcopy, overwrite = overwrite, copy.date = copy.date)

    } else {

      message("copy_dbfile: dbfile ", dbfile, " nonexistence?\n")

      xs <- TRUE

    }

  } else {

    message("copy_dbfile: dbfile ", dbfile, " nonexistence?\n")

    xs <- TRUE

  }

  return(xs)
}

#------------------------------------------------------------------------------#
