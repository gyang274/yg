<!-- README.md is generated from README.Rmd. Please edit that file -->
yg
==

**yg** implements a set of database management tools for communicating with SQLite or ODBC server such as Microsoft Azure SQL.

data.table tools:
-----------------

-   **CJ.dt**: extends `data.table::CJ` to data.table. `CJ.dt(X, Y)` require both X and Y be data.table and generate a data.table with full combination of each row of X w.r.t each row of Y.

``` r
library(yg)
#> Loading required package: data.table
#> Loading required package: magrittr
#> Loading required package: RSQLite
#> Loading required package: DBI
#> Loading required package: RODBC

X <- data.table(a = c(TRUE, FALSE, FALSE), b = c(11L, 22L, 34L))
Y <- data.table(c = c(5.80, 13.21, 34.55), d = c("a", "B", "C"))

(Z <- CJ.dt(X, Y))
#>        a  b     c d
#> 1:  TRUE 11  5.80 a
#> 2: FALSE 22  5.80 a
#> 3: FALSE 34  5.80 a
#> 4:  TRUE 11 13.21 B
#> 5: FALSE 22 13.21 B
#> 6: FALSE 34 13.21 B
#> 7:  TRUE 11 34.55 C
#> 8: FALSE 22 34.55 C
#> 9: FALSE 34 34.55 C
```

-   **dtFillNA**: fill `NA` in data.table in situ, allow fill different values for numeric `NA_real_`, integer `NA_integer_` and character `NA_character_`.

``` r
X <- data.table(a = c(TRUE, FALSE, FALSE), b = c(11L, NA_integer_, 34L))
Y <- data.table(c = c(5.80, 13.21, NA_real_), d = c("a", NA_character_, "C"))

(Z <- CJ.dt(X, Y))
#>        a  b     c  d
#> 1:  TRUE 11  5.80  a
#> 2: FALSE NA  5.80  a
#> 3: FALSE 34  5.80  a
#> 4:  TRUE 11 13.21 NA
#> 5: FALSE NA 13.21 NA
#> 6: FALSE 34 13.21 NA
#> 7:  TRUE 11    NA  C
#> 8: FALSE NA    NA  C
#> 9: FALSE 34    NA  C

dtFillNA(dt = Z, fillNA = 34.54, fillNA_I = 23L, fillNA_C = "b")
#> warning: column 1 is not integer, numeric or character.
#>        a  b     c d
#> 1:  TRUE 11  5.80 a
#> 2: FALSE 23  5.80 a
#> 3: FALSE 34  5.80 a
#> 4:  TRUE 11 13.21 b
#> 5: FALSE 23 13.21 b
#> 6: FALSE 34 13.21 b
#> 7:  TRUE 11 34.54 C
#> 8: FALSE 23 34.54 C
#> 9: FALSE 34 34.54 C
```

-   **dtRfrhTb**: update primary data.table (dt) with a reference data.table (tb) based on matching id value on id columns, and update primary data.table vd value on vd columns using refernce data.table vd valude on vd columns.

    One can specify when id in dt not find a match in tb, should the vd keep intact or use some pre-defined value. Similary, one can specify when id not in dt but find in tb, should it be added into dt or not. Both id and vd can be multiple columns. Value update operations are in situ in default for space and efficiency, e.g., the dt input is being changed in place without copying. This feature can be turned off with in\_situ = FALSE. Insertion operations when nofound = TRUE are not in situ, so one still need to assign the result back - would prefer also in situ?

    This function is inspired by a task on updating sale and inventory. The primary data.table (dt) keeps historical sale and inventory up to yesterday, and the reference data.table (tb) contains today's sale and inventory. Every day, matched id should get updated sale and inventory vd from tb. When id in dt does not find a match in tb, sale should be set to 0 with nomatch = 0, and inventory should keep intact. When id not in dt is shown in tb, new products added in catalogue, and new id should be inserted into primary dt with nofound = TRUE.

``` r

# dtRfrhTb

# master table (dHist): fulfillment center code (fcc), stock keeping unit (sku), 
# begin quantity (bgn), end quantity (end), and sales as shipped quantity (spt).
# note: bgn != end + spt because change in inventory such as new stock comes in, 
# write off and etc. can often happen.

set.seed(285714L)

(dHist <- data.table(
  fcc = c(rep("NJ", 3L), rep("TX", 4L)),
  sku = c(paste0("A", 1L:3L), paste0("A", 1L:4L)),
  bgn = sample(40L, 7L),
  end = sample(20L, 7L),
  spt = sample(10L, 7L)
))
#>    fcc sku bgn end spt
#> 1:  NJ  A1  26   8   8
#> 2:  NJ  A2   1  11   3
#> 3:  NJ  A3  31   1   5
#> 4:  TX  A1  29  14   2
#> 5:  TX  A2  37  17   4
#> 6:  TX  A3  35  16   6
#> 7:  TX  A4  32  10   1

# refresh master table with new day sale and inventory change data - day by day.
# sale and inventory might not match as order - packing - shipping in processing

(dSale <- data.table(
  fcc = c("NJ", "NJ", "TX", "TX"),
  sku = c("A1", "A4", "A1", "A4"),
  spt = c(1L, 2L, 3L, 4L)
))
#>    fcc sku spt
#> 1:  NJ  A1   1
#> 2:  NJ  A4   2
#> 3:  TX  A1   3
#> 4:  TX  A4   4

(dInvt <- data.table(
  fcc = c("NJ", "NJ", "TX", "TX"),
  sku = c("A1", "A4", "A4", "A5"),
  bgn = sample(10L, 4L),
  end = sample(10L, 4L)
))
#>    fcc sku bgn end
#> 1:  NJ  A1   6   7
#> 2:  NJ  A4   9   5
#> 3:  TX  A4   5  10
#> 4:  TX  A5   1   4

# refresh master tbl: use fcc and sku together as id when id nomatch set vd to 0 
# as no sale, and when id nofound in dt (dHist) will be added as new sku in fcc.

(dHist <- dtRfrhTb(
  dt = dHist, tb = dSale, id = c("fcc", "sku"), vd = "spt", 
  nomatch = 0L, nofound = TRUE, in_situ = TRUE
))
#> dtRfrhTb: insert id in tb not in dt into dt.
#>    fcc sku bgn end spt
#> 1:  NJ  A1  26   8   1
#> 2:  NJ  A2   1  11   0
#> 3:  NJ  A3  31   1   0
#> 4:  TX  A1  29  14   3
#> 5:  TX  A2  37  17   0
#> 6:  TX  A3  35  16   0
#> 7:  TX  A4  32  10   4
#> 8:  NJ  A4  NA  NA   2

(dHist <- dtRfrhTb(
  dt = dHist, tb = dInvt, id = c("fcc", "sku"), vd = c("bgn", "end"),
  nomatch = NULL, nofound = TRUE, in_situ = TRUE
))
#> dtRfrhTb: insert id in tb not in dt into dt.
#>    fcc sku bgn end spt
#> 1:  NJ  A1   6   7   1
#> 2:  NJ  A2   1  11   0
#> 3:  NJ  A3  31   1   0
#> 4:  TX  A1  29  14   3
#> 5:  TX  A2  37  17   0
#> 6:  TX  A3  35  16   0
#> 7:  TX  A4   5  10   4
#> 8:  NJ  A4   9   5   2
#> 9:  TX  A5   1   4  NA

dtFillNA(dt = dHist, fillNA = 0)
#>    fcc sku bgn end spt
#> 1:  NJ  A1   6   7   1
#> 2:  NJ  A2   1  11   0
#> 3:  NJ  A3  31   1   0
#> 4:  TX  A1  29  14   3
#> 5:  TX  A2  37  17   0
#> 6:  TX  A3  35  16   0
#> 7:  TX  A4   5  10   4
#> 8:  NJ  A4   9   5   2
#> 9:  TX  A5   1   4   0
```

`sqlite_<>` tools:
------------------

-   `sqlite_<>` tools realize all database managements on a file based concept - one need only provide a **<sqlite>.db file** as db for input, instead of a **connection to dbfile**.

-   `sqlite_refreshtb` and `sqlite_subsetidx` worth a separate mention:

    -   `sqlite_refreshtb`: refreshtb will update value when primary key exist, and also insert new row when primary key not exist.

    -   `sqlite_subsetidx`: given a r data.table (dt) with id column, fetch from sqlite database (db) table (tb) based on id for all rows - works efficient when dt and tb both have millions or hundreds of millions of rows.

`sqodbc_<>` tools:
------------------

-   `sqodbc_<>` tools realize all database managements on file based concept - one need only provide a **<sqodbc>.db list** as db for input, instead of a **connection to RODBC server**. A <sqodbc>.db list should contains all information for establishing a connection, e.g., it should be a list of srv (sql server address/ip), usr (username) and pwd (password), or winAuth (default is FALSE and require usr and pwd, specify TRUE when connection will be established using windows authentication and not require usr and pwd), and dbn (database name in sql server, e.g., master). A dsn (database server name - as in windows odbc data source administrator) was used in earlier version, but removed in current version, instead using srv, usr, pwd (or winAuth) and dbn - in this way, it is self-contained in R, no need to specify dsn in system settings.

``` r

# sqodbc db list prototype

sqodbc_dblist <-function() {

  list(
    
    # dsn = 'databaseSeverNameInWinODBC',

    srv = 'one-sql.database.windows.net',

    usr = 'username',

    pwd = 'password',

    dbn = 'database-in-sql-server'

  )

}

sqodbc_dblist_with_winAuth <-function() {

  list(

    # dsn = 'databaseSeverNameInWinODBC',
    
    srv = 'one-sql.database.windows.net',

    dbn = 'database-in-sql-server',
    
    winAuth = TRUE

  )

}


# list all tables in database in sql-server
sqodbc_executeqy(
  qy = "select * from INFORMATION_SCHEMA.TABLES", db = sqodbc_dblist
)

# use windows authentication for connection
sqodbc_executeqy(
  qy = "select * from INFORMATION_SCHEMA.TABLES", db = sqodbc_dblist_with_winAuth
)
```

-   `sqodbc_refreshtb` and `sqodbc_subsetidx` are corresponding implementation of `sqlite_refreshtb` and `sqlite_subsetidx`.

bcp tools - often much faster than `sqodbc_<>` equivalents:
-----------------------------------------------------------

-   `bcp_azure_table`: wrapper around bcp out - download sql table into local file. An input db should be a list of srv, usr and pwd or winAuth, and dbn - same as db list used in sqodbc tools.

-   `bcp_azure_query`: wrapper around bcp queryout - download query result into local file.

-   `bcp_azure_inrdt`: wrapper around bcp in - upload r data.table or data.frame into sql server. R data.table and data.frame should not have columns of lists. Function can auto detect R data type, integer, numeric and character and specify corresponding format when uploading.

    **TODO**: should add logical in auto data type determination: `det_col_type`.
