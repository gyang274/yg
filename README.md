<!-- README.md is generated from README.Rmd. Please edit that file -->
yg
==

yg implements a set of database management tools for communicating with SQLite or ODBC server such as Microsoft Azure SQL.

data.table tools:
-----------------

1.  CJ.dt: extends data.table::CJ to data.table. CJ.dt(X, Y) requires both X and Y be data.table and generate a fully combine of each row of X w.r.t each row of Y.

``` r
library(yg)
#> Loading required package: data.table
#> Loading required package: magrittr
#> Loading required package: RSQLite
#> Loading required package: DBI
#> Loading required package: RODBC

X <- data.table(a = c(TRUE, FALSE, FALSE), b = c(1L,2L,3L))
Y <- data.table(c = c(5.8,13.21,34.55), d = c("a","B","C"))

Z <- CJ.dt(X, Y)
```

1.  dtFillNA: data.table fill NA in situ, allows fill different values for numeric NA, integer NA and character NA.

``` r
X <- data.table(a = c(TRUE, FALSE, FALSE), b = c(1L,NA_integer_,3L))
Y <- data.table(c = c(5.8,13.21,NA_real_), d = c("a",NA_character_,"C"))

Z <- CJ.dt(X, Y)

dtFillNA(dt = Z, fillNA = 34.54, fillNA_I = 4L, fillNA_C = "b")
#> warning: column 1 is not integer, numeric or character.
#>        a b     c d
#> 1:  TRUE 1  5.80 a
#> 2: FALSE 4  5.80 a
#> 3: FALSE 3  5.80 a
#> 4:  TRUE 1 13.21 b
#> 5: FALSE 4 13.21 b
#> 6: FALSE 3 13.21 b
#> 7:  TRUE 1 34.54 C
#> 8: FALSE 4 34.54 C
#> 9: FALSE 3 34.54 C
```

1.  dtRfrhTb: update primary data.table (dt) with a reference data.table (tb) based on matching id value on id columns, and update primary data.table vd value on vd columns using refernce data.table vd valude on vd columns. Also, one can specify when id in dt not find a match in tb, should the vd keep intact or use some pre-defined value. Similary, one can specify when id not in dt but find in tb, should it be added into dt or not. Both id and vd can be multiple columns. Value update operations are in situ in default for space and efficiency, e.g., the dt input is being changed in place without copying. This feature can be turned off by in\_situ = FALSE. Insertion operations when nofound = TRUE are not in situ, so one still need to assign the result back - would prefer also in situ?

This function is inspired by a task on updating sale and inventory. The primary data.table (dt) keeps historical sale and inventory up to yesterday, and the reference data.table (tb) contains today's sale and inventory. Every day, matched id should get updated sale and inventory vd from tb. When id in dt does not find a match in tb, sale should be set to 0 with nomatch = 0, and inventory should keep intact. When id not in dt is shown in tb, new products added in catalogue, and new id should be inserted into primary dt with nofound = TRUE.

``` r

# master table with fullfillment center code, sku, 
# begin quantity (bgn), end quantity (end) and 
# shipped quantity (spt) which is sale.
# bgn != end + spt because change in inventory can
# often happen: new stock, write off and etc.

set.seed(285714)

dHist <- data.table(fcc = c(rep("NJ", 3), rep("TX", 4)),
                    sku = c(paste0("A", 1:3), paste0("A", 1:4)),
                    bgn = sample(40, 7),
                    end = sample(20, 7),
                    spt = sample(10, 7))

# sale and inventory might not match because delay
# in processing orders: order - packing - shipping

dSale <- data.table(fcc = c("NJ", "NJ", "TX", "TX"),
                    sku = c("A1", "A4", "A1", "A4"),
                    spt = c(1L, 2L, 3L, 4L))

dInvt <- data.table(fcc = c("NJ", "NJ", "TX", "TX"),
                    sku = c("A1", "A4", "A4", "A5"),
                    bgn = sample(10, 4),
                    end = sample(10, 4))
dHist
#>    fcc sku bgn end spt
#> 1:  NJ  A1  26   8   8
#> 2:  NJ  A2   1  11   3
#> 3:  NJ  A3  31   1   5
#> 4:  TX  A1  29  14   2
#> 5:  TX  A2  37  17   4
#> 6:  TX  A3  35  16   6
#> 7:  TX  A4  32  10   1

dSale
#>    fcc sku spt
#> 1:  NJ  A1   1
#> 2:  NJ  A4   2
#> 3:  TX  A1   3
#> 4:  TX  A4   4

dInvt
#>    fcc sku bgn end
#> 1:  NJ  A1   6   7
#> 2:  NJ  A4   9   5
#> 3:  TX  A4   5  10
#> 4:  TX  A5   1   4

# use fcc and sku together as id - id nomatch set vd to 0, id nofound in dt will be added
dHist <- dtRfrhTb(dt = dHist, tb = dSale, id = c("fcc", "sku"), vd = "spt", nomatch = 0L, nofound = TRUE, in_situ = TRUE)
#> dtRfrhTb: insert id in tb not in dt into dt.
  
dHist
#>    fcc sku bgn end spt
#> 1:  NJ  A1  26   8   1
#> 2:  NJ  A2   1  11   0
#> 3:  NJ  A3  31   1   0
#> 4:  TX  A1  29  14   3
#> 5:  TX  A2  37  17   0
#> 6:  TX  A3  35  16   0
#> 7:  TX  A4  32  10   4
#> 8:  NJ  A4  NA  NA   2

dHist <- dtRfrhTb(dt = dHist, tb = dInvt, id = c("fcc", "sku"), vd = c("bgn", "end"), nomatch = NULL, nofound = TRUE, in_situ = TRUE)
#> dtRfrhTb: insert id in tb not in dt into dt.

dHist
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

sqlite\_&lt;&gt; tools:
-----------------------

-   sqlite\_&lt;&gt; tools realize all database managements on file based concept - one need only provide a <sqlite>.db file as db for input, instead of a connection to dbfile.

-   sqlite\_refreshtb and sqlite\_subsetidx worth a separate mention:

    -   sqlite\_refreshtb: refreshtb will update value when primary key exist, and also insert new row when primary key not exist.

    -   sqlite\_subsetidx: given a r data.table (dt) with id column, fetch from sqlite database (db) table (tb) based on id for all rows - works efficient when dt and tb both have millions or hundreds of millions of rows.

sqodbc\_&lt;&gt; tools:
-----------------------

-   sqodbc\_&lt;&gt; tools realize all database managements on file based concept - one need only provide a <sqodbc>.db list as db for input, instead of a connection to RODBC server. A <sqodbc>.db list should contains all information for establishing a connection, e.g., it should be a list of \[dsn\] (database server name - as in windows odbc data source administrator), srv (sql server address/ip), usr (username) pwd (password) and dbn (database name, e.g., master) - \[dsn\] was used in earlier version, but removed so using srv, usr, pwd and dbn. In this way, it is self-contained in R, no need to specify dsn in system settings.

    *TODO*: should sqodbc\_createcnn() being added a capabilty to establish connection using windows authentication, rather than using <sqodbc>.db list only for security purpose?

-   sqodbc\_refreshtb and sqodbc\_subsetidx are corresponding implementation of sqlite\_refreshtb and sqlite\_subsetidx.

bcp tools - often faster than sqodbc\_&lt;&gt; equivalents:
-----------------------------------------------------------

-   bcp\_azure\_table: wrapper around bcp out - download sql table into local file. An input db should be a list of srv usr pwd and dbn.

-   bcp\_azure\_query: wrapper around bcp queryout - download query result into local file.

-   bcp\_azure\_inrdt: wrapper around bcp in - upload r data.table or data.frame into sql server. R data.table and data.frame should not have columns of lists. Function can auto detect R data type, integer, numeric and character and specify corresponding format when uploading.

    *TODO*: should add logical in auto data type determination: det\_col\_type.
