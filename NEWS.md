<!-- README.md is generated from README.Rmd. -->
yg
==

Development Log
---------------

### 2016.07.22

-   Add `winAuth` on `bcp_azure_table`, `bcp_azure_query`, `bcp_azure_inrdt`and `sqodbc_createcnn` for connecting to SQL server using windows authentication. To use windows authentication, user must specify database list `db` with an compenent of `winAuth = TRUE`, e.g., `db[["winAuth"]] = TRUE`. The windows authentication when specified will override username and password when establish connections.

### 2016.07.14

-   Add `[]` around `db[["dbn"]]` in `qy_fmt` in `sqlite.r::bcp_azure_inrdt()`, in case `db[["dbn"]]` is a none-standard database name, e.g., my-database with "-". The `tb` name should be also added `[]` when it is a none-standard name, rely on user, e.g., when the `tb` name is `dbo.a-tbl` then must be specified as `[dbo].[a-tbl]`. The `db[["dbn"]]` is used also in establishing connections, so cann't rely on user to specify with `[]`.
