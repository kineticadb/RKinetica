# RKinetica Connector Changelog

## Version 7.1

### Version 7.1.1.0 -- 2020-09-22

#### Added

-   Support for schema naming of Kinetica DB tables

### Version 7.1.0.0 -- 2020-07-27

#### Changed

-   Version release


## Version 7.0

### Version 7.0.1.3 -- 2020-01-07

#### Fixed

-   An error in JSON deserialization associated with
    handling non-JSON special literals NaN and Infinity.

#### Removed

    Connection option `assume_no_nulls`, since all JSON
    parsing is done through RJSONIO package with built-in
    NULL-handling.

### Version 7.0.1.2 -- 2019-11-14

#### Fixed

-   An error in NULL-handling behavior logic of `FALSE` path
    of `assume_no_nulls` connection option.

### Version 7.0.1.1 -- 2019-11-04

#### Added

-   Connection option `assume_no_nulls` to allow faster
    JSON deserializing of Kinetica result when the expected
    result is not expected to contian NULL values. When the
    flag `assume_no_nulls=TRUE`, JSON parsing uses *purrr*
    library for faster data processing. When flag value is
    `FALSE` (default value), it uses *rjson* to provide
    accurate NULL-handling.

### Version 7.0.1.0 -- 2019-09-21

#### Added

-   High Availability configuration support with options to
    either expect on automatic discovery of backup clusters
    when primary Kinetica DB instance is HA-enabled or to
    configure KineticaConnection manually by adding `ha_ring`
    parameter to initial dbConnect() call.

-   Pagination for queries where results are expected to exceed
    `row_limit` of Kinetica Connection. (Available in
    in dbSendQuery() and dbSendStatement() methods through
    `limit` and `offset` parameters.)

### Version 7.0.0.2 -- 2019-09-11

#### Added

-   Support of SSL connection with self-signed certificates.

### Version 7.0.0.1 -- 2019-06-04

#### Added

-   Configurable row_limit parameter to KineticaConnection object
-   Comprehensive examples folder
-   Improved package documentation

#### Fixed

-   Prepared statement parameter handling

### Version 7.0.0.0 -- 2019-03-15

-   Initial version
