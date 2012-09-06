### hashes that describe the db schema   
### tables hashes have the format "column_name" => "column_definition"

## exposure is the basic defining information about an exposure.
## expectation is a UNIQUE expnum as a record tracker
my %exposure = ( "expnum" => "INT NOT NULL" , 
		 "object" => "VARCHAR(20)",
		 "ra_deg" => "DOUBLE NOT NULL ",
		 "dec_deg" => "DOUBLE NOT NULL ",
		 "exptime" => "DOUBLE NOT NULL ",
		 "mjdate" => "DOUBLE NOT NULL",
		 "filter" => "VARCHAR(10)",
		 "naxis1" => "INT",
		 "naxis2" => "INT",
		 "chipid" => "INT",
		 "runid" => "VARCHAR(10)");

## wcs contains the "standard" wcs information for the exposure.
my %wcs = ( "expnum" => "INT NOT NULL PRIMARY KEY ",
	    "crval1" => "DOUBLE",
	    "crval2" => "DOUBLE",
	    "crpix1" => "DOUBLE",
	    "crpix2" => "DOUBLE",
	    "cd1_1"  => "DOUBLE",
	    "cd1_2"  => "DOUBLE",
	    "cd2_2"  => "DOUBLE",
	    "cd2_1"  => "DOUBLE" );


## status: Contains a status line about an exposure. 
## ie: what has been done to a FILE 
my %status = ( "file_id" => "VARCHAR(20) NOT NULL ",
	       "expnum"  => "INT NOT NULL",
	       "process" => "VARCHAR(20) NOT NULL ",
	       "hostname" => "VARCHAR(20)",
	       "command" => "VARCHAR(80)",
	       "result" => "BLOB",
	       "comment" => "VARCHAR(80)",
	       "status" => "TINYINT" );

## the myTables has contains a list of all the tables that are part of the sytem.
my %myTables = ( "status" => \%status, 
		 "source" => \%source,
		 "object" => \%object,
		 "exposure" => \%exposure, 
		 "wcs" => \%wcs
		 );

