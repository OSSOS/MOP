insert into source_idx ( sourceID, ix, jy, kz ) 
SELECT sourceID, 
convert((360.0*COS(RADIANS(raDeg))*COS(RADIANS(decDeg))),SIGNED), 
convert((360.0*SIN(RADIANS(raDeg))*COS(RADIANS(decDeg))),SIGNED),
convert((360.0*SIN(RADIANS(decDeg))),SIGNED) FROM source 
