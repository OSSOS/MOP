SELECT distinct(t.id),m.ccd 
 FROM triples t 
       JOIN discovery d ON t.id=d.triple 
       JOIN mosaic m
       JOIN bucket.association a ON t.pointing=a.pointing
       JOIN bucket.blocks b ON a.expnum=b.expnum
       LEFT JOIN processing p ON ( p.triple=d.triple AND p.ccd=m.ccd)
 WHERE p.status IS NULL 
       AND m.instrument LIKE 'MEGAPRIME' 
       AND b.qname LIKE '04BQ05A'
 order by t.id, m.ccd
