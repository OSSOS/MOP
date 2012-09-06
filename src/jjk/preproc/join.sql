SELECT t.id,m.ccd 
FROM triples t 
JOIN discovery d ON t.id=d.triple 
LEFT JOIN processing p ON p.triple=t.id 
JOIN mosaic m 
WHERE m.ccd!=p.ccd OR p.triple IS NULL
