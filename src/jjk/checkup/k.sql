select official,qname,max(mjdate)-min(mjdate) as arc from measure m 
join object o on m.provisional like o.provisional
join bucket.blocks b on b.expnum=convert(substring(m.file_id,1,6),unsigned)
group by qname,official
