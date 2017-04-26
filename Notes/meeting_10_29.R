# meeting.r

sum(sapply(1:29, function(x) { 
  distCosine(lines[x,c(1,2)],lines[x+1,c(1,2)])/1600}))

sum(sapply(1:29, function(x) { 
    distGeo(lines[x,c(1,2)],lines[x+1,c(1,2)])/1600}))
