dha = c(2,3,6,8)/0.4
dha = c()
dt = sapply(dha,  function(x) x / seq(1.5,3.5,0.1))
hist(dt)
