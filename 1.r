 p.es = function(n, es, n.sim){
  
t.sim = Vectorize(function(n, es){
    
    d = numeric(n.sim)
    p = numeric(n.sim)
    
for(i in 1:n.sim){
    N = sqrt((n^2)/(2*n))
    x = rnorm(n, es, 1)
    y = rnorm(n, 0, 1)
    a = t.test(x, y, var.equal = TRUE)
 d[i] = a[[1]]/N
 p[i] = a[[3]]
    }
h = hist(p, breaks = 15, xlab = "p-value", main = paste0("sig. p-values = ", mean(p <= .05)*1e2, "%"))
rect(0, 0, h$breaks[2], h$counts[1], col = rgb(0, 0, 1, .4), border = 2)
mtext(paste0("Group Sample Size = ", n), 3, font = 2, cex = .7, xpd = NA)
axis(1, at = h$breaks[2], font = 2, col = 2, col.axis = 2)
hist(d, xlab = "Cohen's d", main = NA) ; abline(v = es, col = 2, lty = 2) ; axis(1, at = es, col = 2, col.axis = 2, font = 2)
  }, c("n", "es"))
  
  input = if(length(n) > 1) n else if(length(es) > 1) es else n
  
  par(mfcol = c(2, length(input)), font.lab = 2)
  invisible(t.sim(n, es))
}
# Example of use:
p.es(n = c(20, 50, 100), es = .1, n.sim = 1e3)