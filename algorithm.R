#Chad Foley
#4/19/15
#Stat 261
#Assignment 9 add on add on
#Algorithm for c/n optimization

c = 0
n = 0
g1 = 1 - pbinom(q = c, size = n, prob = 0.2)
g2 = 1 - pbinom(q = c, size = n, prob = 0.6)
cat(paste("g1 =", g1), paste("  g2 =", g2), paste("  n =", n), paste("  c =", c))
cat("\n")
while (g1 > 0.05 | g2 < 0.9){
    while (g2 < 0.9){
        n = n + 1
        g2 = 1 - pbinom(q = c, size = n, prob = 0.6)
        g1 = 1 - pbinom(q = c, size = n, prob = 0.2)
        cat(paste("g1 =", g1), paste("  g2 =", g2), paste("  n =", n), paste("  c =", c))
        cat("\n")
    }
    #g1 = 1 - pbinom(q = c, size = n, prob = 0.2)
    while (g1 > 0.05){
        c = c + 1
        g1 = 1 - pbinom(q = c, size = n, prob = 0.2)
        g2 = 1 - pbinom(q = c, size = n, prob = 0.6)
        cat(paste("g1 =", g1), paste("  g2 =", g2), paste("  n =", n), paste("  c =", c))
        cat("\n")
    }
    #g2 = 1 - pbinom(q = c, size = n, prob = 0.6)
}



    