a <- t(SUMMARY(Beta, mean))

g <- SUMMARY2(Beta, quantile, probs=c(0.025,0.975))
int <- t(g[,,1])
area <- t(g[,,2])
wall <- t(g[,,3])
road <- t(g[,,4])

c.name <- c("Mean","Lower", "Upper", "Species")
r.name <- c("Intercept","Area","Wall","Road")

Crinia.sig.df <- as.data.frame(cbind(a[,1], c(int[1,1],area[1,1],wall[1,1],road[1,1]),
                               c(int[1,2],area[1,2],wall[1,2],road[1,2]), rep(colnames(Occur)[1],4)), row.names = r.name)
colnames(Crinia.sig.df) <- c.name
row.names(Crinia.sig.df) <- r.name

Lim.dumer.df <- as.data.frame(cbind(a[,2], c(int[2,1],area[2,1],wall[2,1],road[2,1]),
                                     c(int[2,2],area[2,2],wall[2,2],road[2,2]), rep(colnames(Occur)[2],4)), row.names = r.name)
colnames(Lim.dumer.df) <- c.name
row.names(Lim.dumer.df) <- r.name

Lim.peron.df <- as.data.frame(cbind(a[,3], c(int[3,1],area[3,1],wall[3,1],road[3,1]),
                                    c(int[3,2],area[3,2],wall[3,2],road[3,2]), rep(colnames(Occur)[3],4)), row.names = r.name)
colnames(Lim.peron.df) <- c.name
row.names(Lim.peron.df) <- r.name

Lim.tas.df <- as.data.frame(cbind(a[,4], c(int[4,1],area[4,1],wall[4,1],road[4,1]),
                                    c(int[4,2],area[4,2],wall[4,2],road[4,2]), rep(colnames(Occur)[4],4)), row.names = r.name)
colnames(Lim.tas.df) <- c.name
row.names(Lim.tas.df) <- r.name

Para.has.df <- as.data.frame(cbind(a[,5], c(int[5,1],area[5,1],wall[5,1],road[5,1]),
                                  c(int[5,2],area[5,2],wall[5,2],road[5,2]), rep(colnames(Occur)[5],4)), row.names = r.name)
colnames(Para.has.df) <- c.name
row.names(Para.has.df) <- r.name

Lit.ewing.df <- as.data.frame(cbind(a[,6], c(int[6,1],area[6,1],wall[6,1],road[6,1]),
                                   c(int[6,2],area[6,2],wall[6,2],road[6,2]), rep(colnames(Occur)[6],4)), row.names = r.name)
colnames(Lit.ewing.df) <- c.name
row.names(Lit.ewing.df) <- r.name

Lit.verr.df <- as.data.frame(cbind(a[,7], c(int[7,1],area[7,1],wall[7,1],road[7,1]),
                                    c(int[7,2],area[7,2],wall[7,2],road[7,2]), rep(colnames(Occur)[7],4)), row.names = r.name)
colnames(Lit.verr.df) <- c.name
row.names(Lit.verr.df) <- r.name

Lit.peron.df <- as.data.frame(cbind(a[,8], c(int[8,1],area[8,1],wall[8,1],road[8,1]),
                                   c(int[8,2],area[8,2],wall[8,2],road[8,2]), rep(colnames(Occur)[8],4)), row.names = r.name)
colnames(Lit.peron.df) <- c.name
row.names(Lit.peron.df) <- r.name

Lit.rani.df <- as.data.frame(cbind(a[,9], c(int[9,1],area[9,1],wall[9,1],road[9,1]),
                                    c(int[9,2],area[9,2],wall[9,2],road[9,2]), rep(colnames(Occur)[9],4)), row.names = r.name)
colnames(Lit.rani.df) <- c.name
row.names(Lit.rani.df) <- r.name

Beta.df <- rbind(Crinia.sig.df, Lim.dumer.df, Lim.peron.df, Lim.tas.df, Para.has.df, Lit.ewing.df, Lit.verr.df, Lit.peron.df,
                 Lit.rani.df)
write.csv(Beta.df,"Beta-df.csv")



Rho.m <- SUMMARY(Rho, mean)
Rho.quan <- SUMMARY2(Rho, quantile, probs=c(0.025,0.975))
Rho.quan.lower <- rbind(Rho.quan[,,1][1,],Rho.quan[,,2][1,],Rho.quan[,,3][1,],Rho.quan[,,4][1,],Rho.quan[,,5][1,],
                        Rho.quan[,,6][1,],Rho.quan[,,7][1,],Rho.quan[,,8][1,],Rho.quan[,,9][1,])
Rho.quan.upper <- rbind(Rho.quan[,,1][2,],Rho.quan[,,2][2,],Rho.quan[,,3][2,],Rho.quan[,,4][2,],Rho.quan[,,5][2,],
                        Rho.quan[,,6][2,],Rho.quan[,,7][2,],Rho.quan[,,8][2,],Rho.quan[,,9][2,])

write.csv(Rho.m,"Rho_mean.csv")
write.csv(Rho.quan.lower, "Rho_quan_lower.csv")
write.csv(Rho.quan.upper, "Rho_quan_upper.csv")
