standardize = function (X) 
{
   N = nrow(X)
   D = ncol(X)
   NX = X
   for (i in 1:D) {
      mXi = mean(X[,i])
      sXi = sd(X[,i])
      NX[,i]=(X[,i] - mXi)/sXi
   }
   return(NX)
}


library(SOMbrero)
X = read.delim("LiteratureSearchDataindividualpatients.tsv", header=T, stringsAsFactor=T)
N = ncol(X)

#empty cells are NA.
for (i in 3:N) {
 X[which(X[,i] == ""), i] = NA
}

#give a large number to days to death for alive patients.
X$Days.to.death..NA.if.alive.[is.na(X$Days.to.death..NA.if.alive.)] = 15000

#convert NA to mean
for (i in 3:N) {
 if (class(X[,i]) == "integer") {
     mm = mean(X[!is.na(X[,i]), i])
     X[is.na(X[,i]),i] = mm
     next
 } 
 if (class(X[,i]) == "numeric") {
     mm = mean(X[!is.na(X[,i]), i])
     X[is.na(X[,i]),i] = mm
     next
 }
 if (class(X[,i]) == "factor") {
    lpp = length(levels(X[,i]))+1
    X[,i]= as.numeric(X[,i])
    X[is.na(X[,i]),i] = lpp
    next
 }
 if (class(X[,i]) == "logical") {
    X[,i]= as.numeric(X[,i])
    X[is.na(X[,i]),i] = 2
    next
 }
}
for (i in N:3) {
   if (sum(X[,i]) == 0) X=X[,-i]
   else if (is.na(sd(X[,i]))) X=X[,-i]
   else if (sd(X[,i]) == 0) X=X[,-i]
}

n = ncol(X)
Y = standardize(X[,3:n])

Z = Y[, c(1,2,3,7,9,10,11,13,14,19,20,23,25,27,28,30,21,33,34,35,36,37,39,40,41,42,48)] #columns with highest significativity.
m = 9 #this is the biggest number with topographic error = 0.
som = trainSOM(Z, dimension=c(m, m))
summary(som)

pdf("som_obs_hit.pdf")
   plot(som, what="obs")
dev.off()

pdf("som_obs_color.pdf")
   plot(som, what="obs", type="color")
dev.off()

pdf("som_proto_smooth.pdf")
plot(som, what="prototypes", type="smooth.dist")
dev.off()

pdf("som_proto_poly.pdf")
   plot(som, what="prototypes", type="poly.dist")
dev.off()

sc = superClass(som, k = 4)
pdf("som_sc_dendro.pdf")
   plot(sc)
dev.off()

pdf("som_sc_grid.pdf")
   plot(sc, type="grid")
dev.off()

pdf("som_sc_dendro3d.pdf")
   plot(sc, type="dendro3d")
dev.off()

pdf("obs_sc.pdf")
   plot(sc, type="hitmap")
dev.off()

NeuronSize = rep(0, m^2)
for (i in 1:m^2) {
   NeuronSize[i] = length(which(som$clustering == i))
}
pdf("NeuronVsNeuronSize.pdf")
   plot(NeuronSize, type="l", xlab="Neuron", ylab="Neuron Size")
dev.off()

pdf("som_obs_ns.pdf")
plot(som, neuron.col=rainbow(prod(som$parameters$the.grid$dim)), print.title=TRUE, the.titles = paste("NS ", NeuronSize))
dev.off()

#find Super cluser for each patient.
Z$SC = sc$cluster[som$clustering]

n = ncol(Z)
KMX = matrix(,(n-1),2)
for (i in 1:(n-1)) {
   krs = kruskal.test(Z[,i]~Z[,n])
   KMX[i,1] = i
   KMX[i,2] = krs$p.value
}
write.table(KMX, "kmx.txt", sep="\t", quote=FALSE)
for (i in 1:(n-1)) {
   pdf(paste(names(Z)[i], ".pdf", sep=""))
   bp = boxplot(Z[,i]~Z[,n], plot=T, xlab="Super Cluster", ylab=names(Z)[i])
   dev.off()
}

