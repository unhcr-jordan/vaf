####################################################################################
#### Apply MCA (Multiple Correspondance Analysis) to Categorical data
####################################################################################
## http://rpubs.com/gaston/MCA

#### Loading the required R module
library("FactoMineR")

##############
#### Loading the data
##############

homevisitall <- read.csv("data/homevisit6.csv")
homevisitr <- homevisitall[, c( "food","household","water","sanitation","sheltype","accestype","conditiono","splitfam")]
names(homevisitr)
homevisit1 <-sample(homevisitr, 8 ,replace = TRUE)
homevisit1[sample(homevisitr, 8 , replace = FALSE), 5000]
# View(homevisitall)
# summary(homevisitall)

homevisit6min <- read.delim("data/homevisit6min.csv")
homevisit1 <- homevisit6min[, c( "food","household","water","sanitation","sheltype","accestype","conditiono","splitfam","level","location1")]





homevisit1.mca<-MCA(homevisit1)
summary(homevisit1.mca)

print(homevisit1.mca, nb.dec = 3, nbelements=10, ncp = 3, align.names=TRUE, file = "out/summary-mca.txt")


#########
# Plotting the MCA with better readibility
#########
plot.MCA(homevisit1.mca, axes=c(1, 2), col.ind="black", col.ind.sup="blue", col.var="darkred", col.quali.sup="darkgreen", label=c("ind", "ind.sup", "quali.sup", "var", "quanti.sup"), invisible=c("none", new.plot=TRUE))
plot.MCA(homevisit1.mca, axes=c(1, 2), choix="var", col.var="darkred", col.quali.sup="darkgreen", label=c("var", "quali.sup"), invisible=c("none", new.plot=TRUE))
plot.MCA(homevisit1.mca, axes=c(1, 2), choix="quanti.sup", col.quanti.sup="blue", label=c("quanti.sup", new.plot=TRUE))

# plotellipses(homevisit1.mca)


### Describing axis
round(homevisit1.mca$eig,2)
dimdesc(homevisit1.mca,axes=1:2)
dimdesc(homevisit1.mca,axes=1:2,proba=0.05)
dimdesc(homevisit1.mca,axes=1:2,proba=0.20)

### Category description
#res.catdes <- catdes(homevisit1.mca, num.var=2, proba = 0.05)
#plot(res.catdes)
###################
#  clear graphics settings so that it works with multiple windows
dev.off()
Sys.setenv("DISPLAY"=":0.0")

capabilities()
sessionInfo()
options(device="X11")



##################
#### hierarchic clustering
##################
# unable to start device X11cairo  --  
homevisit.hcpc<-HCPC(homevisit1.mca ,nb.clust=-1,consol=TRUE,min=3,max=6,graph=TRUE)


homevisit.hcpc<-HCPC(homevisit1.mca ,nb.clust=-1,consol=TRUE,min=3,max=6,graph=FALSE)
print(homevisit.hcpc$desc.var, file = "out/cluster-description.txt")
print(homevisit.hcpc, file = "out/summary-hpc.txt")
plot(homevisit.hcpc)

#  "$data.clust"           "dataset with the cluster of the individuals"            
#  "$desc.var"             "description of the clusters by the variables"           
#  "$desc.var$test.chi2"   "description of the cluster var. by the categorical var."
#  "$desc.axes$category"   "description of the clusters by the categories."         
#  "$desc.axes"            "description of the clusters by the dimensions"          
#  "$desc.axes$quanti.var" "description of the cluster var. by the axes"            
#  "$desc.axes$quanti"     "description of the clusters by the axes"                
#  "$desc.ind"             "description of the clusters by the individuals"         
#  "$desc.ind$para"        "parangons of each clusters"                             
#  "$desc.ind$dist"        "specific individuals"                                   
#   "$call"                 "summary statistics"                                     
#  "$call$t"               "description of the tree"  

# Describe some variables of the cluster
homevisit.hcpc$desc.var
homevisit.hcpc$desc.axes
homevisit.hcpc$desc.ind
homevisit.hcpc$call$t
homevisit.hcpc$desc.ind$para

# tell in which cluster each indivudal is allocated
print( homevisit.hcpc, file = "out/individual-cluster-allocation.txt", spet = $data.clust[,ncol(homevisit.hcpc$data.clust),drop=F])




############################
#### Visualisation of a classification with the cluster package
############################3
classif = agnes(homevisit1.mca$ind$coord,method="ward")
plot(classif,main="Dendrogram",ask=F,which.plots=2,labels=FALSE)



