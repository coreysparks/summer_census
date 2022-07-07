library(sf)
lrs <- st_read("C:/Users/ozd504/OneDrive - University of Texas at San Antonio/projects/census_summer/data/PDBresources20220511120308/pdb2021_tract_US/pdb2021_tract_US.shp")

library(tidyverse)
tx_lrs <- lrs%>%
  filter(STATEFP==48)%>%
  st_transform(crs = 2163)

library(INLA)
library(spdep)

tx_lrs$struct<-1:dim(tx_lrs)[1]  
tx_lrs$struct2<-1:dim(tx_lrs)[1]  
nbs<-knearneigh(st_centroid(tx_lrs), k = 5, longlat = T) #k=5 nearest neighbors
nbs<-knn2nb(nbs, row.names = tx_lrs$struct, sym = T) #force symmetry!!
mat <- nb2mat(nbs, style="B",zero.policy=TRUE)
colnames(mat) <- rownames(mat) 
mat <- as.matrix(mat[1:dim(mat)[1], 1:dim(mat)[1]])

saveRDS(tx_lrs, file = "C:/Users/ozd504/Github/summer_census/data/tx_lrs.rds")

nb2INLA("cl_graph",nbs)
am_adj <-paste(getwd(),"/cl_graph",sep="")
H<-inla.read.graph(filename="cl_graph")

library(tigris)
sts<- counties(cb=T, state = 48)%>%
  st_transform(crs= 2163)

library(tmap)
tmap_mode("view")  
tm_shape(tx_lrs)+
  tm_polygons("Low_Respon", style="quantile", border.col=NA, border.alpha=0)+
tm_shape(sts)+
  tm_borders(col = "black", lwd=1.5)
#filter(year%in%c(2000))%>%
  # mutate(qrr=cut(I(deaths/E_d), 
  #                breaks = quantile(I(deaths/E_d),
  #                                  p=seq(0,1,length.out = 5)),
  #                include.lowest = T))%>%
  # ggplot()+
  # geom_sf(aes(fill=Low_Respon, color=NA))+
  # geom_sf(data=sts, color="black")+
  # scale_colour_viridis_b()
  # #scale_fill_brewer(palette = "RdBu", na.value="grey")+
  # guides(fill=guide_legend(title="Response Score"))+
  # ggtitle(label="Low Response Score - Texas Tracts")+
  # coord_sf(crs = 2163)

f0<-Low_Respon~1+
  #f(struct, model = "besag",scale.model = T, constr = T, graph = H)+
  f(struct2, model = "iid")

#Model fit
mod0<-inla(formula = f0,data = tx_lrs, #linear predictor - fixed effects
           family = "gaussian",  #marginal distribution for the outcome, expected count
           control.compute = list(waic=T), # compute DIC or not?
           control.predictor = list(link=1), #estimate predicted values & their marginals or not?
           num.threads = 3, 
           verbose = F)


f1<-Low_Respon~1+
  f(struct, model = "besag",scale.model = T, constr = T, graph = H)+
  f(struct2, model = "iid")

#Model fit
mod1<-inla(formula = f1,data = tx_lrs, #linear predictor - fixed effects
           family = "gaussian",  #marginal distribution for the outcome, expected count
           control.compute = list(waic=T), # compute DIC or not?
           control.predictor = list(link=1), #estimate predicted values & their marginals or not?
           num.threads = 3, 
           verbose = F)
#model summary
summary(mod1)

mod0$waic$waic
mod1$waic$waic

saveRDS(mod1, file = "C:/Users/ozd504/Github/summer_census/data/inlamod1.rds")

#### Hyper parameter distributions

#Green line is spatial variation, black line is nonspatial variation


m3a<- inla.tmarginal(
  function(x) (1/x),
  mod1$marginals.hyperpar$`Precision for struct`)
m3b<- inla.tmarginal(
  function(x) (1/x),
  mod1$marginals.hyperpar$`Precision for struct2`)

plot(m3a, type="l",
     main=c("Posterior distibution for between county variance", "- BYM model -"),ylim=c(0, .5),  xlim=c(0,75)) # you may need to change this
#lines(m3b, col="red")
lines(m3b, col="green")

tx_lrs$spre <- mod1$summary.random$struct$mean
tx_lrs$i_re<-mod1$summary.random$struct2$mean

tmap_mode("view")  
tm_shape(tx_lrs)+
  tm_polygons("spre", style="quantile", border.col=NA, border.alpha=0)+
  tm_shape(sts)+
  tm_borders(col = "black", lwd=1.5)


#IID random effect


tm_shape(tx_lrs)+
  tm_polygons("i_re", style="quantile", border.col=NA, border.alpha=0)+
  tm_shape(sts)+
  tm_borders(col = "black", lwd=1.5)

