# final data formatting for model run ----

ID_ch<-everyone_ch_SA%>%arrange(POD, NAME)%>%dplyr::select(ind,NAME,POD,pod_ch,SEX,sex_ch)

# observation data
obs_it<-everyone_ch_SA%>%dplyr::select(-ind,-POD,-pod_ch,-NAME,-SEX,-sex_ch)%>%
  as.matrix()

# effort
eff_it<-long_samp_ch_SA%>%
  dplyr::select(-POD)%>%
  as.matrix()

eff_it[eff_it > 0]<-1
eff_mat<-unname(eff_it)

# occasions
occ<-names(everyone_ch_SA)[5:(ncol(everyone_ch_SA)-3)]

obs_ch_mat<-unname(obs_it)
obs_ch_mat[is.na(obs_ch_mat)]<-0
obs_ch_mat[1,]

# number of individuals 
n_ind <- nrow(obs_ch_mat) 

# number of capture occasions
n_occ <- ncol(obs_ch_mat)

# number Doubtful
doubtful_n<-everyone_ch_SA%>%filter(POD == "DOUBTFUL")%>%nrow()
doubtful_mat<-obs_ch_mat[1:doubtful_n,]

# number Dusky
dusky_mat<-obs_ch_mat[(doubtful_n+1):n_ind,]

doubtful_sum<-NULL

for (j in 1:n_occ){
  doubtful_sum[j]<-sum(doubtful_mat[j,])
  
}

dusky_sum<-NULL

for (j in 1:n_occ){
  dusky_sum[j]<-sum(dusky_mat[j,])
  
}

# first capture
get.first<- function(x) min(which(x!=0))
f<-apply(obs_ch_mat, 1, get.first)
f

# model ----
# model built in phi_model.R
source('./scripts/phi_model.R', local = TRUE)$value

## data ----
mcmc.data<-list(
  eff = eff_mat,
  y = obs_ch_mat,
  n_ind = nrow(obs_ch_mat),
  f = f,
  doubtful_n = doubtful_n,
  n_occ = ncol(obs_ch_mat),
  pod = ID_ch$pod_ch) 

## run model ----
rjags::load.module("glm")
m1 = rjags::jags.model("pod_surv_cap-2026.txt", data = mcmc.data, inits = mcmc.inits, n.chains = 3, n.adapt = 5000)
update(m1) # another burn in
out1 = coda.samples(model = m1, variable.names = mcmc.params, n.iter = 20000)

## draws ----
out1_df = posterior::as_draws_df(out1)