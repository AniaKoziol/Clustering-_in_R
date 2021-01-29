Rcpp::sourceCpp("C:/Users/Ania/Desktop/Studia-SMAD/Analiza danych w R/PD4/ostatnia.cpp")
install.packages("igraph")
library(igraph)


# spectral_clustering function(A)
# {
  A <- Mnn(A) 
  S <- Mnn_graph(S)  
  G <- uspolnienie(S) 
  E <- Laplacian_eigen(G)
  E<- k_means(E)
#   
# }


uspojnianie <- function(S){
 
 S<-  Mnn_graph(S) 
S<- graph_from_adjacency_matrix(S)
skladowe_spojnosi <- clusters(S)
grupy_ilosc <- skladowe_spojnosi$no
for(i in 1:grupy_ilosc-1)
{
}

}


Laplacian_eigen <- function(G,k)
{
  D<- diag(G)
  E <- eigen(D - G)
}















