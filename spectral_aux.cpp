#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
double odleglosc ( NumericMatrix X, int i, int j){
  int n=X.nrow();
  double sum=0;
  for(int l=0;l<n;l++)
  {
    sum=sum+ ((X(j,l)-X(i,l))*(X(j,l)-X(i,l)));
  }
  sum = pow(sum,(double)1.0/2);
  return sum;
}



// [[Rcpp::export]]
NumericMatrix macierz_odleglosci( NumericMatrix X){
  int n= X.nrow();
  NumericMatrix O(n,n);
  for(int j=0; j<n; j++)
  {
    for( int i=0;i<n;i++)
    {
      if(i==j)
      {
        O(j,i)=0;
      }
      else
      { 
        O(j,i)=odleglosc(X,j,i);
      }
    }
  }
  return O;
}



bool porownanie_par(std::pair<int,double> &a, std::pair<int,double> &b){
  return (a.second<b.second);
}
// [[Rcpp::export]]
NumericMatrix Mnn(NumericMatrix X, int M){
  int n= X.nrow();
//int d= X.ncol();
  NumericMatrix S = NumericMatrix(n,M);
  for(int j=0; j<n; j++ ){
    //para trzymajaca obiekty typu int i double 
    std::vector<std::pair<int,double> > s(n);
    for(int i=0;i<n;i++){
      s[i].first=i+1;
      s[i].second=odleglosc(X,j,i);
    }
    partial_sort(s.begin(),s.end(), s.begin()+M, porownanie_par);
    
    for(int i=0; i<M; i++){
      S(j,i)=s[i+1].first;
    }
  }
  return S;
}

// [[Rcpp::export]]
NumericMatrix Mnn_graph(NumericMatrix S){
  
  int n=S.nrow();
  int M=S.ncol(); 
  NumericMatrix G(n,n);
  for(int i=0; i<n; i++)
  {
    for(int j=0;j<M;j++)
    {
      int a= S(i,j);
      G(i,a-1) = 1;
    }
  }
  return G; 
  
}







