#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
int CPL_sfc_n_empty(List sfc){
  int n = sfc.length();
  int n_empty = 0;
  for(int i = 0; i<n; i++){
    RObject sfg = sfc[i];
    int geom  = 0;

    if(sfg.inherits("POINT"))
      geom = 1;

    if(sfg.inherits("MULTIPOINT"))
      geom = 2;
    if(sfg.inherits("LINESTRING"))
      geom = 2;
    if(sfg.inherits("CIRCULARSTRING"))
      geom = 2;
    if(sfg.inherits("CURVE"))
      geom = 2;

    switch(geom){
      case 0:
        {
        List l = as<List>(sfg);
        if(l.length()==0)
          n_empty++;
        continue;
        }
      case 1:
        {
        NumericVector pt = as<NumericVector>(sfg);
        if(is_false(all(is_finite(pt))))
          n_empty++;
        continue;
        }
      case 2:
        {
        NumericMatrix mat = as<NumericMatrix>(sfg);
        if(mat.nrow()==0)
          n_empty++;
        continue;
        }
      default:
        stop("no case");
    }
  }
  return(n_empty);
}

