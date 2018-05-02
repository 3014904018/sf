#include <RcppArmadillo.h>
using namespace Rcpp;
#define ADD  0
#define SUB  1
#define PROD 2
#define MOD  3
#define RANGE 10
// [[Rcpp::depends(RcppArmadillo)]]

RObject arith_geom_picker(RObject x, RObject y, NumericVector bbox, int i, int opp, int level);


int check_num_int(RObject x, RObject y){
  if(TYPEOF(x)!=REALSXP && TYPEOF(x)!=INTSXP)
    stop("X not numeric/integer");
  if(TYPEOF(y)!=REALSXP && TYPEOF(y)!=INTSXP)
    stop("y not numeric/integer");
  return(1);
}

bool check_ints(RObject x, RObject y){
  if(TYPEOF(x)==INTSXP && TYPEOF(y)==INTSXP)
    return(TRUE);
  return(FALSE);
}

int set_bbox(NumericVector bbox, RObject mat_, int i){
  NumericMatrix mat = as<NumericMatrix>(mat_);
  if(i==0){
    bbox[0] = mat(0,0);
    bbox[1] = mat(0,1);
    bbox[2] = mat(0,0);
    bbox[3] = mat(0,1);
  }
  NumericVector bbox_x (4);
  
  bbox_x[0]  = min(mat(_,0));
  bbox_x[1]  = min(mat(_,1));
  bbox_x[2]  = max(mat(_,0));
  bbox_x[3]  = max(mat(_,1));
  bbox[0]  = std::min(bbox[0],bbox_x[0]);
  bbox[1]  = std::min(bbox[1],bbox_x[1]);
  bbox[2]  = std::max(bbox[2],bbox_x[2]);
  bbox[3]  = std::max(bbox[3],bbox_x[3]);
  return(1);
}

IntegerVector modV(IntegerVector x, IntegerVector y){
  if(y.size()==0)
    return(x);
  if(is_true(any(y==0)))
    stop("modV: Mod by zero err");
  if(y.size()==x.size()){
    for(int i=0;i<x.size();i++){
      x[i] = x[i] %y[i];
    }
  }else{
    //if not exact use just the first one 
    //maybe use seq_len instead?
    for(int i=0;i<x.size();i++){
      x[i] = x[i] % y[0];
    }
  }
  return(x);
}

NumericVector fmodV(NumericVector x, NumericVector y){
  if(y.size()==0)
    return(x);
  if(is_true(any(y==0)))
    stop("fmodV: Mod by zero err");
  if(y.size()==x.size()){
    for(int i=0;i<x.size();i++){
      x[i] = fmod(x[i],y[i]);
    }
  }else{
    //if not exact use just the first one 
    //maybe use seq_len instead?
    for(int i=0;i<x.size();i++){
      x[i] = fmod(x[i],y[0]);
    }
  }
  return(x);
}



RObject range_matrix(RObject mat_x_, RObject range_){
  NumericMatrix mat_x = as<NumericMatrix>(mat_x_);
  NumericVector range = as<NumericVector>(range_);
  mat_x(_,0) = (mat_x(_,0)-range[0])/range[2];
  mat_x(_,1) = (mat_x(_,1)-range[1])/range[3];
  
  return(mat_x);
}

RObject v2m(RObject m){
  
  if(Rf_isMatrix(m)){
       return(as<NumericMatrix>(m));
 
  }else{

      NumericVector out = as<NumericVector>(clone(m));
      out.attr("dim") = Dimension(1,out.size());
      
      return(out);
  
  }
}

RObject operation_picker_mat(RObject mat_x_, RObject mat_y_, int opp = -1){
  
 
    NumericMatrix mat_x = as<NumericMatrix>(clone(mat_x_));
    NumericMatrix mat_y = as<NumericMatrix>(v2m(mat_y_));
    
    switch(opp){
/*    case ADD:
      return(add_two_matrix(mat_x, mat_y));
    case SUB:
      return(sub_two_matrix(mat_x, mat_y));
    case PROD:
      return(prod_two_matrix(mat_x, mat_y));
    case MOD:
      return(mod_two_matrix(mat_x, mat_y));
*/
    case RANGE:
      return(range_matrix(mat_x,mat_y));
    default:
      return(mat_x);
    }
    return(mat_x);
    
  
}


RObject range_two_vector(RObject x_, RObject y_){
    NumericVector x = as<NumericVector>(x_);
    NumericVector y = as<NumericVector>(y_);
    NumericVector out = clone(x);
    if(x.size()<2)
        return(out);
    out[0] = out[0] - y[0];
    out[1] = out[1] - y[1];
    out[0] = out[0] / y[2];
    out[1] = out[1] / y[3];

    return(out);
}
RObject operation_picker_vec(RObject x, RObject y, int opp = -1){
  check_num_int(x,y);
  
  switch(opp){
/*
  case ADD:
    return(add_two_vector(x, y));
  case SUB:
    return(sub_two_vector(x, y));
  case PROD:
    return(prod_two_vector(x, y));
  case MOD:
    return(mod_two_vector(x, y));
*/
  case RANGE:
    return(range_two_vector(x, y));
  default:
    return(clone(x));
  }
  return(clone(x));
}


RObject arith_vector(RObject x, RObject  y, NumericVector bbox, int i, int opp){
    RObject out = operation_picker_vec(x,y,opp);
    NumericVector out_b = as<NumericVector>(out);
    if(i==0){
      bbox[0] = out_b[0];
      bbox[1] = out_b[1];
      bbox[2] = out_b[0];
      bbox[3] = out_b[1];
    }
    bbox[0]  = std::min(bbox[0],out_b[0]);
    bbox[1]  = std::min(bbox[1],out_b[1]);
    bbox[2]  = std::max(bbox[2],out_b[0]);
    bbox[3]  = std::max(bbox[3],out_b[1]);
    
    
    return(out);
}

RObject arith_matrix(RObject mat_x, RObject  mat_y, NumericVector bbox, int i, int opp){
  
  check_num_int(mat_x,mat_y);
  //

  RObject out   = operation_picker_mat(mat_x, mat_y, opp);

  set_bbox(bbox,out,i);
  return(out);
}
List arith_list(RObject x_, RObject  y_, NumericVector bbox, int i, int opp){
  List x = as<List>(x_);
  int x_l = x.size();
  int y_l = 0;
  List y;
  List out(x_l);

  if(x.hasAttribute("class")){
    CharacterVector classes= x.attr("class");
    out.attr("class") = clone(classes);
  }
  if(Rf_isNewList(y_)){
    List y = as<List>(y_);
    y_l = y.size();
    
    if(x_l == y_l){//the lists are the same size
      out[0] = arith_matrix(x[0],y[0],bbox,i, opp);
      for(int i= 1;i<x_l;i++){
        out[i] = arith_matrix(x[i],y[i],bbox,1, opp);
      }
    }else{//just use the first
      out[0] = arith_matrix(x[0],y[0],bbox,i, opp);
      for(int i= 1;i<x_l;i++){
        out[i] = arith_matrix(x[i],y[0],bbox,1, opp);
      }
      
    }
  }else if(Rf_isVector(y_)){
    out[0] = arith_matrix(x[0],y_,bbox,i, opp);
    for(int i= 1;i<x_l;i++){
      out[i] = arith_matrix(x[i],y_,bbox,1, opp);
    }
  }else{
    stop("uh oh");
  }
  
  return(out);
}

List arith_list_list(RObject x_, RObject  y_, NumericVector bbox, int i, int opp){
  List x = as<List>(x_);
  int x_l = x.size();
  int y_l = 0;
  List y;
  List out(x_l);
  if(x.hasAttribute("class")){
    CharacterVector classes= x.attr("class");
    out.attr("class") = clone(classes);
  }
  if(Rf_isNewList(y_)){
    List y = as<List>(y_);
    y_l = y.size();
    
    if(x_l == y_l){//the lists are the same size
      out[0] = arith_list(x[0],y[0],bbox,i, opp);
      for(int i= 1;i<x_l;i++){
        out[i] = arith_list(x[i],y[i],bbox,1, opp);
      }
    }else{//just use the first
      out[0] = arith_list(x[0],y[0],bbox,i, opp);
      for(int i= 1;i<x_l;i++){
        out[i] = arith_list(x[i],y[0],bbox,1, opp);
      }
      
    }
  }else if(Rf_isVector(y_)){
    out[0] = arith_list(x[0],y_,bbox,i, opp);
    for(int i= 1;i<x_l;i++){
      out[i] = arith_list(x[i],y_,bbox,1, opp);
    }
  }else{
    stop("uh oh");
  }
  
  return(out);
}
RObject arith_hybrid(RObject x_, RObject y_, NumericVector bbox, int i, int opp, int level){
  //Needs to be checked
  //is it possible for x_ not being a list?
  if(level>10)
    stop("To deep");
  
  List x = as<List>(x_);
  int x_l = x.size();
  List out(x_l);
  if(x_l<1)
    return(x_);
  
  int y_l = 0;
  List y;
  
  if(x.hasAttribute("class")){
    CharacterVector classes= x.attr("class");
    out.attr("class") = clone(classes);
  }
  
  if(Rf_isNewList(y_)){
    List foo = as<List>(y_);
    y_l = foo.size();
    y = foo;
    
  }else if(Rf_isVector(y_)){
    y_l =0;
  }else{
    stop("Uh Oh");
  }
  
  if(x_l == y_l){
    for(int i=0; i<x_l; i++){
      out[i] = arith_geom_picker(x[i], y[i], bbox, i, opp, ++level);
    }
  }else if(y_l>0){
    //just use the first one
    for(int i=0; i<x_l; i++){
      out[i] = arith_geom_picker(x[i], y[0], bbox, i, opp, ++level);
    }
  }else{
    for(int i=0; i<x_l; i++){
      out[i] = arith_geom_picker(x[i], y_, bbox, i, opp, ++level);
    }
  }
  return(out);
}

RObject arith_geom_picker(RObject x, RObject y, NumericVector bbox, int i, int opp, int level){
  
  RObject out;
  
  //change mess to case
  if(x.inherits("POINT"))
    out = arith_vector(x,y,bbox,i,opp);
  
  if(x.inherits("MULTIPOINT"))
     out = arith_matrix(x, y, bbox,i,opp);
  if(x.inherits("LINESTRING"))//NOT TESTED YET
    out = arith_matrix(x, y, bbox,i,opp);
  if(x.inherits("CIRCULARSTRING"))
    out = arith_matrix(x, y, bbox,i,opp); 
  if(x.inherits("CURVE"))//NOT TESTED YET
    out = arith_matrix(x, y, bbox,i,opp); 
  if(x.inherits("SURFACE"))//NOT TESTED YET
    out = arith_matrix(x, y, bbox,i,opp); 
  
  if(x.inherits("POLYGON"))
   out = arith_list(x,y,bbox,i,opp);
  if(x.inherits("MULTILINESTRING"))//NOT YET TESTED
    out = arith_list(x,y,bbox,i,opp);
  if(x.inherits("TRIANGLE"))//NOT YET TESTED
    out = arith_list(x,y,bbox,i,opp);
  
  if(x.inherits("MULTIPOLYGON"))//to hybrid?
    out =arith_list_list(x,y,bbox,i,opp);
  
  
  if(x.inherits("MULTISURFACE"))
   out = arith_hybrid(x, y, bbox, i, opp, ++level);
  if(x.inherits("COMPOUNDCURVE"))
    out =arith_hybrid(x, y, bbox, i, opp, ++level);
  if(x.inherits("CURVEPOLYGON"))
    out =arith_hybrid(x, y, bbox, i, opp, ++level);
  
  if(x.inherits("GEOMETRYCOLLECTION"))//NOT YET TESTED
    out =arith_hybrid(x, y, bbox, i, opp, ++level);
  if(x.inherits("MULTICURVE"))//NOT YET TESTED
    out =arith_hybrid(x, y, bbox, i, opp, ++level);
  if(x.inherits("POLYHEDRALSURFACE"))//NOT YET TESTED
    out =arith_hybrid(x, y, bbox, i, opp, ++level);
  if(x.inherits("TIN"))//NOT YET TESTED
    out =arith_hybrid(x, y, bbox, i, opp, ++level);
  return(out);
}
int build_sfc(List x, List out){
  CharacterVector classes = x.attr("class");
  out.attr("class")       = clone(classes);

  if(x.hasAttribute("precision")){
      NumericVector precision = x.attr("precision");
      out.attr("precision")   = clone(precision);
  }

  if(x.hasAttribute("bbox")){
     NumericVector bbox = x.attr("bbox");
     out.attr("bbox")   = clone(bbox);
  }else{
     out.attr("bbox")   = NumericVector::create(0,0,0,0);
  }


  if(x.hasAttribute("crs")){
//     List crs        = x.attr("crs");
//     out.attr("crs") = clone(crs);
     List crs_l(2);
     crs_l.attr("class") = "crs";
     crs_l.names()   = CharacterVector::create("epsg","proj4string");
     crs_l[0]        = NumericVector::create(NA_REAL);
     crs_l[1]        = CharacterVector::create(NA_STRING);
     out.attr("crs") = crs_l;

  }

  if(x.hasAttribute("n_empty")){
     NumericVector n_empty = x.attr("n_empty");
     out.attr("n_empty")   = clone(n_empty);
  }
  
  return(1);
}
// [[Rcpp::export]]
List  CPL_arith(List x, RObject y_, int opp = -1){
 // if(opp<0 || (opp>3 && opp!=RANGE))
 //   stop("0=add;1=sub;2=prod;3=mod");
  if(opp!=RANGE)
     stop("only opp=RANGE is allowed");

  int n = x.size();
  
  List y;
  List out(n);
  build_sfc(x,out);

  NumericVector bbox = out.attr("bbox");

  if(opp==RANGE){
    NumericVector range = as<NumericVector>(clone(y_));
    if(range.size()!=4)
      stop("Range wrong length.");

    //converting x/y max to x/y range
    range[2] = range[2] - range[0];
    range[3] = range[3] - range[1];

    if(range[2]==0 || range[3]==0)
        stop("range is zero");

    for(int i=0; i<n; i++){
      out[i] = arith_geom_picker(x[i], range, bbox, i, opp, 0);

    }
    return(out);
  }
  
  

  if(Rf_isNewList(y_))
    y = y_;

  
  if(x.size()==y.size()){
    for(int i=0; i<n; i++){
      out[i] = arith_geom_picker(x[i], y[i], bbox, i, opp, 0);
    }
    return(out);
  }else{

    for(int i=0; i<n; i++){
      out[i] = arith_geom_picker(x[i], y_, bbox, i, opp, 0);
    }
  }
  return(out);
}

