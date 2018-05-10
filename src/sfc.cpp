#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

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


//0=rows
void list_mat_rows(List poly, int *rows){
  int mats = poly.length();
  for(int i = 0; i<mats; i++){
    NumericMatrix mat = poly[i];
    rows[0] += mat.nrow();
  }
}
//1=cols(not Ls); 2=type; 3=Z; 4=M
void find_cols(RObject geom, int *rows){
CharacterVector geom_c = geom.attr("class");
    rows[1] = 2;
    if(geom_c[0] == "XYZ"){
        rows[1] = 3;
        rows[2] = 1;
        rows[3] = 2;
	rows[4] = 0;
    }
    if(geom_c[0] == "XYM"){
        rows[1] = 3;
        rows[2] = 2;
        rows[3] = 0;
	rows[4] = 2;
    }
    if(geom_c[0] == "XYZM"){
        rows[1] = 4;
        rows[2] = 3;
        rows[3] = 2;
	rows[4] = 3;
    }

}

int list_mat_set(List poly, int loc, int i, arma::mat* output_p){
  arma::mat output = *output_p;

  return loc;
}



// [[Rcpp::export]]
NumericMatrix CPL_coord_1(List x) {//needs testing
  int length = x.length();
  int rows[5] = {0,0,0,0,0};//0=rows; 1=col; 2=type; 3=Z; 4=M


  find_cols(x[0], rows);
  int col_z = rows[3];
  int col_m = rows[4];

  NumericMatrix out(length,rows[1]);
  CharacterVector row_names(length);

  for(int i=0; i<length; i++){
    NumericVector point = x[i];
    if(point.length()<2)
      continue;
    out(i,0) = point[0];
    out(i,1) = point[1];
    if(col_z)
      out(i,col_z) = point[col_z];
    if(col_m)
      out(i,col_m) = point[col_m];

    row_names[i] = std::to_string(i+1);
  }
  List dimnames(2);
  dimnames[0] = row_names;
  switch(rows[2]){
    case 0:
      dimnames[1] = CharacterVector::create("X","Y");
      break;
    case 1:
      dimnames[1] = CharacterVector::create("X","Y","Z");
      break;
    case 2:
      dimnames[1] = CharacterVector::create("X","Y","M");
      break;
    case 3:
      dimnames[1] = CharacterVector::create("X","Y","Z","M");
      break;
  }

  out.attr("dimnames") = dimnames;
  return(out);
}




// [[Rcpp::export]]
NumericMatrix CPL_coord_2(List x) {//needs testing
  int length = x.length();
  int rows[5] = {0,0,0,0,0};//0=rows; 1=col; 2=type; 3=Z; 4=M
  int lpos[1] = {0}; //0=L1

  list_mat_rows(x, rows);
  find_cols(x[0], rows);

  lpos[0] += rows[1];//add cols variables;

  arma::mat output(rows[0], rows[1] + 1);
  output.fill(0);
  int loc = 0;

  for(int i=0; i<length; i++){
    arma::mat line = as<arma::mat>(x[i]);
    int line_rows = line.n_rows;
      if(line_rows==0)
        continue;

    output.submat(loc, 0, loc + line_rows-1, 1) = output.submat(loc, 0, loc + line_rows-1, 1) + line.submat(0, 0, line_rows-1, 1);
    if(rows[3])
      output.submat(loc, rows[3], loc + line_rows-1, rows[3]) = output.submat(loc, rows[3], loc + line_rows-1, rows[3]) + line.submat(0, rows[3], line_rows-1, rows[3]);
    if(rows[4])
      output.submat(loc, rows[4], loc + line_rows-1, rows[4]) = output.submat(loc, rows[4], loc + line_rows-1, rows[4]) + line.submat(0, rows[4], line_rows-1, rows[4]);

    output.submat(loc, lpos[0], loc + line_rows-1, lpos[0]) = output.submat(loc, lpos[0], loc + line_rows-1, lpos[0]) + i + 1;
    loc += line_rows;
  }

  NumericMatrix out = as<NumericMatrix>(wrap(output));
  List dimnames(2);

  switch(rows[2]){
    case 0:
      dimnames[1] = CharacterVector::create("X","Y","L1");
      break;
    case 1:
      dimnames[1] = CharacterVector::create("X","Y","Z","L1");
      break;
    case 2:
      dimnames[1] = CharacterVector::create("X","Y","M","L1");
      break;
    case 3:
      dimnames[1] = CharacterVector::create("X","Y","Z","M","L1");
      break;
  }


  out.attr("dimnames") = dimnames;
  return(out);

}



// [[Rcpp::export]]
NumericMatrix CPL_coord_3(List x) {
  int length = x.length();

  int rows[5] = {0,0,0,0,0};//0=rows; 1=col; 2=type; 3=Z; 4=M
  int lpos[2] = {0,1}; //0=L1; 1=L2

  for(int i=0;i<length;i++){
    list_mat_rows(x[i], rows);
  }
  find_cols(x[0], rows);

  lpos[0] += rows[1];//add cols variables;
  lpos[1] += rows[1];//add cols variables;

  arma::mat output(rows[0], rows[1] + 2);
  output.fill(0);

  int loc = 0;
  for(int i=0; i<length; i++){
    List poly = x[i];
    int rings = poly.length();
    if(rings==0)
      continue;

    for(int j=0;j<rings;j++){
      arma::mat ring = as<arma::mat>(poly[j]);
      int ring_rows = ring.n_rows;
      int ring_cols = ring.n_cols;
      output.submat(loc, 0, loc + ring_rows-1, 1) = output.submat(loc, 0, loc + ring_rows-1, 1) + ring.submat(0, 0, ring_rows-1, 1);

      if(rows[3])
        output.submat(loc, rows[3], loc + ring_rows-1, rows[3]) = output.submat(loc, rows[3], loc + ring_rows-1, rows[3]) + ring.submat(0, rows[3], ring_rows-1, rows[3]);
      if(rows[4])
        output.submat(loc, rows[4], loc + ring_rows-1, rows[4]) = output.submat(loc, rows[4], loc + ring_rows-1, rows[4]) + ring.submat(0, rows[4], ring_rows-1, rows[4]);

      output.submat(loc, lpos[0], loc + ring_rows-1, lpos[0]) = output.submat(loc, lpos[0], loc + ring_rows-1, lpos[0]) + j + 1;
      output.submat(loc, lpos[1], loc + ring_rows-1, lpos[1]) = output.submat(loc, lpos[1], loc + ring_rows-1, lpos[1]) + i + 1;
      loc += ring_rows;
    }
  }
  NumericMatrix out = as<NumericMatrix>(wrap(output));

  List dimnames(2);
  switch(rows[2]){
    case 0:
      dimnames[1] = CharacterVector::create("X","Y","L1","L2");
      break;
    case 1:
      dimnames[1] = CharacterVector::create("X","Y","Z","L1","L2");
      break;
    case 2:
      dimnames[1] = CharacterVector::create("X","Y","M","L1","L2");
      break;
    case 3:
      dimnames[1] = CharacterVector::create("X","Y","Z","M","L1","L2");
      break;
  }

  out.attr("dimnames") = dimnames;
  return(out);

}

// [[Rcpp::export]]
NumericMatrix CPL_coord_4(List x) {
  int length = x.length();
  int rows[5] = {0,0,0,0,0};//0=rows; 1=col; 2=type; 3=Z; 4=M
  int lpos[3] = {0,1,2}; //0=L1; 1=L2; 2=L3

  for(int i=0;i<length;i++){
    List mpoly = x[i];
    int mpoly_l = mpoly.length();
    for(int j=0; j<mpoly_l;j++){
      list_mat_rows(mpoly[j], rows);
    }
  }
  find_cols(x[0], rows);
  lpos[0] += rows[1];//add cols variables;
  lpos[1] += rows[1];//add cols variables;
  lpos[2] += rows[1];//add cols variables;


  arma::mat output(rows[0], rows[1] + 3);
  output.fill(0);

  int loc = 0;

  for(int i=0; i<length; i++){
    int start = loc;
    List mpoly = x[i];
    int  polys = mpoly.length();
    if(polys==0)
      continue;
    for(int j = 0; j<polys; j++){
      List poly = mpoly[j];
      int rings = poly.length();
      for(int k=0;k<rings;k++){
        arma::mat ring = as<arma::mat>(poly[k]);
        int ring_rows = ring.n_rows;
        output.submat(loc, 0, loc + ring_rows-1, 1) = output.submat(loc, 0, loc + ring_rows-1, 1) + ring.submat(0,0, ring_rows-1,1);

        if(rows[3])
         output.submat(loc, rows[3], loc + ring_rows-1, rows[3]) = output.submat(loc, rows[3], loc + ring_rows-1, rows[3]) + ring.submat(0, rows[3], ring_rows-1, rows[3]);
        if(rows[4])
          output.submat(loc, rows[4], loc + ring_rows-1, rows[4]) = output.submat(loc, rows[4], loc + ring_rows-1, rows[4]) + ring.submat(0, rows[4], ring_rows-1, rows[4]);

        output.submat(loc, lpos[0], loc + ring_rows-1, lpos[0]) = output.submat(loc, lpos[0], loc + ring_rows-1, lpos[0]) + k + 1;
        output.submat(loc, lpos[1], loc + ring_rows-1, lpos[1]) = output.submat(loc, lpos[1], loc + ring_rows-1, lpos[1]) + j + 1;
        loc += ring_rows;
      }
    }
    output.submat(start, lpos[2], loc-1, lpos[2]) = output.submat(start, lpos[2], loc-1, lpos[2]) + i + 1;

  }

  NumericMatrix out = as<NumericMatrix>(wrap(output));

  List dimnames(2);
  switch(rows[2]){
    case 0:
      dimnames[1] = CharacterVector::create("X","Y","L1","L2","L3");
      break;
    case 1:
      dimnames[1] = CharacterVector::create("X","Y","Z","L1","L2","L3");
      break;
    case 2:
      dimnames[1] = CharacterVector::create("X","Y","M","L1","L2","L3");
      break;
    case 3:
      dimnames[1] = CharacterVector::create("X","Y","Z","M","L1","L2","L3");
      break;
  }


  out.attr("dimnames") = dimnames;
  return(out);
}
