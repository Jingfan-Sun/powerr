#include<Rcpp.h>

using namespace Rcpp;

//[[Rcpp::export]]
SEXP hello_world() {
    BEGIN_RCPP
    Rcout << "hello world" << std::endl;
    END_RCPP
}