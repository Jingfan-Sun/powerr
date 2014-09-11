#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp ;
using namespace arma ;

// [[Rcpp::export]]
arma::sp_cx_mat powerSparseMatrix(arma::uvec rowIndex, arma::uvec columnIndex, arma::cx_vec x) {         

    rowIndex -= 1;
    columnIndex -= 1;
    arma::umat locations(2, rowIndex.size(), arma::fill::zeros);
    for (int i = 0; i < rowIndex.size(); ++i){
        locations(0, i) = rowIndex(i);
        locations(1, i) = columnIndex(i);
    }
    
    arma::sp_cx_mat sparse(locations, x);
    
    arma::sp_cx_mat sparseB(rowIndex.size(), 1);
    
    arma::vec b = arma::randu<arma::vec>(rowIndex.size());
	
	arma::vec x2;
    mat A = randu<mat>(rowIndex.size(), rowIndex.size());
	x2 = solve(A, b);
	
	Rcout << sparse << std::endl;
	Rcout << b << std::endl;
	Rcout << x2 << std::endl;
    
    return sparse;
	
	// batch insertion of two values at (5, 6) and (9, 9)
	//arma::umat locations(5,5, arma::fill::zeros);
	//locations << 5 << 9 << arma::endr
	//          << 6 << 9 << arma::endr;
    //locations(2,2) = 1;

	//arma::cx_vec values;
	//values << 1<< arma::endr;

	//arma::sp_cx_mat XX(locations, x);
	
	//Rcout << locations << std::endl;

/*    // create space for values, and copy
    arma::access::rw(sparse.values) = arma::memory::acquire_chunked<double>(x.size() + 1);
    arma::arrayops::copy(arma::access::rwp(sparse.values), x.begin(), x.size() + 1);

    // create space for row_indices, and copy
    arma::access::rw(sparse.row_indices) = arma::memory::acquire_chunked<arma::uword>(rowIndex.size() + 1);
    arma::arrayops::copy(arma::access::rwp(sparse.row_indices), rowIndex.begin(), rowIndex.size() + 1);
    
    // create space for col_ptrs, and copy 
    arma::access::rw(sparse.col_ptrs) = arma::memory::acquire<arma::uword>(columnIndex.size() + 2);
    arma::arrayops::copy(arma::access::rwp(sparse.col_ptrs), columnIndex.begin(), columnIndex.size() + 1);

    // important: set the sentinel as well
    arma::access::rwp(sparse.col_ptrs)[columnIndex.size()+1] = std::numeric_limits<arma::uword>::max();
    
    // set the number of non-zero elements
    arma::access::rw(sparse.n_nonzero) = x.size();

    Rcout << "SpMat sparse:\n" << sparse << std::endl;*/
}