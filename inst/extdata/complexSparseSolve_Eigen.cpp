#include <Rcpp.h>
#include <RcppEigen.h>
//#include <complex.h>
// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp ;
using namespace std ;

//typedef std::complex<double> char_type;
//typedef Eigen::VectorXcd vector_char;
//typedef Eigen::SparseMatrix<char_type> EigenSparseSelfAdjointMatrix;
//
//// [[Rcpp::export]]
//void demo(Eigen::VectorXi row, Eigen::VectorXi col, Eigen::VectorXcd data, vector_char b) {
//	using Eigen::Map;
//	using Eigen::VectorXcd;
//	using Eigen::VectorXi;
//    
//    int aq = 0;
//    int bb = 0;
//    
//    vector_char x(row.maxCoeff());
//    
////	const Map<VectorXi> row(as<Map<VectorXi> >(rowIndex));
////	const Map<VectorXi> col(as<Map<VectorXi> >(columnIndex));
////	const Map<VectorXcd> data(as<Map<VectorXcd> >(x));
//	
//	Eigen::SparseMatrix<char_type , Eigen::ColMajor> A(row.maxCoeff(), col.maxCoeff());
//    
//    for (int i = 0; i < row.size(); i++){
//        aq = row(i) - 1;
//        bb = col(i) - 1;
//        A.insert(aq,bb) = data(i);
//    }
//    
//    // finally decompose and solve
//    Eigen::SimplicialLDLT<EigenSparseSelfAdjointMatrix> ldltOfA(A);
//    x = ldltOfA.solve(b);
//    
//    Rcout << A << std::endl;
//    Rcout << b << std::endl << std::endl;
//    Rcout << x << std::endl << std::endl;
//    
//    Rcout << A * x << std::endl;
//}

typedef std::complex<double> char_type;
typedef Eigen::VectorXcd vector_char;
typedef Eigen::SparseMatrix<char_type> EigenSparseSelfAdjointMatrix;
typedef Eigen::SparseMatrix<char_type> SpMat; // declares a column-major sparse matrix type of double

// [[Rcpp::export]]
void demo(Eigen::VectorXi row, Eigen::VectorXi col, Eigen::VectorXcd data, vector_char b) {
    using Eigen::Map;
	using Eigen::VectorXcd;
	using Eigen::VectorXi;
    
    int aa = 0;
    int bb = 0;
    
    vector_char x(row.maxCoeff());
    
    std::complex<int> test1(1,1);
    std::complex<int> test2(1,-1);
    Rcout << test1*test2 << std::endl;
    
//	const Map<VectorXi> row(as<Map<VectorXi> >(rowIndex));
//	const Map<VectorXi> col(as<Map<VectorXi> >(columnIndex));
//	const Map<VectorXcd> data(as<Map<VectorXcd> >(x));
	
	SpMat A(row.maxCoeff(), col.maxCoeff());
    
    for (int i = 0; i < row.size(); i++){
        aa = row(i) - 1;
        bb = col(i) - 1;
        A.insert(aa,bb) = data(i);
    }
    
    // Solving:
    Eigen::ConjugateGradient<SpMat> cg;  // performs a Cholesky factorization of A
    cg.compute(A);
    cg.setMaxIterations(100);
    x = cg.solve(b);         // use the factorization to solve for the given right hand side

    
    Rcout << A << std::endl;
    Rcout << b << std::endl << std::endl;
    Rcout << x << std::endl << std::endl;
    Rcout << "#iterations:     " << cg.iterations() << std::endl;
    Rcout << "estimated error: " << cg.error()      << std::endl;
    
    Rcout << A * x << std::endl;
}