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
typedef Eigen::MappedSparseMatrix<double> MSpMat;
typedef MSpMat::InnerIterator InIterMat;

// [[Rcpp::export]]
void demo(const MSpMat mRe1, const MSpMat mIm1, const MSpMat mRe2, const MSpMat mIm2) {
    using Eigen::Map;
	using Eigen::VectorXcd;
	using Eigen::VectorXi;
    
    std::complex<int> test1(1,1);
    std::complex<int> test2(1,-1);
    Rcout << test1*test2 << std::endl;
    
//	const Map<VectorXi> row(as<Map<VectorXi> >(rowIndex));
//	const Map<VectorXi> col(as<Map<VectorXi> >(columnIndex));
//	const Map<VectorXcd> data(as<Map<VectorXcd> >(x));
	
	SpMat A(mRe1.rows(), mRe1.cols());
    SpMat B(mRe2.rows(), mRe2.cols());
    
    A.reserve(A.nonZeros() / A.cols());
    for (int j = 0; j < mRe1.cols(); j ++) {
        for (InIterMat i_(mRe1, j); i_; ++i_) {
            double a = mRe1.coeff(i_.index(), j);
            double b = mIm1.coeff(i_.index(), j);
            std::complex<double> value(a, b);
            A.insert(i_.index(), j) = value;
        }
    }
    B.reserve(B.nonZeros() / B.cols());
    for (int j = 0; j < mRe2.cols(); j ++) {
        for (InIterMat i_(mRe2, j); i_; ++i_) {
            double a = mRe2.coeff(i_.index(), j);
            double b = mIm2.coeff(i_.index(), j);
            std::complex<double> value(a, b);
            B.insert(i_.index(), j) = value;
        }
    }
    SpMat result = A * B;
    Rcout << A << std::endl << std::endl;
    Rcout << B << std::endl << std::endl;
    Rcout << result.isCompressed() << std::endl << std::endl;
//    Rcout << result << std::endl;
//    Rcout << result.real() << std::endl << std::endl;
	
//	Rcout << A.imag() << std::endl << std::endl;
}