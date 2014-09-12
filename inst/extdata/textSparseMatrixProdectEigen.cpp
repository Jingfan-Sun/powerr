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
void demo(const MSpMat mRe, const MSpMat mIm, const MSpMat mSp) {
    using Eigen::Map;
	using Eigen::VectorXcd;
	using Eigen::VectorXi;
    
    std::complex<int> test1(1,1);
    std::complex<int> test2(1,-1);
    Rcout << test1*test2 << std::endl;
    
//	const Map<VectorXi> row(as<Map<VectorXi> >(rowIndex));
//	const Map<VectorXi> col(as<Map<VectorXi> >(columnIndex));
//	const Map<VectorXcd> data(as<Map<VectorXcd> >(x));
	
	SpMat A(mRe.rows(), mRe.cols());
    
    A.reserve(A.nonZeros() / A.cols());
    for (int j = 0; j < mRe.cols(); j ++) {
        for (InIterMat i_(mRe, j); i_; ++i_) {
            Rcout << " i,j=" << i_.index() << "," << j << " value=" << i_.value() << std::endl;
            double a = mRe.coeff(i_.index(), j);
            double b = mIm.coeff(i_.index(), j);
            std::complex<double> value(a, b);
            A.insert(i_.index(), j) = value;
        }
    }
    SpMat result = A * mSp;
    Rcout << sizeof(A) << std::endl << std::endl;
    A.makeCompressed();
    Rcout << A.isCompressed() << std::endl << std::endl;
    Rcout << sizeof(A) << std::endl << std::endl;
//    Rcout << result << std::endl;
//    Rcout << result.real() << std::endl << std::endl;
	
//	Rcout << A.imag() << std::endl << std::endl;
}