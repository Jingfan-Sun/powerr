// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppEigen.h which pulls Rcpp.h in for us
#include <RcppEigen.h>

// via the depends attribute we tell Rcpp to create hooks for
// RcppEigen so that the build process will know what to do
//
// [[Rcpp::depends(RcppEigen)]]

// simple example of creating two matrices and
// returning the result of an operatioon on them
//
// via the exports attribute we tell Rcpp to make this function
// available from R
//

typedef std::complex<double> char_type;
typedef Eigen::SparseMatrix<char_type> SpMat; // declares a column-major sparse matrix type of double
typedef Eigen::MappedSparseMatrix<double> MSpMat;
typedef MSpMat::InnerIterator InIterMat;

// [[Rcpp::export]]
Rcpp::List powerSparseProduct(const Eigen::MappedSparseMatrix<double>& mRe1,
const Eigen::MappedSparseMatrix<double>& mIm1,
const Eigen::MappedSparseMatrix<double>& mRe2,
const Eigen::MappedSparseMatrix<double>& mIm2){
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
    A.makeCompressed();
    B.makeCompressed();
    SpMat result = A * B;
    return Rcpp::List::create(Rcpp::Named("mRe")=result.real(),
                              Rcpp::Named("mIm")=result.imag());
}