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
Rcpp::List powerSparseProduct(const Eigen::MappedSparseMatrix<double>& mRe,
const Eigen::MappedSparseMatrix<double>& mIm,
const Eigen::MappedSparseMatrix<double>& mSp) {
    SpMat A(mRe.rows(), mRe.cols());
    
    A.reserve(A.nonZeros() / A.cols());
    for (int j = 0; j < mRe.cols(); j ++) {
        for (InIterMat i_(mRe, j); i_; ++i_) {
            double a = mRe.coeff(i_.index(), j);
            double b = mIm.coeff(i_.index(), j);
            std::complex<double> value(a, b);
            A.insert(i_.index(), j) = value;
        }
    }
    SpMat result = A * mSp;
    return Rcpp::List::create(Rcpp::Named("mRe")=result.real(),
                              Rcpp::Named("mIm")=result.imag());
}