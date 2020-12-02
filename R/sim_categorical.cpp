#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericMatrix simCategorical(int n, NumericVector p) {
  int k = p.size();
  NumericMatrix mat(k, n);
  // Normalise prob if necessary:
  if (sum(p)!=1) {
    p = p/sum(p);
  }
  NumericVector emp_cdf = cumsum(p);
  NumericVector u = Rcpp::runif(n, 0, 1);
  // Matrix for 1-hot-encoding:
  for (int j = 0; j < n; j++) {
    // Perform binary search:
    int l = 0;
    int r = k;
    double target = u[j];
    while (l < r) {
      int m = floor((l+r)/2);
      if (emp_cdf[m] > target) {
        r = m;
      } else {
        l = m+1;
      }
    }
    mat(r,j) = 1;
  }
  return mat;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
simCategorical(10, prob)
*/
