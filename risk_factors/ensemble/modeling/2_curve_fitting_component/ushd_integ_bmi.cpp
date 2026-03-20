// based on FILEPATH

#include <Rcpp.h>
#include <iostream>
#include <iomanip>
#include <numeric>
using namespace std;
using namespace Rcpp;
// integrate bmi to use in overweight and obese optimization using midpoint approximation
// (updated to estimate underweight for USHD)
// [[Rcpp::export]]
List integ_bmi(NumericVector x, NumericVector fx) {
  double n = fx.size(); // length of fx
  std::vector<double> fim(n);
  std::vector<double> fimob(n);
  std::vector<double> fimun(n);
  fim[0] = fx[0];
  fimob[0] = fx[0];
  fimun[0] = fx[0];
  double xp = x[2]-x[1]; // increment size between values of X in support
  // with of interval is (b-a)/n
  long double fraction = (1/3.);
  double h = (x[x.size() - 1]-x[1])/n; // this appears to be the average distance b/w values of x (i.e., dx); slightly different than xp, the increment b/w points. My guess is that this is because of limits to floating point precision
  for (int p = 1; p < n; p++) { // starting with the second value in the approx density function (until the last value) -- then increment by 1
    double xm = x[p] + xp; // increase the x value by the standard increment b/w values in the approx
    if (xm >= 25) { // if the next value of x is >= 25, approx the area under the density curve b/w this point and the next one. This appears to be a version of Simpson's Rule for numerical itnegration.
      fim[p] = (4*((fx[p+1]-fx[p])/2)+fx[p] + fx[p] + fx[p+1])*h*fraction; 
    }
    if (xm >= 30) {
      fimob[p] = (4*((fx[p+1]-fx[p])/2)+fx[p] + fx[p] + fx[p+1])*h*fraction; 
    }
    if( xm < 18.5) {
      fimun[p] = (4*((fx[p+1]-fx[p])/2)+fx[p] + fx[p] + fx[p+1])*h*fraction; // calculate the area under curve below underweight threshold
    }
  }
  
  // set last index to one before it
  fim[fim.size()-1] = fim[fim.size()-2];
  
  // set last index to one before it
  fimob[fimob.size()-1] = fimob[fimob.size()-2];
  
  // set last index to one before it
  fimun[fimun.size()-1] = fimun[fimun.size()-2];
  
  double SUM_over = 0;
  for(std::vector<double>::iterator it = fim.begin(); it != fim.end(); ++it)
    SUM_over += *it;
  
  double SUM_obese = 0;
  for(std::vector<double>::iterator it = fimob.begin(); it != fimob.end(); ++it)
    SUM_obese += *it;
  
  double SUM_under = 0;
  for(std::vector<double>::iterator it = fimun.begin(); it != fimun.end(); ++it)
    SUM_under += *it;
  
  return List::create(Named("over") = SUM_over, Named("obese") = SUM_obese, Named("under", SUM_under));
}

