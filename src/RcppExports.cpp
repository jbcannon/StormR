// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// computeAsymmetry_cpp
List computeAsymmetry_cpp(String asymmetry, NumericVector wind, NumericVector x, NumericVector y, double vx, double vy, double vh, double r, double rmw, double lat);
RcppExport SEXP _StormR_computeAsymmetry_cpp(SEXP asymmetrySEXP, SEXP windSEXP, SEXP xSEXP, SEXP ySEXP, SEXP vxSEXP, SEXP vySEXP, SEXP vhSEXP, SEXP rSEXP, SEXP rmwSEXP, SEXP latSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< String >::type asymmetry(asymmetrySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type wind(windSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type vx(vxSEXP);
    Rcpp::traits::input_parameter< double >::type vy(vySEXP);
    Rcpp::traits::input_parameter< double >::type vh(vhSEXP);
    Rcpp::traits::input_parameter< double >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type rmw(rmwSEXP);
    Rcpp::traits::input_parameter< double >::type lat(latSEXP);
    rcpp_result_gen = Rcpp::wrap(computeAsymmetry_cpp(asymmetry, wind, x, y, vx, vy, vh, r, rmw, lat));
    return rcpp_result_gen;
END_RCPP
}
// willoughby_cpp
NumericVector willoughby_cpp(NumericVector r, double rmw, double msw, double lat);
RcppExport SEXP _StormR_willoughby_cpp(SEXP rSEXP, SEXP rmwSEXP, SEXP mswSEXP, SEXP latSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type rmw(rmwSEXP);
    Rcpp::traits::input_parameter< double >::type msw(mswSEXP);
    Rcpp::traits::input_parameter< double >::type lat(latSEXP);
    rcpp_result_gen = Rcpp::wrap(willoughby_cpp(r, rmw, msw, lat));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_StormR_computeAsymmetry_cpp", (DL_FUNC) &_StormR_computeAsymmetry_cpp, 10},
    {"_StormR_willoughby_cpp", (DL_FUNC) &_StormR_willoughby_cpp, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_StormR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
