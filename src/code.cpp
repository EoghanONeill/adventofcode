# include <RcppArmadillo.h>
#include <cmath>

using namespace Rcpp;

//######################################################################################################################//
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double day6func_recursive_cpp(int init_num,
                             int num_days){

  if((num_days - (init_num+1))<=-1){
    return(0);
  }else{

    double num_extra = 0;

    if(init_num == 5 ){
      num_extra = std::floor(double(num_days)/7);
    }
    if(init_num < 5 ){
      num_extra = std::floor(double(num_days- init_num -1 )/7)+1;

    }
    if(init_num ==7 ){
      num_extra = std::floor(double(num_days -1 )/7);

    }

    // Rcout << "num_days = " << num_days << ". \n";
    // Rcout << "init_num = " << init_num << ". \n";
    //
    // Rcout << "num_extra = " << num_extra << ". \n";


    arma::vec running_counts = arma::zeros<arma::vec>(num_extra);

      // for(j in 1:num_extra){
    for(int j=0; j<num_extra;j++){
      // Rcout << "j = " << j << ". \n";

      int day_created = (j)*7 + ((init_num+1));
      // Rcout << "day_created = " << day_created << ". \n";


      running_counts(j) = day6func_recursive_cpp(7, num_days - day_created -1);


    }

    double ret = num_extra+ arma::sum(running_counts);
    return(ret);

  }

}

//######################################################################################################################//

//######################################################################################################################//
// [[Rcpp::depends(RcppArmadillo)]]


// [[Rcpp::plugins(openmp)]]
#include <omp.h>

// [[Rcpp::export]]
double day6func_apply_rec_cpp(arma::vec data6a,
                                              int num_days1,
                                              int ncore){

  // int total = 0;

  int B = data6a.n_elem;
  arma::vec totals_each_init = arma::zeros<arma::vec>(5);

  arma::uvec temp1s = arma::find(data6a ==1);
  arma::uvec temp2s = arma::find(data6a ==2);
  arma::uvec temp3s = arma::find(data6a ==3);
  arma::uvec temp4s = arma::find(data6a ==4);
  arma::uvec temp5s = arma::find(data6a ==5);

  double num1s = temp1s.n_elem;
  double num2s = temp2s.n_elem;
  double num3s = temp3s.n_elem;
  double num4s = temp4s.n_elem;
  double num5s = temp5s.n_elem;

  Rcout << "num1s = " << num1s << ". \n";
  Rcout << "num2s = " << num2s << ". \n";
  Rcout << "num3s = " << num3s << ". \n";
  Rcout << "num4s = " << num4s << ". \n";
  Rcout << "num5s = " << num5s << ". \n";

#pragma omp parallel num_threads(ncore)
{//start of pragma omp code
  // dqrng::xoshiro256plus lgen(gen);      // make thread local copy of rng
  // lgen.jump(omp_get_thread_num() + 1);  // advance rng by 1 ... ncores jumps
  //
#pragma omp for
  for(int b=0; b<5;b++){
    // arma::uvec id = Get_MBB_ID_cpp(TT,L);

    totals_each_init(b) = day6func_recursive_cpp(b+1, num_days1);

  }

}//end of pragma code

// Rcout << "line 359 . \n";


Rcout << "totals_each_init(0) = " << totals_each_init(0) << ". \n";
Rcout << "totals_each_init(1) = " << totals_each_init(1) << ". \n";
Rcout << "totals_each_init(2) = " << totals_each_init(2) << ". \n";
Rcout << "totals_each_init(3) = " << totals_each_init(3) << ". \n";
Rcout << "totals_each_init(4) = " << totals_each_init(4) << ". \n";


num1s = num1s*totals_each_init(0);
num2s = num2s*totals_each_init(1);
num3s = num3s*totals_each_init(2);
num4s = num4s*totals_each_init(3);
num5s = num5s*totals_each_init(4);

Rcout << "num1s = " << num1s << ". \n";
Rcout << "num2s = " << num2s << ". \n";
Rcout << "num3s = " << num3s << ". \n";
Rcout << "num4s = " << num4s << ". \n";
Rcout << "num5s = " << num5s << ". \n";

double ret = B + num1s+ num2s+ num3s+ num4s+ num5s;

return(ret);
}
