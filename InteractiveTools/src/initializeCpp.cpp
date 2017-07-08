#include <Rcpp.h>
// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
using namespace Rcpp;

typedef Eigen::MappedSparseMatrix<double> MSpMat;
typedef MSpMat::InnerIterator InIterMat;

//[[Rcpp::export]]
IntegerVector initializeCpp(MSpMat component_neighbor_vectors, NumericMatrix available_topic_matrix){
    RNGScope scope;
    
    int N = available_topic_matrix.nrow();
    int K = available_topic_matrix.ncol();
    int k;
    double normalizing_constant;
    IntegerVector assignments(N);
    
    NumericVector rand_vector = runif(N);
    
    for(int i=0; i<N; i++){
        NumericVector available_topic_vector = available_topic_matrix(i, _);
        normalizing_constant = sum(available_topic_vector);
        
        
        //  Check if there are any available topics.
        if(normalizing_constant == 0){
            Rcout << "Error: no available topics!" << std::endl;
            break;
        }
        
        // We choose an available topic for component i randomly.
        available_topic_vector = available_topic_vector/normalizing_constant;
        
        NumericVector cumulative_prob_vector = cumsum(available_topic_vector);
        k = 0;
        while(cumulative_prob_vector(k) < rand_vector(i)){
            k++;
        }

        assignments[i] = k;
        
        // For each neighbor of i, we zero out k as an available topic.
        for (InIterMat i_(component_neighbor_vectors, i); i_; ++i_){
            available_topic_matrix(i_.index(), k) = 0;
        }
    }
    
    return assignments;
}