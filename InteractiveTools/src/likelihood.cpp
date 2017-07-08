#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double loglikelihood(NumericMatrix topics, NumericMatrix document_sums, NumericMatrix topic_sums, double eta, double alpha, bool display_progress=false){
    
    Environment stats("package:stats");
    
    
    Function incProgress = stats["rnorm"];
    Function getDefaultReactiveDomain = stats["rnorm"];
    
    if(display_progress){
        Rcout << "Display progress is true." << std::endl;
        Environment shiny("package:shiny");
        incProgress = shiny["incProgress"];
        getDefaultReactiveDomain = shiny["getDefaultReactiveDomain"];
    }
    
    
    int K = topics.nrow();
    int V = topics.ncol();
    double const_prior = 0;
    double const_ll = 0;
    
    int nd = document_sums.ncol();
    
    //document prior
    const_prior = (K * lgamma(alpha) - lgamma(alpha * K)) * nd;
    //topic prior
    const_ll = (V * lgamma(eta) - lgamma(eta * V)) * K;

    
    double num_steps = double(nd + K);
    
    
    //document likelihood
    double doc_ll = 0;
    for (int dd = 0; dd < nd; dd++){
        double sum = alpha * K;
            
        for (int kk = 0; kk < K;kk++) {
            doc_ll += lgamma(document_sums(kk,dd) + alpha);
            sum += document_sums(kk,dd);
        }
        doc_ll -= lgamma(sum);
        if(display_progress){
            incProgress(1.0/num_steps, "Computing Likelihood", "Document Likelihood", getDefaultReactiveDomain());
        }
    }
    
    //topic likelihood
    double topic_ll = 0;
    for (int kk = 0; kk < K; kk++){
        double sum = eta * V;
            
        for (int ii = 0; ii < V; ii++) {
            topic_ll += topics(ii,kk) + eta;
            sum += topics(ii,kk);
        }
        topic_ll -= lgamma(sum);
        if(display_progress){
            incProgress(1.0/num_steps, "Computing Likelihood", "Topic Likelihood", getDefaultReactiveDomain());
        }
    }

    
    return doc_ll - const_prior + topic_ll - const_ll;
}
