#include <Rcpp.h>
// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
using namespace Rcpp;

typedef Eigen::MappedSparseMatrix<double> MSpMat;
typedef MSpMat::InnerIterator InIterMat;

//[[Rcpp::export]]
List runGibbsCpp(IntegerVector incoming_component_assignments, MSpMat component_doc_matrix, MSpMat component_word_matrix, NumericVector component_sizes, MSpMat component_neighbor_vectors, NumericMatrix incoming_topics, NumericVector incoming_topic_sums, NumericMatrix incoming_document_sums, double alpha, double eta, int num_iterations, bool display_progress=false){
    
    
    Environment stats("package:stats");
    
    
    Function incProgress = stats["rnorm"];
    Function getDefaultReactiveDomain = stats["rnorm"];
    
    if(display_progress){
        Rcout << "Display progress is true." << std::endl;
        Environment shiny("package:shiny");
        incProgress = shiny["incProgress"];
        getDefaultReactiveDomain = shiny["getDefaultReactiveDomain"];
    }
    
    RNGScope scope;
    
    IntegerVector component_assignments = clone(incoming_component_assignments);
    NumericMatrix topics = clone(incoming_topics);
    NumericVector topic_sums = clone(incoming_topic_sums);
    NumericMatrix document_sums = clone(incoming_document_sums);
    
    // This controls the tolerance for the calculation of the prob_vector
    double tol = 11.0;
    double p_max;
    int num_components = component_assignments.size();
    
    int V = topics.ncol();
    int K = topic_sums.size();
    
    double comp_size;
    int current_assignment;
    double word_sum;
    double doc_sum;
    double total_sum;
    double word_topic_val;
    double doc_topic_val;
    
    for(int t = 0; t < num_iterations; t++){
        NumericVector rand_vector = runif(num_components);
        for(int comp = 0; comp < num_components; comp++){
            // Get current assignment
            current_assignment = component_assignments(comp);
            
            // Get component size.
            comp_size = component_sizes(comp);
            
            // Subtract comp_size from topic_sums
            topic_sums(current_assignment) = topic_sums(current_assignment) - comp_size;

            // Subtract component word vector from topics
            for (InIterMat i_(component_word_matrix, comp); i_; ++i_){
                // i_.index() corresponds to the non-zero elements of this column.
                // i_.value() corresponds to the value at this position
                topics.at(current_assignment, i_.index()) = topics.at(current_assignment, i_.index()) - i_.value();
            }
            
            // Subtract component document vector from document_sums
            for (InIterMat i_(component_doc_matrix, comp); i_; ++i_){
                // i_.index() corresponds to the non-zero elements of this column.
                // i_.value() corresponds to the value at this position
                document_sums.at(current_assignment, i_.index()) = document_sums.at(current_assignment, i_.index()) - i_.value();

            }
            
            NumericVector prob_vector(K);
            // Iterate through all topics and calculate the log-proportional probability.
            for(int k=0; k < K; k++){
                // Calculate the influence of words.
                word_sum = 0.0;
                for (InIterMat i_(component_word_matrix, comp); i_; ++i_){
                    word_topic_val = topics.at(k, i_.index());
                    word_sum += lgamma(word_topic_val + i_.value() + eta) - lgamma(word_topic_val + eta);
                }
                
                doc_sum = 0.0;
                for (InIterMat i_(component_doc_matrix, comp); i_; ++i_){
                    doc_topic_val = document_sums.at(k, i_.index());
                    doc_sum += lgamma(doc_topic_val + i_.value() + alpha) - lgamma(doc_topic_val + alpha);
                }
                
                total_sum = lgamma(topic_sums(k)+ comp_size + V*eta) - lgamma(topic_sums(k) + V*eta);

                prob_vector(k) = word_sum + doc_sum - total_sum;

            }

            // Iterate through neighbors and zero out unavailable topics.
            LogicalVector avail_topics(K, true);
            for (InIterMat i_(component_neighbor_vectors, comp); i_; ++i_){
                avail_topics(component_assignments(i_.index())) = false;
            }

            // Set prob_vector so that smallest element is 0.
            prob_vector = prob_vector - min(prob_vector);
            
            // Get largest available proportional probability.
            p_max = max(ifelse(avail_topics, prob_vector, 0.0));
            
            // For all available topics, if it is within a factor of tol of p_max, keep it.
            prob_vector = ifelse(avail_topics & (prob_vector >= p_max - tol), exp(prob_vector - p_max + tol), 0.0);

            
            // Normalize probability vector and sample from it using rand_vector.
            // Store the result in current_assignment
            if(sum(prob_vector) > 0){
                prob_vector = prob_vector/sum(prob_vector);
                NumericVector cumulative_prob_vector = cumsum(prob_vector);
                int k = 0;
                while(cumulative_prob_vector(k) < rand_vector(comp)){
                    k++;
                }
                current_assignment = k;
            }
            else{
                Rcout << "Error: No available topic to assign in iteration " << t << " in component " << comp << std::endl;
                stop("Stopping here");
            }

            // Update assignment vector and add back on sizes
            component_assignments(comp) = current_assignment;

            
            // Add comp_size to topic_sums
            topic_sums(current_assignment) = topic_sums(current_assignment) + comp_size;

            // Add component word vector to topics
            for (InIterMat i_(component_word_matrix, comp); i_; ++i_){
                topics.at(current_assignment, i_.index()) = topics.at(current_assignment, i_.index()) + i_.value();

            }
            // Add component document vector to document_sums
            for (InIterMat i_(component_doc_matrix, comp); i_; ++i_){
                document_sums.at(current_assignment, i_.index()) = document_sums.at(current_assignment, i_.index()) + i_.value();
            }
        }
        if(display_progress){
            incProgress(1.0/double(num_iterations), "Fitting model", "Gibbs sampling", getDefaultReactiveDomain());
        }
    }
    
    List component_model;
    component_model["component.assignments"] = component_assignments;
    component_model["topics"] = topics;
    component_model["topic_sums"] = topic_sums;
    component_model["document_sums"] = document_sums;
    
    return component_model;
}