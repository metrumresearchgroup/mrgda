#include <Rcpp.h>
#include <RcppParallel.h>
#include <algorithm>
#include <vector>
#include <string>

using namespace Rcpp;
using namespace RcppParallel;

// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::export]]

// Helper function to convert a string to lowercase
std::string to_lower(const std::string& input) {
  std::string result = input;
  std::transform(result.begin(), result.end(), result.begin(), ::tolower);
  return result;
}

struct FindMatchesWorker : public Worker {
  const List matches;
  const std::string target_string;

  // thread-local storage
  std::vector<std::string> local_domain, local_column, local_value;
  std::vector<int> local_index;

  FindMatchesWorker(const List& matches, const std::string& target_string)
    : matches(matches), target_string(target_string) {}

  // Splitting the data
  FindMatchesWorker(const FindMatchesWorker& other, Split)
    : matches(other.matches), target_string(other.target_string) {}

  // Processing method
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t df_index = begin; df_index < end; df_index++) {
      CharacterVector match_names = matches.names();
      std::string df_name = Rcpp::as<std::string>(match_names[df_index]);
      DataFrame df = Rcpp::as<DataFrame>(matches[df_index]);
      int n_rows = df.nrows();
      CharacterVector df_colnames = df.names();

      // Convert DataFrame to vectors
      std::vector<std::vector<std::string>> cols_as_strings;
      for (int col = 0; col < df.length(); col++) {
        CharacterVector col_values = df[col];
        std::vector<std::string> col_as_string = Rcpp::as<std::vector<std::string>>(col_values);
        cols_as_strings.push_back(col_as_string);
      }

      for (int row = 0; row < n_rows; row++) {
        bool has_match = false;
        std::vector<std::string> matched_cols, matched_values;

        for (int col = 0; col < df.length(); col++) {
          std::string cell_value = cols_as_strings[col][row];
          if (to_lower(cell_value).find(target_string) != std::string::npos) {
            has_match = true;
            matched_cols.push_back(Rcpp::as<std::string>(df_colnames[col]));
            matched_values.push_back(cell_value);
          }
        }

        if (has_match) {
          CharacterVector domain_col = df["DOMAIN"];
          std::string domain_value = Rcpp::as<std::string>(domain_col[row]);
          std::string domain_name = df_name != "mrgda_labels" ? df_name : "mrgda_labels (" + domain_value + ")";
          for (size_t i = 0; i < matched_cols.size(); i++) {
            local_domain.push_back(domain_name);
            local_column.push_back(matched_cols[i]);
            local_value.push_back(matched_values[i]);
            local_index.push_back(row + 1);
          }
        }
      }
    }
  }

  // Joining results
  void join(const FindMatchesWorker& other) {
    local_domain.insert(local_domain.end(), other.local_domain.begin(), other.local_domain.end());
    local_column.insert(local_column.end(), other.local_column.begin(), other.local_column.end());
    local_value.insert(local_value.end(), other.local_value.begin(), other.local_value.end());
    local_index.insert(local_index.end(), other.local_index.begin(), other.local_index.end());
  }
};

// [[Rcpp::export]]
DataFrame findMatchesCppParallel(List matches, std::string target_string) {
  target_string = to_lower(target_string);
  FindMatchesWorker worker(matches, target_string);
  RcppParallel::parallelReduce(0, matches.size(), worker);

  return DataFrame::create(Named("DOMAIN") = worker.local_domain,
                           Named("COLUMN") = worker.local_column,
                           Named("VALUE") = worker.local_value,
                           Named("I") = worker.local_index);
}
