#include <Rcpp.h>
#include <algorithm>  // for the transform function
using namespace Rcpp;

// Helper function to convert a string to lowercase
std::string to_lower(const std::string& input) {
  std::string result = input;
  std::transform(result.begin(), result.end(), result.begin(), ::tolower);
  return result;
}

// [[Rcpp::export]]
DataFrame findMatchesCpp(List matches, std::string target_string) {
  std::vector<std::string> domain, column, value;
  std::vector<int> index;

  // Precompute lowercase target string
  target_string = to_lower(target_string);

  CharacterVector match_names = matches.names();
  int num_dfs = matches.size();

  for (int df_index = 0; df_index < num_dfs; df_index++) {
    std::string df_name = Rcpp::as<std::string>(match_names[df_index]);
    DataFrame df = Rcpp::as<DataFrame>(matches[df_index]);
    int n_rows = df.nrows();
    CharacterVector df_colnames = df.names();

    // Convert DataFrame to vectors outside the nested loop
    std::vector<std::vector<std::string>> cols_as_strings;
    for (int col = 0; col < df.length(); col++) {
      CharacterVector col_values = df[col];
      std::vector<std::string> col_as_string = Rcpp::as<std::vector<std::string>>(col_values);
      cols_as_strings.push_back(col_as_string);
    }

    for (int row = 0; row < n_rows; row++) {
      bool has_match = false;
      CharacterVector matched_cols, matched_values;

      for (int col = 0; col < df.length(); col++) {
        std::string cell_value = cols_as_strings[col][row];

        if (to_lower(cell_value).find(target_string) != std::string::npos) {
          has_match = true;
          matched_cols.push_back(df_colnames[col]);
          matched_values.push_back(cell_value);
        }
      }

      if (has_match) {
        CharacterVector domain_col = df["DOMAIN"];
        std::string domain_value = Rcpp::as<std::string>(domain_col[row]);
        std::string domain_name = df_name != "mrgda_labels" ? df_name : "mrgda_labels (" + domain_value + ")";

        for (int i = 0; i < matched_cols.size(); i++) {
          domain.push_back(domain_name);
          column.push_back(Rcpp::as<std::string>(matched_cols[i]));
          value.push_back(Rcpp::as<std::string>(matched_values[i]));
          index.push_back(row + 1);
        }
      }
    }
  }
  return DataFrame::create(Named("DOMAIN") = domain,
                           Named("COLUMN") = column,
                           Named("VALUE") = value,
                           Named("I") = index);
}
