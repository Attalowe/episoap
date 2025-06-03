#' Determine input data type from the configuration file
#'
#' Automatically detects the data type (count_data, linelist, or incidence)
#' based on parameters defined in the configuration file. This function reads
#' the configuration file created by `create_config()`, loads any specified
#' data files, and analyzes the parameters to determine the appropriate data type.
#'
#' @param data A dataframe-like object that can be either data.frame, linelist or incidence. Default is NULL.
#' @param total_count A numeric with the total number of cases. Default is NULL.
#' @param total_death A numeric with the total number of deaths. Default is NULL.
#'
#' @returns  A character with one of the following values: "count_data", "linelist" or "incidence".
#'
#' @details
#' This function:
#' 1. Loads the configuration file from the default location (`tempdir()/config.yaml`)
#' 2. Reads parameters and loads data files if specified
#' 3. Validates the configuration structure
#' 4. Determines data type based on:
#'    - Presence of total_cases/total_deaths (count_data)
#'    - Structure of loaded data (incidence or linelist)
#'
#' The configuration file must be created and edited using `create_config()` first.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Typical workflow:
#'
#' # 1. Create and edit configuration file
#' create_config()
#'
#' # 2. After editing config.yaml, determine data type
#' result <- get_data_type()
#' print(result)
#'
#' # Example outputs:
#' # [1] "count_data"
#' # [1] "linelist"
#' # [1] "incidence"
#'
#' # Example with custom config path:
#' # First create config in working directory:
#' create_config("my_config.yaml")
#'
#' # Then run detection (still uses default tempdir config):
#' result <- get_data_type()
#'
#' # To use custom path, modify default in load_config:
#' # (Advanced: Not recommended for most users)
#' }
#'
#' @seealso
#' - [create_config()] to create/edit configuration files
#' - [load_config()] for advanced configuration handling

get_data_type <- function(){
# Load configuration using default tempdir path
params <- load_config()

# Extract the required parameters
data <- params[["data"]]
total_count <- params[["severity"]][["total_cases"]]
total_death <-params[["severity"]][["total_deaths"]]

# validate inputs
checkmate::assert(
    checkmate::check_null(data),
    checkmate::check_data_frame(data),
    checkmate::check_class(data, classes = "linelist"),
    checkmate::check_class(data, classes = "incidence"),
    combine = "or"
)
# Check if 'total_count'  and total_death' are a single numeric value or NULL and are non-negative
checkmate::assert_number(total_count, null.ok = TRUE, lower = 0)
checkmate::assert_number(total_death, null.ok = TRUE, lower = 0)

# Check for count data
if ( !is.null(total_count) && !is.null(total_death)) {
    return("count_data")
}

#  Check for incidence objects
if (inherits(data, "incidence")) {
    return("incidence")
}

# check for data.frame/linelist
if (inherits(data, "data.frame")) {
# Convert column names to lowercase for consistent checks
actual_cols <- tolower(names(data))
required_incidence <- c("date", "cases", "dead") # atleast
# linelist features
linelist_keywords <- c("id", "case", "date", "onset", "report",
                           "age", "sex", "gender", "outcome", "symptom", "hospital")

#  Incident data check
if (all(required_incidence %in% actual_cols)) {
# Check if pure incident or has extras
if (length(actual_cols) == 3) {
return("incidence")
} else {
# Check extra columns for linelist features
extra_cols <- actual_cols[!actual_cols %in% required_incidence]
has_linelist <- any(extra_cols %in% linelist_keywords)
return(ifelse(has_linelist, "linelist", "incidence"))
  }
}


# Count matches in original column names (case-insensitive)
col_matches <- grepl(paste(linelist_keywords, collapse = "|"),
                        names(data), ignore.case = TRUE)

if (sum(col_matches) >= 4) {
     return("linelist")
 }
}

# Default/error case
  stop("unknown_data_type! Either provide a non-negative value for  total_count and total_death arguements or  a dataframe-like object (data.frame, linelist or incidence),in the data arguement")
}




