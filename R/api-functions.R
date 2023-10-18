# Data package identifier reservation ------------------------------------------
#' Reserve EDI Data Package Identifier 
#' @description This package reserves and returns a unique EDI number. 
#' @param user_id EDI data portal user ID. Create an account an
#' EDI \href{https://portal.edirepository.org/nis/login.jsp}{here}
#' @param password EDI data portal user password
#' @param environment EDI portal environment to run command in. 
#' Can be: "production" - environment for publishing to EDI , 
#' "staging" - environment to test upload and rendering of new environment, "development"
#' @details For more information about the identifier reservation services 
#' see \href{https://pastaplus-core.readthedocs.io/en/latest/doc_tree/pasta_api/data_package_manager_api.html#reservations}{the PASTAplus docs}
#' @return This function returns a edi identifier number. 
#' @examples 
#' \dontrun{
#' reserve_edi_id(user_id = "samuelwright")}
#' @export                

reserve_edi_id <- function(user_id, password, environment = c("production", "staging", "development")) {
  environment <- match.arg(environment)
  
  base_url <- as.character(BASE_URLS[environment])
  
  response <- httr::POST(
    url = httr::modify_url(base_url, path = "package/reservations/eml/edi"),
    config = httr::authenticate(paste0("uid=", user_id, ",o=EDI", ",dc=edirepository,dc=org"), password)
  )
  if (identical(response$status_code, 201L)) {
    edi_number <- httr::content(response, as = "text", encoding = "UTF-8")
    cli::cli_alert_success("edi number: \"edi.{edi_number}.1\" has been reserved.")
    invisible(paste0("edi.", edi_number, ".1", sep = ""))
  } else {
    cli::cli_abort(c(
      "Failed to reserve an EDI number under {.var environment} = {environment}", 
      "x" = "response returned status code `{response$status_code}` with message {httr::content(response)}"
    ))
    
  }
}

# Evaluate EDI Data package -------------------------------------------------------
#' Validate EDI Data Package 
#' @description This function takes in authentication info for EDI and an EML file to 
#' be evaluated using the EDI congruence checker. This package returns a data frame that contains the status of the 
#' package. 
#' @param user_id EDI data portal user ID. Create an account an
#' EDI \href{https://portal.edirepository.org/nis/login.jsp}{here}
#' @param password EDI data portal user password
#' @param environment EDI portal environment to run command in. Can be: "production" - environment for publishing to EDI , 
#' "staging" - environment to test upload and rendering of new environment, "development"
#' @param eml_file_path The file path to the EML metadata document that you wish to evaluate. 
#' (A web link to the csv must be included in the dataset information in the EML in order for a data package to be evaluated.) 
#' @param .max_timeout the maximim number of minutes to wait before timing out on xml upload
#' @details For more information about the validation services 
#' see \href{https://pastaplus-core.readthedocs.io/en/latest/doc_tree/pasta_api/data_package_manager_api.html#upload-and-evaluation}{the PASTAplus docs}
#' @return This package returns a data frame that contains the status of the 
#' package. The data frame contains the following information:
#' * The status of each check - Can be valid, info, warn, or error. Errors must be fixed before package can be uploaded to EDI. 
#' * A description of what each status is referring to 
#' * A suggestion of how to fix the status or improve the package. 
#' @examples 
#' \dontrun{evaluate_edi_package(user_id = "samuelwright", 
#'                               eml_file_path = "data/edi20.1.xml")}
#' @export   
evaluate_edi_package <- function(user_id, password, eml_file_path, 
                                 environment = c("production", "staging", "development"), .max_timout = 5) {
  environment <- match.arg(environment) 
  
  # Select package environment 
  base_url <- as.character(BASE_URLS[environment])
  
  # post package to EDI for evaluation 
  response <- httr::POST(
    url = httr::modify_url(base_url, path = "package/evaluate/eml"),
    config = httr::authenticate(paste0("uid=", user_id, ",o=EDI", ",dc=edirepository,dc=org"), password),
    body = httr::upload_file(eml_file_path)
  )
  
  if (identical(response$status_code, 202L)) {
    # pull transaction id from response content 
    transaction_id <- httr::content(response, as = "text", encoding = "UTF-8")
    
    response <- poll_endpoint_at_dynamic_interval(
      endpoint = httr::modify_url(base_url, path =  glue::glue("package/evaluate/report/eml/{transaction_id}")), 
      user_id = user_id, 
      password = password,
      time_out_seconds = .max_timout * 60,
      verbose = TRUE, 
      
    )
    
    report_df <- generate_report_df(response)
    assign("report_df", report_df, envir = .GlobalEnv) # why not return this ?
    cli::cli_alert_info("Please check for errors in the report_df in .GlobalEnv")
    
  } else {
    cli::cli_abort(c(
      "Failed to evaluate EDI package", 
      "x" = "status code: {response$status_code} and message {httr::content(response)}"
    ))
  }
}


# TODO add test api funcitons
# Upload EDI Data package -------------------------------------------------------
#' Upload EDI Data Package 
#' @description This function takes in authentication info for EDI and an EML file to 
#' be evaluated uploaded to EDI. 
#' @param user_id EDI data portal user ID. Create an account an
#' EDI \href{https://portal.edirepository.org/nis/login.jsp}{here}
#' @param password EDI data portal user password
#' @param environment EDI portal environment to run command in. Can be: "production" - environment for publishing to EDI , 
#' "staging" - environment to test upload and rendering of new environment, "development"
#' @param eml_file_path The file path to the EML metadata document that you wish to evaluate. 
#' (A web link to the csv must be included in the dataset information in the EML in order for a data package to be evaluated.) 
#' @details For more information about the validation services see 
#' \href{https://pastaplus-core.readthedocs.io/en/latest/doc_tree/pasta_api/data_package_manager_api.html#upload-and-evaluation}{the PASTAplus docs}
#' @return Message describing if your package was successfully updated or not. 
#' @examples 
#' \dontrun{upload_edi_package(user_id = "samuelwright", 
#'                             eml_file_path = "data/edi20.1.xml")}
#' @export   
upload_edi_package <- function(user_id, password, eml_file_path, environment = "production", 
                               .max_timeout = 5) {
  
  base_url <- as.character(BASE_URLS[environment])
  # Select package environment& define 
  
  # post package to EDI for upload 
  response <- httr::POST(
    url = httr::modify_url(base_url, path = "package/eml/"),
    config = httr::authenticate(paste0("uid=", user_id, ",o=EDI", ",dc=edirepository,dc=org"), password),
    body = httr::upload_file(eml_file_path)
  )
  
  if (identical(response$status_code, 202L)) {
    
    transaction_id <- httr::content(response, as = "text", encoding = "UTF-8")
    
    # upload post success but errors can still happen in the upload process, check for them there
    check_error <- httr::GET(
      url = httr::modify_url(base_url, path = glue::glue("package/error/eml/{transaction_id}")), 
      config = httr::authenticate(paste0("uid=", user_id, ",o=EDI", ",dc=edirepository,dc=org"), password)
    )
    # If check error == 200 the check for error was successful  
    # if check error == 404 then there is no error in this upload
    # https://pasta.lternet.edu/package/docs/api#GET%20:%20/error/eml/{transaction}
    if (identical(check_error$status_code, 200L)) {
      
      message <- httr::content(check_error, encoding = "utf-8")
      # the data package already exists in the staging area - first if statement
      if (startsWith(message, "Attempting to insert a data package that already exists in PASTA")) {
        cli::cli_abort(c(
          "uploading process failed", 
          "x" = message
        ))
      } else { # this should be merged with the above 
        cli::cli_abort(c(
          "uploading process failed", 
          "x" = message
        ))
      }
    } else if (identical(check_error$status_code, 404L)) {
      poll_response <- poll_endpoint_at_dynamic_interval(
        endpoint = httr::modify_url(base_url, path = "package/report/eml"), 
        user_id = user_id, 
        password = password, 
        time_out_seconds = .max_timeout * 60, 
        verbose = TRUE
      )
      
      if (identical(poll_response$status_code, 200L)) {
        cli::cli_alert_success("your package succesfully posted to EDI!")
      } else {
        resp_message <- httr::content(poll_response, encoding = "utf-8")
        cli::cli_abort(c(
          "while checking upload process", 
          "x" = "api returned with message {resp_message}"
        ))
      }
      
    } else {
      resp_message <- httr::content(response, encoding = "utf-8")
      cli::cli_abort(c(
        "in upload POST", 
        "x" = "api returned with message {resp_message}"
      ))
    }
  }
}

# TODO add test api functions
# Update Data package on EDI ---------------------------------------------------
#' Update EDI Data Package 
#' @description This function takes in authentication info for EDI, a package number, and an updated EML file to 
#' updated an existing package on EDI. 
#' @param user_id EDI data portal user ID. Create an account an
#' EDI \href{https://portal.edirepository.org/nis/login.jsp}{here}
#' @param password EDI data portal user password
#' @param environment EDI portal environment to run command in. Can be: "production" - environment for publishing to EDI , 
#' "staging" - environment to test upload and rendering of new environment, "development"
#' @param existing_package_identifier The current edi number of the package that you are trying to update.(ex: "edi.101.1")
#' @param eml_file_path The file path to the EML metadata document that you wish to use to update. 
#' (A web link to the csv must be included in the dataset information in the EML in order for a data package to be evaluated.) 
#' @return Message describing if your package was successfully updated or not. 
#' @examples 
#' \dontrun{update_edi_package(user_id = "samuelwright", 
#'                             existing_package_identifier = "edi.740.1",
#'                             eml_file_path = "data/edi20.1.xml")}
#' @export
update_edi_package <- function(user_id, password, existing_package_identifier, 
                               eml_file_path, environment = "production", .max_timeout = 5) {
  
  base_url <- as.character(BASE_URLS[environment])
  
  # Define scope (edi) and identifier (package number)
  scope <- unlist(strsplit(existing_package_identifier, "\\."))[1]
  identifier <- unlist(strsplit(existing_package_identifier, "\\."))[2]
  revision <- unlist(strsplit(basename(eml_file_path), "\\."))[3]
  
  # post package to EDI for update
  response <- httr::PUT(
    url = httr::modify_url(base_url, path = glue::glue("package/eml/{scope}/{identifier}")),
    config = httr::authenticate(paste0("uid=", user_id, ",o=EDI", ",dc=edirepository,dc=org"), password),
    body = httr::upload_file(eml_file_path)
  )
  
  if (identical(response$status_code, 202L)) {
    
    Sys.sleep(2)
    transaction_id <- httr::content(response, as = "text", encoding = "UTF-8")
    
    check_error <- httr::GET(url = paste0(base_url, "error/eml/", transaction_id), 
                             config = httr::authenticate(paste0("uid=", user_id, ",o=EDI", ",dc=edirepository,dc=org"), password))
    
    # if error check response is 200 then an error was found
    # if error check response is 400 then an error was NOT found
    if (identical(check_error$status, 200L)) {
      
      error_message <- httr::content(check_error, as = "text", encoding = "UTF-8")
      report_df <- generate_report_df(check_error)
      cli::cli_abort(c(
        "during the upload process", 
        "x" = "api responded with the following: {error_message}"
      ))
    } else if (identical(check_error$status, 404L)) { # no error found 
      poll_response <- poll_endpoint_at_dynamic_interval(
        endpoint = httr::modify_url(base_url, path = glue::glue("package/report/eml/{scope}/{identifier}/{revision}")), 
        user_id = user_id, 
        password = pasword,
        time_out_seconds = .max_timeout * 60, 
        verbose = TRUE
      )
      
      if (identical(poll_response$status_code, 200L)) {
        cli::cli_alert_success("data package posted to EDI. Check EDI {environment} to confirm")
        return()
      } else {
        msg <- httr::content(poll_response)
        cli::cli_abort(c(
          "while polling for upload progress", 
          "x" = "api responded with the following: {msg}"
        ))
      }
      
    } else { # some other thing happened, catch and abort here
      error_message <- httr::content(check_error, as = "text", encoding = "UTF-8")
      cli::cli_abort(c(
        "while attempting to check for error", 
        "x" = "api responded with the following: {error_message}"
      ))
    }
    # Adds error handling message for 505, 405 & other errors that come from bad initial response 
  } else {
    msg <- httr::content(response, encoding = "UTF-8")
    cli::cli_abort(c(
      "while attempting to upload", 
      "x" = "api responded with the following: {msg}"
    ))
  }
}

# Helper functions 
#' Generate Report Dataframe 
#' @description Generates Package Report in Data Frame 
#' @export   
generate_report_df <- function(response) {
  report <- httr::content(response, as = "text", encoding = "UTF-8")
  name <- stringr::str_extract_all(report, "(?<=<name>)(.*)(?=</name>)")[[1]]
  status <- stringr::str_extract_all(report, "[:alpha:]+(?=</status>)")[[1]]
  suggestion <- stringr::str_extract_all(report, "(?<=<suggestion>)(.*)(?=</suggestion>)")[[1]]
  
  report_df <- dplyr::tibble("Status" = as.vector(status), 
                             "Element Checked" = as.vector(name),
                             "Suggestion to fix/imporve" = as.vector(suggestion))
  if (nchar(report) <= 500) {
    print(report)
  }
  return(report_df)
}


poll_endpoint_at_fixed_interval <- function(endpoint, seconds) {
  
}

#' Poll Endpoint at Dynamic Interval
#' @param endpoint endpoint to poll
#' @param user_id user id for auth
#' @param password password for auth
#' @param init_sleep the sleep in seconds on first iteration
#' @param grow_by the multiple to grow init_sleep and subsequent sleep amounts by
#' @keywords internal
poll_endpoint_at_dynamic_interval <- function(endpoint, user_id, password, time_out_seconds,
                                              init_sleep = 2, grow_by = 2, verbose = FALSE) {
  sleep_time <- init_sleep
  verbose_counter <- 0
  while (TRUE) { # Loop through a few times to give EDI time to evaluate package
    if (verbose) {
      verbose_counter <- verbose_counter + 1
      cli::cli_alert_info("polling upload progress on {endpoint}, attempt number: {verbose_counter}")
    }
    Sys.sleep(sleep_time) 
    response <- httr::GET(
      url = endpoint,
      config = httr::authenticate(paste0("uid=", user_id, ",o=EDI", ",dc=edirepository,dc=org"), password)
    )
    if (identical(response$status_code, 200L)) {
      return(response)
    }
    
    if (sleep_time > time_out_seconds) {
      cli::cli_abort(c(
        "Request Timed Out", 
        "x" = "check to make sure inputs are valid and try again, if correct try increasong {.var .max_timeout}."
      ))
    }
    
    sleep_time <- sleep_time * grow_by # next time around wait twice as long
  }
}

