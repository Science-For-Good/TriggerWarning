#' Enhanced Gemini API function with rate limiting, retries, and improved logging
#' 
#' @param term The trigger term to analyze
#' @param context The text context around the term
#' @param grant_title The title of the grant
#' @param log_file Path to log file
#' @param max_retries Maximum number of retry attempts (default: 3)
#' @param base_delay Base delay between requests in seconds (default: 1)
#' @return List with category and explanation
gemini_api_function <- function(term, context, grant_title, log_file, 
                                max_retries = 3, base_delay = 1) {
  # Get API key
  api_key <- get_api_key()
  
  # Log successful key retrieval (don't log the actual key)
  log_message(paste0("Using API key (length: ", nchar(api_key), ")"), log_file)
  
  # Create prompt with sanitized inputs
  clean_text <- function(text) {
    if (is.na(text) || is.null(text)) return("")
    text <- gsub("[[:cntrl:]]", "", text)
    text <- gsub("\"", "'", text)
    return(text)
  }
  
  prompt <- paste0(
    "Task: Classify the usage context of the term '", clean_text(term), 
    "' in the following scientific grant text.\n\n",
    "Grant title: ", clean_text(grant_title), "\n\n",
    "Context: \"", clean_text(context), "\"\n\n",
    "Classify the term's usage as exactly ONE of the following:\n",
    "1. SCIENTIFIC - The term is used in a technical or scientific context.\n",
    "2. POLITICAL - The term is used in a social, political, or identity context.\n",
    "3. AMBIGUOUS - Cannot clearly determine the context.\n\n",
    "Return only the classification (SCIENTIFIC, POLITICAL, or AMBIGUOUS) followed by a brief explanation."
  )
  
  # Prepare the request body in the EXACT format Gemini expects
  request_body <- list(
    contents = list(
      list(
        parts = list(
          list(text = prompt)
        )
      )
    )
  )
  
  # Convert request to JSON
  json_body <- jsonlite::toJSON(request_body, auto_unbox = TRUE)
  
  # Initialize variables for retry logic
  attempt <- 0
  success <- FALSE
  result_text <- NULL
  
  # Construct the base URL - IMPORTANT: No parameters attached yet
  base_url <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent"
  
  # Retry loop
  while (attempt < max_retries && !success) {
    attempt <- attempt + 1
    
    # Apply backoff for retries
    if (attempt > 1) {
      delay <- base_delay * (2^(attempt - 1))
      log_message(paste0("Retry attempt ", attempt, ", waiting ", delay, " seconds."), log_file)
      Sys.sleep(delay)
    }
    
    response <- tryCatch({
      # Use httr directly but with the key as a parameter, not part of the URL string
      httr::POST(
        url = base_url,
        query = list(key = api_key),  # Add API key as a query parameter
        body = json_body,
        httr::add_headers("Content-Type" = "application/json"),
        encode = "json"  # Changed to "json" from "raw"
      )
    }, error = function(e) {
      log_message(paste0("API call error on attempt ", attempt, ": ", e$message), log_file)
      return(NULL)
    })
    
    # Process response
    if (!is.null(response) && httr::status_code(response) == 200) {
      # Parse the response
      result <- httr::content(response, "parsed")
      
      if (!is.null(result) && !is.null(result$candidates) && 
          length(result$candidates) > 0 && 
          !is.null(result$candidates[[1]]$content$parts[[1]]$text)) {
        
        success <- TRUE
        result_text <- result$candidates[[1]]$content$parts[[1]]$text
        log_message(paste0("API call successful for term '", term, "' on attempt ", attempt), log_file)
      } else {
        log_message("API call succeeded but returned invalid data structure", log_file)
      }
    } else if (!is.null(response)) {
      log_message(paste0("API call failed with status ", httr::status_code(response), 
                         ": ", httr::content(response, "text")), log_file)
    }
  }
  
  # Process response or return default
  if (success && !is.null(result_text)) {
    # Extract classification
    classification <- toupper(trimws(strsplit(result_text, "\\s|-|:")[[1]][1]))
    
    # Validate classification
    if (!classification %in% c("SCIENTIFIC", "POLITICAL", "AMBIGUOUS")) {
      classification <- "AMBIGUOUS"
    }
    
    # Get explanation 
    explanation <- sub("^\\w+\\s*[-:]?\\s*", "", result_text)
    
    return(list(
      category = classification,
      explanation = explanation
    ))
  } else {
    # Return default result
    return(list(
      category = "AMBIGUOUS",
      explanation = paste0("Unable to classify due to API issues after ", max_retries, " attempts.")
    ))
  }
}

#' Updated test function for troubleshooting
test_gemini_key <- function() {
  require(httr)
  require(jsonlite)
  
  cat("Testing Gemini API key...\n")
  
  # Get API key
  api_key <- get_api_key()
  cat("API key length:", nchar(api_key), "\n")
  
  # Create test request body
  test_body <- list(
    contents = list(
      list(
        parts = list(
          list(text = "Return only the word SUCCESS if you can read this.")
        )
      )
    )
  )
  
  # Convert to JSON
  json_body <- jsonlite::toJSON(test_body, auto_unbox = TRUE)
  
  # Base URL without parameters
  base_url <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent"
  
  cat("Testing with httr::POST...\n")
  
  # Execute the HTTP request properly
  response <- tryCatch({
    httr::POST(
      url = base_url,
      query = list(key = api_key),  # Add as query parameter
      body = json_body,
      httr::add_headers("Content-Type" = "application/json"),
      encode = "json"  # Use json encoding
    )
  }, error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    return(NULL)
  })
  
  if (!is.null(response)) {
    status <- httr::status_code(response)
    cat("Response status code:", status, "\n")
    
    if (status == 200) {
      cat("Success! API response received.\n")
      
      # Try to parse the response
      parsed <- tryCatch({
        httr::content(response, "parsed")
      }, error = function(e) {
        cat("Error parsing response:", conditionMessage(e), "\n")
        cat("Raw response:", httr::content(response, "text"), "\n")
        return(NULL)
      })
      
      if (!is.null(parsed) && !is.null(parsed$candidates)) {
        response_text <- parsed$candidates[[1]]$content$parts[[1]]$text
        cat("Response text:", response_text, "\n")
        return(TRUE)
      } else {
        cat("Unexpected response structure\n")
        cat("Raw response:", httr::content(response, "text"), "\n")
        return(FALSE)
      }
    } else {
      cat("Failed with status code:", status, "\n")
      cat("Response body:", httr::content(response, "text"), "\n")
      return(FALSE)
    }
  } else {
    cat("Failed: No response from API\n")
    return(FALSE)
  }
}

# Test the fixed function directly if run as script
if (!interactive()) {
  test_gemini_key()
}