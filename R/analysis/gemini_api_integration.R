#' Securely retrieve and validate the API key
#' @return The API key as a string
get_api_key <- function() {
  # Try environment variable first (most secure method)
  env_key <- Sys.getenv("GEMINI_API_KEY")
  if (env_key != "") {
    # Trim any whitespace that might have been accidentally included
    env_key <- trimws(env_key)
    return(env_key)
  }
  
  # Then try config file
  config_file <- file.path(Sys.getenv("HOME"), ".gemini_config")
  if (file.exists(config_file)) {
    key_data <- readLines(config_file, warn = FALSE)
    if (length(key_data) > 0) {
      # Get first non-comment line
      for (line in key_data) {
        if (!startsWith(trimws(line), "#") && nchar(trimws(line)) > 10) {
          # Trim whitespace to avoid invisible characters
          return(trimws(line))
        }
      }
    }
  }
  
  # Get a key from another potential location - project-specific config
  local_config <- ".gemini_config"
  if (file.exists(local_config)) {
    key_data <- readLines(local_config, warn = FALSE)
    if (length(key_data) > 0) {
      for (line in key_data) {
        if (!startsWith(trimws(line), "#") && nchar(trimws(line)) > 10) {
          return(trimws(line))
        }
      }
    }
  }
  
  # Try checking for a different environment variable name
  alt_key <- Sys.getenv("GOOGLE_API_KEY")
  if (alt_key != "") {
    return(trimws(alt_key))
  }
  
  # If no valid key found, create a placeholder for manual update
  cat("⚠️ No valid API key found. Please enter a valid Google Gemini API key: ")
  user_key <- readline()
  
  if (nchar(user_key) > 10) {
    # Save the key to config file for future use
    dir.create(dirname(config_file), recursive = TRUE, showWarnings = FALSE)
    writeLines(c("# Google Gemini API key", user_key), config_file)
    Sys.chmod(config_file, mode = "0600")  # Secure permissions
    cat("✅ API key saved to", config_file, "\n")
    return(user_key)
  } else {
    stop("No valid API key provided. Please obtain a key from https://ai.google.dev/tutorials/setup")
  }
}

#' Validate API key with a simple request
#' @param api_key The API key to validate
#' @return Logical indicating if the key is valid
validate_api_key <- function(api_key = NULL) {
  if (is.null(api_key)) {
    api_key <- get_api_key()
  }
  
  cat("Validating API key (length:", nchar(api_key), ")...\n")
  
  # Print the first and last few characters to help with debugging
  if (nchar(api_key) > 10) {
    first_chars <- substr(api_key, 1, 5)
    last_chars <- substr(api_key, nchar(api_key) - 4, nchar(api_key))
    cat("Key format: ", first_chars, "...", last_chars, "\n", sep = "")
  }
  
  # Create a minimal test request
  test_body <- list(
    contents = list(
      list(
        parts = list(
          list(text = "Return only the word SUCCESS")
        )
      )
    )
  )
  
  # Convert to JSON
  json_body <- jsonlite::toJSON(test_body, auto_unbox = TRUE)
  
  # Base URL without parameters
  base_url <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent"
  
  cat("Sending test request to Gemini API...\n")
  
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
  
  if (is.null(response)) {
    cat("❌ API test failed: No response received\n")
    return(FALSE)
  }
  
  status <- httr::status_code(response)
  cat("Response status code:", status, "\n")
  
  if (status == 200) {
    cat("✅ API key is valid!\n")
    return(TRUE)
  } else {
    # Print the error response to help diagnose
    error_content <- httr::content(response, "text")
    cat("❌ API key validation failed with status", status, "\n")
    cat("Error response:", error_content, "\n")
    
    # Check if it's an API key issue
    if (status == 400 && grepl("API key not valid", error_content)) {
      cat("\n⚠️ The API key appears to be invalid. Please verify it in the Google AI Studio.\n")
      cat("Visit: https://ai.google.dev/ to create or find your API key.\n")
    }
    
    return(FALSE)
  }
}

#' Get a new API key from the user and test it
#' @return A valid API key or NULL if validation fails
get_and_test_new_key <- function() {
  cat("\n===== Google Gemini API Key Setup =====\n")
  cat("To use the Gemini API, you need a valid API key from Google AI Studio.\n")
  cat("1. Visit https://ai.google.dev/\n")
  cat("2. Create a project if you don't have one\n")
  cat("3. Create an API key for Gemini\n\n")
  
  cat("Please enter your Gemini API key: ")
  user_key <- readline()
  
  if (nchar(user_key) < 10) {
    cat("❌ The entered key is too short to be valid.\n")
    return(NULL)
  }
  
  # Test the key
  if (validate_api_key(user_key)) {
    # Save to config file
    config_file <- file.path(Sys.getenv("HOME"), ".gemini_config")
    dir.create(dirname(config_file), recursive = TRUE, showWarnings = FALSE)
    writeLines(c("# Google Gemini API key", user_key), config_file)
    Sys.chmod(config_file, mode = "0600")  # Secure permissions
    cat("✅ Valid API key saved to", config_file, "\n")
    
    # Also set environment variable for this session
    Sys.setenv(GEMINI_API_KEY = user_key)
    
    return(user_key)
  } else {
    cat("❌ The API key could not be validated. Please try again with a different key.\n")
    return(NULL)
  }
}

# Function to completely refresh API key setup
setup_gemini_api <- function() {
  cat("\n===== Gemini API Setup Utility =====\n")
  
  # Check for existing key
  existing_key <- tryCatch({
    get_api_key()
  }, error = function(e) {
    return(NULL)
  })
  
  if (!is.null(existing_key) && nchar(existing_key) > 10) {
    cat("Found existing API key. Validating...\n")
    if (validate_api_key(existing_key)) {
      cat("✅ Existing key is valid. Setup complete!\n")
      return(TRUE)
    } else {
      cat("❌ Existing key is invalid. Let's set up a new one.\n")
    }
  } else {
    cat("No existing API key found. Let's set one up.\n")
  }
  
  # Try to get a new key from the user
  new_key <- get_and_test_new_key()
  return(!is.null(new_key))
}

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

# Update the original test function to use our improved validation
test_gemini_key <- function() {
  cat("\n===== Testing Gemini API Connection =====\n")
  
  # Get and validate API key
  api_key <- get_api_key()
  valid <- validate_api_key(api_key)
  
  if (!valid) {
    # If key is invalid, try to get a new one
    cat("Would you like to set up a new API key? (y/n): ")
    response <- tolower(readline())
    
    if (startsWith(response, "y")) {
      return(setup_gemini_api())
    } else {
      cat("Skipping setup. Test failed.\n")
      return(FALSE)
    }
  }
  
  # If we got here, the key is valid
  return(TRUE)
}