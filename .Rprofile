# ============================================================================
# .Rprofile  (project root, beside the .Rproj file)
# Asks before launching so it doesn't get in the way of just editing files.
# Has no package dependencies: Run_This.R installs everything it needs.
# ============================================================================

if (interactive()) {
  setHook("rstudio.sessionInit", function(newSession) {
    if (!newSession) return(invisible())
    
    proj_dir <- getwd()
    
    prompt_and_launch <- function() {
      setwd(proj_dir)
      
      if (!file.exists("Run_This.R")) {
        message("Run_This.R not found in the project root; skipping dashboard prompt.")
        return(invisible())
      }
      
      answer <- tryCatch(
        .rs.api.showQuestion(
          title   = "Launch Metadata Dashboard?",
          message = "Would you like to launch the Metadata Dashboard now?\n\nThe first run installs any missing packages and can take several minutes.",
          ok      = "Launch",
          cancel  = "Not now"
        ),
        error = function(e) {
          message("Could not show prompt: ", conditionMessage(e))
          NA
        }
      )
      
      if (isTRUE(answer)) {
        message("Launching Metadata Dashboard...")
        # Hand off to the console so the app runs at top level,
        # not inside this nested callback.
        .rs.api.sendToConsole(
          "source('Run_This.R')",
          execute = TRUE,
          echo    = TRUE,
          focus   = FALSE
        )
      } else {
        message("Skipped. Run source('Run_This.R') any time to start it.")
      }
    }
    
    # Small delay so the RStudio UI is ready; fall back to running directly
    # if 'later' isn't installed yet (e.g., a brand-new user).
    if (requireNamespace("later", quietly = TRUE)) {
      later::later(prompt_and_launch, delay = 2)
    } else {
      prompt_and_launch()
    }
  }, action = "append")
}
