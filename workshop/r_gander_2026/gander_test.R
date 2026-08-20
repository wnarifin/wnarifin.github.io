# Library ----
library(ellmer)
library(gander)

# .RProfile ----

# Paste into your .RProfile later
# usethis::edit_r_profile()

## Providers with API ----
Sys.setenv(GOOGLE_API_KEY = readLines("api"))
options(
  gander.chat = ellmer::chat_google_gemini(
    model = "gemini-3.1-flash-lite",
    api_args = list(
      generationConfig = list(
        thinkingConfig = list(thinkingLevel = "minimal")
      )
    )
  )
)

## Ollama ----
options(
  gander.chat = ellmer::chat_ollama(
    base_url = Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434"),
    # models:
    # model = "gemma4:12b",
    # model = "gpt-oss:20b",
    model = "qwen-64k", # qwen3.8 with 64k ctx_window
    # echo = "none", # gander SHOULD not output thinking, but not working right now
    params = params(reasoning_effort = "none"), # only works with qwen
    # must be "minimal", "low", "medium", "high", "xhigh", "ultra", "max", or "none"
    # api_args = list(
    #   num_ctx = 8000 # must increase ctx window for code, usually 64k
    # ) # does not seem to work too, must create specific ctx_window using Modelfile
  )
)

# Gander style ----
options(gander.style = "Use base R.")
# Default in gander
options(gander.style = "Use tidyverse style and, when relevant, tidyverse packages.
        For example, when asked to plot something, use ggplot2, or when asked to
        transform data, using dplyr and/or tidyr unless explicitly instructed otherwise.")

# Test `gander` ----
# Ctrl+Alt+G from scratch or highlight related R object & use the provided prompt

# prompt:
# Load mtcars builtin dataset as data

# prompt: 
# Overview the data structure. Load relevant libraries.
# highlight `data` and Ctrl+Alt+G
data
gander_peek()  # to see how it did it

# prompt: 
# Perform correlation between all variables. Load relevant libraries.
data

# prompt: 
# Perform linear regression with mpg as outcome, and other variables as predictors. Load relevant libraries.
data

# prompt: 
# Visualize the pairwise correlations between mpg as outcome and other variables. Add fit line. Load relevant libraries.
data

# prompt: 
# Perform linear regression with mpg as outcome, and other variables as predictors. Remove insignificant variables in the final model. Prepare table using tbl_regression from gtsummary. Load relevant libraries.
data

# prompt:
# Other prompts you want to try?

