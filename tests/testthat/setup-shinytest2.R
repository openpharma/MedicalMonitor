# Because the old headless mode is removed from chrome since v132.
# Note: this setting is standard since R package chromote v0.4.0
options(chromote.headless = "new")

# Load application support files into testing environment
if (FALSE) shinytest2::load_app_env()
