.onLoad <- function(libname, pkgname) {
  if (Sys.info()[1] == "Linux") {
    dir.create('~/.fonts')
    file.copy("inst/extdata/fonts/Caveat-Regular.ttf", "~/.fonts")
    file.copy("inst/extdata/fonts/Caveat-Bold.ttf", "~/.fonts")
    file.copy("inst/extdata/fonts/PermanentMarker-Regular.ttf", "~/.fonts")
    file.copy("inst/extdata/fonts/IndieFlower.ttf", "~/.fonts")
    system('fc-cache -f ~/.fonts')
  }
  if (Sys.info()[1] == "Windows") {
    windowsFonts()
    extrafont::font_import(pattern = "Caveat", prompt = FALSE)
    extrafont::font_import(pattern = "Permanent", prompt = FALSE)
    extrafont::font_import(pattern = "Indie", prompt = FALSE)
    extrafont::loadfonts(device = "win")
    windowsFonts()
  }
  print(extrafont::fonts())
}
