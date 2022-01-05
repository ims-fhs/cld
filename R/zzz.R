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
    # windowsFonts()
    # extrafont::font_import(pattern = "caveat.ttf", prompt = FALSE)
    # extrafont::font_import(pattern = "Permanent", prompt = FALSE)
    # extrafont::font_import(pattern = "Indie", prompt = FALSE)
    # extrafont::loadfonts(device = "win")
    # windowsFonts()

    # https://babichmorrowc.github.io/post/2019-10-11-google-fonts/
    library(showtext)
    font_add_google("Permanent Marker", "permanent")
    font_add_google("Caveat", "caveat")
    font_add_google("Indie Flower", "indie")
    font_add_google("Amatic SC", "amatic")
    # font_add_google("Chelsea Market", "chelsea")
    font_add_google("Open Sans", "open")

  }
  print(extrafont::fonts())
}
