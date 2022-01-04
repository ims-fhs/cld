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
    sysfonts::font_add_google("Permanent Marker")
    sysfonts::font_add_google("Caveat")
    sysfonts::font_add_google("Indie Flower")
    sysfonts::font_add_google("Amatic SC")
    sysfonts::font_add_google("Chelsea Market")
    sysfonts::font_add_google("Open Sans")

    print(sysfonts::font_families())
  }
  print(extrafont::fonts())
}
