# Changelog

## 2022-01-02 - [0.2.0.0](https://github.com/ad-si/Perspec/releases/tag/v0.2.0.0)

- Add sidebar with save buttons (normal, grayscale, or binary)
- Add sub-command "rename" for easy renaming of photos
- Make corners of selection draggable
- Center Perspec window on screen
- Add support for > macOS 11
- Display better usage information when opening the macOS app
- Add a welcome banner
- Support registering the app with a license key


## 2020-11-17 - [0.1.3.0](https://github.com/ad-si/Perspec/releases/tag/v0.1.3.0)

- Apply EXIF rotation to JPEG images
- Correctly scale the image and marked corners when resizing the window
- Display helpful error message when wrong file format is dropped
- Support installing with `brew cask`
- Use latest version of ImageMagick


## 2020-02-25 - [0.1.2.0](https://github.com/ad-si/Perspec/releases/tag/v0.1.2.0)

- ImageMagick is now embedded in the bundle
    and doesn't need to be installed globally anymore. 🎉


## 2020-02-23 - [0.1.1.0](https://github.com/ad-si/Perspec/releases/tag/v0.1.1.0)

This is basically a MVP release.
I've used it to correct hundreds of images,
so it works and is already quite useful, but there are a few limitations:

- ImageMagick is not bundled and must therefore be installed globally
- Due to a [render bug of gloss] the initial view is a little broken
    and one needs to rescale the window to force a redraw

[render bug of gloss]:
    https://groups.google.com/forum/#!topic/haskell-gloss/iEZbzwpwvtA
