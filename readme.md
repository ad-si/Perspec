# Perspec

App and workflow to perspectively correct images.
For example whiteboards, document scans or facades.


Original | In Progress | Corrected
---------|-------------|----------
![Original image][doc] | ![Image correction][mark] | ![Corrected image][fix]

[doc]: images/doc.jpg
[mark]: images/doc-marking.jpg
[fix]: images/doc-fixed.jpg

Uses ImageMagick under the hood.
Once the corners are marked, the correction is equivalent to:

```sh
convert \
  images/example.jpg \
  -distort Perspective \
    '8,35 0,0 27,73 0,66 90,72 63,66 67,10 63,0' \
  -crop 63x66+0+0 \
  images/example-fixed.jpg
```


## Manual

### Interpolation of Missing Parts

Perspect automatically interpolates missing parts by using the closest pixel.
(https://www.imagemagick.org/Usage/misc/#edge)


### Workflow

1. Take images
    1. Use camera app wich lets you lock rotation (e.g. [OpenCamera]).
      Otherwise check out the guide below to fix rotation.
1. Use `number-files-{even,odd,reversed}` commands to fix order and names
1. Verify that
    - All pages were captured and have the correct filename
    - Images are sharp enough
    - Images have a high contrast
    - Images have correct orientation
1. Convert images to lossless format, apply rotations
  and convert them to grayscale.
  Attention: Exclude the covers!
    ```sh
    mogrify -verbose -format png -auto-orient -colorspace gray ./*.jpg
    ```
1. Use Perspec to crop images
1. Normalize dynamic range:
    ```sh
    mogrify -verbose -normalize ./*.png
    ```
1. Convert to black and white:
    ```sh
    #! /usr/bin/env bash

    find . -iname "*.png" | \
    while read -r file
    do
      convert \
        -verbose \
        "$file" \
        \( +clone -blur 0x60 -brightness-contrast 40 \) \
        -compose minus \
        -composite \
        -negate \
        -auto-threshold otsu \
        "$(basename "$file" ".png")"-fixed.png
    done
    ```

[OpenCamera]:
  https://play.google.com/store/apps/details?id=net.sourceforge.opencamera


In order to rotate all photos to portrait mode you can use
either
```sh
mogrify -verbose -auto-orient -rotate "90>" ./*.jpg
```
or
```sh
mogrify -verbose -auto-orient -rotate "-90>" ./*.jpg
```


## Development


### TODO

- [ ] "Skip" button
- [ ] "Reset" button
- [ ] "Submit" button
- [ ] Label corner markers
- [ ] Rescale image on viewport change
- [ ] Handle JPEG rotation
- [ ] Manual rotation buttons
- [ ] Zoom view for corners
- [ ] Drag'n'Drop for corner markers
- [ ] "Convert to Grayscale" button
- [ ] Add support for custom output size (e.g. A4)
- [ ] Draw lines between corners to simplify guessing of clipped corners
- [ ] Bundle Imagemagick
  - https://stackoverflow.com/q/16007654/1850340
  - https://blog.schdbr.de/imagemagic-osx-static-relocatable-build
  - https://www.imagemagick.org/discourse-server/viewtopic.php?f=1&t=13145&hilit=static+Mac&sid=dc6ddd4cd1629cf7935e21e9b473b400&start=15
- [ ] Better error if wrong file format is dropped (images/error-message.jpg)


### Benchmarking

Use the `-bench` flag to benchmark Imagemagick operations.
For example:

```sh
convert \
  doc.jpg \
  -bench 50 \
  -virtual-pixel black \
  -define distort:viewport=1191x598+0+0 \
  -distort Perspective \
    '277,181 0,0 214,776 0,598 1405,723 1191,598 1256,175 1191,0' \
  +repage \
  doc-fixed.jpg
```


### Generate Icons

With <https://gist.github.com/zlbruce/883605a635df8d5964bab11ed75e46ad:>

```sh
svg2icns icon.svg
```


### Generate App

Include Imagemagick by following the guide
<https://blog.schdbr.de/imagemagic-osx-static-relocatable-build>.

```sh
brew install imagemagick \
  --without-modules \
  --without-freetype \
  --without-libtiff \
  --with-zero-configuration
cp /usr/local/bin/convert app-aux-files/convert
chmod 755 app-aux-files/convert
```

Set `convertBin = "./convert"` in `app/Main.hs` and build
the core binary with `stack install`.

Finally run the make build script:

```sh
make Perspec.app
```


## Related

Check out https://github.com/adius/awesome-scanning for an extensive list
of related projects.
