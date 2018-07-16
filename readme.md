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

```bash
convert \
  images/example.jpg \
  -distort Perspective \
    '8,35 0,0 27,73 0,66 90,72 63,66 67,10 63,0' \
  -crop 63x66+0+0 \
  images/example-fixed.jpg
```


## Manual

### Interpolation of Missing Parts

Perspect automatically interpolates missing parts by using the closes pixel.
(https://www.imagemagick.org/Usage/misc/#edge)



## Development


### TODO

- [ ] "Skip" button
- [ ] "Reset" button
- [ ] Label corner markers
- [ ] Rescale image on viewport change
- [ ] Handle JPEG rotation
- [ ] Zoom view for corners
- [ ] Drag'n'Drop for corner markers
- [ ] "Convert to Grayscale" button
- [ ] Add support for custom output size (e.g. A4)
- [ ] Draw lines between corners to simplify guessing of clipped corners


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

With https://gist.github.com/zlbruce/883605a635df8d5964bab11ed75e46ad:

```sh
svg2icns icon.svg
```


### Generate App

```sh
make Perspec.app
```
