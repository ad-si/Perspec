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


## TODO

- [ ] "Skip" button
- [ ] "Reset" button
- [ ] Label corner markers
- [ ] Rescale image on viewport change
- [ ] Handle JPEG rotation
- [ ] Zoom view for corners
- [ ] Drag'n'Drop for corner markers
- [ ] "Convert to Grayscale" button
