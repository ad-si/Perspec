# Development

## Calibration

Corners of landscape calibration image:

```hs
( (200, 100)  -- Red
, (900, 100)  -- Green
, (800, 400)  -- Blue
, (100, 300)  -- Magenta
)
```

## Generate Icons

With <https://gist.github.com/zlbruce/883605a635df8d5964bab11ed75e46ad:>

```sh
svg2icns icon.svg
```


## New Release

- [ ] Bump version number
- [ ] Create new release on GitHub
  - [ ] Download artifacts, fix file permission, zip them, add them to release
- [ ] Update the [cask file]
- [ ] Update version on [Gumroad]

[cask file]: https://github.com/ad-si/homebrew-tap/blob/master/Casks/perspec.rb
[Gumroad]: https://gumroad.com/feram
