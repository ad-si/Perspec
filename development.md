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

- [ ] Bump version number in all of:
  - `package.yaml` (`version:`)
  - `makefile` (`--app-version`)
  - `app-aux-files/Info.plist` (`CFBundleShortVersionString`)
- [ ] Regenerate `perspec.cabal` via `hpack` (or let `stack build` do it)
      and commit the result.
- [ ] Update `changelog.md` with the new release heading and notes.
- [ ] Run `make test` and smoke-test the GUI locally.
- [ ] Push and verify all CI matrix jobs pass.
- [ ] Create new release on GitHub
  - [ ] Download artifacts, fix file permissions, zip them, attach to release
- [ ] Update the [cask file]
- [ ] Update version on [Gumroad]

[cask file]: https://github.com/ad-si/homebrew-tap/blob/master/Casks/perspec.rb
[Gumroad]: https://feram.gumroad.com/l/perspec
