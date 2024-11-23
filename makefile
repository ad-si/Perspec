.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


Perspec.app: ~/.local/bin/perspec imagemagick
	platypus \
		--name Perspec \
		--app-icon images/icon.icns \
		--interface-type 'Text Window' \
		--app-version 0.2.0.0-$$(date -u "+%Y-%m-%dT%H:%M") \
		--author "Adrian Sieber" \
		--bundled-file ~/.local/bin/perspec \
		--bundled-file app-aux-files/Credits.html \
		--bundled-file imagemagick \
		--bundled-file scripts \
		--bundle-identifier org.adrian.Perspec \
		--droppable \
		--optimize-nib \
		--xml-property-lists \
		--overwrite \
		app-aux-files/perspec.sh \
		$@


# For macOS
imagemagick:
	curl -L \
		https://download.imagemagick.org/ImageMagick/download/binaries/ImageMagick-x86_64-apple-darwin20.1.0.tar.gz \
		-o imagemagick.tar.gz
	tar -xzf imagemagick.tar.gz

	rm -rf imagemagick
	mv ImageMagick-7.* imagemagick


~/.local/bin/perspec: app source images/banner.bmp
	stack install


.PHONY: perspec
perspec: ~/.local/bin/perspec


images/banner.bmp: images/banner.png
	magick $< $@


.PHONY: install
install: Perspec.app
	rm -rf /Applications/Perspec.app
	cp -R Perspec.app /Applications/Perspec.app


.PHONY: clean
clean:
	-rm -rf \
		~/.local/bin/perspec \
		.stack-work \
		imagemagick \
		imagemagick.tar.gz \
		Perspec.app \
