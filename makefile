Perspec.app: ~/.local/bin/perspec imagemagick
	platypus \
		--name Perspec \
		--app-icon images/icon.icns \
		--interface-type 'Text Window' \
		--app-version 0.1.0.0-$$(date -u "+%Y-%m-%dT%H:%M") \
		--author "Adrian Sieber" \
		--bundled-file ~/.local/bin/perspec \
		--bundled-file app-aux-files/Credits.html \
		--bundled-file imagemagick \
		--bundle-identifier org.adrian.Perspec \
		--droppable \
		--optimize-nib \
		--xml-property-lists \
		--overwrite \
		app-aux-files/perspec.sh \
		$@


imagemagick:
	rm -f imagemagick.tar.gz
	curl \
		https://imagemagick.org/download/binaries/ImageMagick-x86_64-apple-darwin19.2.0.tar.gz \
		-o imagemagick.tar.gz
	tar -xf imagemagick.tar.gz

	rm -rf imagemagick
	mv ImageMagick-7.0.9 imagemagick


~/.local/bin/perspec: app source
	stack install
