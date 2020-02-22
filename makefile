Perspec.app: ~/.local/bin/perspec
	platypus \
		--name Perspec \
		--app-icon images/icon.icns \
		--interface-type 'Text Window' \
		--app-version 0.1.0.0-$$(date -u "+%Y-%m-%dT%H:%M") \
		--author "Adrian Sieber" \
		--bundled-file ~/.local/bin/perspec \
		--bundled-file app-aux-files/Credits.html \
		--bundled-file app-aux-files/convert \
		--bundle-identifier org.adrian.Perspec \
		--droppable \
		--optimize-nib \
		--xml-property-lists \
		--overwrite \
		app-aux-files/perspec.sh \
		$@


~/.local/bin/perspec: app source
	stack install
