Perspec.app: ~/.local/bin/perspec
	platypus \
		--name Perspec \
		--app-icon images/icon.icns \
		--interface-type 'Text Window' \
		--app-version 0.1.0.0 \
		--author "Adrian Sieber" \
		--bundled-file /Users/adrian/.local/bin/perspec \
		--bundle-identifier org.adrian.Perspec \
		--droppable \
		--optimize-nib \
		--xml-property-lists \
		--overwrite \
		perspec.sh \
		$@


~/.local/bin/perspec:
	stack install
