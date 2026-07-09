.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: format
format:
	fourmolu --mode inplace $$(fd -e hs)


.PHONY: test
test:
	stack test


# TODO: Don't show icon in dock (https://stackoverflow.com/a/25462666/1850340)
Perspec.app: ~/.local/bin/perspec
	platypus \
		--name Perspec \
		--app-icon images/icon.icns \
		--interface-type 'None' \
		--app-version 0.2.0.0-$$(date -u "+%Y-%m-%dT%H:%M") \
		--author "Adrian Sieber" \
		--bundled-file ~/.local/bin/perspec \
		--bundled-file app-aux-files/Credits.html \
		--bundled-file scripts \
		--bundle-identifier com.adriansieber.Perspec \
		--droppable \
		--optimize-nib \
		--overwrite \
		--quit-after-execution \
		--suffixes 'png|jpg|jpeg|bmp|gif|tiff|tif' \
		--interpreter '/bin/dash' \
		app-aux-files/perspec-gui.sh \
		$@


~/.local/bin/perspec: app source
	stack install $(STACK_OPTS)


.PHONY: perspec
perspec: ~/.local/bin/perspec


.PHONY: install
install: Perspec.app
	rm -rf /Applications/Perspec.app
	cp -R Perspec.app /Applications/Perspec.app


.PHONY: clean
clean:
	-rm -rf \
		~/.local/bin/perspec \
		.stack-work \
		Perspec.app \
