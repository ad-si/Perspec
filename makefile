.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: format
format:
	fourmolu --mode inplace $$(fd -e hs)
	npx --yes markdown-toc --maxdepth 3 -i readme.md


.PHONY: test
test:
	stack test


# `Contents/Resources/perspec` is kept as a link for the CLI usage
Perspec.app: ~/.local/bin/perspec app-aux-files/Info.plist
	rm -rf $@
	mkdir -p $@/Contents/MacOS $@/Contents/Resources
	cp ~/.local/bin/perspec $@/Contents/MacOS/perspec
	ln -s ../MacOS/perspec $@/Contents/Resources/perspec
	cp app-aux-files/Info.plist $@/Contents/Info.plist
	plutil -replace CFBundleVersion \
		-string "$$(plutil -extract CFBundleShortVersionString raw \
			app-aux-files/Info.plist)-$$(date -u "+%Y-%m-%dT%H:%M")" \
		$@/Contents/Info.plist
	cp images/icon.icns $@/Contents/Resources/AppIcon.icns
	cp app-aux-files/Credits.html $@/Contents/Resources/Credits.html
	app-aux-files/bundle-dylibs.sh $@
	codesign --force --sign - $@


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
