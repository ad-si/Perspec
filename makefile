.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: test
test:
	gcc -Wall cbits/test.c cbits/simplecv.c cbits/perspectivetransform.c -o test_bin && ./test_bin
	gcc -Wall cbits/simplecv.c cbits/perspectivetransform.c cbits/apply_test.c -o apply_test && ./apply_test
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


# TODO: Fix crash after dropping image
# TODO: Implement drag & drop for dock icon (WIP at macos-app-wrapper)
PerspecSimple.app: ~/.local/bin/perspec
	mkdir -p $@
	mkdir -p $@/Contents

	mkdir -p $@/Contents/MacOS
	cp $< $@/Contents/MacOS/PerspecSimple

	mkdir -p $@/Contents/Resources
	cp app-aux-files/Info.plist $@/Contents
	cp images/icon.icns $@/Contents/Resources/AppIcon.icns
	cp app-aux-files/Credits.html $@/Contents/Resources
	cp $< $@/Contents/Resources


PerspecWithMagick.app: ~/.local/bin/perspec imagemagick
	platypus \
		--name PerspecWithMagick \
		--app-icon images/icon.icns \
		--interface-type 'Text Window' \
		--app-version 0.2.0.0-$$(date -u "+%Y-%m-%dT%H:%M") \
		--author "Adrian Sieber" \
		--bundled-file ~/.local/bin/perspec \
		--bundled-file app-aux-files/Credits.html \
		--bundled-file imagemagick \
		--bundled-file scripts \
		--bundle-identifier com.adriansieber.PerspecWithMagick \
		--droppable \
		--optimize-nib \
		--overwrite \
		--quit-after-execution \
		--suffixes 'png|jpg|jpeg|bmp|gif|tiff|tif' \
		--interpreter '/bin/dash' \
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
