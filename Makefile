
racebox-recorder: manifest.txt src/*.lisp *.asd
	buildapp --output racebox-recorder \
             --manifest-file manifest.txt \
             --load-system asdf \
             --load-system sb-posix \
             --load-system alexandria \
             --load-system racebox-tools \
             --entry 'racebox-tools:main'

manifest.txt: *.asd
	sbcl --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load "$(HOME)/quicklisp/setup.lisp" \
         --eval "(ql:quickload :alexandria)" \
		 --eval "(ql:write-asdf-manifest-file \"$(CURDIR)/manifest.txt\")"

clean:
	rm -Rf manifest.txt  *.fasl

.PHONY: clean
