install: build app
	./install.sh && prismaParser -s -a

test:
	spago test

watch:
	spago build --watch --then ./build-post.sh

build:
	spago build

app:
	spago bundle-app  --platform=node -t Main.mjs

