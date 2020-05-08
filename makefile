all: FRC
	@echo "usage: make compile | make serve"
	@echo "assuming you want to compile..."
	@make compile
compile:
	elm make src/PhotoGroove.elm --output=app.js
serve:
	elm reactor
FRC:
