ENGINE := algebra
CLI := alsh

# Find all OCaml source files
ENGINE_SOURCES := $(shell find engine/src -name '*.ml' -or -name '*.mli')

# Find all Rust source files for the CLI
CLI_SOURCES := $(shell find cli/src -name '*.rs')

all: $(CLI)

# The 'algebra' executable depends on the dune-built executable.
$(ENGINE): engine/_build/default/src/main.exe
	@echo "Copying engine executable..."
	@cp $< $@

# The dune-built executable depends on all OCaml source files.
# If any OCaml file changes, this target will run.
engine/_build/default/src/main.exe: $(ENGINE_SOURCES)
	@echo "Building OCaml engine..."
	@cd engine && dune build --profile release

# The 'alsh' CLI depends on the 'algebra' engine and its own source.
$(CLI): $(ENGINE) $(CLI_SOURCES)
	@echo "Building Rust CLI..."
	@cd cli && cargo build
	@echo "Copying CLI executable..."
	@cp ./cli/target/debug/cli ./alsh

.PHONY: fmt clean build re clean-engine clean-cli

fmt:
	@echo "Formatting OCaml code..."
	@cd engine && opam exec -- dune fmt
	@echo "Formatting Rust code..."
	@cd cli && cargo fmt

clean: clean-cli clean-engine
	@echo "Removing top-level executables..."
	@rm -f $(CLI) $(ENGINE)

clean-cli:
	@echo "Cleaning Rust CLI..."
	@cd cli && cargo clean

clean-engine:
	@echo "Cleaning OCaml engine..."
	@cd engine && dune clean

build:
	docker-compose run --rm builder

re: clean all