

ENGINE := algebra
CLI := alsh


all: $(CLI)

$(ENGINE): 
	cd engine && dune build 
	cp ./engine/_build/default/src/main.exe ./algebra

$(CLI): $(ENGINE)
	cd cli && cargo build
	cp ./cli/target/debug/cli ./alsh

fmt: 
	cd engine && opam exec -- dune fmt
	cd cli && cargo fmt

clean:
	rm -rf $(CLI) $(ENGINE)

re: clean all

