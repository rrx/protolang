build:
	cargo build

doc:
	cargo doc --all --no-deps

test:
	cargo test -- --nocapture

deps:
	cargo modules generate graph --package lang3 | dot -Tpng > modules.png ; open modules.png
	cargo depgraph --build-deps --workspace-only | dot -Tpng > crates.png ; open crates.png
