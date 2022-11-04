default:
	cargo doc --all --no-deps
	cargo build

test:
	cargo test -- --nocapture

