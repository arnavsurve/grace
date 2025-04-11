.PHONY: build test grace run clean

GRACE_BIN=grace
GRACE_SRC=./cmd/grace
OUT_DIR=out

# Default: build the grace binary
build:
	go build -o $(GRACE_BIN) $(GRACE_SRC)

# Run test runner (good + bad)
test:
	go run ./test/run.go

# Compile .grc to COBOL only
grace:
	@echo "🔧 Compiling Grace source to COBOL: $(FILE)"
	@go run ./cmd/grace $(FILE)

# Compile to COBOL, then compile COBOL to binary and run it
run:
	@echo "🔧 Compiling Grace → COBOL → binary and running: $(FILE)"
	@go run ./cmd/grace $(FILE)

	@# Get name without extension
	@NAME=$$(basename $(FILE) .grc); \
	COBOL_FILE=$(OUT_DIR)/$$NAME.cbl; \
	BIN_FILE=$(OUT_DIR)/$$NAME.out; \
	\
	echo "🛠️  Compiling COBOL with GnuCOBOL..."; \
	cobc -x -o $$BIN_FILE $$COBOL_FILE; \
	echo "🚀 Output:"; \
	./$$BIN_FILE

# Clean build artifacts
clean:
	rm -f $(GRACE_BIN)
	rm -rf $(OUT_DIR)/*

