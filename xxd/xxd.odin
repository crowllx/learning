package main

import "core:encoding/hex"
import "core:fmt"
import "core:io"
import "core:os"
import "core:slice"

is_printable :: proc(b: byte) -> bool {
	return b <= 126 && b >= 32
}

hex_dump :: proc(buf: []byte, offset: int) {
	encoded := hex.encode(buf)
	encoded_format := make([dynamic]byte, 0, 40)
	defer delete(encoded)
	defer delete(encoded_format)

	left, rest := slice.split_at(encoded, 4)
	for len(rest) > 4 {
		append(&encoded_format, ..left)
		append(&encoded_format, ' ')
		left, rest = slice.split_at(rest, 4)
	}
	if len(rest) > 0 do append(&encoded_format, ..rest)

	for b, index in buf {
		if is_printable(b) {
			buf[index] = b
		} else {
			buf[index] = '.'
		}
	}

	fmt.printfln("%08x: %-40s %s", offset, encoded_format, buf)
}


main :: proc() {
	input_stream: io.Stream
	stdin_fd: os.Handle
	done := false
	remainder: []byte
	offset: int

	if len(os.args) > 1 {
		fd, err := os.open(os.args[1])
		if err != nil {
			fmt.eprintfln("Error opening file: ", err)
		}
		stdin_fd = fd
		input_stream = os.stream_from_handle(fd)
	} else {
		input_stream = os.stream_from_handle(os.stdin)
	}
	defer os.close(stdin_fd)


	buf: [4096]byte

	for !done {
		read_n, err := io.read(input_stream, buf[:])

		switch {
		case err != nil:
			done = true
		case:
			i: int
			for i = 0; i < read_n; i += 16 {
				n := 16
				if read_n - i < 16 {
					n = read_n - i
				}
				hex_dump(buf[i:i + n], offset)
				offset += 16
			}

			if i < read_n {
				remainder = buf[i:read_n]
				done = true
			}
		}
	}
}
