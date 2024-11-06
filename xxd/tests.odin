package main
import "core:fmt"
import "core:os"
import "core:testing"


@(test)
main_program :: proc(t: ^testing.T) {
	fd, err := os.open("sample.txt")
	if err != nil {
		fmt.eprintln(err)
        os.exit(1)

	}
    os.stdin = fd
	main()
}
