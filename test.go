package main

import "fmt"

func main() {
	// Test integers
	fmt.Print("Integer tests:\n")
	fmt.Print(0, "\n")
	fmt.Print(42, "\n")
	fmt.Print(-17, "\n")
	fmt.Print(2147483647, "\n")

	// Test booleans
	fmt.Print("Boolean tests:\n")
	fmt.Print(true, "\n")
	fmt.Print(false, "\n")
	fmt.Print(true, " ", false, "\n")

	// Test strings
	fmt.Print("String tests:\n")
	fmt.Print("Hello, World!\n")
	fmt.Print("", "\n") // empty string
	fmt.Print("Tab:\there\n")
	fmt.Print("Quote:\"test\"\n")
	fmt.Print("Backslash:\\\n")

	// Test mixed types in single Print
	fmt.Print("Mixed tests:\n")
	fmt.Print("Number: ", 100, "\n")
	fmt.Print("Bool: ", true, "\n")
	fmt.Print(1, " + ", 1, " = ", 2, "\n")
	fmt.Print(true, " and ", false, " = ", false, "\n")

	// Test string reuse (deduplication)
	fmt.Print("Reuse test:\n")
	fmt.Print("same\n")
	fmt.Print("same\n")
	fmt.Print("same\n")

	// Test multiple arguments
	fmt.Print("Multi-arg test:\n")
	fmt.Print(1, 2, 3, 4, 5, "\n")
	fmt.Print(true, false, true, false, "\n")
	fmt.Print("a", "b", "c", "d", "\n")

	// Edge cases
	fmt.Print("Edge cases:\n")
	fmt.Print(0, false, "", "\n")
	fmt.Print("\n\n\n")
}
