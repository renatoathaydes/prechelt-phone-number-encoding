package tests

import "core:fmt"

Entry :: struct {
    values: [dynamic]string
}

Struct :: struct {
    entries: [dynamic]^Entry
}

main :: proc () {
    s := new(Struct)
    s.entries = make([dynamic]^Entry, 0, 8)
    go(s)
    fmt.println(s^)
}

go :: proc (s: ^Struct) {
    e := new(Entry)
    append(&s.entries, e)
    append(&e.values, "foo")
    append(&e.values, "bar")
}

