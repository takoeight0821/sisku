package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"log"

	"github.com/takoeight0821/sisku/lsif"
)

func main() {
	// open dump.lsif
	filename := flag.String("i", "dump.lsif", "input file")
	query := flag.String("q", "", "query")

	flag.Parse()

	index, hovers, err := lsif.Load(*filename)
	if err != nil {
		log.Fatal(err)
	}

	json, err := json.MarshalIndent(index.Search(hovers, *query), "", "  ")
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(string(json))
}
