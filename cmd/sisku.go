package main

import (
	"bufio"
	"encoding/json"
	"flag"
	"fmt"
	"github.com/takoeight0821/sisku/lsif"
	"log"
	"os"
	"regexp"
)

func main() {
	// open dump.lsif
	filename := flag.String("i", "dump.lsif", "input file")
	query := flag.String("q", "", "query")

	flag.Parse()

	file, err := os.Open(*filename)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	// Read file and parse LSIF items
	scanner := bufio.NewScanner(file)
	buf := make([]byte, 0, 64*1024)
	scanner.Buffer(buf, 1024*1024)

	var elems []sisku.Element
	var hovers []sisku.HoverResult
	var edges []sisku.Edge
	for scanner.Scan() {
		var elem sisku.Element
		var hover sisku.HoverResult
		var edge sisku.Edge
		if err := json.Unmarshal(scanner.Bytes(), &elem); err != nil {
			log.Fatal(err, scanner.Text())
		} else {
			elems = append(elems, elem)
		}
		if err := json.Unmarshal(scanner.Bytes(), &hover); err != nil {
			if elem.Label == "hoverResult" {
				log.Fatal(err)
			}
		} else {
			hovers = append(hovers, hover)
		}
		if err := json.Unmarshal(scanner.Bytes(), &edge); err != nil {
			if elem.Type == sisku.ElementEdge {
				log.Fatal(err)
			}
		} else {
			edges = append(edges, edge)
		}
	}
	if err := scanner.Err(); err != nil {
		fmt.Fprintln(os.Stderr, "reading standard input:", err)
	}

	for _, e := range edges {
		fmt.Println(e.OutV, e.InVs)
	}

	for i, h := range hovers {
		if *query == "" {
			fmt.Println("<!-- Entry ", i, "-->")
			fmt.Println(h)
		} else if r, err := regexp.Compile(*query); err == nil && h.IsMatch(*r) {
			fmt.Println("<!-- Entry ", i, "-->")
			h.PrintHead(10)
		}
	}
}
