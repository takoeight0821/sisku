package main

import (
	"bufio"
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"os"
	"regexp"

	"github.com/takoeight0821/sisku/lsif"
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

	var elems []lsif.Element
	var hovers []lsif.HoverResult
	var edges []lsif.Edge
	for scanner.Scan() {
		var elem lsif.Element
		var hover lsif.HoverResult
		var edge lsif.Edge
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
			if elem.Type == lsif.ElementEdge {
				log.Fatal(err)
			}
		} else {
			edges = append(edges, edge)
		}
	}
	if err := scanner.Err(); err != nil {
		fmt.Fprintln(os.Stderr, "reading standard input:", err)
	}

	index := lsif.Index{Edges: edges, Vertexes: elems}

	for i, h := range hovers {
		if *query == "" {
			fmt.Println("<!-- Entry ", i, "-->")
			fmt.Println(h)
			v := index.GetVertex(h.Id)
			for _, p := range index.Back(v) {
				for _, r := range index.Results(p) {
					if r.Label == "definitionResult" {
						fmt.Println(index.Forward(r))
					}
					if r.Label == "moniker" {
						fmt.Println(r)
					}
				}
			}
		} else if r, err := regexp.Compile(*query); err == nil && h.IsMatch(*r) {
			fmt.Println("<!-- Entry ", i, "-->")
			h.PrintHead(10)
			v := index.GetVertex(h.Id)
			for _, p := range index.Back(v) {
				for _, r := range index.Results(p) {
					if r.Label == "definitionResult" {
						fmt.Println("definitionResult", index.Forward(r))
					} else if r.Label == "moniker" {
						fmt.Println("moniker", r)
					} else {
						fmt.Println("other", r.Id, r.Label)
					}

				}
			}
		}
	}
}
