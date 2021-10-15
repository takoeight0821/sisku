package main

import (
	"flag"
	"fmt"
	"log"
	"net/http"
	"regexp"

	"github.com/takoeight0821/sisku/lsif"
)

func searchHandler(index lsif.Index, hovers []lsif.HoverResult) func(http.ResponseWriter, *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "<h1>Sisku</h1>\n")
		fmt.Fprintf(w, "<form action=\"/search\" method=\"GET\">\n")
		fmt.Fprintf(w, "<input type=\"search\" name=\"q\">\n")
		fmt.Fprintf(w, "<input type=\"submit\" value=\"Search\">\n")
		fmt.Fprintf(w, "</form>\n")

		query := r.URL.Query()
		if query["q"] != nil {
			fmt.Fprintf(w, "<p>You searched for: %q</p>\n", query.Get("q"))
			if r, err := regexp.Compile(query.Get("q")); err != nil {
				fmt.Fprintf(w, "Error: %v", err)
			} else {
				for i, r := range index.Search(hovers, *r) {
					fmt.Fprintf(w, "<h2> Result %d </h2>", i)
					fmt.Fprint(w, r.Render(index))
				}
			}
		}
	}
}

func main() {
	filename := flag.String("i", "dump.lsif", "input file")
	flag.Parse()

	index, hovers, err := lsif.Load(*filename)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("Loaded index")

	http.HandleFunc("/search/", searchHandler(index, hovers))
	log.Fatal(http.ListenAndServe(":8080", nil))
}
