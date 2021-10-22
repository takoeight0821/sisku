package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"net/http"
	"regexp"

	"github.com/takoeight0821/sisku/lsif"
)

type ServerError struct {
	Message string `json:"message"`
}

func searchHandler(index lsif.Index, hovers []lsif.HoverResult) func(http.ResponseWriter, *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		query := r.URL.Query()
		if query["q"] == nil {
			sErr, err := json.Marshal(ServerError{Message: "query parameter is required"})
			if err != nil {
				log.Fatal(err)
			}
			http.Error(w, string(sErr), http.StatusBadRequest)
			return
		}
		reg, err := regexp.Compile(query.Get("q"))
		if err != nil {
			sErr, err := json.Marshal(ServerError{Message: err.Error()})
			if err != nil {
				log.Fatal(err)
			}
			http.Error(w, string(sErr), http.StatusBadRequest)
			return
		}
		result, err := json.MarshalIndent(index.Search(hovers, *reg), "", "  ")
		if err != nil {
			sErr, err := json.Marshal(ServerError{Message: err.Error()})
			if err != nil {
				log.Fatal(err)
			}
			http.Error(w, string(sErr), http.StatusInternalServerError)
			return
		}
		fmt.Fprintln(w, string(result))
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

	http.HandleFunc("/api/search/", searchHandler(index, hovers))
	log.Fatal(http.ListenAndServe(":8080", nil))
}
