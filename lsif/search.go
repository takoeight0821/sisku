package lsif

import (
	"fmt"
	"regexp"
	"strings"
)

type SearchResult struct {
	Hover      HoverResult
	Definition Element
	Moniker    Element
	Others     []Element
}

// render SearchResult
func (r SearchResult) Render(index Index) string {
	var buf strings.Builder
	fmt.Fprintln(&buf, "Hover:", r.Hover)
	fmt.Fprintln(&buf, "Definition:", r.Definition)
	for _, defRange := range index.Forward(r.Definition) {
		fmt.Fprintln(&buf, "\t", defRange)
		fmt.Fprintln(&buf, "\t", index.Back(defRange))
	}
	fmt.Fprintln(&buf, "Moniker:", r.Moniker)
	fmt.Fprintln(&buf, "Others:")
	for _, other := range r.Others {
		fmt.Fprintln(&buf, "\t", other.Id, other.Label)
	}

	return buf.String()
}

// search hoverResult in index
func (i Index) Search(hovers []HoverResult, query regexp.Regexp) (results []SearchResult) {
	for _, h := range hovers {
		var result SearchResult
		if h.IsMatch(query) {
			result.Hover = h
			for _, p := range i.Back(i.GetElement(h.Id)) {
				for _, r := range i.Results(p) {
					if r.Label == "definitionResult" {
						result.Definition = r
					} else if r.Label == "moniker" {
						result.Moniker = r
					} else {
						result.Others = append(result.Others, r)
					}
				}
			}
			results = append(results, result)
		}
	}
	return results
}
