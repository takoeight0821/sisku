package lsif

import (
	"regexp"
)

type SearchResult struct {
	Hover      HoverResult
	Definition Element
	DefRanges  []Element
	Moniker    Element
	Others     []Element
}

// search hoverResult in index
func (i Index) Search(hovers []HoverResult, query regexp.Regexp) []SearchResult {
	results := []SearchResult{}
	for _, h := range hovers {
		var result SearchResult
		result.DefRanges = []Element{}
		result.Others = []Element{}
		if h.IsMatch(query) {
			result.Hover = h
			for _, p := range i.Back(i.GetElement(h.Id)) {
				for _, r := range i.Results(p) {
					switch r.Label {
					case "definitionResult":
						result.Definition = r
						result.DefRanges = append(result.DefRanges, i.Forward(r)...)
					case "moniker":
						result.Moniker = r
					case "hoverResult":
					default:
						result.Others = append(result.Others, r)
					}
				}
			}
			results = append(results, result)
		}
	}
	return results
}
