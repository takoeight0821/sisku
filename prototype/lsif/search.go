package lsif

import "strings"

type SearchResult struct {
	Hover      HoverResult
	Definition Element
	DefRanges  []Element
	Moniker    Element
	Others     map[int]Element
}

// search hoverResult in index
func (i Index) Search(hovers []HoverResult, query string) []SearchResult {
	results := []SearchResult{}
	for _, h := range hovers {
		if strings.Contains(h.Contents, query) {
			result := SearchResult{DefRanges: []Element{}, Others: make(map[int]Element)}
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
						result.Others[r.Id] = r
					}
				}
			}
			results = append(results, result)
		}
	}
	return results
}
