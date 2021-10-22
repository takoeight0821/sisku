package lsif

import (
	"fmt"
	"regexp"
	"strings"

	"github.com/yuin/goldmark"
	"github.com/yuin/goldmark/extension"
	"github.com/yuin/goldmark/renderer/html"
)

type SearchResult struct {
	Hover      HoverResult
	Definition Element
	Moniker    Element
	Others     []Element
}

// render SearchResult
func (r SearchResult) Render(index Index) string {
	md := goldmark.New(goldmark.WithExtensions(extension.GFM), goldmark.WithRendererOptions(
		html.WithXHTML(),
	))
	var buf strings.Builder
	fmt.Fprintln(&buf, "<h3>Definition</h3>", r.Definition, "<br/>")
	fmt.Fprintln(&buf, "<ul>")
	for _, defRange := range index.Forward(r.Definition) {
		fmt.Fprintln(&buf, "<li>", defRange)
		fmt.Fprintln(&buf, "<ul>")
		fmt.Fprintln(&buf, "<li>", index.Back(defRange), "</li>")
		fmt.Fprintln(&buf, "</ul>")
		fmt.Fprintln(&buf, "</li>")
	}
	fmt.Fprintln(&buf, "</ul>")
	fmt.Fprintln(&buf, "<h3>Moniker</h3>", r.Moniker)
	fmt.Fprintln(&buf, "<h3>Hover</h3>")
	md.Convert(([]byte)(r.Hover.Contents), &buf)
	fmt.Fprintln(&buf, "<h3>Others</h3>")
	fmt.Fprintln(&buf, "<ul>")
	for _, other := range r.Others {
		fmt.Fprintln(&buf, "<li>", other.Id, other.Label, "</li>")
	}
	fmt.Fprintln(&buf, "</ul>")

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
					switch r.Label {
					case "definitionResult":
						result.Definition = r
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
