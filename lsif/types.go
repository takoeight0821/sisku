package lsif

import (
	"encoding/json"
	"errors"
	"fmt"
	"regexp"
	"strings"
)

// noFieldErr returns an error for a field that is not present in a map.
func noFieldErr(field string, m map[string]interface{}) error {
	if m[field] == nil {
		return errors.New(fmt.Sprint("no `", field, "` field\n", m))
	}
	return nil
}

type ElementType = string

const (
	ElementVertex ElementType = "vertex"
	ElementEdge   ElementType = "edge"
)

type Element struct {
	Id      int
	Label   string
	Type    ElementType
	Payload interface{}
}

func (i *Element) UnmarshalJSON(b []byte) error {
	var m map[string]interface{}
	if err := json.Unmarshal(b, &m); err != nil {
		return err
	}
	if id, ok := m["id"].(float64); ok {
		i.Id = int(id)
	} else {
		return noFieldErr("id", m)
	}
	if label, ok := m["label"].(string); ok {
		i.Label = label
	} else {
		return noFieldErr("label", m)
	}
	if typ, ok := m["type"].(string); ok {
		if typ == "vertex" {
			i.Type = ElementVertex
		} else {
			i.Type = ElementEdge
		}
	} else {
		return noFieldErr("type", m)
	}
	i.Payload = m
	return nil
}

type Edge struct {
	Id    int
	Label string
	OutV  int
	InVs  []int
}

func (e *Edge) UnmarshalJSON(b []byte) error {
	var m map[string]interface{}
	if err := json.Unmarshal(b, &m); err != nil {
		return err
	}

	if id, ok := m["id"].(float64); ok {
		e.Id = int(id)
	} else {
		return noFieldErr("id", m)
	}
	if label, ok := m["label"].(string); ok {
		e.Label = label
	} else {
		return noFieldErr("label", m)
	}
	if outV, ok := m["outV"].(float64); ok {
		e.OutV = int(outV)
	} else {
		return noFieldErr("outV", m)
	}
	if inV, ok := m["inV"].(float64); ok {
		e.InVs = []int{int(inV)}
	} else if inVs, ok := m["inVs"].([]interface{}); ok {
		e.InVs = make([]int, len(inVs))
		for i, inV := range inVs {
			switch inV := inV.(type) {
			case float64:
				e.InVs[i] = int(inV)
			default:
				return fmt.Errorf("inVs[%d] is not a float64: %T", i, inV)
			}
		}
	} else {
		return noFieldErr("inV or inVs", m)
	}

	return nil
}

type Index struct {
	Edges        []Edge
	Elements     map[int]Element
	lookupMap    map[string][]Element
	back         map[int][]Element
	results      map[int][]Element
	searchResult map[int]SearchResult
}

func NewIndex(edges []Edge, elements []Element) Index {
	elems := make(map[int]Element)
	lookupMap := make(map[string][]Element)
	for _, e := range elements {
		elems[e.Id] = e
		lookupMap[e.Label] = append(lookupMap[e.Label], e)
	}
	return Index{
		Edges:        edges,
		Elements:     elems,
		lookupMap:    lookupMap,
		back:         make(map[int][]Element),
		results:      make(map[int][]Element),
		searchResult: make(map[int]SearchResult),
	}
}

func (i Index) GetElement(id int) Element {
	return i.Elements[id]
}

type HoverResult struct {
	Id       int
	Contents string
	rng      *Range
}

func (h *HoverResult) UnmarshalJSON(b []byte) error {
	var m map[string]interface{}
	if err := json.Unmarshal(b, &m); err != nil {
		return err
	}

	if id, ok := m["id"].(float64); ok {
		h.Id = int(id)
	} else {
		return noFieldErr("id", m)
	}
	r, ok := m["result"].(map[string]interface{})
	if !ok {
		return noFieldErr("result", m)
	}
	if r["contents"] == nil {
		return noFieldErr("contents", r)
	} else {
		h.Contents = contentsString(r["contents"])
	}
	if r["range"] != nil {
		rng, err := mapToRange(m["range"].(map[string]interface{}))
		if err != nil {
			return err
		}
		h.rng = &rng
	}
	return nil
}

// contentsString returns the contents of a hover result as a string.
func contentsString(c interface{}) string {
	switch contents := c.(type) {
	// simple string
	case string:
		return contents
	// {"language": lang, "value": str}
	// as same as
	//   ```lang
	//   str
	//   ```
	case map[string]interface{}:
		if contents["language"] != nil {
			return "```" + contents["language"].(string) + "\n" + contents["value"].(string) + "\n```"
		}
		if contents["value"] != nil {
			return contents["value"].(string)
		}
		return fmt.Sprint(contents)
	// string list
	case []string:
		return strings.Join(contents, "\n")
	// string and/or {"language": lang, "value": str} list
	case []interface{}:
		var result string
		for _, contents := range contents {
			switch contents := contents.(type) {
			case string:
				result += contents + "\n"
			case map[string]interface{}:
				if contents["language"] != nil {
					result = result + "```" + contents["language"].(string) + "\n" + contents["value"].(string) + "\n```\n"
				}
			}
		}
		return result
	default:
		return fmt.Sprint(contents)
	}
}

// check the given word is included in h.Contents
func (h *HoverResult) IsMatch(r regexp.Regexp) bool {
	return r.MatchString(h.Contents)
}

// print first n lines of Contents
func (h *HoverResult) PrintHead(n int) {
	splitted := strings.Split(h.Contents, "\n")
	if len(splitted) < n {
		fmt.Println(strings.Join(splitted, "\n"))
	} else {
		fmt.Print(strings.Join(strings.Split(h.Contents, "\n")[:n], "\n"))
		fmt.Println("...")
	}
}

type Range struct {
	Start Position
	End   Position
}

// mapToRange converts a map as [start: s, end: e] to a Range{Start: s, End: e}.
func mapToRange(m map[string]interface{}) (rng Range, err error) {
	if m["start"] == nil {
		err = noFieldErr("start", m)
		return
	}
	if m["end"] == nil {
		err = noFieldErr("end", m)
		return
	}
	rng.Start, err = mapToPosition(m["start"].(map[string]interface{}))
	if err != nil {
		return
	}
	rng.End, err = mapToPosition(m["end"].(map[string]interface{}))
	return
}

type Position struct {
	Line      int
	Character int
}

// mapToPosition converts a map as [line: l, character: c] to a Position{Line: l, Character: c}.
func mapToPosition(m map[string]interface{}) (pos Position, err error) {
	if m["line"] == nil {
		err = noFieldErr("line", m)
		return
	}
	if m["character"] == nil {
		err = noFieldErr("character", m)
		return
	}
	pos = Position{Line: m["line"].(int), Character: m["character"].(int)}
	return
}
