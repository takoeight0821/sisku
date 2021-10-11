package main

import (
	"bufio"
	"encoding/json"
	"errors"
	"fmt"
	"log"
	"os"
	"strings"
)

// noFieldErr returns an error for a field that is not present in a map.
func noFieldErr(field string, m map[string]interface{}) error {
	if m[field] == nil {
		return errors.New(fmt.Sprint("no `", field, "` field\n", m))
	}
	return nil
}

type ItemType = int

const (
	Vertex = iota
	Edge
)

type IsItem interface {
	Id() int
	Label() string
	Type() ItemType
}

type Item struct {
	id       int
	label    string
	typ      ItemType
	original map[string]interface{}
}

func (i *Item) Id() int {
	return i.id
}
func (i *Item) Label() string {
	return i.label
}
func (i *Item) Type() ItemType {
	return i.typ
}

func (i *Item) UnmarshalJSON(b []byte) error {
	var m map[string]interface{}
	if err := json.Unmarshal(b, &m); err != nil {
		return err
	}
	if id, ok := m["id"].(float64); ok {
		i.id = int(id)
	} else {
		return noFieldErr("id", m)
	}
	if label, ok := m["label"].(string); ok {
		i.label = label
	} else {
		return noFieldErr("label", m)
	}
	if typ, ok := m["type"].(string); ok {
		if typ == "vertex" {
			i.typ = Vertex
		} else {
			i.typ = Edge
		}
	} else {
		return noFieldErr("type", m)
	}
	i.original = m
	return nil
}

type Hover struct {
	id int
	// Label = "hoverResult"
	// Type = "vertex"
	Contents string
	rng      *Range
}

func (h *Hover) Id() int {
	return h.id
}
func (h *Hover) Label() string {
	return "hoverResult"
}
func (h *Hover) Type() ItemType {
	return Vertex
}

func (h *Hover) UnmarshalJSON(b []byte) error {
	var m map[string]interface{}
	if err := json.Unmarshal(b, &m); err != nil {
		return err
	}

	if id, ok := m["id"].(float64); !ok {
		return noFieldErr("id", m)
	} else {
		h.id = int(id)
	}

	if label, ok := m["label"].(string); !ok {
		return noFieldErr("label", m)
	} else if label != "hoverResult" {
		return errors.New(fmt.Sprint("`label` field must be \"hoverResult\"\n", m))
	}

	if typ, ok := m["type"].(string); !ok {
		return noFieldErr("type", m)
	} else if typ != "vertex" {
		return errors.New(fmt.Sprint("`type` field must be \"vertex\"\n", m))
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
		rng, err := MapToRange(m["range"].(map[string]interface{}))
		if err != nil {
			return err
		} else {
			h.rng = &rng
		}
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

type Range struct {
	Start Position
	End   Position
}

// MapToRange converts a map as [start: s, end: e] to a Range{Start: s, End: e}.
func MapToRange(m map[string]interface{}) (rng Range, err error) {
	if m["start"] == nil {
		err = noFieldErr("start", m)
		return
	}
	if m["end"] == nil {
		err = noFieldErr("end", m)
		return
	}
	rng.Start, err = MapToPosition(m["start"].(map[string]interface{}))
	if err != nil {
		return
	}
	rng.End, err = MapToPosition(m["end"].(map[string]interface{}))
	return
}

type Position struct {
	Line      int
	Character int
}

// MapToPosition converts a map as [line: l, character: c] to a Position{Line: l, Character: c}.
func MapToPosition(m map[string]interface{}) (pos Position, err error) {
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

func main() {
	// open dump.lsif
	// TODO: get input filename from argv
	file, err := os.Open("dump.lsif")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	// Read file and parse LSIF items
	scanner := bufio.NewScanner(file)
	buf := make([]byte, 0, 64*1024)
	scanner.Buffer(buf, 1024*1024)

	var hovers []Hover
	for scanner.Scan() {
		var item Item
		var hover Hover
		if err := json.Unmarshal(scanner.Bytes(), &item); err != nil {
			log.Fatal(err, scanner.Text())
		}
		if err := json.Unmarshal(scanner.Bytes(), &hover); err != nil {
			if item.label == "hoverResult" {
				log.Fatal(err)
			}
		} else {
			hovers = append(hovers, hover)
		}
	}
	if err := scanner.Err(); err != nil {
		fmt.Fprintln(os.Stderr, "reading standard input:", err)
	}

	for i, hover := range hovers {
		fmt.Println("<!-- Entry ", i, "-->")
		fmt.Println(hover.Contents)
	}
}
