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

type Item struct {
	Id       int                    `json:"id"`
	Label    string                 `json:"label"`
	Type     string                 `json:"type"`
	Original map[string]interface{} `json:"-"`
}

func ParseItem(b []byte) (item Item, err error) {
	if err = json.Unmarshal(b, &item); err == nil {
		err = json.Unmarshal(b, &item.Original)
	}
	return
}

type Hover struct {
	Contents string
	Range    *Range
}

func MapToHover(m map[string]interface{}) (hover Hover) {
	if m["contents"] != nil {
		hover.Contents = contentsString(m["contents"])
	}
	if m["range"] != nil {
		rng, err := MapToRange(m["range"].(map[string]interface{}))
		if err != nil {
			hover.Range = nil
		} else {
			hover.Range = &rng
		}
	}
	return
}

func contentsString(c interface{}) string {
	switch contents := c.(type) {
	case string:
		return contents
	case map[string]interface{}:
		if contents["language"] != nil {
			return "```" + contents["language"].(string) + "\n" + contents["value"].(string) + "\n```"
		}
		if contents["value"] != nil {
			return contents["value"].(string)
		}
		return fmt.Sprint(contents)
	case []string:
		return strings.Join(contents, "\n")
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

func MapToRange(m map[string]interface{}) (rng Range, err error) {
	if m["start"] == nil {
		err = errors.New("`start` is not found")
		return
	}
	if m["end"] == nil {
		err = errors.New("`end` is not found")
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

func MapToPosition(m map[string]interface{}) (pos Position, err error) {
	if m["line"] == nil {
		err = errors.New("`line` is not found")
		return
	}
	if m["character"] == nil {
		err = errors.New("`character` is not found")
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
	var items []Item
	for scanner.Scan() {
		item, err := ParseItem(scanner.Bytes())
		if err != nil {
			log.Fatal(err, scanner.Text())
		}
		items = append(items, item)
	}
	if err := scanner.Err(); err != nil {
		fmt.Fprintln(os.Stderr, "reading standard input:", err)
	}

	// Collect hover infomations
	var hovers []Hover
	for _, item := range items {
		if item.Label == "hoverResult" {
			hover := MapToHover(item.Original["result"].(map[string]interface{}))
			hovers = append(hovers, hover)
		}
	}
	for i, hover := range hovers {
		fmt.Println("<!-- Entry ", i, "-->")
		fmt.Println(hover.Contents)
	}
}
