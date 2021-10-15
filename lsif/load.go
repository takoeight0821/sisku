package lsif

import (
	"bufio"
	"encoding/json"
	"os"
)

func Load(path string) (index Index, hovers []HoverResult, err error) {
	file, err := os.Open(path)
	if err != nil {
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	buf := make([]byte, 0, 64*1024)
	scanner.Buffer(buf, 1024*1024)

	for scanner.Scan() {
		var elem Element
		if err = json.Unmarshal(scanner.Bytes(), &elem); err != nil {
			return
		} else {
			index.Elements = append(index.Elements, elem)
		}
		var hover HoverResult
		if err = json.Unmarshal(scanner.Bytes(), &hover); err != nil {
			if elem.Label == "hoverResult" {
				return
			}
		} else {
			hovers = append(hovers, hover)
		}
		var edge Edge
		if err = json.Unmarshal(scanner.Bytes(), &edge); err != nil {
			if elem.Type == ElementEdge {
				return
			}
		} else {
			index.Edges = append(index.Edges, edge)
		}
	}
	if err = scanner.Err(); err != nil {
		return
	}
	return index, hovers, nil
}
