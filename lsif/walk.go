package lsif

// look up the vertex with the given label
func (i Index) Lookup(l string) []Element {
	var result []Element
	for _, v := range i.Elements {
		if v.Label == l {
			result = append(result, v)
		}
	}
	return result
}

// all vertexes that are reachable to the given vertex
func (i Index) Back(v Element) []Element {
	var result []Element
	for _, edge := range i.Edges {
		for _, inV := range edge.InVs {
			if inV == v.Id {
				result = append(result, i.GetElement(edge.OutV))
			}
		}
	}
	return result
}

// all vertexes that are reachable from the given vertex
func (i Index) Forward(v Element) []Element {
	result := make([]Element, 0)
	for _, edge := range i.Edges {
		if edge.OutV == v.Id {
			for _, inV := range edge.InVs {
				result = append(result, i.GetElement(inV))
			}
		}
	}
	return result
}

// all vertexes that are reachable from the given "resultSet" or "range"
func (i Index) Results(from Element) []Element {
	var result []Element
	for _, v := range i.Elements {
		if v.Id == from.Id {
			for _, next := range i.Forward(v) {
				if next.Label == "resultSet" {
					result = append(result, i.Forward(next)...)
				} else {
					result = append(result, next)

				}
			}
		}
	}
	return result
}
