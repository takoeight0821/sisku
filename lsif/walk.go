package lsif

func Back(v Element, i Index) []Element {
	var result []Element
	for _, edge := range i.Edges {
		for _, inV := range edge.InVs {
			if inV == v.Id {
				result = append(result, i.GetVertex(edge.OutV))
			}
		}
	}
	return result
}

func Forward(v Element, i Index) []Element {
	var result []Element
	for _, edge := range i.Edges {
		if edge.OutV == v.Id {
			for _, inV := range edge.InVs {
				result = append(result, i.GetVertex(inV))
			}
		}
	}
	return result
}
