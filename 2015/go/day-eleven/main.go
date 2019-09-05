package main

func increment(s string) string {
	bs := []byte(s)
	for i := len(bs) - 1; i >= 0; i-- {
		if bs[i] < 'z' {
			bs[i]++
			return string(bs)
		}
	}

	return string(bs)
}
