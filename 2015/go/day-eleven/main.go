package main

func increment(s string) string {
	bs := []byte(s)
	b := innercrement(bs)
	if b {
		return string(append(bs, 'a'))
	}

	return string(bs)
}

func innercrement(bs []byte) bool {
	for i := len(bs) - 1; i >= 0; i-- {
		if bs[i] < 'z' {
			bs[i]++
			return false
		}
		if bs[i] == 'z' {
			bs[i] = 'a'
			if i == 0 {
				return true
			}
		}
	}
	return false
}
