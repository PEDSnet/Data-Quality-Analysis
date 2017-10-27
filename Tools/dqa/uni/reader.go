package uni

import "io"

// UniversalReader wraps an io.Reader to replace carriage returns with newlines.
// This is used with the csv.Reader so it can properly delimit lines.
type Reader struct {
	r io.Reader
}

func (r *Reader) Read(buf []byte) (int, error) {
	n, err := r.r.Read(buf)

	// Replace carriage returns with newlines
	for i, b := range buf {
		if b == '\r' {
			buf[i] = '\n'
		}
	}

	return n, err
}

func New(r io.Reader) *Reader {
	return &Reader{r}
}
