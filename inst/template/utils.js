function seq_by(from, to, by = 1) {
	let x = [];
    for (i = 0; from + i * by <= to; i++) {
		x.push(from + i * by);
	}
	return x;
}
