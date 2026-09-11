// Generates images/icon.svg and images/icon.png (the marketplace icon) from one set of shapes.
// Run with: node scripts/make-icon.mjs
import { deflateSync } from "node:zlib";
import { writeFileSync } from "node:fs";

const SIZE = 256;          // icon is drawn on a 256x256 grid
const SS = 4;              // supersampling factor per axis (antialiasing)

const BG = "#20242E", FUR = "#E8722E", FUR_DARK = "#C9521C", EAR = "#F6B98D", LIGHT = "#FFF1E2", INK = "#2A1A12";

// Head outline: broad skull narrowing to a pointed snout.
const head = [[40, 110], [128, 92], [216, 110], [196, 160], [128, 216], [60, 160]];
const earL = [[40, 110], [62, 34], [112, 95]];
const earR = [[216, 110], [194, 34], [144, 95]];
const inner = (t) => { // shrink a triangle toward its centroid
	const c = [(t[0][0] + t[1][0] + t[2][0]) / 3, (t[0][1] + t[1][1] + t[2][1]) / 3];
	return t.map(([x, y]) => [x + (c[0] - x) * 0.42, y + (c[1] - y) * 0.42]);
};
const snout = [[88, 152], [168, 152], [128, 216]];
const nose = [[117, 184], [139, 184], [128, 198]];

const shapes = [
	{ round: [0, 0, SIZE, SIZE, 56], fill: BG },
	{ poly: earL, fill: FUR_DARK },
	{ poly: earR, fill: FUR_DARK },
	{ poly: inner(earL), fill: EAR },
	{ poly: inner(earR), fill: EAR },
	{ poly: head, fill: FUR },
	{ poly: snout, fill: LIGHT },
	{ ellipse: [100, 134, 13, 9, -14], fill: INK },
	{ ellipse: [156, 134, 13, 9, 14], fill: INK },
	{ poly: nose, fill: INK },
];

const hex = (h) => [parseInt(h.slice(1, 3), 16), parseInt(h.slice(3, 5), 16), parseInt(h.slice(5, 7), 16)];

function hit(s, x, y) {
	if (s.poly) {
		let inside = false;
		for (let i = 0, j = s.poly.length - 1; i < s.poly.length; j = i++) {
			const [xi, yi] = s.poly[i], [xj, yj] = s.poly[j];
			if ((yi > y) !== (yj > y) && x < ((xj - xi) * (y - yi)) / (yj - yi) + xi) inside = !inside;
		}
		return inside;
	}
	if (s.ellipse) {
		const [cx, cy, rx, ry, deg] = s.ellipse, a = (-deg * Math.PI) / 180;
		const dx = x - cx, dy = y - cy;
		const u = (dx * Math.cos(a) - dy * Math.sin(a)) / rx, v = (dx * Math.sin(a) + dy * Math.cos(a)) / ry;
		return u * u + v * v <= 1;
	}
	const [rx, ry, w, h, r] = s.round;
	if (x < rx || y < ry || x > rx + w || y > ry + h) return false;
	const qx = Math.min(Math.max(x, rx + r), rx + w - r), qy = Math.min(Math.max(y, ry + r), ry + h - r);
	return (x - qx) ** 2 + (y - qy) ** 2 <= r * r;
}

// Rasterize: average SS*SS subsamples per pixel, painting shapes back to front.
const px = Buffer.alloc(SIZE * SIZE * 4);
for (let y = 0; y < SIZE; y++) for (let x = 0; x < SIZE; x++) {
	let r = 0, g = 0, b = 0, a = 0;
	for (let sy = 0; sy < SS; sy++) for (let sx = 0; sx < SS; sx++) {
		const fx = x + (sx + 0.5) / SS, fy = y + (sy + 0.5) / SS;
		let c = null;
		for (const s of shapes) if (hit(s, fx, fy)) c = s.fill;
		if (c) { const [cr, cg, cb] = hex(c); r += cr; g += cg; b += cb; a += 255; }
	}
	const n = SS * SS, i = (y * SIZE + x) * 4;
	if (a) { px[i] = Math.round(r / (a / 255)); px[i + 1] = Math.round(g / (a / 255)); px[i + 2] = Math.round(b / (a / 255)); }
	px[i + 3] = Math.round(a / n);
}

// Minimal PNG writer (RGBA8, no filtering).
const crcTable = Array.from({ length: 256 }, (_, n) => { let c = n; for (let k = 0; k < 8; k++) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1; return c >>> 0; });
const crc = (buf) => { let c = 0xffffffff; for (const byte of buf) c = crcTable[(c ^ byte) & 0xff] ^ (c >>> 8); return (c ^ 0xffffffff) >>> 0; };
function chunk(type, data) {
	const len = Buffer.alloc(4); len.writeUInt32BE(data.length);
	const body = Buffer.concat([Buffer.from(type, "latin1"), data]);
	const sum = Buffer.alloc(4); sum.writeUInt32BE(crc(body));
	return Buffer.concat([len, body, sum]);
}
const ihdr = Buffer.alloc(13);
ihdr.writeUInt32BE(SIZE, 0); ihdr.writeUInt32BE(SIZE, 4); ihdr[8] = 8; ihdr[9] = 6;
const raw = Buffer.alloc(SIZE * (SIZE * 4 + 1));
for (let y = 0; y < SIZE; y++) px.copy(raw, y * (SIZE * 4 + 1) + 1, y * SIZE * 4, (y + 1) * SIZE * 4);
writeFileSync("images/icon.png", Buffer.concat([
	Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]),
	chunk("IHDR", ihdr), chunk("IDAT", deflateSync(raw, { level: 9 })), chunk("IEND", Buffer.alloc(0)),
]));

// Same geometry as SVG, so the icon can be re-rendered or tweaked by hand.
const pts = (p) => p.map(([x, y]) => `${+x.toFixed(1)},${+y.toFixed(1)}`).join(" ");
const svg = [`<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${SIZE} ${SIZE}" width="${SIZE}" height="${SIZE}">`];
for (const s of shapes) {
	if (s.round) svg.push(`<rect x="${s.round[0]}" y="${s.round[1]}" width="${s.round[2]}" height="${s.round[3]}" rx="${s.round[4]}" fill="${s.fill}"/>`);
	else if (s.poly) svg.push(`<polygon points="${pts(s.poly)}" fill="${s.fill}"/>`);
	else svg.push(`<ellipse cx="${s.ellipse[0]}" cy="${s.ellipse[1]}" rx="${s.ellipse[2]}" ry="${s.ellipse[3]}" fill="${s.fill}" transform="rotate(${s.ellipse[4]} ${s.ellipse[0]} ${s.ellipse[1]})"/>`);
}
svg.push("</svg>\n");
writeFileSync("images/icon.svg", svg.join("\n"));
console.log("wrote images/icon.png and images/icon.svg");
