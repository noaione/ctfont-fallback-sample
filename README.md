coretext fallback implementation

---

i've ripped out part of the subrandr code to simulate the implementation.

the fallback works like this:
1. receive a list of font families (similar to dwrite implementation)
2. receive the actual fallback for missing glyphs/codepoint
   - This will first try to find from the list of fallback
   - If we got empty result, we let macOS decide the fallback font themselves

the implementation also support variable fonts axes:
- weight (wght)
- width (wdth)

---

to test/run: `cargo run`

it should output something like this:
```
Total fonts available: 781
[FaceInfo { family_names: ["Helvetica"], width: Fixed(100), weight: Fixed(400), italic: false, source: File { path: "/System/Library/Fonts/Helvetica.ttc", index: 0 } }]
[FaceInfo { family_names: ["Apple Color Emoji"], width: Fixed(100), weight: Fixed(400), italic: false, source: File { path: "/System/Library/Fonts/Apple Color Emoji.ttc", index: 0 } }]
[FaceInfo { family_names: ["Hiragino Sans"], width: Fixed(100), weight: Fixed(400), italic: false, source: File { path: "/System/Library/Fonts/ヒラキ\u{3099}ノ角コ\u{3099}シック W3.ttc", index: 0 } }]
[FaceInfo { family_names: ["Helvetica"], width: Fixed(100), weight: Fixed(400), italic: true, source: File { path: "/System/Library/Fonts/Helvetica.ttc", index: 2 } }]
```
