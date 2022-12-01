input ← ⊃⎕NGET 'inputs/day01.input' 1
parsed ← ⍎¨¨{(×≢¨⍵)⊆⊢⍵}input
⎕ ← ⌈/+/¨parsed
⎕ ← +/3↑{⍵[⍒⍵]}+/¨parsed
