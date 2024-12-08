```liteeffects
function addAndLet() {
  const x = 5
  const y = 3
  const result = x + y * 2
  result
}
// 11

function withArgs(a) {
  a + 3
}

function application () {
  1 + withArgs(2)
}a
// 6

function circleSquare(r) {
  const pi = perform readPi()
  pi * r * r
}

function estimatePi() {
  3
}

function calculate() {
  const r = 5
  handle circleSquare(r) with {
    readPi: estimatePi
  }
}
```