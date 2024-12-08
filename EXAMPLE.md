```liteeffects
const addAndLet = () => {
  const x = 5
  const y = 3
  const result = x + y * 2
  result
}
// 11

const withArgs = (a) => {
  a + 3
}

const application = () => {
  1 + withArgs(2)
}
// 6

effect Math {
  pi,
  sin
}

const circleSquare = (r) => {
  const pi = perform Math.pi()
  pi * r * r
}

const calculate = () => {
  const r = 5
  handle circleSquare(r) with Math {
    pi: () => 3,
    sin: (a) => a * 2
  }
}
```