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
}a
// 6

const circleSquare = (r) => {
  const pi = perform readPi()
  pi * r * r
}

const estimatePi = () => {
  3
}

const calculate = () => {
  const r = 5
  handle circleSquare(r) with {
    readPi: estimatePi
  }
}
```