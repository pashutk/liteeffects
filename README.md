# LiteEffects

LiteEffects is a lightweight, TypeScript-inspired programming language with built-in support for algebraic effects.

## Features

- TypeScript-like syntax
- Static typing with type inference
- First-class functions
- Algebraic effects system

## Quick Start

1. Clone the repository

2. Build the project:
```
dune build
```

3. Run the LiteEffects compiler:
```
dune exec ./liteeffects.exe
```

4. Write your first LiteEffects program:
```
// hello.le
let greet = (name: string): void => {
  perform Console.log(`Hello, ${name}!`);
};

handle(() => {
  greet("World");
}).with({
  "Console.log": (msg: string) => {
    perform Console.log(msg);
  }
});
```

5. Compile and run:
```
dune exec liteeffect.exe
```
