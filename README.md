# replit

`replit` makes your favorite CLI-tools a bit more interactive: you do not have to
type the program name and base args anymore, just some last args,
usually specific subcommands

Go from repeating

```bash
𝛌 <long tool name> <long list of args> <changed args 1>
𝛌 <long tool name> <long list of args> <changed args 2>
𝛌 # ... 
𝛌 <long tool name> <long list of args> <changed args N>
```

to just

```bash
... 𝛌 <changed args 1>
... 𝛌 <changed args 2>
... 𝛌 # ...
... 𝛌 <changed args N>
```

and `replit` will handle the left constant part,
while not changing the program behaviour at all
(for example tuis like `vim`, colored output).
So, the idea is that this might feel like a repl =)

Currently under active development, features might not present
or not work as expected and should expect tons of bugs

## Installation

Works on macOS Monterey 12.4 (M1), should work on lower versions, Linux
and other stuff as long as there is a binary for it or tools needed
for build (more info below).

For all further steps you need to clone the repo and get to the folder:

```bash
git clone https://github.com/s1m0000n/replit
cd replit
```

### Try it out

Just run `replit <your command> <left args>`
where `<left args>` can be omited if not needed

### Binaries

Right now only Apple Silicon binary is prebuilt.
To "install" it you can do (this should be pretty safe):

```bash
sudo cp replit /usr/local/bin/replit
```

To uninstall it just do (please, be cautious doing this to not delete
`/usr/local/bin`, do not use `rf -rf`,
unless you are totally sure what you are doing and why):

```bash
sudo rm /usr/local/bin/replit
```

### Building from source

You will need [stack](https://docs.haskellstack.org/en/stable/README/) which will download the deps, including compiler itself.

- To build the project just do `stack build`.
- To build & run `stack run`.
- Path to the build `stack path --local-install-root` (+ `/bin/replit` - runnable binary)
- To build and install `stack install` (do not forget to restart shell to "activate" it, so it appears as a regular shell command like `ls`)

Additional info:

- To build for macOS with Apple Silicon (M1*, M2 chips) with native `stack` i have to add `--verbose` flag for each command
- I had troubles using `stack run` with how args are passed, so for development instead `stack build; $(stack path --local-install-root)/bin/replit ...` can be used

## Configuring

Settings can be found in `src/Config.hs`.
To apply it is required to rebuild the project.
This decision is further explained in `The Why's` section.

## The Why's

### Why this project was started? (example use case)

I recently started using a new fantastic email client [himalaya](https://github.com/soywod/himalaya) a lot.
The main problem with it for me is that I always have to type the
long command name in my regular pipeline working with it like: list inbox -> read -> action like delete or reply.
The developed tool allows me to go from

```bash
𝛌 himalaya list
𝛌 himalaya read ...
𝛌 himalaya reply ...
𝛌 himalaya delete ...
...
𝛌
```

to

```bash
𝛌 replit himalaya
... himalaya 𝛌 list
... himalaya 𝛌 read
... himalaya 𝛌 reply
... himalaya 𝛌 delete
...
... himalaya 𝛌 !! q
```

where the only things I had to type are to the right of '𝛌' (your good old prompt separator)

So, this project started as a small quality of life improvement for me and I am happy to share this tiny tool.

### Why Haskell?

- Fast enough
- Feels natural to be used in the project of this kind
  - In near future much more sophisticated parsing might be needed (which is great to do in Haskell)
  - Has pretty decent tools to interact with OS (`System.Process` etc.) and terminal (like `System.Console.ANSI`)
  - Nerdy project for shell power users is fun to write using a geeky language
- Personal interest to further learn it

For those who want to start learning Haskell [and contribute]:
there is a great brief tutorial [Learn X in Y minutes](https://learnxinyminutes.com/docs/haskell/)

### Why rebuild to config?

Right now it's such a tiny tool that adding reading configs from something
like json / yaml would be an overkill and might require more code that the main logic.
I personally like this approach, especially for projects of the size.
It's planned to add support when the project grows and users really demand it.

### Why only macOS Apple Silicon binary?

Again, the project is small, for me right now it is the easiest to this thing.
Plus building and instaling is very easy - one simple tool `stack` is enough.
This would be reconsidered as the project grows.

