# hdm: Haskell Download Manager

A download manager for in the terminal, that supports multi-part download acceleration, and is written in Haskell.

## Building
Compilation requires `stack`. To build it execute the following.

```
stack build
```
## Usage
The program can be executed with `stack exec hdm --`. For usage see below.

```hdm - Haskell download manager

Usage: hdm [--dir DIR] [-n|--num-threads NUM] [-d|--enable-debug]
  Start the download manager

Available options:
  --dir DIR                Directory to save
                           downloads (default: "/home/user/Downloads")
  -n,--num-threads NUM     Number of threads per download (default: 1)
  -d,--enable-debug        Whether to enable debugging
  -h,--help                Show this help text
```
## Screenshot
![](screenshot/hdm.png)
