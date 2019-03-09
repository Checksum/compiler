# Elm compiler (native modules for all)

## Warning: This is an experiment only

This is a fork of the [Elm compiler](https://github.com/elm/compiler) version 0.19.0 with Kernel (native) modules for everyone.

## Why?

I was interested in understanding how the compiler worked and thought it would be fun to mod it do something extra. Even though I started out with v0.19.0 of Elm and have never written a line of Kernel code, I figured it was something of practical value. And here we are!

This is an extremely limited hack and works only for local applications. (Example coming soon)

## Should I use this?

__Definitely not!__ This is an experiment and nothing more. If you want to just play around with and understand how Elm kernels work, continue reading. If you want to actually use it, use ports instead.

## Installing

* Clone this repo and checkout the `0.19.0-kernel4all` branch
* Install [Stack](https://www.stackage.org)
* Run `stack build` to build the `elm` binary
