Bunny - A Haskell compiler for Android
======================================

## Build environment requirements

Bunny can be built on Windows (MINGW) or Linux with GHC (Haskell Platform).
In addition to what is included in the Haskell Platform,
the following tools and libraries must be installed beforehand.

- Tools
 - GNU Make
 - HLint
 - Alex
 - Happy
- Libraries
 - ansi-wl-pprint
 - optparse-applicative

To make Android applications with Bunny,
[Android Studio](https://developer.android.com/studio)
and
[Android SDK Platform Tools](https://developer.android.com/studio/releases/platform-tools) are needed.

After installing Android Studio, you have to create at least one project
on your system because Bunny needs `local.properties` file of it.

And you need to add `jre/bin` of the Android Studio and `platform-tools` of
the Android SDK Platform Tools into your `PATH` environment variable.
By doing this, `javac`, `java` and `adb` can be found in the `PATH`
like the following example.

```
# On Windows (MINGW)
$ which javac
/c/Program Files/Android/Android Studio/jre/bin/javac
$ which java
/c/Program Files/Android/Android Studio/jre/bin/java
$ which adb
/c/Users/<user name>/android/platform-tools/adb
```

```
# On Linux
$ which javac
/home/<user name>/android-studio/jre/bin/javac
$ which java
/home/<user name>/android-studio/jre/bin/java
$ which adb
/home/<user name>/Android/Sdk/platform-tools/adb
```

## Building

The Bunny project is built with GNU Make.

```sh
cd compiler
make
```

When succeeded, you can run the sample program on the same system you build
Bunny. 


```sh
bin/bunny testrun Hello.hs
```

This will print the following message.

```
Hello, World!
```

## Installing

Before installing Bunny to your system, you have to import
`local.properties` from an Android Studio project.
This can be done like the following. MyFirstApp is just an example, and
please use the name of the project that you created by the Android Studio.

```sh
cp ~/AndroidStudioProjects/MyFirstApp/local.properties AndroidProjectPrototype/
```

Then you can install Bunny with `./install.sh`.
The installation directory will be `$HOME/bunny/0.9.0` by default.
If you want to change this, specify the path as an argument of `./install.sh`.

```sh
./install.sh
```

As the `./install.sh` will not change your `PATH` environment variable,
the directory should be added manually to your `PATH`.

## Usage

```sh
bunny android <source-file>
```

```sh
bunny android sample/Hello.hs
cd ~/BunnyProjects/Hello
./gradlew assembleDebug
```

## License

Bunny (except the Typing module) is distributed under the terms of the
MIT License, see [LICENSE.txt](LICENSE.txt).

The Typing module ([Typing module](compiler/src/Typing.hs)) is based on `Typing Haskell in Haskell', and
distributed under the terms in the file [License-Thih.txt](License-Thih.txt).
