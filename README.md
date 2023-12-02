# Haskell-Hex-Editor

This repository contains a hex editor in Haskell with [the `brick` library](https://github.com/jtdaugherty/brick/) as the terminal UI. A hex editor is a computer program that allows for manipulation of the fundamental binary data that constitutes a computer file. We aim to build a hex editor that is easy to use and has a friendly user interface in command line.

Key features of the hex editor will include:

1. **Hexadecimal Display**: The application will present the binary content of files in a hexadecimal format, allowing users to visually inspect the data.
2. **ASCII Representation**: Alongside the hexadecimal display, the editor will show the ASCII representation of the data, making it easier for users to interpret text-based information within the binary file.
3. **Navigation and Editing**: Users will be able to navigate through the file, moving to specific offsets, and edit the content at the byte level. This includes functions like inserting, deleting, or modifying individual bytes.
4. **Search and Replace**: The hex editor will provide search and replace functionalities, allowing users to locate specific byte sequences within the file and replace them with desired values.

Some advanced features of the hex editor will include:

1. **Large File Optimization**: The editor will be able to handle large files without crashing or slowing down.
2. **Multiple Charset Support**: The editor will support multiple charsets, allowing users to view and edit files in different encodings.

## Environment Setup

* This project is developed with [cse130-assignments/cse130-devcontainer](https://github.com/cse130-assignments/cse130-devcontainer/pkgs/container/cse130-devcontainer) on Docker.
* The image `cse130-assignments/cse130-devcontainer` is based on Debian GNU/Linux 11 and GHC 9.2.7 with `stack`.
* To launch the program, run `stack run` at the root directory.

## Codebase Overview

The code is located in the [`src`](src) directory, while the tests are located in the [`test`](test) directory. The codebase is roughly divided into 2 components: the core ([`Lib.hs`](Lib.hs)) and the interface ([`UI.hs`](UI.hs)).

### The Core

The core editor is responsible for operations involving File I/O and editing. Binary files are loaded into memory with `System.IO.MMap`, and stored as buffer frames in the buffer. The core editor also responds to the user inputs, and make modifications to the file.

### The Interface

The interface is responsible for displaying the menu, file content and the corresponding ASCII representation. It's implemented with the `brick` library in the event-driven manner, by providing hooks to functionalities in the core editor.

### Implementation Highlights

Our implementation features optimizations to various console sizes, and lazy loading of large files. We followed the docs and demos of the `brick` library to solve the challenge. We are expecting to meet our goal before deadline.

## Collaborators

* Junqi Xie
* Chengsong Diao
* Dier Hou
* Kaifeng Jin

## License

* MIT License applies to this project. See `LICENSE` for more information.
