# My Haskell Solutions for Advent of Code

[Advent of Code](https://adventofcode.com/)

Principles:
- Strive for code readability
- Minimize dependency on obscure libraries
- Avoid language extensions when possible
- Each file is self-contained
- Optimize performance without compromising readability

Build Dependencies:
Requires `libsdl2-dev` and `libsdl2-ttf-dev`.  
For Debian-based Linux distributions, run:

```bash
sudo apt install libsdl2-dev libsdl2-ttf-dev
```

Execution Instructions:  
This project uses Stack for building and compilation.

To run the second problem of Day 6 (2024) with input from `inputs/2024/input6.txt`:

```bash
stack run -- 2024 6 2
```

To run the first problem of Day 1 (2024) with test input from `inputs/2024/test1.txt`:

```bash
stack run -- 2024 1 1 test
```

Some solutions feature animation demonstrations:
- Problem 1 animation corresponds to problem ID 3
- Problem 2 animation corresponds to problem ID 4
- Frame duration can be specified (default: 1 second per frame)

Example 1: Run animation for 2024 Day 15 Problem 2 with test input (0.5s/frame):

```bash
stack run -- 2024 15 4 test 0.5
```

Example 2: Run animation for 2024 Day 15 Problem 1 with regular input (0.5s/frame):

```bash
stack run -- 2024 15 3 0.5
```

This is my [Bilibili video record](https://www.bilibili.com/video/BV1vPC5YUEQZ) of the solutions.

# 我的 Advent of Code 的 Haskell 解法

[Advent of Code](https://adventofcode.com/)

原则：

- 代码尽量保证可读性
- 尽量不依赖于冷门的库
- 尽量不使用语言扩展
- 每个文件是独立的
- 尽量在不影响可读性的前提下提高代码性能

编译依赖：
需要 `libsdl2-dev` 与 `libsdl2-ttf-dev`
对于 `Debian` 系列的 linux 系统，可以运行以下命令进行安装：

```bash
sudo apt install libsdl2-dev libsdl2-ttf-dev
```

运行方法：

本项目依靠 stack 构建与编译

运行以下命令，将运行2024年第6天的第二道题目，题目输入为 inputs/2024/input6.txt：

```bash
stack run -- 2024 6 2
```

运行以下命令，将运行2024年第1天的第一道题目，题目输入为测试输入，即 inputs/2024/test1.txt:

```bash
stack run -- 2024 1 1 test
```

部分程序拥有动画演示功能，1、2题的动画演示分别对应的题目编号为3、4，此外，动画演示程序可以指定每帧多少秒，如不指定，默认为每帧1秒。例如，运行以下命令，会运行2024年第15天第二题的动画演示，每帧0.5秒，输入为测试输入：

```bash
stack run -- 2024 15 4 test 0.5
```

运行以下命令，会运行第15天第一道题目的动画演示，每帧0.5秒，非测试输入内容：

```bash
stack run -- 2024 15 3 0.5
```

这是我的[B站视频记录](https://www.bilibili.com/video/BV1vPC5YUEQZ)。
