# My Haskell Solutions for Advent of Code

[Advent of Code](https://adventofcode.com/)

Principles:
- Strive for code readability
- Minimize dependency on obscure libraries
- Avoid language extensions when possible
- Each file is self-contained
- Optimize performance without compromising readability


Execution Instructions:  

This project uses Stack for building and compilation.

Build:

For windows:

```powershell
chcp 65001; stack build --fast
```

Other platform:

```bash
stack build --fast
```

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
- Frame per second (FPS) can be specified (default: 1 FPS)

Example 1: Run animation for 2024 Day 15 Problem 2 with test input (2 FPS):

```bash
stack run -- 2024 15 4 test 2
```

Example 2: Run animation for 2024 Day 15 Problem 1 with regular input (2 FPS):

```bash
stack run -- 2024 15 3 2 
```

Animation can be controlled by following keys:

- Arrow keys: Move around
- `-`/`=` key: Zoom out and Zoom in

This is my [Bilibili video record](https://www.bilibili.com/video/BV1vPC5YUEQZ) of the solutions.

# 我的 Advent of Code 的 Haskell 解法

[Advent of Code](https://adventofcode.com/)

原则：

- 代码尽量保证可读性
- 尽量不依赖于冷门的库
- 尽量不使用语言扩展
- 每个文件是独立的
- 尽量在不影响可读性的前提下提高代码性能


运行方法：

本项目依靠 stack 构建与编译

编译：

Windows 平台：

```powershell
chcp 65001; stack build --fast
```

其他平台：

```bash
stack build --fast
```

运行以下命令，将运行2024年第6天的第二道题目，题目输入为 inputs/2024/input6.txt：

```bash
stack run -- 2024 6 2
```

运行以下命令，将运行2024年第1天的第一道题目，题目输入为测试输入，即 inputs/2024/test1.txt:

```bash
stack run -- 2024 1 1 test
```

部分程序拥有动画演示功能，1、2题的动画演示分别对应的题目编号为3、4，此外，动画演示程序可以指定每秒多少帧(FPS)，如不指定，默认为每秒1帧。例如，运行以下命令，会运行2024年第15天第二题的动画演示，2 FPS，输入为测试输入：

```bash
stack run -- 2024 15 4 test 2
```

运行以下命令，会运行第15天第一道题目的动画演示，2 FPS，非测试输入内容：

```bash
stack run -- 2024 15 3 2
```

动画可以用以下按键来控制：

- 方向键: 移动视角
- `-`或者`=`: 缩放视角

这是我的[B站视频记录](https://www.bilibili.com/video/BV1vPC5YUEQZ)。
