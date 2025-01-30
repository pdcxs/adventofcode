# 我的 Advent of Code 的 Haskell 解法

项目地址: https://adventofcode.com/

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
