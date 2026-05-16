
## Common项目

- 本仓库简称Common，以/Common/开始的命名空间/路径默认指这个库
- Github公共库有最新版本：https://github.com/lchenmay/Common
- 本仓库大部分源码为F#

- Util项目为最基础依赖，其余所有项目都要引用它
- UtilKestrel提供基于Kestrel的web服务所需的公共功能
- UtilAvalonia提供基于Avalonia的桌面应用所需的公共功能
- UtilVortice提供基于Vortice的2D桌面应用所需的公共功能


## F#函数式编程

编程规范：

当我给你提交源码时，你要理解我做出的修改，学习记忆我的编程规范

Open的顺序
- System.
- 第三方NuGet包开头的如Avalanoian.
- Util.
- 公共库开头的如UtilKestrel.
- 项目Code.Shared.
- Code.BizLogics.
- 本命名空间的
总的来说按照依赖关系，依赖越强的越靠后，分组open中间要有空行

代码生成后的复查：
- 复查open，防止找不到命名空间
- 调用自身的函数加上rec
- 尽可能减少函数签名中的类型标注，交给类型推断
- 避免使用sprintf，而是用${}
- output输出时管道后置： |> ouptut
- 函数的单个参数避免使用括号，用foo a b而非foo(a)(b)
- 很多的Collection.Add都是有返回值的，如果你不加 |> ignore 轻则警告重则编译不过
- 类的创建都要用new


