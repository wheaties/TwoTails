package twotails

private[twotails] trait Compat { self: SizeLimited =>
  import self.global._

  // https://github.com/scala/scala/commit/870131bf4b
  type NamedArg = AssignOrNamedArg
}
