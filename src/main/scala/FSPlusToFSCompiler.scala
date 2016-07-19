package rehearsal

import rehearsal.{FSPlusSyntax => FSP}
import rehearsal.{FSSyntax => FS}

case class FSPlusCompilationError(msg: String) extends RuntimeException(msg)

object FSPlusToFSCompiler {
  type Env = Map[String, FSP.Const]

  def compile(stmt: FSP.Statement): FS.Expr = compileStatement(stmt)(Map())

  def compileStatement(stmt: FSP.Statement)(implicit env: Env): FS.Expr = stmt match {
    case FSP.SError => FS.EError
    case FSP.SSkip => FS.ESkip
    case FSP.SLet(id, e, body) => compileStatement(body)(env + (id -> compileExpr(e)))
    case FSP.SIf(p, s1, s2) => FS.ite(
      compilePred(p), compileStatement(s1), compileStatement(s2)
    )
    case FSP.SSeq(s1, s2) => FS.seq(compile(s1), compile(s2))
    case FSP.SMkdir(path) => FS.mkdir(constToPath(compileExpr(path)))
    case FSP.SCreateFile(path, contents) => FS.createFile(
      constToPath(compileExpr(path)), constToString(compileExpr(contents))
    )
    case FSP.SRm(path) => FS.rm(constToPath(compileExpr(path)))
    case FSP.SCp(src, dst) => FS.cp(
      constToPath(compileExpr(src)), constToPath(compileExpr(dst))
    )
  }

  def constToPath(const: FSP.Const): Path = const match {
    case FSP.CPath(p, _) => p.path
    case _ => throw FSPlusCompilationError(s"Expected path, found `$const`.")
  }

  def constToString(const: FSP.Const): String = const match {
    case FSP.CString(str, _) => str
    case _ => throw FSPlusCompilationError(s"Expected string, found `$const`.")
  }

  def compileExpr(expr: FSP.Expr)(implicit env: Env): FSP.Const = expr match {
    case FSP.EId(id) => env.get(id) match {
      case Some(const) => const
      case None => throw FSPlusCompilationError(s"Found unbound identifier `$id`.")
    }
    case FSP.EPath(path) => path
    case FSP.EString(str) => str
    case FSP.EParent(expr) => compileExpr(expr) match {
      case FSP.CPath(path, loc) => FSP.CPath(FSP.Parent(path), loc)
      case const => throw FSPlusCompilationError(s"Expected path, found `$const`.")
    }
    case FSP.EConcat(lhs, rhs) => (compileExpr(lhs), compileExpr(rhs)) match {
      case (FSP.CPath(p1, loc), FSP.CPath(p2, _)) => FSP.CPath(FSP.Concat(p1, p2), loc)
      case (FSP.CPath(p, loc), FSP.CString(s, _)) => FSP.CPath(FSP.Concat(p, FSP.JavaPath(s)), loc)
      case (const, _) => throw FSPlusCompilationError(s"Expected path, found `$const`.")
    }
    case FSP.EIf(_, _, _) => throw FSPlusCompilationError(s"Cannot compile if expressions.\nFound: `$expr`")
  }

  def compilePred(pred: FSP.Pred)(implicit env: Env): FS.Pred = pred match {
    case FSP.PTrue => FS.PTrue
    case FSP.PFalse => FS.PFalse
    case FSP.PAnd(lhs, rhs) => compilePred(lhs) && compilePred(rhs)
    case FSP.POr(lhs, rhs) => compilePred(lhs) || compilePred(rhs)
    case FSP.PNot(pred) => !compilePred(pred)
    case FSP.PTestFileState(path, st) => FS.testFileState(constToPath(compileExpr(path)), compileFileState(st))
  }

  def compileFileState(st: FSP.FileState): FS.FileState = st match {
    case FSP.IsFile => FS.IsFile
    case FSP.IsDir => FS.IsDir
    case FSP.DoesNotExist => FS.DoesNotExist
  }
}
