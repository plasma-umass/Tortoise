package rehearsal

object SubstitutionPlus {
  import FSPlusSyntax._

  def applySubst(stmt: Statement)(implicit subst: Substitution): Statement = stmt match {
    case SError | SSkip => stmt
    case SLet(id, e, body) => SLet(id, applySubstExpr(e), applySubst(body))
    case SIf(p, s1, s2) => ite(applySubstPred(p), applySubst(s1), applySubst(s2))
    case SSeq(s1, s2) => seq(applySubst(s1), applySubst(s2))
    case SMkdir(path) => mkdir(applySubstExpr(path))
    case SCreateFile(path, contents) => mkfile(applySubstExpr(path), applySubstExpr(contents))
    case SRm(path) => rm(applySubstExpr(path))
    case SCp(src, dst) => cp(applySubstExpr(src), applySubstExpr(dst))
  }

  def applySubstExpr(expr: Expr)(implicit subst: Substitution): Expr = expr match {
    case EId(_) => expr
    case EPath(path) => EPath(applySubstConst(path))
    case EString(str) => EString(applySubstConst(str))
    case EParent(e) => EParent(applySubstExpr(e))
    case EConcat(lhs, rhs) => EConcat(applySubstExpr(lhs), applySubstExpr(rhs))
    case EIf(p, e1, e2) => EIf(applySubstPred(p), applySubstExpr(e1), applySubstExpr(e2))
  }

  def applySubstPred(pred: Pred)(implicit subst: Substitution): Pred = pred match {
    case PTrue | PFalse => pred
    case PAnd(lhs, rhs) => applySubstPred(lhs) && applySubstPred(rhs)
    case POr(lhs, rhs) => applySubstPred(lhs) || applySubstPred(rhs)
    case PNot(pred) => !applySubstPred(pred)
    case PTestFileState(path, state) => PTestFileState(applySubstExpr(path), state)
    case PTestFileContains(p, cts) => PTestFileContains(applySubstExpr(p), applySubstExpr(cts))
  }

  def applySubstConst(const: Const)(implicit subst: Substitution): Const = const match {
    case CPath(path, loc) if subst.contains(loc) => subst(loc)
    case CString(str, loc) if subst.contains(loc) => subst(loc)
    case CPath(_, _) | CString(_, _) => const
  }
}

object SubstitutionPuppet {
  import PuppetSyntax._

  type Substitution = Map[Int, String]

  def convertSubst(subst: FSPlusSyntax.Substitution): Substitution = subst.mapValues {
    case FSPlusSyntax.CPath(p, _) => p.path.toString
    case FSPlusSyntax.CString(s, _) => s
  }

  def applySubst(m: Manifest)(implicit subst: Substitution): Manifest = applySubstManifest(m)

  def applySubstManifest(m: Manifest)(implicit subst: Substitution): Manifest = m match {
    case MEmpty => m
    case MSeq(m1, m2) => applySubstManifest(m1) >> applySubstManifest(m2)
    case MResources(resources) => MResources(resources.map(applySubstResource(_)))
    case MDefine(n, params, body) => MDefine(
      n, params.map(applySubstArgument(_)), applySubstManifest(body)
    )
    case MClass(n, params, inherits, body) => {
      MClass(n, params.map(applySubstArgument(_)), inherits, applySubstManifest(body))
    }
    case MSet(name, e) => MSet(name, applySubstExpr(e))
    case MCase(e, cases) => MCase(applySubstExpr(e), cases.map(applySubstCase(_)))
    case MIte(pred, m1, m2) => {
      MIte(applySubstExpr(pred), applySubstManifest(m1), applySubstManifest(m2))
    }
    case MInclude(es) => MInclude(es.map(applySubstExpr(_)))
    case MRequire(e) => MRequire(applySubstExpr(e))
    case MApp(name, args) => MApp(name, args.map(applySubstExpr(_)))
    case MResourceDefault(typ, attrs) => {
      MResourceDefault(typ, attrs.map(applySubstAttribute(_)))
    }
  }

  def applySubstAttribute(attr: Attribute)(implicit subst: Substitution): Attribute = Attribute(
    applySubstExpr(attr.name), applySubstExpr(attr.value)
  )

  def applySubstArgument(arg: Argument)(implicit subst: Substitution): Argument = Argument(
    arg.id, arg.default.map(applySubstExpr(_))
  )

  def applySubstCase(c: Case)(implicit subst: Substitution): Case = c match {
    case CaseDefault(m) => CaseDefault(applySubstManifest(m))
    case CaseExpr(e, m) => CaseExpr(applySubstExpr(e), applySubstManifest(m))
  }

  def applySubstResource(r: Resource)(implicit subst: Substitution): Resource = r match {
    case ResourceDecl(typ, resources) => ResourceDecl(typ, resources.map({
      case (e, attrs) => (applySubstExpr(e), attrs.map(applySubstAttribute(_)))
    }))
    case ResourceRef(typ, title, attrs) => ResourceRef(
      typ, applySubstExpr(title), attrs.map(applySubstAttribute(_))
    )
    case RCollector(typ, rexpr) => RCollector(typ, applySubstRExpr(rexpr))
  }

  def applySubstRExpr(re: RExpr)(implicit subst: Substitution): RExpr = re match {
    case REAttrEqual(attr, value) => REAttrEqual(attr, applySubstExpr(value))
    case REAnd(r1, r2) => REAnd(applySubstRExpr(r1), applySubstRExpr(r2))
    case REOr(r1, r2) => REOr(applySubstRExpr(r1), applySubstRExpr(r2))
    case RENot(r) => RENot(applySubstRExpr(r))
  }

  def applySubstExpr(e: Expr)(implicit subst: Substitution): Expr = e match {
    case EUndef | ENum(_) | EVar(_) | EBool(_) => e
    case EStr(_) if subst.contains(e.loc) => EStr(subst(e.loc))
    case EStr(_) => e
    case EStrInterp(terms) => EStrInterp(terms.map(applySubstExpr(_)))
    case ENot(e) => ENot(applySubstExpr(e))
    case EAnd(e1, e2) => EAnd(applySubstExpr(e1), applySubstExpr(e2))
    case EOr(e1, e2) => EOr(applySubstExpr(e1), applySubstExpr(e2))
    case EEq(e1, e2) => EEq(applySubstExpr(e1), applySubstExpr(e2))
    case ELT(n1, n2) => ELT(applySubstExpr(n1), applySubstExpr(n2))
    case EMatch(n1, n2) => EMatch(applySubstExpr(n1), applySubstExpr(n2))
    case EIn(n1, n2) => EIn(applySubstExpr(n1), applySubstExpr(n2))
    case EArray(es) => EArray(es.map(applySubstExpr(_)))
    case EApp(name, args) => EApp(name, args.map(applySubstExpr(_)))
    case ERegex(_) => e
    case ECond(test, tru, fls) => ECond(
      applySubstExpr(test), applySubstExpr(tru), applySubstExpr(fls)
    )
    case EResourceRef(typ, title) => EResourceRef(typ, applySubstExpr(title))
  }
}
