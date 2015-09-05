%% Specific modules to include in cover.
{
 incl_mods,
 [
  clj_analyzer,
  clj_compiler,clj_core,clj_emitter,clj_env,clj_namespace,
  clj_reader,clj_utils,'clojerl.Counted','clojerl.IDeref',
  'clojerl.ILookup','clojerl.IMeta','clojerl.ISeq',
  'clojerl.Keyword','clojerl.Keyword.clojerl.IMeta',
  'clojerl.Keyword.clojerl.Named','clojerl.List',
  'clojerl.List.clojerl.Counted','clojerl.List.clojerl.ISeq',
  'clojerl.Map','clojerl.Map.clojerl.Counted','clojerl.Named',
  'clojerl.Set','clojerl.Set.clojerl.Counted',
  'clojerl.Stringable','clojerl.Symbol',
  'clojerl.Symbol.clojerl.IMeta',
  'clojerl.Symbol.clojerl.Named',
  'clojerl.Symbol.clojerl.Stringable','clojerl.Var',
  'clojerl.Var.clojerl.IDeref','clojerl.Vector',
  'clojerl.Vector.clojerl.Counted',
  'clojerl.Vector.clojerl.ISeq','clojerl.protocol'
 ]
}.
%% Export coverage data for jenkins.
{export, "logs/cover.data"}.
