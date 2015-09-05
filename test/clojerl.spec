%% Specific modules to include in cover.
{
 incl_mods,
  [
   clj_env,
   clj_reader,
   'clojerl.Symbol',
   'clojerl.Keyword',
   clj_utils
  ]
}.
%% Export coverage data for jenkins.
{export, "logs/cover.data"}.
