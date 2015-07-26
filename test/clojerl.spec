%% Specific modules to include in cover.
{
 incl_mods,
  [
   clj_env,
   clj_keyword,
   clj_meta,
   clj_reader,
   clj_symbol,
   clj_utils
  ]
}.
%% Export coverage data for jenkins.
{export, "logs/cover.data"}.
