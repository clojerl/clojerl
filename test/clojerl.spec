%% Specific modules to include in cover.
{
 incl_mods,
  [
   clj_reader,
   clj_utils,
   clj_env,
   clj_symbol,
   clj_keyword
  ]
}.
%% Export coverage data for jenkins.
{export, "logs/cover.data"}.
