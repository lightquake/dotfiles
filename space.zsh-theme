local to_hash="$(hostname)"
local host_color=$(printf "%03d" "$(echo ${to_hash} | openssl sha1 -binary | od -N1 -tu2 -An)")
PROMPT='$FG[$host_color]%2c%f $ '
RPROMPT='$(git_prompt_info)'

ZSH_THEME_GIT_PROMPT_PREFIX="%F{yellow}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%f"
ZSH_THEME_GIT_PROMPT_DIRTY=" %F{red}!%f"
ZSH_THEME_GIT_PROMPT_CLEAN=""
