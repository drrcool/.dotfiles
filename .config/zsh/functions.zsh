function cd() { 
  builtin cd $1

  if [[ -n "$VIRTUAL_ENV" && -d ./.venv ]] ; then
    deactivate
  fi 

  if [[ -d ./.venv ]] ; then
    source ./.venv/bin/activate
  fi
}
