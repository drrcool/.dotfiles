# name: Slacker
# ---------------
# Based on clearance and nelsonjchen. Display the following bits on the left:
# - Current directory name
# - Git branch and dirty state (if inside a git repo)
# And
# - Date and time on left

function _git_branch_name
  echo (command git symbolic-ref HEAD 2> /dev/null | sed -e 's|^refs/heads/||')
end

function _git_is_dirty
  echo (command git status -s --ignore-submodules=dirty 2> /dev/null)
end

function fish_prompt
  set -l last_status $status
  set -l cyan (set_color -o cyan)
  set -l yellow (set_color -o yellow)
  set -l red (set_color -o red)
  set -l green (set_color -o green)
  set -l magenta (set_color -o magenta)
  set -l normal (set_color -o normal)
  set -l cwd $magenta(pwd | sed "s:^$HOME:~:")

  # Add a newline before new prompts
  echo -e ''

  # Print pwd or full path
  echo -n -s $cwd $normal

  # Show git branch and status
  if [ (_git_branch_name) ]
    set -l git_branch (_git_branch_name)

    if [ (_git_is_dirty) ]
      set git_info '(' $yellow $git_branch " ±" $normal ')'
    else
      set git_info '(' $green $git_branch " ✓" $normal ')'
    end
    echo -n -s ' · ' $git_info $normal
  end

  set -l prompt_color $red
  if test $last_status = 0
    set prompt_color $green
  end

  # Terminate with a nice prompt char
  echo -e ''
  echo -e -n -s $prompt_color '➜  ' $green
end