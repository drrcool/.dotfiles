local status_ok, Worktree = pcall(require, 'git-worktree')
if not status_ok then return end

Worktree.on_tree_change(function(op, metadata)
  if op == Worktree.Operations.Switch then
    print("Switched from" .. metadata.prev_path .. " to " .. metadata.path)
  end
end)
