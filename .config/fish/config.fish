if status is-interactive
    # Commands to run in interactive sessions can go here
end

ssh-add -q --apple-use-keychain /Users/rcool2/.ssh/id_rsa
ssh-add -q --apple-use-keychain /Users/rcool2/.ssh/id_github
ssh-add -q --apple-use-keychain /Users/rcool2/.ssh/id_ed25519
