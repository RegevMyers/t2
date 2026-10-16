set shell := [ "bash", "-cu" ]

@__default: 
    just --list --unsorted
    echo

# Run t2c
[group("run")]
@t2c +cli="":
    cabal run t2c {{ cli }}

# Build all
[group("run")]
@build:
    cabal build all --enable-test

# Rebuild
[group("run")]
@rebuild:
    just clean
    just build

# Run tests
[group("run")]
@test:
    cabal build -v0 all --enable test
    cabal test -v1 all --test-show-details=failures

# Clean
[group("run")]
@clean:
    cabal clean

# Fetch
[group("vcs")]
@_fetch:
    git fetch origin

# Pull
[group("vcs")]
@pull: (_fetch)
    git pull

timestamp := shell("date -u")

# Push
[group("vcs")]
@push +msg=timestamp: (ci)
    git add -A
    - git commit -m "{{ msg }}"
    git push -u origin HEAD

# Create brach
[group("vcs")]
@create-branch name: (_fetch)
    git switch -c {{ name }} origin/dev

# Lint
[group("lint")]
@lint: 
    echo ...

# CI: Lint...
[group("lint")]
@ci: 
    just lint

# Lines of Code
[group("util")]
@loc:
    fdfind -e hs --strip-cwd-prefix -0 | xargs -0 wc -l | sort -n -r | sed -E $'1s|(.*)|\033[1m\\1\033[0m|'

# Clear screen
[group("util")]
@clear:
    clear

