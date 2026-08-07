# D:\DEV Codex policy engine

This directory contains the deterministic enforcement shared by repositories
under `D:\DEV`. Repository-specific facts remain in each repository's
`.codex\policy.json`; this directory supplies the reviewed implementation.

The enforcement layers are:

1. Global and repository `AGENTS.md` files define durable engineering
   contracts.
2. Permission profiles restrict writes to the active repository, deny secret
   files, and expose sibling repositories as read-only dependencies.
3. `PreToolUse.ps1` blocks destructive Git operations, recursive deletion,
   forceful process termination, generated-file hand edits, direct generator
   invocation, and cross-repository source writes.
4. `PostToolUse.ps1` records writes and successful verification in a temporary,
   session-scoped state file. It stores hashes and rule names, not raw command
   text.
5. `Stop.ps1` gives an agent one automatic continuation when repository changes
   have not been followed by the configured verification command. A genuine
   failing check can still be reported without creating an infinite loop.

Run `Test-AllPolicies.ps1` after changing the engine or a repository policy.
Changed hooks must be reviewed again with `/hooks` because Codex trusts the
exact hook definition and content hash.

To onboard another repository, copy a neighboring repository's `.codex`
configuration, change its repository name/root, generated paths, sibling roots,
and verification command, then add a repository-owned `AGENTS.md` and
`scripts\codex-verify.ps1`.
