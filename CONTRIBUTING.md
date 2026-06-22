# Contributing

Thanks for helping improve Easy NPC. Please keep reports and pull requests focused, reproducible,
and easy to review.

## Bug and Crash Reports

Before opening a bug or crash report, please check for an existing issue and make sure the problem
can be reproduced with a clean, supported setup unless a maintainer asks for a different test case.

Supported report setups are:

- Official Minecraft installation
- CurseForge App
- Modrinth App
- Clean manual installation using official or authorized Minecraft game files

Bug and crash reports should include the Minecraft version, mod version, mod loader, reproduction
steps, and the relevant `logs/latest.log` or `crash-reports/*.txt` file when available.

## Unsupported Environments

We do not provide support for unofficial launchers, cracked clients, account bypass systems,
modified authentication flows, or installations that do not use official or authorized Minecraft
game files.

Reports from these environments may be closed because authentication, skin handling, libraries, game
files, or logs can be modified in ways that make issues impossible to debug reliably.

Bug reports must be reproducible with a legitimate Minecraft installation, a supported mod loader,
and an otherwise clean mod setup unless requested otherwise by a maintainer.

## Before Opening a Pull Request

Small fixes may be submitted directly. This includes typo fixes, small documentation updates, and
obvious one-line bug fixes.

For larger changes, please open or comment on an issue before submitting a pull request.
This applies to new features, refactors, architecture changes, dependency changes, behavior changes,
broad formatting changes, and other changes with a large review surface.

Large pull requests should link the related issue and stay within the agreed scope. Maintainers may
close large unplanned pull requests if the scope was not discussed first.

## Pull Request Expectations

- Keep changes focused on one problem or feature.
- Follow the conventions already used in the touched files.
- Avoid unrelated formatting or cleanup.
- Include tests or manual verification notes when behavior changes.
- Update documentation when user-facing behavior changes.
