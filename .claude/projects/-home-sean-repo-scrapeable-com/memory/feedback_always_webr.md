---
name: Always run webR Docker build
description: webR packages are the primary deliverable - never skip the Docker build step
type: feedback
---

Always run the webR Docker build for every client. The R package build is just a convenience step — the true goal is creating webR packages. Never mark a client as `done_no_webr` without actually attempting the Docker build first.

**Why:** The entire project exists to produce webR packages. Skipping Docker means the client isn't actually usable for its intended purpose.

**How to apply:** After R CMD build/check/install succeeds, always run the Docker webR build. Only mark `done_no_webr` if Docker genuinely fails after attempting it.
