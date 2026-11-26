#!/usr/bin/env bash
# build isolated agentic-ai-review mamba environment
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if command -v mamba >/dev/null 2>&1; then
  SOLVER="mamba"
elif command -v conda >/dev/null 2>&1; then
  SOLVER="conda"
else
  echo "error: mamba or conda not found on PATH" >&2
  exit 1
fi

if "${SOLVER}" env list | awk '{print $1}' | grep -qx "agentic-ai-review"; then
  echo "info: agentic-ai-review environment already exists; skipping ${SOLVER} env create"
else
  "${SOLVER}" env create -f "${SCRIPT_DIR}/environment.yml" -y
fi

"${SOLVER}" env export -n agentic-ai-review --no-builds > "${SCRIPT_DIR}/environment_frozen.yml"
echo "ok: agentic-ai-review environment ready and exported to env/environment_frozen.yml"
