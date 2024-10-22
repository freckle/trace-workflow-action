#!/usr/bin/env bash

set -euo pipefail

docker build --progress=plain -t freckle/trace-workflow-action .
# docker tag freckle/trace-workflow-action:latest public.ecr.aws/d6u9s6h9/freckle/trace-workflow-action:latest
# docker push public.ecr.aws/d6u9s6h9/freckle/trace-workflow-action:latest





gh repo list freckle --visibility public | awk '{ print $1 }'  | while read repo; do gh search code --filename 'Dockerfile' --repo "$repo"; done;

freckle/ajax-js, freckle/react-hooks, freckle/npm-package-template, freckle/resource-status-js, freckle/non-empty-js, freckle/aws-s3-lock-action, freckle/stack-action, freckle/commenter-action, freckle/await-statuses-action, freckle/grep-action, freckle/wiz-action, freckle/typescript-action-template, freckle/halt-action, freckle/freckle-app, freckle/flakes, freckle/stack-cache-action, freckle/wai-middleware-openapi, freckle/setup-platform-action, freckle/action-gh-release, freckle/slack-notify-action, freckle/parser-js, freckle/recurly-client, freckle/haskell-library-template, freckle/maybe-js, freckle/cancelable-promise-js, freckle/query-params-js, freckle/bcp47, freckle/mergeabot-action, freckle/github-workflow-commands, freckle/github-app-token,
