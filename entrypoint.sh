#!/bin/bash

set -e

# This container can operate in two different ways:
# * Initialize the Shiny app Git repository.
# * Run the Shiny app.
main() {
  if test "$1" = "git-init"; then
    git_init
  else
    run_shiny_app
  fi
}

git_init() {
  if test $REPO_HTTP_TOKEN; then
    # Store REPO_HTTP_TOKEN in the credential cache to avoid writing token to disk
    # This will only work with Github tokens!
    git config --global credential.helper 'cache --timeout=10000000'
    git credential approve <<EOF
protocol=https
host=github.com
username=$REPO_HTTP_TOKEN
password=x-oauth-basic
EOF
  fi

  FULL_REPO_URI="https://$REPO_HTTP_URI"

  # Attempt a shallow clone to start.  If a commit hash is specified as the repo ref, this will fail and we need to do a full clone instead.
  if ! git clone "$FULL_REPO_URI" --depth=1 --branch "$REPO_REF" "$APP_DIR"; then
    git clone "$FULL_REPO_URI" "$APP_DIR"
    cd "$APP_DIR"
    git checkout "$REPO_REF"
  fi
}

run_shiny_app() {
  export R_LIBS="${R_LIBS:-$LIB_DIR}"
  export GITHUB_PAT="${GITHUB_PAT:-$REPO_HTTP_TOKEN}"

  ABSOLUTE_APP_DIR="$APP_DIR/$REPO_PATH_DIR"
  exec R -e "shiny::runApp(appDir='$ABSOLUTE_APP_DIR', port=3838, host='0.0.0.0')"
}

main "$1"
