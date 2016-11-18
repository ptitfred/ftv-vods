# FroggedTV videos analyzer

## Configuration

The matcher consumes YouTube APIs which require an API key. This key is read
from the environment in the key `API_KEY`. Make sure to have it set before
calling the binary.

You could set it in config.sh which is ignored by git.

```bash
# In config.sh (those are fake ids obviously)
export APPLICATION_CODE="ftv-cli"
export APPLICATION_NAME="FTV-CLI YouTube"
export API_KEY="My4PI-k3Y"
export OAUTH_CLIENT_ID="1232451531-lzlkrjezlrkjzelehlkhnlkhlzekhflh.apps.googleusercontent.com"
export OAUTH_CLIENT_SECRET="eoizeujzZLRTJzLETkeREAja"
```

## Build

This is a Haskell project, built with [stack](http://haskellstack.org).

To build:

```bash
stack build
```

Or to install the binaries in your PATH (in ~/.local/bin on Unix platforms):

```bash
stack install
```

To develop:

```bash
stack (build|install) --file-watch
```

## Matching

Let you link YouTube videos to Dota2 wiki from Liquipedia.

Example :

Match the last 100 YouTube VODs

```bash
source ./config.sh

# From the project root directory
stack exec -- ftv-cli match 100

# From everywhere if binaries were installed in the PATH
ftv-cli match 100
```

## Casters listing

Detect famous casters from the FroggedTV in YouTube videos descriptions.

Example :

Detect casters in the last 100 YouTube VODs

```bash
source ./config.sh

# From the project root directory
stack exec -- ftv-cli casters 100

# From everywhere if binaries were installed in the PATH
ftv-cli casters 100
```
