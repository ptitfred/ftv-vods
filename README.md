# FroggedTV videos analyzer

## Build

This is a Haskell project, built with [stack](http://haskellstack.org).

To build:

```bash
stack build
```

## Configuration

The matcher consumes YouTube APIs which require an API key. This key is read
from the environment in the key `API_KEY`. Make sure to have it set before
calling the binary.

You could set it in config.sh which is ignored by git.

```bash
# In config.sh
export API_KEY="My4PI-k3Y"
```

## Matching

Let you link YouTube videos to Dota2 wiki from Liquipedia.

Example :

Match the last 100 YouTube VODs

```bash
# In your shell

# Once
source ./config.sh

# To match VODs to tournaments
stack exec froggedtv-vods-exe match 100
```
