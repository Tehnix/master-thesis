#!/bin/bash

# Build the frontend
stack build --stack-yaml=frontend/stack.yaml

# Copy over the javascript
#rm -f server/static/all.js
#cp $(stack path --stack-yaml=client/stack.yaml --local-install-root)/bin/frontend.jsexe/all.js server/static/all.js

# Build the backend
stack build --stack-yaml=backend/stack.yaml
