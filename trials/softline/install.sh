#!/bin/sh
ENVIRONMENT_PATH=".env"
if [ ! -d "$ENVIRONMENT_PATH" ]; then
    echo "Deploying virtual environment in '$ENVIRONMENT_PATH'"
    virtualenv "$ENVIRONMENT_PATH"
    ./"$ENVIRONMENT_PATH"/bin/pip install -r ./requirements.txt
fi
exit 0
