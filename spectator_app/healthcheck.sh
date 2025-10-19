#!/bin/sh
#
# This script is run periodically by the Docker container engine to check if
# the application is healthy. If the application instance is determined to be
# unhealthy then Podman will terminate the container, causing systemd to
# replace it with a new instance.
#
# If this script returns exit code 0 then the check is a pass.
# Any other exit code is a failure, with multiple failures in a row meaning the
# instance is unhealthy.
#
# wget is used to send a HTTP request to the application, to check it is
# serving traffic.
# You may choose to add additional health check logic to this script.
#

exec wget --spider --quiet 'http://127.0.0.1:3000'